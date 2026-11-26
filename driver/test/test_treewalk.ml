open! Import
open Source_helpers

(* The captures of each closure in [source]: its toplevel functions and the
   local functions within them. *)
let print source =
  Option.iter (typecheck source) ~f:(fun t ->
    let interpreter = Treewalk.of_interpreter (Driver.interpreter (driver t)) in
    let print_captures ~ident ~args ~body =
      let captures =
        Treewalk.captures interpreter ~ident ~args ~body
        |> Hash_set.to_list
        |> List.sort ~compare:Typed.Ident.compare
      in
      print_s [%sexp (ident : Typed.Ident.t), (captures : Typed.Ident.t list)]
    in
    let rec locals (expr : Typed.Expr.t) =
      (match expr.desc with
       | Let { ident; args = _ :: _ as args; body; _ } ->
         print_captures ~ident ~args ~body
       | _ -> ());
      List.iter (Typed.Expr.sub_exprs expr) ~f:locals
    in
    List.iter (members t) ~f:(fun (member : Typed.Toplevel.t) ->
      (match member.kind with
       | Function args ->
         print_captures
           ~ident:member.ident
           ~args:(Nonempty_list.to_list args)
           ~body:member.body
       | Value | Mut -> ());
      locals member.body))
;;

let%expect_test "arguments, locals and toplevels" =
  print
    {|
let k = 1
let g a b =
  let f x = add x a in
  let unused = b in
  let h y = add (f y) k in
  0
|};
  [%expect
    {|
    (g/5 (add/0 k/4))
    (f/17 (a/14 add/0))
    (h/20 (add/0 f/17 k/4))
    |}]
;;

let%expect_test "recursion" =
  print
    {|
let loop x = loop x
let even x = odd x
let odd x = even x
let local n = let go x = go n in go
|};
  [%expect
    {|
    (loop/4 (loop/4))
    (even/5 (odd/6))
    (odd/6 (even/5))
    (local/7 ())
    (go/21 (go/21 n/19))
    |}]
;;

let%expect_test "pattern binders are not captures" =
  print
    {|
let f p c =
  let g q = match q with (x, _) -> add x c in
  let h r = match r with {`a = a; ..rest} -> (a, rest, p) in
  let v w = match w with `A z -> z | other -> c in
  0
|};
  [%expect
    {|
    (f/4 (add/0))
    (g/16 (add/0 c/14))
    (h/19 (p/13))
    (v/23 (c/14))
    |}]
;;

let%expect_test "poly records and variants" =
  print
    {|
let f r s x =
  let g u = ({`a = x | r}, {s with `b = u}, r \ `c, s.`d, {`e = x}, `T x) in
  0
|};
  [%expect {|
    (f/4 ())
    (g/17 (r/13 s/14 x/15))
    |}]
;;

let%expect_test "quotes and splices" =
  print
    {|
let q c = .<let inner z = add z .~c in 0>.
let h u = .<let w = 1 in .~u>.
let build c = let mk x = forget x in .<add .~c x>. in 0
let persistent n = let k y = y in .<let g x = k x in g 1>.
let explicit c d = let e x = .<.~c.{y = .~d}>. in 0
|};
  [%expect
    {|
    (q/4 ())
    (inner/19 (add/0 inner/19))
    (h/5 ())
    (build/6 ())
    (mk/24 (c/22))
    (persistent/7 ())
    (k/27 ())
    (g/29 (k/27))
    (explicit/8 ())
    (e/33 (c/30 d/31))
    |}]
;;

let%expect_test "toplevel splices" =
  print {|
let n = 3
let f x = let g y = .~(.<add x y>.) in 0
|};
  [%expect {|
    (f/5 (add/0))
    (g/16 (add/0 x/14))
    |}]
;;

let%expect_test "env slots" =
  let stamps = Stamp_counter.create () in
  let f = Typed.Ident.fresh stamps "f" in
  let x = Typed.Ident.fresh stamps "x" in
  let unbound = Typed.Ident.fresh stamps "unbound" in
  let env = Treewalk.Env.create () in
  let show env ident =
    print_s [%sexp (Treewalk.Env.lookup env ~ident : int Or_error.t)]
  in
  show env unbound;
  Treewalk.Env.declare env ~ident:f;
  Treewalk.Env.set env ~ident:x ~value:1;
  show env f;
  show env x;
  let closure =
    Treewalk.Env.of_captures
      env
      ~fallback:(Treewalk.Env.create ())
      ~captures:(Typed.Ident.Hash_set.of_list [ f ])
    |> Or_error.ok_exn
  in
  Treewalk.Env.set env ~ident:f ~value:2;
  show closure f;
  show closure x;
  print_s
    [%sexp
      (Treewalk.Env.of_captures
         env
         ~fallback:(Treewalk.Env.create ())
         ~captures:(Typed.Ident.Hash_set.of_list [ unbound ])
       |> Or_error.map ~f:(fun _ -> ())
       : unit Or_error.t)];
  [%expect
    {|
    (Error ("Unbound ident" (ident unbound/2)))
    (Error
     ("Used value before it was defined (eg. because of recursion on values)"
      (ident f/0)))
    (Ok 1)
    (Ok 2)
    (Error ("Unbound ident" (ident x/1)))
    (Error ("Captured ident not in scope" (ident unbound/2)))
    |}]
;;

let%expect_test "extended envs" =
  let stamps = Stamp_counter.create () in
  let ident name = Typed.Ident.fresh stamps name in
  let captured = ident "captured" in
  let param = ident "param" in
  let local = ident "local" in
  let toplevel = ident "toplevel" in
  let fallback = Treewalk.Env.create () in
  Treewalk.Env.set fallback ~ident:toplevel ~value:0;
  let closure = Treewalk.Env.create () in
  Treewalk.Env.set closure ~ident:captured ~value:1;
  let first = Treewalk.Env.extend closure in
  let second = Treewalk.Env.extend closure in
  Treewalk.Env.set first ~ident:param ~value:2;
  Treewalk.Env.set second ~ident:param ~value:3;
  Treewalk.Env.declare first ~ident:local;
  Treewalk.Env.set first ~ident:captured ~value:4;
  let show name env =
    print_s
      [%sexp
        (name : string)
        , (List.map [ captured; param; local; toplevel ] ~f:(fun ident ->
             ( ident
             , Treewalk.Env.mem env ident
             , Treewalk.Env.lookup env ~fallback ~ident
               |> Result.map_error ~f:(fun _ -> ()) ))
           : (Typed.Ident.t * bool * (int, unit) Result.t) list)]
  in
  show "closure" closure;
  show "first" first;
  show "second" second;
  [%expect
    {|
    (closure
     ((captured/0 true (Ok 4)) (param/1 false (Error ()))
      (local/2 false (Error ())) (toplevel/3 false (Ok 0))))
    (first
     ((captured/0 true (Ok 4)) (param/1 true (Ok 2)) (local/2 true (Error ()))
      (toplevel/3 false (Ok 0))))
    (second
     ((captured/0 true (Ok 4)) (param/1 true (Ok 3)) (local/2 false (Error ()))
      (toplevel/3 false (Ok 0))))
    |}]
;;

let%expect_test "binding in an extended env raises" =
  let stamps = Stamp_counter.create () in
  let ident name = Typed.Ident.fresh stamps name in
  let bound = ident "bound" in
  let later = ident "later" in
  let env = Treewalk.Env.create () in
  Treewalk.Env.set env ~ident:bound ~value:0;
  let extended = Treewalk.Env.extend env in
  let attempt name f =
    print_s [%sexp (name : string), (Or_error.try_with f : unit Or_error.t)]
  in
  attempt "set bound" (fun () -> Treewalk.Env.set env ~ident:bound ~value:1);
  attempt "set later" (fun () -> Treewalk.Env.set env ~ident:later ~value:2);
  attempt "declare later" (fun () -> Treewalk.Env.declare env ~ident:later);
  attempt "set later in extension" (fun () ->
    Treewalk.Env.set extended ~ident:later ~value:3);
  print_s
    [%sexp
      (Treewalk.Env.lookup extended ~ident:bound : int Or_error.t)
      , (Treewalk.Env.lookup env ~ident:later : int Or_error.t)];
  [%expect
    {|
    ("set bound" (Ok ()))
    ("set later"
     (Error ("Env: binding in an env already extended" (ident later/1))))
    ("declare later"
     (Error ("Env: binding in an env already extended" (ident later/1))))
    ("set later in extension" (Ok ()))
    ((Ok 1) (Error ("Unbound ident" (ident later/1))))
    |}]
;;

let%expect_test "a shared stamp counter" =
  let stamps = Stamp_counter.create () in
  let interpreter = Treewalk.create ~stamps () in
  let idents program =
    match
      Typecheck.typecheck
        (Typecheck.create
           ~stamps
           ~interpreter:(Treewalk.to_interpreter interpreter)
           ())
        ~program
    with
    | Error error ->
      raise_s
        [%sexp ((error : Typecheck.Failed.t).errors : Typecheck_error.t list)]
    | Ok { program; _ } -> Map.keys program.ident_info
  in
  print_s [%sexp (idents (parse_exn "let f x = x") : Typed.Ident.t list)];
  print_s [%sexp (idents (parse_exn "let g y = y") : Typed.Ident.t list)];
  print_s [%sexp (Typed.Ident.fresh stamps "minted" : Typed.Ident.t)];
  [%expect {|
    (f/0 x/1)
    (g/2 y/3)
    minted/4
    |}]
;;

let%expect_test "with_output restores the previous output" =
  let eval_printing_to ~f =
    let t = Option.value_exn (typecheck {|let x = print_string "hello"|}) in
    let interpreter = Treewalk.of_interpreter (Driver.interpreter (driver t)) in
    let buffers = String.Table.create () in
    let to_ name =
      Buffer.add_string
        (Hashtbl.find_or_add buffers name ~default:(fun () -> Buffer.create 16))
    in
    Treewalk.set_output interpreter (to_ "outer");
    f interpreter ~to_ ~eval:(fun () -> ok_exn (Driver.eval (driver t)));
    print_s
      [%sexp (Hashtbl.map buffers ~f:Buffer.contents : string String.Table.t)]
  in
  eval_printing_to ~f:(fun interpreter ~to_ ~eval ->
    Treewalk.with_output interpreter (to_ "inner") ~f:(fun () ->
      Treewalk.with_output interpreter (to_ "innermost") ~f:Fn.id;
      eval ()));
  [%expect {| ((inner hello) (innermost "") (outer "")) |}];
  eval_printing_to ~f:(fun interpreter ~to_ ~eval ->
    (match
       Treewalk.with_output interpreter (to_ "raising") ~f:(fun () ->
         raise_s [%message "boom"])
     with
     | () -> ()
     | exception exn -> print_s [%sexp (exn : exn)]);
    eval ());
  [%expect {|
    boom
    ((outer hello) (raising ""))
    |}]
;;

let%expect_test "a toplevel whose evaluation an exception abandons is \
                 evaluated again when next used"
  =
  let t =
    Option.value_exn
      (typecheck
         {|
let head ty =
  match Stag.Type.expr ty with
  | Stag.Type_expr.Con (Stag.Path.Ref r, _) -> r
let x = print_endline "evaluating x"; Stag.Type.decl (head (type int))
let y = x
|})
  in
  let interpreter = Driver.interpreter (driver t) in
  let ident name =
    List.find_map_exn (members t) ~f:(fun { ident; _ } ->
      Option.some_if (String.equal ident.name name) ident)
  in
  let y = ident "y" in
  let use_y =
    { Typed.Expr.desc = Var (Bound (Toplevel y))
    ; ty = Type.error
    ; loc = Loc.none
    ; made = Typed.Expr.Made.none
    }
  in
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         Interpreter.run interpreter use_y ~level:0 ~reflect:(fun _ ->
           raise_s [%message "abandoned"]))
       : Interpreter.Value.t Or_error.t Or_error.t)];
  print_s
    [%sexp
      (Interpreter.eval_toplevel interpreter ~ident:(ident "x")
       : unit Or_error.t)];
  print_s
    [%sexp (Interpreter.eval_toplevel interpreter ~ident:y : unit Or_error.t)];
  [%expect
    {|
    evaluating x
    (Error abandoned)
    evaluating x
    (Error ("Stag.Type.decl is only available at compile time" @ t.stag:5:38-70))
    (Error ("Stag.Type.decl is only available at compile time" @ t.stag:5:38-70))
    |}]
;;

let%expect_test "an expect test run within another leaves the outer one running"
  =
  let contents =
    {x|
let%expect_test "t" = [%expect {|first|}]; [%expect {|second|}]
|x}
  in
  match Driver.check_files [ { Source_file.path = "t.stag"; contents } ] with
  | Error errors -> print_s [%sexp (errors : Driver.Error.t list)]
  | Ok driver ->
    Driver.eval driver |> ok_exn;
    let interpreter = Driver.interpreter driver in
    let test = List.hd_exn (Driver.checked driver).program.expect_tests in
    let nested = ref false in
    let rec run name =
      Interpreter.run_expect_test
        interpreter
        test
        ~on_expect:(fun (expected : string Located.t) ->
          printf "%s: %s\n" name expected.txt;
          if not !nested
          then (
            nested := true;
            print_s [%sexp (run "inner" : unit Or_error.t)]))
    in
    print_s [%sexp (run "outer" : unit Or_error.t)];
    [%expect
      {|
      outer: first
      inner: first
      inner: second
      (Ok ())
      outer: second
      (Ok ())
      |}]
;;
