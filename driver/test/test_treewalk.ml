open! Import
open Source_helpers

(* The captures of each closure in [source]: its toplevel functions and the
   local functions within them. *)
let print source =
  Option.iter (typecheck source) ~f:(fun t ->
    let interpreter = Driver.interpreter (driver t) in
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
      if not (List.is_empty member.args)
      then
        print_captures ~ident:member.ident ~args:member.args ~body:member.body;
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
    (f/18 (a/15 add/0))
    (h/21 (add/0 f/18 k/4))
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
    (go/22 (go/22 n/20))
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
    (g/17 (add/0 c/15))
    (h/20 (p/14))
    (v/24 (c/15))
    |}]
;;

let%expect_test "poly records and variants" =
  print
    {|
let f r s x =
  let g u = ({`a = x | r}, {s with `b = u}, r \ `c, s.`d, {`e = x}, `T x) in
  0
|};
  [%expect
    {|
    (f/4 ())
    (g/18 (r/14 s/15 x/16))
    |}]
;;

let%expect_test "quotes and splices" =
  print
    {|
let q c = .<let inner z = add z .~c in 0>.
let h u = .<let w = 1 in .~u>.
let build c = let mk x = .<add .~c x>. in 0
let persistent n = let k y = y in .<let g x = k x in g 1>.
let explicit c d = let e x = .<.~c.{y = .~d}>. in 0
|};
  [%expect
    {|
    (q/4 (add/0))
    (inner/20 (add/0 inner/20))
    (h/5 ())
    (build/6 (add/0))
    (mk/25 (add/0 c/23))
    (persistent/7 ())
    (k/28 ())
    (g/30 (k/28))
    (explicit/8 ())
    (e/34 (c/31 d/32))
    |}]
;;

let%expect_test "toplevel splices" =
  print
    {|
let n = 3
let f x = let g y = .~(.<add x y>.) in 0
|};
  [%expect
    {|
    (f/5 (add/0))
    (g/17 (add/0 x/15))
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
    Treewalk.Env.of_captures env ~captures:(Typed.Ident.Hash_set.of_list [ f ])
    |> Or_error.ok_exn
  in
  Treewalk.Env.set env ~ident:f ~value:2;
  show closure f;
  show closure x;
  print_s
    [%sexp
      (Treewalk.Env.of_captures
         env
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

let%expect_test "a shared stamp counter" =
  let stamps = Stamp_counter.create () in
  let interpreter = Treewalk.create ~stamps () in
  let idents program =
    match
      Typecheck.typecheck (Typecheck.create ~stamps ~interpreter ()) ~program
    with
    | Error error ->
      raise_s
        [%sexp ((error : Typecheck.Failed.t).errors : Typecheck_error.t list)]
    | Ok { program; _ } -> Map.keys program.ident_info
  in
  print_s [%sexp (idents (parse_exn "let f x = x") : Typed.Ident.t list)];
  print_s [%sexp (idents (parse_exn "let g y = y") : Typed.Ident.t list)];
  print_s [%sexp (Typed.Ident.fresh stamps "minted" : Typed.Ident.t)];
  [%expect
    {|
    (f/0 x/1)
    (g/2 y/3)
    minted/4
    |}]
;;
