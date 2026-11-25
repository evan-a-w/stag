open! Import
open Source_helpers

let run ?(runtime = limits) ~show source =
  match Driver.check ~limits (program source) with
  | Error error ->
    List.iter (error : Typecheck.Failed.t).errors ~f:(fun error ->
      print_s [%sexp (error : Typecheck_error.t)])
  | Ok driver ->
    (match Driver.eval ~limits:runtime driver with
     | Error error -> print_s [%sexp "eval failed", (error : Error.t)]
     | Ok () ->
       List.iter show ~f:(fun name ->
         let { Typecheck.Checked.program; _ } = Driver.checked driver in
         let ident =
           List.find_map_exn program.sccs ~f:(fun scc ->
             List.find_map scc.members ~f:(fun { ident; _ } ->
               Option.some_if (String.equal ident.name name) ident))
         in
         print_s
           [%sexp
             (name : string)
             , (Interpreter.find (Driver.interpreter driver) ~ident |> ok_exn
                : Interpreter.Value.t)]))
;;

let%expect_test "closures share mutable slots with their scope" =
  run
    ~show:
      [ "seen_after"
      ; "set_through_closure"
      ; "counters"
      ; "shared_counter"
      ; "captured_param"
      ]
    {|
let seen_after =
  let mut x = 1 in
  let get _ = x in
  x <- 2;
  get ()
let set_through_closure =
  let mut x = 1 in
  let set v = x <- v in
  set 5;
  x
let make_counter _ =
  let mut n = 0 in
  fun _ -> n <- Int.add n 1; n
let counters =
  let a = make_counter () in
  let b = make_counter () in
  let _ = a () in
  let _ = a () in
  (a (), b ())
let shared_counter =
  let mut n = 0 in
  let incr _ = n <- Int.add n 1 in
  let get _ = n in
  incr (); incr (); incr ();
  get ()
let captured_param =
  let f x =
    let mut y = x in
    let bump _ = y <- Int.add y x in
    bump (); bump ();
    y
  in
  (f 1, f 10)
|};
  [%expect
    {|
    (seen_after 2)
    (set_through_closure 5)
    (counters (Tuple 3 1))
    (shared_counter 3)
    (captured_param (Tuple 3 30))
    |}]
;;

let%expect_test "partial applications are independent" =
  run
    ~show:[ "partials"; "prim_partials"; "nested"; "rebound" ]
    {|
let add3 a b c = Int.add a (Int.add b c)
let partials =
  let f = add3 1 in
  let g = f 10 in
  (g 100, g 200, f 20 300, add3 2 3 4)
let prim_partials =
  let inc = Int.add 1 in
  let sep = String.concat ", " in
  (inc 1, inc 41, sep ["a"; "b"], sep [])
let nested =
  let outer a =
    let inner b = fun c -> (a, b, c) in
    inner
  in
  let i = outer 1 in
  (i 2 3, i 4 5, outer 6 7 8)
let rebound =
  let f x =
    let mut acc = x in
    fun y -> acc <- Int.add acc y; acc
  in
  let a = f 0 in
  let b = f 100 in
  (a 1, a 1, b 1, a 1)
|};
  [%expect
    {|
    (partials (Tuple 111 211 321 9))
    (prim_partials (Tuple 2 42 (String "a, b") (String "")))
    (nested (Tuple (Tuple 1 2 3) (Tuple 1 4 5) (Tuple 6 7 8)))
    (rebound (Tuple 1 2 101 3))
    |}]
;;

let%expect_test "recursive locals" =
  run
    ~show:[ "parities"; "fact" ]
    {|
let even_odd n =
  let parity n = match Int.equal n 0 with true -> true | false -> match parity (Int.sub n 1) with true -> false | false -> true in
  let flip b = match b with true -> false | false -> true in
  (parity n, flip (parity n))
let fact =
  let go n = match Int.equal n 0 with true -> 1 | false -> Int.mul n (go (Int.sub n 1)) in
  (go 0, go 5, go 10)
let parities = (even_odd 10, even_odd 7)
|};
  [%expect
    {|
    (parities (Tuple (Tuple true false) (Tuple false true)))
    (fact (Tuple 1 120 3628800))
    |}]
;;

let loop =
  {|
let loop n acc =
  match Int.equal n 0 with
  | true -> acc
  | false ->
    let next = Int.sub n 1 in
    let mut acc = acc in
    acc <- Int.add acc 1;
    (fun x -> x) (loop next acc)
let tail n acc =
  match Int.equal n 0 with
  | true -> acc
  | false -> let m = Int.sub n 1 in tail m (Int.add acc 1)
|}
;;

let%expect_test "tail calls run in constant depth" =
  let deep = { limits with max_depth = Some 100 } in
  run
    ~runtime:{ deep with fuel = Some 100_000_000 }
    ~show:[ "x" ]
    (loop ^ "let x = tail 1000000 0\n");
  run
    ~runtime:{ deep with fuel = Some 1_000 }
    ~show:[ "x" ]
    (loop ^ "let x = tail 1000000 0\n");
  run ~runtime:deep ~show:[ "x" ] (loop ^ "let x = loop 1000 0\n");
  [%expect
    {|
    (x 1000000)
    ("eval failed"
     ("Out of fuel: evaluation took more than 1000 steps" @ t.stag:13:21-30))
    ("eval failed"
     ("Stack depth exceeded: evaluation nested more than 100 steps deep" @
      t.stag:8:11-18))
    |}]
;;

let%expect_test "a toplevel already evaluated can't be queued again" =
  Option.iter (typecheck "let x = 1\nlet y = x\n") ~f:(fun t ->
    let driver = driver t in
    print_s [%sexp (Driver.eval ~limits driver : unit Or_error.t)];
    let interpreter = Driver.interpreter driver in
    List.iter (members t) ~f:(fun member ->
      print_s
        [%sexp
          (Or_error.try_with (fun () ->
             Interpreter.add_scc interpreter { members = [ member ] })
           : unit Or_error.t)]));
  [%expect
    {|
    (Ok ())
    (Error ("Treewalk.add_scc: toplevel already evaluated" (ident x/4)))
    (Error ("Treewalk.add_scc: toplevel already evaluated" (ident y/5)))
    |}]
;;

let%expect_test "toplevels are evaluated in program order, those used before \
                 their turn on demand"
  =
  run
    ~show:[]
    {|
let a = print_endline "a"; b
let b = print_endline "b"
let mut c = print_endline "c"
module M = struct
  let d = print_endline "d"; e
end
let e = print_endline "e"
let f = print_endline "f"
|};
  [%expect {|
    a
    b
    c
    d
    e
    f
    |}]
;;

let%expect_test "prelude toplevels are evaluated on demand, but in order when \
                 the prelude is the program"
  =
  let prelude =
    force Prelude.toplevels
    @ parse_exn
        ~file:"extra.stag"
        {|
let p1 = print_endline "p1"
let p2 = print_endline "p2"; p1
let p3 = print_endline "p3"
|}
  in
  let eval = function
    | Error error ->
      print_s
        [%sexp ((error : Typecheck.Failed.t).errors : Typecheck_error.t list)]
    | Ok driver ->
      print_s [%sexp (Driver.eval ~limits driver : unit Or_error.t)]
  in
  eval
    (Driver.check
       ~limits
       ~prelude
       (parse_exn {|
let a = print_endline "a"
let b = p2
|}));
  [%expect {|
    a
    p2
    p1
    (Ok ())
    |}];
  eval (Driver.check_prelude ~limits prelude);
  [%expect {|
    p1
    p2
    p3
    (Ok ())
    |}]
;;
