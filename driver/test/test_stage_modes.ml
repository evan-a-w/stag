open! Import
open Source_helpers

(* Checks [source] after the test prelude, printing its signature or the
   messages of its errors. *)
let modes ?(const_toplevels = false) source =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let typecheck = Typecheck.create ~stamps ~interpreter ~const_toplevels () in
  let prelude = program ~prelude:true "" in
  match Typecheck.typecheck typecheck ~program:(program source) with
  | Ok checked ->
    print_string
      (Signature.to_string_hum
         (List.drop checked.signature (List.length prelude)))
  | Error { errors; partial = _ } ->
    List.iter errors ~f:(fun error ->
      print_endline (Typecheck_error.message error))
;;

let%expect_test "toplevel constants resolve directly inside quotes" =
  run {|
let n = 3
let code = .<Int.add n 1>.
let ran = .~code
|};
  [%expect
    {|
    val n : int
    val code : <int>{..'a}
    val ran : int
    -- evaluated at compile time
    (code
     (Code
      (App
       (App (Var (Prim Int.add) : "int -> int -> int")
        (Var (Persistent (Toplevel n/4)) : int) : "int -> int")
       (Lit (Int 1) : int) : int)))
    -- evaluated
    (n 3)
    (code
     (Code
      (App
       (App (Var (Prim Int.add) : "int -> int -> int")
        (Var (Persistent (Toplevel n/4)) : int) : "int -> int")
       (Lit (Int 1) : int) : int)))
    (ran 4)
    |}]
;;

let%expect_test "computed toplevels are free variables of quotes" =
  check {|
let f x = x
let n = f 3
let code = .<n>.
|};
  [%expect
    {|
    val f : 'a -> 'a
    val n : int
    val code : <'a>{n : 'a; ..'b}
    |}]
;;

let%expect_test "quote lock: locals are embedded only when portable" =
  check
    {|
let constant = let k = 1 in .<k>.
let closure = let k = 1 in let g u = k in .<g ()>.
let computed = let k = Int.add 1 2 in .<k>.
let argument x = .<x>.
let mutable_ = let mut m = 1 in let g u = m in .<g ()>.
|};
  [%expect
    {|
    val constant : <int>{..'a}
    val closure : <int>{..'a}
    val computed : <'_a>{k : '_a; ..'_b}
    val argument : 'a -> <'b>{x : 'b; ..'c}
    val mutable_ : <'_a>{g : unit -> '_a; ..'_b}
    |}]
;;

let%expect_test "closure lock: a closure is as portable as what it captures" =
  check
    {|
let through_closures =
  let k = Int.add 1 2 in
  let f u = k in
  let g u = f () in
  .<g ()>.
let through_data =
  let pair = (1, fun u -> 2) in
  let g u = match pair with (_, h) -> h () in
  .<g ()>.
|};
  [%expect
    {|
    val through_closures : <'_a>{g : unit -> '_a; ..'_b}
    val through_data : <int>{..'a}
    |}]
;;

let%expect_test "generalization is decided by const" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let nil = Nil
let applied = let f x = x in f Nil
let quoted = .<Nil>.
let matched = match Nil with x -> x
let local = let x = Nil in let y = (x, x) in y
let computed = let x = Int.add 1 2 in Nil
|};
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    val nil : list['a]
    val applied : list['_a]
    val quoted : <list['a]>{..'b}
    val matched : list['a]
    val local : list['a] * list['b]
    val computed : list['_a]
    |}]
;;

let%expect_test "mode annotations on the stage axis" =
  modes
    {|
let ok @ const = 1
let portable_closure @ portable = let k = 1 in fun u -> k
let bad @ const = Int.add 1 2
let bad_local = let x @ portable = (fun u -> u) 1 in x
|};
  [%expect
    {|
    This is nonportable, because it applies a function (at t.stag:4:18-29), but must be at most const, because it is annotated const (at t.stag:4:10-15)
    This is nonportable, because it applies a function (at t.stag:5:35-49), but must be at most portable, because it is annotated portable (at t.stag:5:24-32)
    |}]
;;

let%expect_test "const_toplevels" =
  let source =
    {|
let n = 1
let f x = x
let applied = f 1
let mut counter = 0
let mut computed_counter = f 0
|}
  in
  modes source;
  modes ~const_toplevels:true source;
  [%expect
    {|
    val n : int
    val f : 'a -> 'a
    val applied : int
    val counter : int
    val computed_counter : int
    This is nonportable, because it applies a function (at t.stag:4:14-17), but must be at most const, because applied is a toplevel (at t.stag:4:14-17)
    This is nonportable, because it applies a function (at t.stag:6:27-30), but must be at most const, because computed_counter is a toplevel (at t.stag:6:27-30)
    |}]
;;

let%expect_test "initialization cycles through non-functions are reported" =
  modes {|
let x = f 1
let f y = x
|};
  [%expect {| The value of x may be needed to compute itself: x -> f -> x |}]
;;
