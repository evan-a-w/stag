open! Import
open Source_helpers

let modes source =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let typecheck = Typecheck.create ~stamps ~interpreter () in
  let prelude = program ~prelude:true "" in
  match Typecheck.typecheck typecheck ~program:(program source) with
  | Ok checked ->
    print_string
      (Signature.to_string_hum
         ~modes:true
         (List.drop checked.signature (List.length prelude)))
  | Error { errors; partial = _ } ->
    List.iter errors ~f:(fun error ->
      print_endline (Typecheck_error.message error))
;;

let%expect_test "arrow modes are polymorphic" =
  modes
    {|
let ident x = x
let apply f x = f x
let const_ x y = x
let n = ident 3
let code = .<n>.
let m = Int.add 1 2
let code2 = .<m>.
|};
  [%expect
    {|
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1
    val apply : ('a @ 'm2 -> 'b @ 'm3) @ 'm0 -> ('a @ 'm5 -> 'b @ 'm4) @ 'm1 with 'm0 <= 'm1 and 'm3 <= 'm4 and 'm5 <= 'm2
    val const_ : 'a @ 'm0 -> ('b @ 'm3 -> 'a @ 'm1) @ 'm2 with 'm0 <= 'm1 and 'm0 <= 'm2
    val n : int
    val code : <int>{..'a}
    val m : int
    val code2 : <'a>{m : 'a; ..'b}
    |}]
;;

let%expect_test "instances are independent" =
  modes
    {|
let ident x = x
let a = ident 1
let b = ident (Int.add 1 2)
let qa = .<a>.
let qb = .<b>.
|};
  [%expect
    {|
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1
    val a : int
    val b : int
    val qa : <int>{..'a}
    val qb : <'a>{b : 'a; ..'b}
    |}]
;;

let%expect_test "annotations" =
  modes
    {|
let f (x @ portable) = .<x>.
let g : 'a @ 'm -> 'a @ 'm = fun x -> x
let h (x @ 'm) : int @ 'm = x
type t = { k : int @ portable -> int }

|};
  [%expect
    {|
    val f : 'a @ 'm0 -> <'a>{..'b} @ 'm1 with 'm0 <= 'm1
    val g : 'a @ 'm0 -> 'a @ 'm0
    val h : int @ 'm0 -> int @ 'm1 with 'm0 <= 'm1 and 'm1 <= 'm0
    type t = { k : int @ portable -> int }
    |}]
;;

let%expect_test "parameters used in quotes are free variables" =
  modes {|
let f x = .<x>.
let g x = let h u = x in .<h>.
|};
  [%expect
    {|
    val f : 'a @ 'm0 -> <'b>{x : 'b; ..'c} @ 'm1
    val g : 'a @ 'm0 -> <'b>{h : 'b; ..'c} @ 'm1 with 'm0 <= 'm1
    |}]
;;

let%expect_test "an argument annotated portable must be" =
  modes {|
let f (x @ portable) = .<x>.
let bad = f (Int.add 1 2)
|};
  [%expect
    {| This is nonportable, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:3:12-25), then it is passed to a function (at t.stag:3:10-25), but must be at most portable, because it is annotated portable (at t.stag:2:11-19), then the function types are unified |}]
;;
