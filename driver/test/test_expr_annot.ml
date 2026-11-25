open! Import

let show source =
  match Mode_helpers.signature source with
  | Ok s -> print_string s
  | Error errors -> List.iter errors ~f:print_endline
;;

let%expect_test "annotations on any expression" =
  show
    {|
let ext (x : string) = let y @ external = x in y
let a = ("s" @ static)
let b = (Int.add 1 2 : int)
let c u = (ext "s" @ external)
let d u = (String.append "a" "b" : string @ gc)
let e (s : string @ local) = (String.length s : int @ global)
let f x = ((x : int) : int)
let g u = (fun (x : int) -> x : int -> int)
|};
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val a : string
    val b : int
    val c : 'a @ 'm0 -> string @ 'm1 with external <= 'm1
    val d : 'a @ 'm0 -> string @ 'm1 with nonportable gc <= 'm1
    val e : string @ 'm0 -> int @ 'm1 with local <= 'm0
    val f : int @ 'm0 -> int @ 'm1
    val g : 'a @ 'm0 -> (int -> int) @ 'm1
    |}];
  show
    {|
let ext (x : string) = let y @ external = x in y
let bad u = (ext "s" @ gc)
|};
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:3:13-20), but must be at most gc, because it is annotated gc (at t.stag:3:23-25) |}];
  show {|
let bad = (Int.to_string 1 : string @ const)
|};
  [%expect
    {| This is nonportable, because a function type fixes it, as the function types are unified, then the function returns it (at t.stag:2:11-26), but must be at most const, because it is annotated const (at t.stag:2:38-43) |}];
  show {|
let bad = (1 : string)
|};
  [%expect
    {| This expression has type string but an expression was expected of type int |}]
;;

let%expect_test "an annotation only checks" =
  show
    {|
let one = 1
let n = (one @ portable)
let t (u : unit) = exclave_ (stack_ (Ptr.make 1) @ local)
|};
  [%expect
    {|
    val one : int
    val n : int
    val t : unit @ 'm0 -> ptr[int @@ 'm2] @ 'm1 with nonportable gc local <= 'm1
    |}];
  show {|
let s u = (stack_ (Ptr.make 1) @ local)
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:2:11-30), then it is part of the value (at t.stag:2:11-30), then it is used (at t.stag:2:10-39), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:10-39) |}]
;;

let%expect_test "in quotes, checked again where spliced" =
  show
    {|
let q = .< (Int.add 1 2 : int) >.
let r = .< fun (s : string @ local) -> (String.length s @ global) >.
let s = .~(.< (Int.add 1 2 : int) >.)
let erased = Stag.Code.erase .< (Int.add 1 2 : int @ portable) >.
|};
  [%expect
    {|
    val q : <int>{..'a}
    val r : <string @ 'm0 -> int @ 'm1>{..'a} with local <= 'm0
    val s : int
    val erased : Stag.Expr.t
    |}]
;;
