open! Import
open Source_helpers

let locality source =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let typecheck = Typecheck.create ~stamps ~interpreter () in
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

let%expect_test "a local value can't be returned" =
  locality {|
let f u = let p = local_ (1, 2) in p
|};
  [%expect
    {| This is local, because it is allocated by [local_] (at t.stag:2:18-31), then it is part of the value (at t.stag:2:18-31), then p is used (at t.stag:2:35-36), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:10-36) |}];
  locality
    {|
let f u = exclave_ local_ (1, 2)
let g u = match f () with (a, _) -> a
|};
  [%expect {|
    val f : 'a -> int * int
    val g : 'a -> int
    |}];
  locality {|
let f u = let x = exclave_ (1, 2) in x
|};
  [%expect
    {| [exclave_] ends the function's region, so must be the function's result, in tail position |}]
;;

let%expect_test "toplevels are global" =
  locality {|
let x = local_ (1, 2)
|};
  [%expect
    {| This is local, because it is allocated by [local_] (at t.stag:2:8-21), then it is part of the value (at t.stag:2:8-21), but must be at most global, because x is a toplevel (at t.stag:2:8-21) |}];
  locality
    {|
let x = let p = local_ (1, 2) in match p with (a, b) -> Int.add a b
|};
  [%expect {| val x : int |}]
;;

let%expect_test "plain data crosses locality" =
  locality
    {|
let f u = let p = local_ ((1, 2), 3) in match p with (a, _) -> a
|};
  [%expect
    {| This is local, because it is allocated by [local_] (at t.stag:2:18-36), then it is part of the value (at t.stag:2:18-36), then p is used (at t.stag:2:46-47), then a is used (at t.stag:2:63-64), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:10-64) |}];
  locality
    {|
let f u = let p = local_ ((1, 2), 3) in match p with (_, b) -> b
|};
  [%expect {| val f : 'a -> int |}]
;;

let%expect_test "a closure capturing a local value is local" =
  locality {|
let f u = let p = local_ (1, 2) in let g v = p in g
|};
  [%expect
    {| This is local, because it is allocated by [local_] (at t.stag:2:18-31), then it is part of the value (at t.stag:2:18-31), then p is captured by a closure (at t.stag:2:45-46), then g is used (at t.stag:2:50-51), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:10-51) |}];
  locality
    {|
let f u =
  let p = local_ ((1, 2), 3) in
  let g v = match p with (_, b) -> b in
  g ()
|};
  [%expect
    {| This is local, because it is allocated by [local_] (at t.stag:3:10-28), then it is part of the value (at t.stag:3:10-28), then p is captured by a closure (at t.stag:4:18-19), then g is used (at t.stag:5:2-3), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:5:2-3) |}];
  locality
    {|
let f u =
  let p = local_ ((1, 2), 3) in
  let g v = match p with (_, b) -> b in
  g () [@nontail]
|};
  [%expect {| val f : 'a -> int |}]
;;

let%expect_test "a tail call can't be passed what is local to the region" =
  locality
    {|
let use (p @ local) = match p with (a, _) -> a
let caller u = use (local_ (1, 2))
|};
  [%expect
    {| This is local, because it is allocated by [local_] (at t.stag:3:19-34), then it is part of the value (at t.stag:3:19-34), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:3:19-34) |}];
  locality
    {|
let use (p @ local) = match p with (a, _) -> a
let caller u = use (local_ (1, 2)) [@nontail]
let pass (p : (int * int) @ local) = use p
let in_exclave u = exclave_ use (local_ (1, 2))
|};
  [%expect
    {|
    val use : 'a * 'b -> 'a
    val caller : 'a -> int
    val pass : int * int -> int
    val in_exclave : 'a -> int
    |}]
;;

let%expect_test "parameters" =
  locality
    {|
let keep (x @ global) = Ptr.make x
let f u = keep (local_ (1, 2)) [@nontail]
|};
  [%expect
    {| This is local, because it is allocated by [local_] (at t.stag:3:15-30), then it is part of the value (at t.stag:3:15-30), then it is passed to a function (at t.stag:3:10-30), but must be at most global, because it is annotated global (at t.stag:2:14-20), then the function types are unified |}];
  locality {|
let escape (x @ local) = Ptr.make x
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:2:16-21), then x is passed to the function (at t.stag:2:25-35), then x is used (at t.stag:2:34-35), then it is passed to a function (at t.stag:2:25-35), but must be at most global, because a function type fixes it, then the function types are unified |}];
  locality
    {|
let same x = x
let f u = let p = same (local_ (1, 2)) in Ptr.make p
let g u = let p = same (1, 2) in Ptr.make p
|};
  [%expect
    {|
    This is local, because it is allocated by [local_] (at t.stag:3:23-38), then it is part of the value (at t.stag:3:23-38), then it is passed to a function (at t.stag:3:18-38), then the function types are unified, then x is passed to the function (at t.stag:2:13-14), then x is used (at t.stag:2:13-14), then the function returns it (at t.stag:2:13-14), then the function types are unified, then the function returns it (at t.stag:3:18-38), then p is used (at t.stag:3:51-52), then it is passed to a function (at t.stag:3:42-52), but must be at most global, because a function type fixes it, then the function types are unified
    This is local, because it is allocated by [local_] (at t.stag:3:23-38), then it is part of the value (at t.stag:3:23-38), then it is passed to a function (at t.stag:3:18-38), then the function types are unified, then x is passed to the function (at t.stag:2:13-14), then x is used (at t.stag:2:13-14), then the function returns it (at t.stag:2:13-14), then the function types are unified, then the function returns it (at t.stag:3:18-38), then p is used (at t.stag:3:51-52), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:3:51-52)
    |}]
;;

let%expect_test "stores through pointers are global" =
  locality {|
let f (p : ptr[int * int]) = p.* <- local_ (1, 2)
|};
  [%expect
    {| This is local, because it is allocated by [local_] (at t.stag:2:36-49), then it is part of the value (at t.stag:2:36-49), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:2:36-49) |}];
  locality
    {|
let f (p : ptr[(int * int) * int]) =
  let q = local_ ((1, 2), 3) in
  match p.* with (a, _) -> a
|};
  [%expect {| val f : ptr[(int * int) * int] -> int * int |}]
;;

let%expect_test "global_ fields" =
  locality
    {|
type r = { g : global_ int * int; l : int * int }
let get (r @ local) = Ptr.make r.g
let get_l (r @ local) = Ptr.make r.l
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:4:15-20), then r is passed to the function (at t.stag:4:24-36), then r is used (at t.stag:4:33-34), then its field is read (at t.stag:4:33-36), then it is passed to a function (at t.stag:4:24-36), but must be at most global, because a function type fixes it, then the function types are unified |}];
  locality
    {|
type r = { g : global_ int * int; l : int * int }
let mk u = local_ { g = local_ (1, 2); l = (3, 4) }
|};
  [%expect
    {|
    This is local, because it is allocated by [local_] (at t.stag:3:11-51), then it is part of the value (at t.stag:3:11-51), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:3:11-51)
    This is local, because it is allocated by [local_] (at t.stag:3:24-37), then it is part of the value (at t.stag:3:24-37), but must be at most global, because it is held by a [global_] field (at t.stag:3:24-37)
    |}]
;;

let%expect_test "let mut" =
  locality
    {|
let f u =
  let mut x = (1, 2) in
  x <- (local_ (3, 4));
  match x with (a, _) -> a
|};
  [%expect {| val f : 'a -> int |}];
  locality
    {|
let f u =
  let mut x = (1, 2) in
  let g v = match x with (a, _) -> a in
  x <- (local_ (3, 4));
  g
|};
  [%expect
    {| This is local, because it is allocated by [local_] (at t.stag:5:7-22), then it is part of the value (at t.stag:5:7-22), then it is assigned to x (at t.stag:5:2-22), then x holds it, and is captured by a closure (at t.stag:4:18-19), then g is used (at t.stag:6:2-3), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:3:2-6:3) |}];
  locality
    {|
let f u =
  let mut x = (1, 2) in
  let g v = match x with (a, _) -> a in
  x <- (local_ (3, 4));
  g () [@nontail]
|};
  [%expect {| val f : 'a -> int |}];
  locality
    {|
let f u =
  let mut x = (1, 2) in
  let set v = x <- (local_ (3, 4)) in
  set ();
  0
|};
  [%expect {| val f : 'a -> int |}]
;;

let%expect_test "higher-order functions take local closures" =
  locality
    {|
let iter f l =
  match l with
  | [] -> ()
  | Cons *(x, rest) -> f x [@nontail]; iter f rest
let sum l =
  let p = local_ (1, 2) in
  let mut total = 0 in
  iter (fun x -> match p with (a, _) -> total <- Int.add total (Int.add a x)) l [@nontail];
  total
|};
  [%expect
    {|
    val iter : ('a -> 'b) -> list['a] -> unit
    val sum : list[int] -> int
    |}]
;;

let%expect_test "Unsafe.assume_global" =
  locality
    {|
let f u = let p = local_ (1, 2) in let q = Unsafe.assume_global p in q
|};
  [%expect {| val f : 'a -> int * int |}]
;;
