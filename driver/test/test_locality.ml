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
      (Signature.to_string_hum (List.drop checked.signature (List.length prelude)))
  | Error { errors; partial = _ } ->
    List.iter errors ~f:(fun error -> print_endline (Typecheck_error.message error))
;;

let%expect_test "a local value can't be returned" =
  locality {|
let f u = let p = local_ (1, 2) in p
|};
  [%expect {| This is local, because it is allocated by [local_] (at t.stag:2:18-31), then it is part of the value (at t.stag:2:18-31), then p is used (at t.stag:2:35-36), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:10-36) |}];
  locality {|
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
  [%expect {| [exclave_] ends the function's region, so must be the function's result, in tail position |}]
;;

let%expect_test "toplevels are global" =
  locality {|
let x = local_ (1, 2)
|};
  [%expect {| This is local, because it is allocated by [local_] (at t.stag:2:8-21), then it is part of the value (at t.stag:2:8-21), but must be at most global, because x is a toplevel (at t.stag:2:8-21) |}];
  locality {|
let x = let p = local_ (1, 2) in match p with (a, b) -> Int.add a b
|};
  [%expect {| val x : int |}]
;;

let%expect_test "plain data crosses locality" =
  locality {|
let f u = let p = local_ ((1, 2), 3) in match p with (a, _) -> a
|};
  [%expect {| This is local, because it is allocated by [local_] (at t.stag:2:18-36), then it is part of the value (at t.stag:2:18-36), then p is used (at t.stag:2:46-47), then a is used (at t.stag:2:63-64), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:10-64) |}];
  locality {|
let f u = let p = local_ ((1, 2), 3) in match p with (_, b) -> b
|};
  [%expect {| val f : 'a -> int |}]
;;

let%expect_test "a closure capturing a local value is local" =
  locality {|
let f u = let p = local_ (1, 2) in let g v = p in g
|};
  [%expect {| This is local, because it is allocated by [local_] (at t.stag:2:18-31), then it is part of the value (at t.stag:2:18-31), then p is captured by a closure (at t.stag:2:45-46), then g is used (at t.stag:2:50-51), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:10-51) |}];
  locality {|
let f u =
  let p = local_ ((1, 2), 3) in
  let g v = match p with (_, b) -> b in
  g ()
|};
  [%expect {| This is local, because it is allocated by [local_] (at t.stag:3:10-28), then it is part of the value (at t.stag:3:10-28), then p is captured by a closure (at t.stag:4:18-19), then g is used (at t.stag:5:2-3), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:5:2-3) |}];
  locality {|
let f u =
  let p = local_ ((1, 2), 3) in
  let g v = match p with (_, b) -> b in
  g () [@nontail]
|};
  [%expect {| val f : 'a -> int |}]
;;

let%expect_test "a tail call can't be passed what is local to the region" =
  locality {|
let use (p @ local) = match p with (a, _) -> a
let caller u = use (local_ (1, 2))
|};
  [%expect {| This is local, because it is allocated by [local_] (at t.stag:3:19-34), then it is part of the value (at t.stag:3:19-34), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:3:19-34) |}];
  locality {|
let use (p @ local) = match p with (a, _) -> a
let caller u = use (local_ (1, 2)) [@nontail]
let pass (p @ local) = use p
let in_exclave u = exclave_ use (local_ (1, 2))
|};
  [%expect {| This is local, because it is annotated local (at t.stag:2:13-18), then p is passed to the function (at t.stag:2:22-46), then p is used (at t.stag:2:28-29), then a is used (at t.stag:2:45-46), then the function returns it (at t.stag:2:22-46), then the function types are unified, then the function returns it (at t.stag:4:23-28), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:23-28) |}]
;;

let%expect_test "parameters" =
  locality {|
let keep (x @ global) = Ptr.make x
let f u = keep (local_ (1, 2)) [@nontail]
|};
  [%expect {| This is local, because it is allocated by [local_] (at t.stag:3:15-30), then it is part of the value (at t.stag:3:15-30), then it is passed to a function (at t.stag:3:10-30), but must be at most global, because it is annotated global (at t.stag:2:14-20), then the function types are unified |}];
  locality {|
let escape (x @ local) = Ptr.make x
|};
  [%expect {| This is regional, because it is annotated local (at t.stag:2:16-21), then x is passed to the function (at t.stag:2:25-35), then x is used (at t.stag:2:34-35), then it is passed to a function (at t.stag:2:25-35), but must be at most global, because a function type fixes it, then the function types are unified |}];
  locality {|
let id x = x
let f u = let p = id (local_ (1, 2)) in Ptr.make p
let g u = let p = id (1, 2) in Ptr.make p
|};
  [%expect {|
    The value id is defined more than once in this module
    This is local, because it is allocated by [local_] (at t.stag:3:21-36), then it is part of the value (at t.stag:3:21-36), then it is passed to a function (at t.stag:3:18-36), then the function types are unified, then x is passed to the function (at prelude.stag:4:11-12), then x is used (at prelude.stag:4:11-12), then the function returns it (at prelude.stag:4:11-12), then the function types are unified, then the function returns it (at t.stag:3:18-36), then p is used (at t.stag:3:49-50), then it is passed to a function (at t.stag:3:40-50), but must be at most global, because a function type fixes it, then the function types are unified
    This is local, because it is allocated by [local_] (at t.stag:3:21-36), then it is part of the value (at t.stag:3:21-36), then it is passed to a function (at t.stag:3:18-36), then the function types are unified, then x is passed to the function (at prelude.stag:4:11-12), then x is used (at prelude.stag:4:11-12), then the function returns it (at prelude.stag:4:11-12), then the function types are unified, then the function returns it (at t.stag:3:18-36), then p is used (at t.stag:3:49-50), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:3:49-50)
    |}]
;;

let%expect_test "stores through pointers are global" =
  locality {|
let f (p : ptr[int * int]) = p.* <- local_ (1, 2)
|};
  [%expect {| This is local, because it is allocated by [local_] (at t.stag:2:36-49), then it is part of the value (at t.stag:2:36-49), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:2:36-49) |}];
  locality {|
let f (p : ptr[(int * int) * int]) =
  let q = local_ ((1, 2), 3) in
  match p.* with (a, _) -> a
|};
  [%expect {| val f : ptr[(int * int) * int] -> int * int |}]
;;

let%expect_test "global_ fields" =
  locality {|
type r = { g : global_ int * int; l : int * int }
let get (r @ local) = Ptr.make r.g
let get_l (r @ local) = Ptr.make r.l
|};
  [%expect {| This is regional, because it is annotated local (at t.stag:3:13-18), then r is passed to the function (at t.stag:3:22-34), then r is used (at t.stag:3:31-32), then it is passed to a function (at t.stag:3:22-34), but must be at most global, because a function type fixes it, then the function types are unified |}];
  locality {|
type r = { g : global_ int * int; l : int * int }
let mk u = local_ { g = local_ (1, 2); l = (3, 4) }
|};
  [%expect {| This is local, because it is allocated by [local_] (at t.stag:3:11-51), then it is part of the value (at t.stag:3:11-51), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:3:11-51) |}]
;;

let%expect_test "let mut" =
  locality {|
let f u =
  let mut x = (1, 2) in
  x <- local_ (3, 4);
  match x with (a, _) -> a
|};
  [%expect {| This expression has type int but an expression was expected of type int * int |}];
  locality {|
let f u =
  let mut x = (1, 2) in
  let g v = match x with (a, _) -> a in
  x <- local_ (3, 4);
  g
|};
  [%expect {| This expression has type '_a -> int but an expression was expected of type int * int |}];
  locality {|
let f u =
  let mut x = (1, 2) in
  let g v = match x with (a, _) -> a in
  x <- local_ (3, 4);
  g () [@nontail]
|};
  [%expect {| This expression has type int but an expression was expected of type int * int |}];
  locality {|
let f u =
  let mut x = (1, 2) in
  let set v = x <- local_ (3, 4) in
  set ();
  0
|};
  [%expect {| val f : 'a -> int |}]
;;

let%expect_test "higher-order functions take local closures" =
  locality
    {|
let rec iter f l =
  match l with
  | [] -> ()
  | Cons *(x, rest) -> f x [@nontail]; iter f rest
let sum l =
  let p = local_ (1, 2) in
  let mut total = 0 in
  iter (fun x -> match p with (a, _) -> total <- Int.add total (Int.add a x)) l [@nontail];
  total
|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  "t.stag:2:4-7: lets are always recursive; remove `rec`"
  Raised at Base__Error0.raise in file "src/error0.ml", line 15, characters 38-66
  Called from Base__Error0.raise_s in file "src/error0.ml", line 24, characters 52-76
  Called from Test_driver__Source_helpers.program in file "driver/test/source_helpers.ml", line 51, characters 60-76
  Called from Test_driver__Test_locality.locality in file "driver/test/test_locality.ml", line 10, characters 47-63
  Called from Test_driver__Test_locality.(fun) in file "driver/test/test_locality.ml", lines 172-183, characters 2-280
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 360, characters 10-25
  |}]
;;

let%expect_test "Unsafe.assume_global" =
  locality {|
let f u = let p = local_ (1, 2) in Unsafe.assume_global p
|};
  [%expect {| This is local, because it is allocated by [local_] (at t.stag:2:18-31), then it is part of the value (at t.stag:2:18-31), then p is used (at t.stag:2:56-57), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:2:56-57) |}]
;;
