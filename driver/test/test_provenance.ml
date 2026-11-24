open! Import
open Source_helpers

let provenance source =
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

let%expect_test "an annotation makes a value external" =
  provenance
    {|
let f (x : string) = let y @ external = x in y
let g (x : string) = x
|};
  [%expect
    {|
    val f : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and portable <= 'm1 and external <= 'm1
    val g : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and portable <= 'm1
    |}]
;;

let%expect_test "provenance is deep: through pointers, records and closures" =
  provenance
    {|
let ext (x : string) = let y @ external = x in y
let in_ptr u = Ptr.make (ext "a")
let in_record u = (ext "a", 1)
let in_closure u = let s = ext "a" in fun v -> s
let read p = p.*
let read_ext u = (in_ptr ()).*
let field u = match in_record () with (s, _) -> s
|};
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and portable <= 'm1 and external <= 'm1
    val in_ptr : 'a -> ptr[string] @ nonportable external
    val in_record : 'a -> string * int @ 'm0 with portable <= 'm0 and external <= 'm0
    val in_closure : 'a -> ('b -> string @ 'm1) @ 'm0 with portable <= 'm1 and portable <= 'm0 and external <= 'm1 and external <= 'm0
    val read : ptr['a] -> 'a @ nonportable
    val read_ext : 'a -> string @ nonportable external
    val field : 'a -> string @ 'm0 with portable <= 'm0 and external <= 'm0
    |}]
;;

let%expect_test "only what a value reaches counts, not what is evaluated" =
  provenance
    {|
let ext (x : string) = let y @ external = x in y
let evaluated u = let s = ext "a" in match s with _ -> 1
let ignored u = let s = ext "a" in "b"
let sequenced u = ext "a"; "b"
|};
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and portable <= 'm1 and external <= 'm1
    val evaluated : 'a -> int @ 'm0 with portable <= 'm0
    val ignored : 'a -> string @ 'm0 with portable <= 'm0
    val sequenced : 'a -> string @ 'm0 with portable <= 'm0
    |}]
;;

let%expect_test "stores into a pointer take only gc values" =
  provenance
    {|
let ext (x : string) = let y @ external = x in y
let p = Ptr.make "a"
let store u = p.* <- ext "b"
|};
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:4:21-28), but must be at most gc, because it is stored through a pointer, whose cell may be managed by the GC (at t.stag:4:21-28) |}];
  provenance
    {|
let p = Ptr.make "a"
let store (s : string) = p.* <- s
let literal u = p.* <- "b"
let ints (q : ptr[int]) = q.* <- 1
|};
  [%expect
    {|
    val p : ptr[string]
    val store : string @ 'm0 -> unit @ 'm1 with portable <= 'm1 and 'm0 <= gc
    val literal : 'a -> unit @ 'm0 with portable <= 'm0
    val ints : ptr[int] -> unit @ 'm0 with portable <= 'm0
    |}];
  provenance
    {|
let store_param p s = p.* <- s
let ext (x : string) = let y @ external = x in y
let bad u = store_param (Ptr.make "a") (ext "b")
|};
  [%expect
    {| This is external, because it is annotated external (at t.stag:3:31-39), then y is used (at t.stag:3:47-48), then the function returns it (at t.stag:3:23-48), then the function types are unified, then the function returns it (at t.stag:4:39-48), then it is passed to a function (at t.stag:4:12-48), but must be at most gc, because it is stored through a pointer, whose cell may be managed by the GC (at t.stag:2:29-30), then s is used (at t.stag:2:29-30), then s is passed to the function (at t.stag:2:22-30), then the function types are unified |}]
;;

let%expect_test "pointer-free values cross provenance" =
  provenance
    {|
let ext (x : int) = let y @ external = x in y
let p = Ptr.make 0
let store u = p.* <- ext 1
let in_closure u = let n = ext 1 in fun v -> n
|};
  [%expect
    {|
    val ext : int -> int @ 'm0 with portable <= 'm0
    val p : ptr[int]
    val store : 'a -> unit @ 'm0 with portable <= 'm0
    val in_closure : 'a -> ('b -> int @ 'm1) @ 'm0 with portable <= 'm1 and portable <= 'm0
    |}]
;;

let%expect_test "let mut holds what is assigned to it" =
  provenance
    {|
let ext (x : string) = let y @ external = x in y
let inferred u = let mut s = "a" in s <- ext "b"; s
let gc_only u = let mut s = "a" in s <- "b"; s
|};
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and portable <= 'm1 and external <= 'm1
    val inferred : 'a -> string @ nonportable external
    val gc_only : 'a -> string @ nonportable
    |}];
  provenance
    {|
let ext (x : string) = let y @ external = x in y
let annotated u = let mut s @ gc = "a" in s <- ext "b"; s
|};
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:3:47-54), then it is assigned to s (at t.stag:3:42-54), but must be at most gc, because it is annotated gc (at t.stag:3:30-32) |}];
  provenance
    {|
let ext (x : string) = let y @ external = x in y
let opted_in u = let mut s @ external = "a" in s
|};
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and portable <= 'm1 and external <= 'm1
    val opted_in : 'a -> string @ nonportable external
    |}]
;;

let%expect_test "mutable globals hold what is assigned to them" =
  provenance
    {|
let ext (x : string) = let y @ external = x in y
let mut g = "a"
let set u = g <- ext "b"
let get u = g
|};
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and portable <= 'm1 and external <= 'm1
    val g : string @ external
    val set : 'a -> unit @ 'm0 with portable <= 'm0
    val get : 'a -> string @ nonportable external
    |}];
  provenance
    {|
let ext (x : string) = let y @ external = x in y
let mut g @ gc = "a"
let set u = g <- ext "b"
|};
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:4:17-24), then it is assigned to g (at t.stag:4:12-24), but must be at most gc, because it is annotated gc (at t.stag:3:12-14) |}]
;;

let%expect_test "static: literals and const values, but not run-time \
                 allocations"
  =
  provenance
    {|
let literal u = let s @ static = "a" in s
let closed u = let f @ static = fun x -> x in f
|};
  [%expect
    {|
    val literal : 'a -> string
    val closed : 'a -> 'b @ 'm0 -> 'b @ 'm1 with 'm0 <= 'm1 and portable <= 'm1
    |}];
  provenance {|
let made u = let p @ static = Ptr.make 1 in p
|};
  [%expect
    {| This is gc, because Ptr.make allocates it at run time, then the function types are unified, then the function returns it (at t.stag:2:30-40), then it is bound by an annotated let (at t.stag:2:21-27), but must be at most static, because it is annotated static (at t.stag:2:21-27) |}];
  provenance {|
let captures (s : string) = let f @ static = fun v -> s in f
|};
  [%expect
    {| This is gc, because s is a parameter, which may differ between calls (at t.stag:2:28-60), then s is captured by a closure (at t.stag:2:54-55), then the closure is made at run time, unless it is [const] (at t.stag:2:45-55), then the closure is made (at t.stag:2:45-55), then it is bound by an annotated let (at t.stag:2:36-42), but must be at most static, because it is annotated static (at t.stag:2:36-42) |}];
  provenance {|
let built u = let s @ static = String.append "a" "b" in s
|};
  [%expect
    {| This is gc, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:2:31-52), then it is bound by an annotated let (at t.stag:2:22-28), but must be at most static, because it is annotated static (at t.stag:2:22-28) |}]
;;

let%expect_test "declared function types default to gc" =
  provenance
    {|
let ext (x : string) = let y @ external = x in y
type t = { f : string -> string }
let r = { f = fun s -> s }
let bad u = r.f (ext "a")
|};
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:5:16-25), then it is passed to a function (at t.stag:5:12-25), but must be at most gc, because a function type fixes it, then the function types are unified |}];
  provenance
    {|
type t = { f : string @ external -> string @ external }
let r = { f = fun s -> s }
let call u = r.f "a"
|};
  [%expect
    {|
    type t = { f : string @ external -> string @ external }
    val r : t
    val call : 'a -> string @ nonportable external
    |}]
;;

let%expect_test "prims take and return gc values" =
  provenance
    {|
let ext (x : string) = let y @ external = x in y
let length u = String.length (ext "a")
|};
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:3:29-38), then it is passed to a function (at t.stag:3:15-38), but must be at most gc, because a function type fixes it, then the function types are unified |}];
  provenance {|
let built u = String.append "a" "b"
|};
  [%expect {| val built : 'a -> string @ nonportable |}]
;;

let%expect_test "a closure is static only if it is const" =
  provenance
    {|
let captures (s : string) = let f @ static = fun v -> s in f
let literal u = captures "a"
|};
  [%expect
    {| This is gc, because s is a parameter, which may differ between calls (at t.stag:2:28-60), then s is captured by a closure (at t.stag:2:54-55), then the closure is made at run time, unless it is [const] (at t.stag:2:45-55), then the closure is made (at t.stag:2:45-55), then it is bound by an annotated let (at t.stag:2:36-42), but must be at most static, because it is annotated static (at t.stag:2:36-42) |}];
  provenance
    {|
let captures (s : string) = let f @ static = fun v -> s in f
let built u = captures (Int.to_string 1)
|};
  [%expect
    {| This is gc, because s is a parameter, which may differ between calls (at t.stag:2:28-60), then s is captured by a closure (at t.stag:2:54-55), then the closure is made at run time, unless it is [const] (at t.stag:2:45-55), then the closure is made (at t.stag:2:45-55), then it is bound by an annotated let (at t.stag:2:36-42), but must be at most static, because it is annotated static (at t.stag:2:36-42) |}];
  provenance
    {|
let ptr u = let p = Ptr.make 1 in let f @ static = fun v -> p in f
|};
  [%expect
    {| This is gc, because Ptr.make allocates it at run time, then the function types are unified, then the function returns it (at t.stag:2:20-30), then p is captured by a closure (at t.stag:2:60-61), then the closure is made at run time, unless it is [const] (at t.stag:2:51-61), then the closure is made (at t.stag:2:51-61), then it is bound by an annotated let (at t.stag:2:42-48), but must be at most static, because it is annotated static (at t.stag:2:42-48) |}]
;;

let%expect_test "a closure capturing an external value is external" =
  provenance
    {|
let ext (x : string) = let y @ external = x in y
let p = Ptr.make (fun (v : int) -> "a")
let store u = let s = ext "a" in p.* <- (fun v -> s)
|};
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:4:22-29), then s is captured by a closure (at t.stag:4:50-51), then the closure is made (at t.stag:4:40-52), but must be at most gc, because it is stored through a pointer, whose cell may be managed by the GC (at t.stag:4:40-52) |}]
;;

let%expect_test "toplevel constants are static" =
  provenance
    {|
let s = "a"
let pair = (s, 1)
let use u = let y @ static = pair in y
|};
  [%expect
    {|
    val s : string
    val pair : string * int
    val use : 'a -> string * int
    |}];
  provenance {|
let s = Int.to_string 1
let use u = let y @ static = s in y
|};
  [%expect
    {| This is gc, because a function type fixes it, then the function types are unified, then the function returns it (at t.stag:2:8-23), then s is used (at t.stag:3:29-30), then it is bound by an annotated let (at t.stag:3:20-26), but must be at most static, because it is annotated static (at t.stag:3:20-26) |}]
;;

let%expect_test "global_ fields and exclave_ keep provenance" =
  provenance
    {|
let ext (x : string) = let y @ external = x in y
type r = { g : global_ string; n : int }
let make u = { g = ext "a"; n = 1 }
let read u = (make ()).g
let returned u = exclave_ ext "a"
|};
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and portable <= 'm1 and external <= 'm1
    type r = { g : global_ string; n : int }
    val make : 'a -> r @ 'm0 with portable <= 'm0 and external <= 'm0
    val read : 'a -> string @ 'm0 with portable <= 'm0 and external <= 'm0
    val returned : 'a -> string @ 'm0 with portable <= 'm0 and external <= 'm0
    |}]
;;

let%expect_test "a toplevel closure capturing nothing is static" =
  provenance
    {|
let f @ static = fun (x : int) -> x
let g @ static = fun (x : int) -> f x
|};
  [%expect
    {|
    val f : int -> int @ 'm0 with portable <= 'm0
    val g : int -> int @ 'm0 with portable <= 'm0
    |}]
;;

let%expect_test "a partial application is allocated at run time" =
  provenance
    {|
let k (a : int) (b : int) = a
let g3 (n : int) = let h @ static = k n in h
|};
  [%expect
    {| This is gc, because a partial application is made at run time (at t.stag:2:28-29), then the partial application is made at run time, unless it is [const] (at t.stag:2:28-29), then the function types are unified, then the function returns it (at t.stag:3:36-39), then it is bound by an annotated let (at t.stag:3:27-33), but must be at most static, because it is annotated static (at t.stag:3:27-33) |}];
  provenance
    {|
let f (a : string) (b : int) = a
let g u = let h @ static = f "x" in h
|};
  [%expect
    {| This is gc, because a partial application is made at run time (at t.stag:2:31-32), then the partial application is made at run time, unless it is [const] (at t.stag:2:31-32), then the function types are unified, then the function returns it (at t.stag:3:27-32), then it is bound by an annotated let (at t.stag:3:18-24), but must be at most static, because it is annotated static (at t.stag:3:18-24) |}];
  provenance {|
let mk x = fun v -> x
let g u = let h @ static = mk "a" in h
|};
  [%expect
    {| This is gc, because a partial application is made at run time (at t.stag:2:11-21), then the partial application is made at run time, unless it is [const] (at t.stag:2:11-21), then the function types are unified, then the function returns it (at t.stag:3:27-33), then it is bound by an annotated let (at t.stag:3:18-24), but must be at most static, because it is annotated static (at t.stag:3:18-24) |}];
  provenance {|
let k (a : int) (b : int) = a
let g = k 1
|};
  [%expect
    {|
    val k : int @ 'm0 -> (int -> int @ 'm2) @ 'm1 with 'm0 <= 'm1 and portable <= 'm2 and portable <= 'm1
    val g : int @ '_m0 -> int @ '_m1 with portable <= '_m1
    |}]
;;

let%expect_test "readers of a mutable global see what is assigned later" =
  provenance
    {|
let ext (x : string) = let y @ external = x in y
let mut g = "a"
let get u = g
let p = Ptr.make "x"
let copy u = p.* <- get ()
let set u = g <- ext "b"
|};
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:7:17-24), then it is assigned to g (at t.stag:7:12-24), but must be at most gc, because it is stored through a pointer, whose cell may be managed by the GC (at t.stag:6:20-26), then the function returns it (at t.stag:6:20-26), then the function types are unified, then the function returns it (at t.stag:4:12-13), then g is used (at t.stag:4:12-13) |}];
  provenance
    {|
let ext (x : string) = let y @ external = x in y
let mut g = "a"
let get u = g
let keep u = let s @ gc = get () in s
let set u = g <- ext "b"
|};
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:6:17-24), then it is assigned to g (at t.stag:6:12-24), but must be at most gc, because it is annotated gc (at t.stag:5:21-23), then it is bound by an annotated let (at t.stag:5:21-23), then the function returns it (at t.stag:5:26-32), then the function types are unified, then the function returns it (at t.stag:4:12-13), then g is used (at t.stag:4:12-13) |}];
  provenance
    {|
let ext (x : string) = let y @ external = x in y
let mut g = "a"
let h = fun (u : int) -> g
let keep u = let s @ gc = h 1 in s
let set u = g <- ext "b"
|};
  [%expect
    {| This is external, because it is annotated external (at t.stag:2:31-39), then y is used (at t.stag:2:47-48), then the function returns it (at t.stag:2:23-48), then the function types are unified, then the function returns it (at t.stag:6:17-24), then it is assigned to g (at t.stag:6:12-24), but must be at most gc, because it is annotated gc (at t.stag:5:21-23), then it is bound by an annotated let (at t.stag:5:21-23), then the function returns it (at t.stag:5:26-29), then the function types are unified, then the function returns it (at t.stag:4:25-26), then g is used (at t.stag:4:25-26) |}]
;;

let%expect_test "a raised payload is allocated at run time" =
  provenance
    {|
exception E of string
let f u =
  match raise (E (Int.to_string 1)) with
  | x -> x
  | exception E s -> let t @ static = s in t
|};
  [%expect
    {| This is gc, because it is a raised exception (at t.stag:4:2-6:44), then s is used (at t.stag:6:38-39), then it is bound by an annotated let (at t.stag:6:29-35), but must be at most static, because it is annotated static (at t.stag:6:29-35) |}]
;;

let%expect_test "a parameter is never const in the body" =
  provenance
    {|
let captures (s : string) = let f @ static = fun v -> s in f
let a u = captures "a"
|};
  [%expect
    {| This is gc, because s is a parameter, which may differ between calls (at t.stag:2:28-60), then s is captured by a closure (at t.stag:2:54-55), then the closure is made at run time, unless it is [const] (at t.stag:2:45-55), then the closure is made (at t.stag:2:45-55), then it is bound by an annotated let (at t.stag:2:36-42), but must be at most static, because it is annotated static (at t.stag:2:36-42) |}];
  provenance
    {|
let r = .~(let q (s @ portable) = let c @ static = .< s >. in c in q 1)
|};
  [%expect
    {| This is gc, because s is a parameter, which may differ between calls (at t.stag:2:10-71), then s is embedded in a quote (at t.stag:2:54-55), then the code is built at run time, unless it is [const] (at t.stag:2:51-58), then the code is built (at t.stag:2:51-58), then it is bound by an annotated let (at t.stag:2:42-48), but must be at most static, because it is annotated static (at t.stag:2:42-48) |}];
  provenance
    {|
let closed (s : string) = let f @ static = fun v -> "k" in f
let ident x = x
let not_const = let c @ const = ident 1 in c
|};
  [%expect
    {| This is portable, because it applies a function (at t.stag:4:32-39), then it is bound by an annotated let (at t.stag:4:24-29), but must be at most const, because it is annotated const (at t.stag:4:24-29) |}]
;;
