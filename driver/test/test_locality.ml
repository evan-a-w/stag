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

(* Records, tuples and variants are values: what is local is what they point to,
   here a [ptr] allocated by [stack_]. *)

let%expect_test "a local value can't be returned" =
  locality {|
let f u = let p = stack_ (Ptr.make (1, 2)) in p
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:2:18-42), then it is part of the value (at t.stag:2:18-42), then p is used (at t.stag:2:46-47), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:10-47) |}];
  locality
    {|
let f u = exclave_ stack_ (Ptr.make (1, 2))
let g u = match (f ()).* with (a, _) -> a
|};
  [%expect
    {|
    val f : 'a -> ptr[int * int] @ local
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
let x = stack_ (Ptr.make (1, 2))
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:2:8-32), then it is part of the value (at t.stag:2:8-32), but must be at most global, because x is a toplevel (at t.stag:2:8-32) |}];
  locality
    {|
let x = let p = stack_ (Ptr.make (1, 2)) in match p.* with (a, b) -> Int.add a b
|};
  [%expect {| val x : int |}]
;;

let%expect_test "plain data crosses locality" =
  locality
    {|
let f u = let p = stack_ (Ptr.make ((1, 2), 3)) in match p.* with (a, _) -> a
|};
  [%expect {| val f : 'a -> int * int |}];
  locality
    {|
let f u =
  let q = Ptr.make 1 in
  let p = stack_ (Ptr.make (q, 3)) in
  match p.* with (a, _) -> a
|};
  [%expect {| val f : 'a -> ptr[int] |}]
;;

let%expect_test "stack_ is only on allocations" =
  locality {|
let f u = stack_ (1, 2)
let g (p : ptr[int]) = stack_ (p, 1)
|};
  [%expect
    {|
    [stack_] allocates its value on the stack, so must be on an allocation: a closure, a quote, [Ptr.make e], or a record, tuple or constructor holding one (records, tuples and constructors are values)
    [stack_] allocates its value on the stack, so must be on an allocation: a closure, a quote, [Ptr.make e], or a record, tuple or constructor holding one (records, tuples and constructors are values)
    |}];
  locality
    {|
let f u = let p = stack_ (Ptr.make 1, 2) in match p with (q, _) -> q.*
let g u = let h = stack_ (fun x -> x) in h 1 [@nontail]
|};
  [%expect {|
    val f : 'a -> int
    val g : 'a -> int
    |}]
;;

let%expect_test "a closure capturing a local value is local" =
  locality {|
let f u = let p = stack_ (Ptr.make (1, 2)) in let g v = p in g
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:2:18-42), then it is part of the value (at t.stag:2:18-42), then p is captured by a closure (at t.stag:2:56-57), then g is used (at t.stag:2:61-62), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:2:10-62) |}];
  locality
    {|
let f u =
  let p = stack_ (Ptr.make (1, 2)) in
  let g v = match p.* with (_, b) -> b in
  g ()
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:10-34), then it is part of the value (at t.stag:3:10-34), then p is captured by a closure (at t.stag:4:18-19), then g is used (at t.stag:5:2-3), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:5:2-3) |}];
  locality
    {|
let f u =
  let p = stack_ (Ptr.make (1, 2)) in
  let g v = match p.* with (_, b) -> b in
  g () [@nontail]
|};
  [%expect {| val f : 'a -> int |}]
;;

let%expect_test "a tail call can't be passed what is local to the region" =
  locality
    {|
let use (p @ local) = match p.* with (a, _) -> a
let caller u = use (stack_ (Ptr.make (1, 2)))
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:19-45), then it is part of the value (at t.stag:3:19-45), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:3:19-45) |}];
  locality
    {|
let use (p @ local) = match p.* with (a, _) -> a
let caller u = use (stack_ (Ptr.make (1, 2))) [@nontail]
let pass (p : ptr[int * int] @ local) = use p
let in_exclave u = exclave_ use (stack_ (Ptr.make (1, 2)))
|};
  [%expect
    {|
    val use : ptr['a * 'b] @ local -> 'a
    val caller : 'a -> int
    val pass : ptr[int * int] @ local -> int
    val in_exclave : 'a -> int
    |}]
;;

let%expect_test "parameters" =
  locality
    {|
let keep (x @ global) = Ptr.make x
let f u = keep (stack_ (Ptr.make (1, 2))) [@nontail]
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:15-41), then it is part of the value (at t.stag:3:15-41), then it is passed to a function (at t.stag:3:10-41), but must be at most global, because it is annotated global (at t.stag:2:14-20), then a function is used at a type with other modes |}];
  locality {|
let escape (x @ local) = Ptr.make x
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:2:16-21), then x is passed to the function (at t.stag:2:25-35), then x is used (at t.stag:2:34-35), then it is passed to a function (at t.stag:2:25-35), but must be at most global, because Ptr.make holds it, then a function is used at a type with other modes |}];
  locality
    {|
let same x = x
let f u = let p = same (stack_ (Ptr.make (1, 2))) in Ptr.make p
let g u = let p = same (Ptr.make (1, 2)) in Ptr.make p
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:23-49), then it is part of the value (at t.stag:3:23-49), then it is passed to a function (at t.stag:3:18-49), then a function is used at a type with other modes, then x is passed to the function (at t.stag:2:13-14), then x is used (at t.stag:2:13-14), then the function returns it (at t.stag:2:13-14), then a function is used at a type with other modes, then the function returns it (at t.stag:3:18-49), then p is used (at t.stag:3:62-63), then it is passed to a function (at t.stag:3:53-63), but must be at most global, because Ptr.make holds it, then a function is used at a type with other modes |}]
;;

let%expect_test "stores through pointers are global" =
  locality {|
let f (p : ptr[ptr[int]]) = p.* <- stack_ (Ptr.make 1)
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:2:35-54), then it is part of the value (at t.stag:2:35-54), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:2:35-54) |}];
  locality
    {|
let f (p : ptr[ptr[int] * int]) =
  let q = stack_ (Ptr.make 1) in
  match p.* with (a, _) -> a
|};
  [%expect {| val f : ptr[ptr[int] * int] -> ptr[int] |}]
;;

let%expect_test "global_ fields" =
  locality
    {|
type r = { g : ptr[int] @@ global; l : ptr[int] }
let get (r @ local) = Ptr.make r.g
let get_l (r @ local) = Ptr.make r.l
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:4:15-20), then r is passed to the function (at t.stag:4:24-36), then r is used (at t.stag:4:33-34), then its type turned out to be r, which has pointers, then its field is read (at t.stag:4:33-36), then it is passed to a function (at t.stag:4:24-36), but must be at most global, because Ptr.make holds it, then a function is used at a type with other modes |}];
  locality
    {|
type r = { g : ptr[int] @@ global; l : ptr[int] }
let mk u = { g = stack_ (Ptr.make 1); l = Ptr.make 2 }
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:17-36), then it is part of the value (at t.stag:3:17-36), but must be at most global, because it is held by a field or argument with a modality (at t.stag:3:17-36) |}];
  (* Reviewer: record update must check [global_] fields too. *)
  locality
    {|
type r = { g : ptr[int] @@ global; l : ptr[int] }
let mk u = { g = Ptr.make 1; l = Ptr.make 2 }
let f u = let r = mk () in let r2 = { r with g = stack_ (Ptr.make 5) } in Ptr.make r2.g
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:4:49-68), then it is part of the value (at t.stag:4:49-68), but must be at most global, because it is held by a field or argument with a modality (at t.stag:4:49-68) |}];
  (* Reviewer: a [global_] field bound by a pattern is global, as [r.g] is. *)
  locality
    {|
type r = { g : ptr[int] @@ global; l : ptr[int] }
let f (r @ local) = match r with { g = g; l = _ } -> Ptr.make g
let f_l (r @ local) = match r with { g = _; l = l } -> Ptr.make l
type t = A of ptr[int] @@ global * ptr[int]
let h (x @ local) = match x with A (g, _) -> Ptr.make g
|};
  [%expect
    {| This is regional, because it is annotated local (at t.stag:4:13-18), then r is passed to the function (at t.stag:4:22-65), then r is used (at t.stag:4:28-29), then its type turned out to be r, which has pointers, then l is used (at t.stag:4:64-65), then it is passed to a function (at t.stag:4:55-65), but must be at most global, because Ptr.make holds it, then a function is used at a type with other modes |}]
;;

let%expect_test "let mut" =
  locality
    {|
let f u =
  let mut x = Ptr.make 1 in
  x <- (stack_ (Ptr.make 3));
  x.*
|};
  [%expect {| val f : 'a -> int |}];
  locality
    {|
let f u =
  let mut x = Ptr.make 1 in
  let g v = x.* in
  x <- (stack_ (Ptr.make 3));
  g
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:5:7-28), then it is part of the value (at t.stag:5:7-28), then it is assigned to x (at t.stag:5:2-28), then x holds it, and is captured by a closure (at t.stag:4:12-13), then g is used (at t.stag:6:2-3), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:3:2-6:3) |}];
  locality
    {|
let f u =
  let mut x = Ptr.make 1 in
  let g v = x.* in
  x <- (stack_ (Ptr.make 3));
  g () [@nontail]
|};
  [%expect {| val f : 'a -> int |}];
  locality
    {|
let f u =
  let mut x = Ptr.make 1 in
  let set v = x <- stack_ (Ptr.make 3) in
  set ();
  0
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:4:19-38), then it is part of the value (at t.stag:4:19-38), but must be at most global, because it is assigned to x from a closure, which may be called from a shorter region (at t.stag:4:14-38) |}]
;;

let%expect_test "exclave_" =
  (* A [let mut] cell captured under [exclave_] is put where it outlives the
     closure: here on the heap. *)
  locality
    {|
let f u = let mut x = 1 in exclave_ stack_ (fun v -> x)
let f2 u = let mut x = 0 in exclave_ stack_ (fun v -> x <- 1)
|};
  [%expect
    {|
    val f : 'a -> ('b -> int) @ local
    val f2 : 'a -> ('b -> unit) @ local
    |}];
  (* Reviewer: [p] is in [outer]'s region, which outlives [f]'s [exclave_]. *)
  locality
    {|
let outer u =
  let p = stack_ (Ptr.make (1, 2)) in
  let f v = exclave_ p in
  match (f ()).* with (a, _) -> a
|};
  [%expect {| val outer : 'a -> int |}];
  locality
    {|
let f u = let p = stack_ (Ptr.make (1, 2)) in exclave_ (fun v -> p)
let g u = let p = stack_ (Ptr.make (1, 2)) in exclave_ p
|};
  [%expect
    {|
    This is local, because it is allocated by [stack_] (at t.stag:2:18-42), then it is part of the value (at t.stag:2:18-42), then p is used in an [exclave_] (at t.stag:2:65-66), but must be at most regional, because the [exclave_] has ended its region (at t.stag:2:65-66)
    This is local, because it is allocated by [stack_] (at t.stag:3:18-42), then it is part of the value (at t.stag:3:18-42), then p is used in an [exclave_] (at t.stag:3:55-56), but must be at most regional, because the [exclave_] has ended its region (at t.stag:3:55-56)
    |}]
;;

let%expect_test "higher-order functions take local closures" =
  locality
    {|
let iter f l =
  match l with
  | [] -> ()
  | Cons *(x, rest) -> f x [@nontail]; iter f rest
let sum l =
  let p = stack_ (Ptr.make 1) in
  let mut total = 0 in
  iter (fun x -> total <- Int.add total (Int.add p.* x)) l [@nontail];
  total
|};
  [%expect
    {|
    val iter : ('a -> 'b) -> list['a] -> unit
    val sum : list[int] -> int
    |}];
  (* Reviewer: [a]'s type isn't known when it's used, so whether it crosses is
     decided late. *)
  locality
    {|
let f g = let p = stack_ (Ptr.make (1, 2)) in g p [@nontail]
let h u = f (fun x -> match x.* with (a, _) -> a)
let h2 u = f (fun (x : ptr[int * int]) -> match x.* with (a, _) -> a)
|};
  [%expect
    {|
    val f : (ptr[int * int] @ local -> 'a) -> 'a
    val h : 'a -> int
    val h2 : 'a -> int
    |}]
;;

let%expect_test "Unsafe.assume_global" =
  locality
    {|
let f u = let p = stack_ (Ptr.make (1, 2)) in let q = Unsafe.assume_global p in q
let g u = let c = Unsafe.assume_global (fun x -> x) in .<c>.
|};
  [%expect
    {|
    val f : 'a -> ptr[int * int]
    val g : 'a -> <'b -> 'b>{..'c}
    |}]
;;

let%expect_test "exceptions' arguments are global" =
  locality
    {|
exception E of ptr[int]
let f u = let p = stack_ (Ptr.make 1) in raise (E p)
|};
  [%expect
    {|
    This is local, because it is allocated by [stack_] (at t.stag:3:18-37), then it is part of the value (at t.stag:3:18-37), then p is used (at t.stag:3:50-51), then it is held (at t.stag:3:50-51), then it is passed to a function (at t.stag:3:41-52), but must be at most global, because a function type fixes it, as a function is used at a type with other modes
    This is local, because it is allocated by [stack_] (at t.stag:3:18-37), then it is part of the value (at t.stag:3:18-37), then p is used (at t.stag:3:50-51), then it is held (at t.stag:3:50-51), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:3:47-52)
    |}];
  locality
    {|
exception E of ptr[int]
let g h = match h () with x -> x | exception E p -> Ptr.make p
|};
  [%expect
    {|
    exception E of ptr[int]
    val g : (unit -> ptr[ptr[int]]) -> ptr[ptr[int]]
    |}]
;;

let%expect_test "a value in its own definition is at least its value" =
  locality
    {|
let f (q : ptr[unit -> unit]) =
  let x = (let p = stack_ (Ptr.make 1) in fun u -> let y = p in q.* <- x) in
  x () [@nontail]
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:19-38), then it is part of the value (at t.stag:3:19-38), then p is captured by a closure (at t.stag:3:59-60), then the closure is made (at t.stag:3:42-72), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:3:71-72), then its type turned out to be unit -> unit, which has pointers, then x is used (at t.stag:3:71-72), then it defines x (at t.stag:3:2-4:17) |}];
  locality
    {|
let f (q : ptr[unit -> unit]) =
  let x = stack_ (fun u -> q.* <- x) in
  x () [@nontail]
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:10-36), then it is part of the value (at t.stag:3:10-36), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:3:34-35), then its type turned out to be unit -> unit, which has pointers, then x is used (at t.stag:3:34-35), then it defines x (at t.stag:3:2-4:17) |}]
;;

let%expect_test "explicit substitutions are bound at their own modes" =
  locality
    {|
let f (q : ptr[ptr[int]]) = .~(.<q.* <- x>.).{q; x = stack_ (Ptr.make 1)}
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:2:53-72), then it is part of the value (at t.stag:2:53-72), then x is used (at t.stag:2:40-41), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:2:40-41) |}];
  locality
    {|
let f (q : ptr[ptr[int]]) = .~(.<q.* <- x>.).{q; x = Ptr.make 1}
let g (q : ptr[int]) = .~(.<match x.* with (a, _) -> a>.).{x = stack_ (Ptr.make (1, 2))}
|};
  [%expect
    {|
    val f : ptr[ptr[int]] -> unit
    val g : ptr[int] -> int
    |}]
;;

let%expect_test "a toplevel splice's expansion is in tail position if it is" =
  locality
    {|
let use (p @ local) = match p.* with (a, _) -> a
let f u = let p = stack_ (Ptr.make (1, 2)) in .~(.<use p>.)
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:18-42), then it is part of the value (at t.stag:3:18-42), then p is used (at t.stag:3:55-56), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:3:55-56) |}];
  locality
    {|
let f u = .~(.<exclave_ stack_ (Ptr.make (1, 2))>.)
let g u = let p = stack_ (Ptr.make (1, 2)) in .~(.<p>.)
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:18-42), then it is part of the value (at t.stag:3:18-42), then p is used (at t.stag:3:46-55), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:3:10-55) |}];
  locality {|
let c = .<exclave_ stack_ (Ptr.make 1)>.
|};
  [%expect {| val c : <ptr[int]>{..'a} |}];
  locality {|
let f u = let p = .~(.<exclave_ stack_ (Ptr.make 1)>.) in 1
|};
  [%expect
    {| [exclave_] ends the function's region, so must be the function's result, in tail position |}]
;;

let%expect_test "a pointer's pointee is global in patterns" =
  locality
    {|
let f (p : ptr[ptr[int]] @ local) = match p with *q -> Ptr.make q
|};
  [%expect {| val f : ptr[ptr[int]] @ local -> ptr[ptr[int]] |}]
;;

let%expect_test "stack_ doesn't apply to what a global_ field holds" =
  locality
    {|
type r = { g : ptr[int] @@ global }
let f u = stack_ { g = Ptr.make 1 }
|};
  [%expect
    {| [stack_] allocates its value on the stack, so must be on an allocation: a closure, a quote, [Ptr.make e], or a record, tuple or constructor holding one (records, tuples and constructors are values) |}]
;;

let%expect_test "a tail call can't call a function value local to the region" =
  locality
    {|
let mk u = exclave_ stack_ (fun (x : int) -> x)
let f u = mk () 1
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:2:20-47), then it is part of the value (at t.stag:2:20-47), then the function returns it (at t.stag:2:11-47), then a function is used at a type with other modes, then a tail call gives it further arguments, after the current region has ended (mark the call [@nontail]) (at t.stag:3:10-15), but must be at most regional, because the function returns a function value, not the closure of a partial application (at t.stag:2:11-47), then the function types are unified, including whether their results may take arguments after the region ends (not printed) |}];
  locality
    {|
let mk u = exclave_ stack_ (fun (x : int) -> x)
let g u = let h = mk () in h 1
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:2:20-47), then it is part of the value (at t.stag:2:20-47), then the function returns it (at t.stag:2:11-47), then a function is used at a type with other modes, then the function returns it (at t.stag:3:18-23), then h is used (at t.stag:3:27-28), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:3:27-28) |}];
  locality
    {|
let mk (p : ptr[int]) = exclave_ stack_ (fun (x : int) -> Int.add x p.*)
let f u = mk (Ptr.make 1) 1
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:2:33-72), then it is part of the value (at t.stag:2:33-72), then the function returns it (at t.stag:2:24-72), then a function is used at a type with other modes, then a tail call gives it further arguments, after the current region has ended (mark the call [@nontail]) (at t.stag:3:10-25), but must be at most regional, because the function returns a function value, not the closure of a partial application (at t.stag:2:24-72), then the function types are unified, including whether their results may take arguments after the region ends (not printed) |}];
  (* Not in tail position, or the closure of a partial application, which a tail
     call never makes. *)
  locality
    {|
let mk u = exclave_ stack_ (fun (x : int) -> x)
let f u = mk () 1 [@nontail]
let plus (p @ local) (x : int) = Int.add p.* x
let g (p @ local) = plus p 2
let k f = f 1 2
let use u = k (fun (x : int) (y : int) -> Int.add x y)
let use2 u = k Int.add
let use3 (f : int -> int -> int) = k f
|};
  [%expect
    {|
    val mk : 'a -> (int -> int) @ local
    val f : 'a -> int
    val plus : ptr[int] @ local -> (int -> int) @ local
    val g : ptr[int] @ local -> int
    val k : (int -> int -> 'a) -> 'a
    val use : 'a -> int
    val use2 : 'a -> int
    val use3 : (int -> int -> int) -> int
    |}];
  (* A function whose body is a [fun] is one function of both's parameters. *)
  locality
    {|
let plus (p @ local) = fun (x : int) -> Int.add p.* x
let plus2 = fun (p @ local) -> fun (x : int) -> Int.add p.* x
let g (p @ local) = plus p 2
let g2 (p @ local) = plus2 p 2
let k f = f 1 2
let use u = k (fun (x : int) -> fun (y : int) -> Int.add x y)
|};
  [%expect
    {|
    val plus : ptr[int] @ local -> (int -> int) @ local
    val plus2 : ptr[int] @ local -> (int -> int) @ local
    val g : ptr[int] @ local -> int
    val g2 : ptr[int] @ local -> int
    val k : (int -> int -> 'a) -> 'a
    val use : 'a -> int
    |}];
  (* So the closure of its partial application holds the outer arguments, even
     if the inner [fun] doesn't use them, at every level. *)
  locality
    {|
let f (p @ local) = fun (x : int) -> x
let mut cell : int -> int = fun (x : int) -> x
let h u = let p = stack_ (Ptr.make 1) in cell <- f p; 0
|};
  [%expect
    {| This is local, because it is annotated local (at t.stag:2:11-16), then a partial application holds it (at t.stag:2:20-38), then a function is used at a type with other modes, then the function returns it (at t.stag:4:49-52), then it is assigned to cell (at t.stag:4:41-52), but must be at most global, because cell is a toplevel (at t.stag:3:28-46) |}];
  locality
    {|
let f (p @ local) = fun (x : int) -> x
let cell = Ptr.make (fun (x : int) -> x)
let h u = let p = stack_ (Ptr.make 1) in cell.* <- f p; 0
|};
  [%expect
    {| This is local, because it is annotated local (at t.stag:2:11-16), then a partial application holds it (at t.stag:2:20-38), then a function is used at a type with other modes, then the function returns it (at t.stag:4:51-54), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:4:51-54) |}];
  locality
    {|
let f (p @ local) = fun (x : int) -> fun (y : int) -> y
let cell = Ptr.make (fun (x : int) -> x)
let h u = let p = stack_ (Ptr.make 1) in cell.* <- f p 1; 0
|};
  [%expect
    {| This is local, because it is annotated local (at t.stag:2:11-16), then a partial application holds it (at t.stag:2:20-55), then a function is used at a type with other modes, then the function returns it (at t.stag:4:51-56), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:4:51-56) |}];
  locality
    {|
let f (p @ local) = fun (x : int) -> fun (y : int) -> y
let g (p @ local) = exclave_ f p
let h (p @ local) = exclave_ f p 1
let i (p @ local) = f p 1 2
let plus x = fun (y : int) -> Int.add x y
let j u = plus 1 2
let shadow = fun (x : int) -> fun (x : bool) -> x
|};
  [%expect
    {|
    val f : 'a @ local -> (int -> (int -> int) @ local) @ local
    val g : 'a @ local -> (int -> (int -> int) @ local) @ local
    val h : 'a @ local -> (int -> int) @ local
    val i : 'a @ local -> int
    val plus : int -> int -> int
    val j : 'a -> int
    val shadow : int -> bool -> bool
    |}];
  (* A function returning a function value, called with both arguments by a tail
     call, must return one that outlives the region. *)
  locality
    {|
let k f = f 1 2
let use u = k (fun (x : int) -> exclave_ stack_ (fun (y : int) -> Int.add x y))
|};
  [%expect
    {| This is local, because it is allocated by [stack_] (at t.stag:3:41-78), then it is part of the value (at t.stag:3:41-78), then the function returns it (at t.stag:3:32-78), but must be at most regional, because the function returns a function value, not the closure of a partial application (at t.stag:3:14-79), then the function types are unified, including whether their results may take arguments after the region ends (not printed), then a tail call gives it further arguments, after the current region has ended (mark the call [@nontail]) (at t.stag:2:10-13), then a function is used at a type with other modes |}]
;;

let%expect_test "locality in declarations" =
  locality
    {|
type r = { f : int -> int -> int; g : ptr[int] @ local -> int -> int }
let plus (p @ local) (x : int) = Int.add p.* x
let one p = Int.add p
let v = { f = Int.add; g = plus }
let w = { f = one; g = plus }
let h (r : r) = r.f 1 2
let i (r : r) (p @ local) = r.g p 2 [@nontail]
|};
  [%expect
    {|
    type r = { f : int -> int -> int; g : ptr[int] @ local -> (int -> int) @ local }
    val plus : ptr[int] @ local -> (int -> int) @ local
    val one : int -> int -> int
    val v : r
    val w : r
    val h : r -> int
    val i : r -> ptr[int] @ local -> int
    |}];
  locality
    {|
type r = { g : ptr[int] @ local -> int -> int }
let i (r : r) (p @ local) = r.g p 2
|};
  [%expect
    {| This is local, because a function type fixes it, as a function is used at a type with other modes, then a tail call gives it further arguments, after the current region has ended (mark the call [@nontail]) (at t.stag:3:28-33), but must be at most regional, because a function type fixes it, as the function types are unified |}];
  locality
    {|
type r = { f : int -> int -> int }
let mk (x : int) = exclave_ stack_ (fun (y : int) -> Int.add x y)
let v = { f = mk }
|};
  [%expect
    {|
    This expression has type int -> (int -> int) @ local but an expression was expected of type int -> int -> int
    Their modes differ: This is local, because it is allocated by [stack_] (at t.stag:3:28-65), then it is part of the value (at t.stag:3:28-65), then the function returns it (at t.stag:3:19-65), but must be at most global, because a function type fixes it, as a function is used at a type with other modes
    |}];
  locality
    {|
type r = { f : ptr[int] @ local -> int }
let f (p : ptr[int]) = p.*
let v = { f = f }
|};
  [%expect
    {|
    type r = { f : ptr[int] @ local -> int }
    val f : ptr[int] -> int
    val v : r
    |}];
  locality
    {|
type r = { f : ptr[int] @ local -> ptr[ptr[int]] }
let g (p : ptr[int]) = Ptr.make p
let w = { f = g }
|};
  [%expect
    {|
    This expression has type ptr[int] -> ptr[ptr[int]] but an expression was expected of type ptr[int] @ local -> ptr[ptr[int]]
    Their modes differ: This is local, because a function type fixes it, as a function is used at a type with other modes, but must be at most global, because Ptr.make holds it, then a function is used at a type with other modes, then it is passed to a function (at t.stag:3:23-33), then p is used (at t.stag:3:32-33), then p is passed to the function (at t.stag:3:23-33)
    |}];
  locality
    {|
type r = { f : int -> ptr[int] @ local }
let mk (x : int) = exclave_ stack_ (Ptr.make x)
let v = { f = mk }
let g (r : r) = let p = r.f 1 in p.*
|};
  [%expect
    {|
    type r = { f : int -> ptr[int] @ local }
    val mk : int -> ptr[int] @ local
    val v : r
    val g : r -> int
    |}];
  locality {|
type r = { f : int -> ptr[int] @ local }
let g (r : r) = r.f 1
|};
  [%expect
    {|
    type r = { f : int -> ptr[int] @ local }
    val g : r -> ptr[int] @ local
    |}]
;;

let%expect_test "arrows differing only in what they don't print" =
  locality
    {|
type r = { f : int -> (int -> int) @ local }
let k (f : int -> (int -> int) @ local) = f 1 2
let use (r : r) = k r.f
|};
  [%expect
    {|
    This expression has type int -> (int -> int) @ local but an expression was expected of type int -> (int -> int) @ local
    Their modes differ, in ways types don't print: This is local, because it is annotated local (at t.stag:3:33-38), then a function is used at a type with other modes, then a tail call gives it further arguments, after the current region has ended (mark the call [@nontail]) (at t.stag:3:42-45), then the function types are unified, including whether their results may take arguments after the region ends (not printed), but must be at most regional, because a function type fixes it, as the function types are unified
    |}]
;;

let%expect_test "a curried result less local than its arguments prints so" =
  locality
    {|
type r = { f : ptr[int] @ local -> (int -> int) @ global; g : ptr[int] @ local -> int -> int }
let same (x : int) = x
let f (p @ local) = same
let v = { f = f; g = f }
|};
  [%expect
    {|
    type r = { f : ptr[int] @ local -> (int -> int) @ global; g : ptr[int] @ local -> (int -> int) @ local }
    val same : int -> int
    val f : 'a @ local -> (int -> int) @ global
    val v : r
    |}];
  (* Read back, the printed types are the same. *)
  locality
    {|
type r = { f : ptr[int] @ local -> (int -> int) @ global; g : ptr[int] @ local -> (int -> int) @ local }
let same (x : int) = x
let f (p @ local) = same
let v : r = { f = f; g = f }
|};
  [%expect
    {|
    type r = { f : ptr[int] @ local -> (int -> int) @ global; g : ptr[int] @ local -> (int -> int) @ local }
    val same : int -> int
    val f : 'a @ local -> (int -> int) @ global
    val v : r
    |}];
  locality
    {|
type r = { f : ptr[int] @ local -> int -> int }
let same (x : int) = x
let f (p @ local) = same
let g (r : r) (p @ local) = Ptr.make (r.f p) [@nontail]
|};
  [%expect
    {| This is local, because a function type fixes it, as a function is used at a type with other modes, then the function returns it (at t.stag:5:37-44), then it is passed to a function (at t.stag:5:28-44), but must be at most global, because Ptr.make holds it, then a function is used at a type with other modes |}]
;;
