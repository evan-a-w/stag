open! Import
open Mode_helpers

let lib =
  {|
let mk (x : int) = Box.make x
let sum (b : box[int]) = match b with *n -> n
|}
;;

let%expect_test "an allocation returned is made where its caller uses it" =
  allocations
    {|
let mk (x : int) = Box.make x
let cell (x : int) = Ptr.make x
let adder (n : int) = fun (x : int) -> Int.add n x
let wrap x = Some (Box.make x)
let pair (x : int) = (Box.make x, Box.make (Int.add x 1))
|};
  [%expect
    {|
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val cell : int @ _ -> ptr[int @@ 'm1] @ 'm0 with portable <= 'm1 and nonportable gc <= 'm0
    val adder : int @ 'm0 -> (int @ _ -> int @ _) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    val wrap : 'a @ 'm0 -> option[box['a]] @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and gc <= 'm1
    val pair : int @ _ -> box[int] * box[int] @ 'm0 with gc <= 'm0
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    t.stag:3:21-31 Pointer in cell: heap, caller's region if res is local
    t.stag:4:22-50 Partial_application in adder: heap, caller's region if res is local
    t.stag:5:18-30 Box in wrap: heap, caller's region if res is local
    t.stag:6:22-32 Box in pair: heap, caller's region if res is local
    t.stag:6:34-56 Box in pair: heap, caller's region if res is local
    |}]
;;

let%expect_test "a caller picks: on the heap, or in its own region" =
  allocations
    (lib
     ^ {|
let on_heap (u : unit) = mk 1
let stored : ptr[box[int]] = Ptr.make (mk 0)
let store (u : unit) = stored.* <- mk 2
let in_region (u : unit) = let b @ local = mk 1 in sum b [@nontail]
let inferred (u : unit) = let b = mk 1 in sum b
let passed_on (u : unit) = exclave_ mk 1
|}
    );
  [%expect
    {|
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val on_heap : unit @ _ -> box[int] @ 'm0 with gc <= 'm0
    val stored : ptr[box[int]]
    val store : unit @ _ -> unit @ _
    val in_region : unit @ _ -> int @ _
    val inferred : unit @ _ -> int @ _
    val passed_on : unit @ _ -> box[int] @ 'm0 with gc <= 'm0
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    t.stag:6:29-44 Pointer in stored: heap
    |}]
;;

let%expect_test "a local instance can't escape" =
  List.iter
    ~f:(fun source -> allocations (lib ^ source))
    [ {|let escapes (u : unit) = let b @ local = mk 1 in b|}
    ; {|let stored : ptr[box[int]] = Ptr.make (mk 0)
let store (u : unit) = let b @ local = mk 1 in stored.* <- b|}
    ; {|let returned (u : unit) = stack_ (Box.make 1)|}
    ];
  [%expect
    {|
    This is local, because it is annotated local (at t.stag:4:33-38), then b is used (at t.stag:4:49-50), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:25-50)
    This is local, because it is annotated local (at t.stag:5:31-36), then b is used (at t.stag:5:59-60), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:5:59-60)
    This is local, because it is allocated by [stack_] (at t.stag:4:26-45), then it is part of the value (at t.stag:4:26-45), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:26-45)
    |}]
;;

let%expect_test "what isn't returned is made in the region, or on the heap" =
  allocations
    (lib
     ^ {|
let kept (x : int) = let b = Box.make x in let c = Box.make (sum b) in c
let local_only (x : int) = let b = Box.make x in sum b
let forced (x : int) = let b = stack_ (Box.make x) in sum b [@nontail]
let always_caller (x : int) = exclave_ stack_ (Box.make x)
let in_exclave (x : int) = exclave_ (let b = Box.make x in Box.make (sum b))
let captured (x : int) = let b = Box.make x in fun (y : int) -> Int.add (sum b) y
|}
    );
  [%expect
    {|
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val kept : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val local_only : int @ _ -> int @ _
    val forced : int @ _ -> int @ _
    val always_caller : int @ _ -> box[int] @ 'm0 with gc local <= 'm0
    val in_exclave : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val captured : int @ _ -> (int @ _ -> int @ _) @ 'm0 with gc <= 'm0
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    t.stag:5:29-39 Box in kept: region
    t.stag:5:51-67 Box in kept: heap
    t.stag:6:35-45 Box in local_only: heap
    t.stag:7:38-50 Box in forced: region
    t.stag:8:46-58 Box in always_caller: caller's region
    t.stag:9:45-55 Box in in_exclave: caller's region
    t.stag:9:59-75 Box in in_exclave: heap, caller's region if res is local
    t.stag:10:33-43 Box in captured: heap
    t.stag:10:47-81 Closure in captured: heap, caller's region if res is local
    |}]
;;

let%expect_test "recursion: a list built where the caller wants it" =
  (* Without [exclave_], the recursive call's list is in this call's region,
     which ends as it returns, so only a global list can be built. *)
  allocations
    {|
let range (n : int) = match n with 0 -> Nil | _ -> Cons (Box.make (n, range (Int.sub n 1)))
let range_exclave (n : int) = match n with 0 -> Nil | _ -> exclave_ Cons (Box.make (n, range_exclave (Int.sub n 1)))
let length l = match l with Nil -> 0 | Cons *(_, rest) -> Int.add 1 (length rest)
let on_heap (u : unit) = range 3
let in_region (u : unit) = let l @ local = range 3 in let m @ local = range_exclave 3 in Int.add (length l) (length m) [@nontail]
let sum_region (u : unit) = region_ (let l @ local = range 3 in length l)
|};
  [%expect
    {|
    val range : int @ 'm0 -> list[int] @ 'm1 with portable <= 'm0 and gc <= 'm1 and 'm1 <= global
    val range_exclave : int @ 'm0 -> list[int] @ 'm1 with portable <= 'm0 and gc <= 'm1
    val length : list['a] @ 'm0 -> int @ _ with portable <= 'm0
    val on_heap : unit @ _ -> list[int] @ 'm0 with gc <= 'm0
    val in_region : unit @ _ -> int @ _
    val sum_region : unit @ _ -> int @ _
    t.stag:2:56-91 Box in range: heap
    t.stag:3:73-116 Box in range_exclave: heap, caller's region if res is local
    |}]
;;

let%expect_test "region_" =
  List.iter
    ~f:(fun source ->
      print_endline "----";
      allocations (lib ^ source))
    [ (* What is local to it is freed when it ends. *)
      {|let count (u : unit) = region_ (let b @ local = mk 1 in sum b)|}
    ; {|let leaks (u : unit) = region_ (let b @ local = mk 1 in b)|}
    ; (* A call at its end is not a tail call: the region ends after it. *)
      {|let call (u : unit) = region_ (let b @ local = mk 1 in sum b)|}
    ; {|let tail (u : unit) = let b @ local = mk 1 in sum b|}
    ; (* [exclave_] ends it early, giving a value in the region around it. *)
      {|let early (u : unit) = let n @ local = region_ (exclave_ mk 1) in sum n [@nontail]|}
    ; {|let early_leak (u : unit) = region_ (exclave_ mk 1)|}
    ; (* What is local outside it outlives it. *)
      {|let outer (u : unit) = let o @ local = mk 1 in region_ (let i @ local = Box.make o in sum (match i with *b -> b))|}
    ; {|let outer_kept (u : unit) = let o @ local = mk 1 in let r @ local = region_ (Box.make o) in 0|}
    ; {|let outer_leak (u : unit) = let o @ local = mk 1 in region_ (Box.make o)|}
    ; (* Nested. *)
      {|let nested (u : unit) = region_ (let a @ local = mk 1 in region_ (let b @ local = mk 2 in Int.add (sum a) (sum b)))|}
    ; {|let nested_leak (u : unit) = region_ (let a @ local = mk 1 in region_ (let b @ local = mk 2 in a))|}
    ; {|let nested_inner_leak (u : unit) = region_ (let a @ local = mk 1 in let c @ local = region_ (let b @ local = mk 2 in b) in 0)|}
    ; (* Assigning out of it. *)
      {|let assigned (u : unit) = let mut c @ local = mk 0 in region_ (let b @ local = mk 1 in c <- b; 0)|}
    ; {|let assigned_ok (u : unit) = let o @ local = mk 0 in let mut c @ local = mk 0 in region_ (c <- o; 0)|}
    ; {|let assigned_nested (u : unit) = let mut c @ local = mk 0 in region_ (let b @ local = mk 1 in region_ (c <- b; 0))|}
    ; (* Closures inside it. *)
      {|let closure (u : unit) = region_ (let b @ local = mk 1 in let f = fun (x : int) -> Int.add x (sum b) in f 2 [@nontail])|}
    ; {|let closure_leak (u : unit) = region_ (let b @ local = mk 1 in fun (x : int) -> Int.add x (sum b))|}
    ; (* A region in a closure. *)
      {|let in_closure (u : unit) = fun (x : int) -> region_ (let b @ local = mk x in sum b)|}
    ; (* Its value is the function's: allocations at its end are made in the
         region around it. *)
      {|let gives (x : int) = region_ (let b @ local = mk x in Box.make (sum b))|}
    ];
  [%expect
    {|
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val count : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    ----
    This is local, because it is annotated local (at t.stag:4:40-45), then b is used (at t.stag:4:56-57), then the [region_] returns it (at t.stag:4:31-58), then the [region_] gives it (at t.stag:4:23-58), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:23-58)
    This is local, because it is annotated local (at t.stag:4:40-45), then b is used (at t.stag:4:56-57), but must be at most regional, because the [region_] returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:31-58)
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val call : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    ----
    This is local, because it is annotated local (at t.stag:4:30-35), then b is used (at t.stag:4:50-51), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:4:50-51)
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val early : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val early_leak : unit @ _ -> box[int] @ 'm0 with gc <= 'm0
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val outer : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    t.stag:4:72-82 Box in outer: region
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val outer_kept : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    t.stag:4:76-88 Box in outer_kept: caller's region
    ----
    This is local, because it is annotated local (at t.stag:4:36-41), then o is used (at t.stag:4:70-71), then it is passed to a function (at t.stag:4:60-72), then a function is used at a type with other modes, then Box.make holds it, then a function is used at a type with other modes, then the function returns it (at t.stag:4:60-72), then the [region_] returns it (at t.stag:4:60-72), then the [region_] gives it (at t.stag:4:52-72), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:28-72)
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val nested : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    ----
    This is local, because it is annotated local (at t.stag:4:46-51), then a is used (at t.stag:4:95-96), then the [region_] returns it (at t.stag:4:70-97), then the [region_] gives it (at t.stag:4:62-97), then the [region_] returns it (at t.stag:4:37-98), then the [region_] gives it (at t.stag:4:29-98), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:29-98)
    This is local, because it is annotated local (at t.stag:4:46-51), then a is used (at t.stag:4:95-96), then the [region_] returns it (at t.stag:4:70-97), then the [region_] gives it (at t.stag:4:62-97), but must be at most regional, because the [region_] returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:37-98)
    ----
    This is local, because it is annotated local (at t.stag:4:101-106), then b is used (at t.stag:4:117-118), but must be at most regional, because the [region_] returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:92-119)
    ----
    This is local, because it is annotated local (at t.stag:4:71-76), then b is used (at t.stag:4:92-93), then it is assigned to c (at t.stag:4:87-93), but must be at most regional, because c outlives the [region_], which it is assigned in (at t.stag:4:87-93)
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val assigned_ok : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    t.stag:4:53-100 Cell in assigned_ok: region
    ----
    This is local, because it is annotated local (at t.stag:4:78-83), then b is used (at t.stag:4:108-109), then it is assigned to c (at t.stag:4:103-109), but must be at most regional, because c outlives the [region_], which it is assigned in (at t.stag:4:103-109)
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val closure : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    t.stag:4:66-100 Closure in closure: region
    ----
    This is local, because it is annotated local (at t.stag:4:47-52), then b is captured by a closure (at t.stag:4:95-96), then the closure is made (at t.stag:4:63-97), then the [region_] returns it (at t.stag:4:38-98), then the [region_] gives it (at t.stag:4:30-98), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:30-98)
    This is local, because it is annotated local (at t.stag:4:47-52), then b is captured by a closure (at t.stag:4:95-96), then the closure is made (at t.stag:4:63-97), but must be at most regional, because the [region_] returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:38-98)
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val in_closure : unit @ 'm0 -> (int @ _ -> int @ _) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    t.stag:4:28-84 Partial_application in in_closure: heap, caller's region if res is local
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val gives : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    t.stag:4:55-71 Box in gives: heap
    |}]
;;

let%expect_test "region_ runs" =
  Option.iter ~f:Source_helpers.eval_and_print_values
  @@ Source_helpers.typecheck
       {|
let mk (x : int) = Box.make x
let sum (b : box[int]) = match b with *n -> n
let total = region_ (let a @ local = mk 1 in let b @ local = mk 2 in Int.add (sum a) (sum b))
let early = sum (region_ (exclave_ mk 3))
|};
  [%expect {|
    (mk <fun>)
    (sum <fun>)
    (total 3)
    (early 3)
    |}]
;;

let%expect_test "placements hold in every instance" =
  allocations
    (lib
     ^ {|
let keep : ptr[box[int]] = Ptr.make (Box.make 0)
let f g = Int.add 0 (g (Box.make 1))
let use2 (u : unit) = f (fun b -> keep.* <- b; 0)
let inner (x : int) = let b = Box.make x in fun (y : int) -> Box.make (Int.add y (sum b))
let use (u : unit) = let g = inner 1 in keep.* <- g 2
let letpoly (x : int) =
  let g = fun (y : int) -> Box.make y in
  let a @ local = g 1 in
  keep.* <- g 2;
  sum a [@nontail]
|}
    );
  [%expect
    {|
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val keep : ptr[box[int]]
    val f : (box[int] @ 'm0 -> int @ _) @ _ -> int @ _ with gc <= 'm0
    val use2 : unit @ _ -> int @ _
    val inner : int @ _ -> (int @ _ -> box[int] @ 'm1) @ 'm0 with gc <= 'm1 and gc <= 'm0
    val use : unit @ _ -> unit @ _
    val letpoly : int @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap, caller's region if res is local
    t.stag:5:27-48 Pointer in keep: heap
    t.stag:5:36-48 Box in keep: heap
    t.stag:6:23-35 Box in f: heap, region if arg.arg is local
    t.stag:7:24-49 Closure in use2: heap
    t.stag:8:30-40 Box in inner: heap
    t.stag:8:44-89 Closure in inner: heap, caller's region if res is local
    t.stag:8:61-89 Box in inner: heap, caller's region if res.res is local
    t.stag:11:10-37 Closure in letpoly.g: region
    t.stag:11:27-37 Box in letpoly.g: heap, caller's region if res is local
    |}]
;;

let%expect_test "the cell of a [let mut] outlives what captures it" =
  allocations
    {|
let f1 (u : unit) = let mut c = 0 in fun (z : int) -> c
let f2 (u : unit) = let g @ local = region_ (let mut c = 0 in fun (z : int) -> c) in g 1 [@nontail]
let f3 (u : unit) = let mut c = 0 in exclave_ fun (z : int) -> c
let f4 (u : unit) = let mut c = 0 in let g @ local = region_ (exclave_ fun (z : int) -> c) in g 1 [@nontail]
let f5 (u : unit) = let mut c = 0 in let g @ local = fun (z : int) -> c in g 1 [@nontail]
let f6 (u : unit) = let mut c = 0 in c <- 1; c
|};
  [%expect
    {|
    val f1 : unit @ _ -> (int @ _ -> int @ _) @ 'm0 with nonportable gc <= 'm0
    val f2 : unit @ _ -> int @ _
    val f3 : unit @ _ -> (int @ _ -> int @ _) @ 'm0 with nonportable gc <= 'm0
    val f4 : unit @ _ -> int @ _
    val f5 : unit @ _ -> int @ _
    val f6 : unit @ _ -> int @ _
    t.stag:2:20-55 Cell in f1: heap
    t.stag:2:37-55 Closure in f1: heap, caller's region if res is local
    t.stag:3:44-81 Cell in f2: heap
    t.stag:3:62-80 Closure in f2: caller's region
    t.stag:4:20-64 Cell in f3: heap
    t.stag:4:46-64 Closure in f3: heap, caller's region if res is local
    t.stag:5:20-108 Cell in f4: region
    t.stag:5:71-89 Closure in f4: caller's region
    t.stag:6:20-89 Cell in f5: region
    t.stag:6:53-71 Closure in f5: region
    t.stag:7:20-46 Cell in f6: region
    |}]
;;

let%expect_test "partial applications are made by the caller" =
  allocations
    {|
let add2 (a : box[int]) (b : int) = b
let p1 (u : unit) = add2 (Box.make 1)
let adder (n : int) = fun (x : int) -> Int.add n x
let three (a : int) (b : int) (c : int) = Int.add a (Int.add b c)
|};
  [%expect
    {|
    val add2 : box[int] @ 'm0 -> (int @ _ -> int @ _) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    val p1 : unit @ _ -> (int @ _ -> int @ _) @ 'm0 with gc <= 'm0
    val adder : int @ 'm0 -> (int @ _ -> int @ _) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    val three : int @ 'm0 -> (int @ 'm2 -> (int @ _ -> int @ _) @ 'm3) @ 'm1 with 'm0 <= 'm3 and 'm0 <= 'm1 and 'm2 <= 'm3 and gc <= 'm3 and gc <= 'm1
    t.stag:2:36-37 Partial_application in add2: heap, caller's region if res is local
    t.stag:3:25-37 Box in p1: heap
    t.stag:4:22-50 Partial_application in adder: heap, caller's region if res is local
    t.stag:5:42-65 Partial_application in three: heap, caller's region if res is local
    t.stag:5:42-65 Partial_application in three: heap, caller's region if res.res is local
    |}]
;;

let%expect_test "what a returned box boxes is returned too" =
  allocations
    {|
let two (u : unit) = [1; 2]
let nested (x : int) = Box.make (Box.make x, 1)
let apply f x = f x
let boxed (x : int) = apply Box.make x
|};
  [%expect
    {|
    val two : unit @ _ -> list[int] @ 'm0 with gc <= 'm0
    val nested : int @ _ -> box[box[int] * int] @ 'm0 with gc <= 'm0
    val apply : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> ('a @ 'm4 -> 'b @ 'm5) @ 'm3 with 'm0 <= 'm3 and portable <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= regional and stage('m4) <= stage('m1) and provenance('m4) <= provenance('m1) and meet(locality('m4), regional) <= locality('m1) and gc <= 'm3
    val boxed : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    t.stag:2:21-27 Box in two: heap, caller's region if res is local
    t.stag:2:23-27 Box in two: heap, caller's region if res is local
    t.stag:3:23-47 Box in nested: heap, caller's region if res is local
    t.stag:3:33-43 Box in nested: heap, caller's region if res is local
    t.stag:4:16-19 Partial_application in apply: heap, caller's region if res is local
    |}]
;;

(* [Region] is the innermost region around the allocation: the function's or a
   [region_]'s; [Caller_region] is the one around that. *)
let%expect_test "placements are relative to the innermost region" =
  allocations
    {|
let sum (b : box[int]) = match b with *n -> n
let f (x : int) = region_ (exclave_ (let n = sum (Box.make 1) in Box.make x))
let g (x : int) = exclave_ (let n = sum (Box.make 1) in region_ (let m = sum (Box.make 2) in exclave_ Box.make x))
let h (x : int) = exclave_ (let n = sum (stack_ (Box.make 1)) in stack_ (Box.make x))
|};
  [%expect
    {|
    val sum : box[int] @ _ -> int @ _
    val f : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val g : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val h : int @ _ -> box[int] @ 'm0 with gc local <= 'm0
    t.stag:3:49-61 Box in f: caller's region
    t.stag:3:65-75 Box in f: heap
    t.stag:4:40-52 Box in g: caller's region
    t.stag:4:77-89 Box in g: region
    t.stag:4:102-112 Box in g: heap, caller's region if res is local
    t.stag:5:48-60 Box in h: caller's region
    t.stag:5:72-84 Box in h: caller's region
    |}]
;;

let reviewed =
  {|
let keep : ptr[box[int]] = Ptr.make (Box.make 0)
let sum (b : box[int]) = match b with *n -> n
|}
;;

(* Placed once the toplevel is checked: a [let]'s sites may be constrained by
   the code after it. *)
let%expect_test "sites are placed once their toplevel is checked" =
  allocations
    (reviewed
     ^ {|
let keep2 : ptr[int -> int] = Ptr.make (fun (x : int) -> x)
let f (u : unit) = let g = fun (y : int) -> y in keep2.* <- g
let f2 (u : unit) = let g = fun (y : int) -> y in g
let f3 (u : unit) = let g = (fun (y : int) -> y, fun (z : int) -> z) in g
|}
    );
  [%expect
    {|
    val keep : ptr[box[int]]
    val sum : box[int] @ _ -> int @ _
    val keep2 : ptr[int -> int]
    val f : unit @ _ -> unit @ _
    val f2 : unit @ _ -> (int @ _ -> int @ _) @ _
    val f3 : unit @ _ -> (int @ _ -> int * (int @ _ -> int @ _) @ _) @ _
    t.stag:2:27-48 Pointer in keep: heap
    t.stag:2:36-48 Box in keep: heap
    t.stag:5:30-59 Pointer in keep2: heap
    t.stag:5:39-59 Closure in keep2: heap
    t.stag:6:27-45 Closure in f.g: heap
    t.stag:7:28-46 Closure in f2.g: heap
    t.stag:8:28-68 Closure in f3.g: heap
    t.stag:8:49-67 Closure in f3.g: heap, caller's region if res is local
    |}]
;;

(* A site of a [let] may follow a side of the function around it. *)
let%expect_test "sites follow the sides of the owners around them" =
  allocations
    (reviewed
     ^ {|
let f h = let g (y : int) = exclave_ h (Box.make y) in Int.add (g 1) 0
let use (u : unit) = f (fun b -> keep.* <- b; 0)
let f' h = let g = fun (y : int) -> exclave_ h (Box.make y) in Int.add (g 1) 0
|}
    );
  [%expect
    {|
    val keep : ptr[box[int]]
    val sum : box[int] @ _ -> int @ _
    val f : (box[int] @ 'm0 -> int @ _) @ _ -> int @ _ with gc <= 'm0
    val use : unit @ _ -> int @ _
    val f' : (box[int] @ 'm0 -> int @ _) @ _ -> int @ _ with gc <= 'm0
    t.stag:2:27-48 Pointer in keep: heap
    t.stag:2:36-48 Box in keep: heap
    t.stag:5:10-70 Closure in f: region
    t.stag:5:39-51 Box in f.g: heap, caller's region if f's arg.arg is local
    t.stag:6:23-48 Closure in use: heap
    t.stag:7:19-59 Closure in f': region
    t.stag:7:47-59 Box in f': heap, caller's region if arg.arg is local
    |}]
;;

let%expect_test "sides in records, variants and long curried types" =
  allocations
    (reviewed
     ^ {|
let v (u : unit) = `A (fun (y : int) -> Box.make y)
let r (u : unit) = {`g = fun (y : int) -> Box.make y}
let usev (u : unit) = match v () with `A g -> keep.* <- g 1
let c (u : unit) = .< fun (y : int) -> y >.
let many (a1 : int) (a2 : int) (a3 : int) (a4 : int) (a5 : int) (a6 : int) (a7 : int) (a8 : int) (a9 : int) (a10 : int) (a11 : int) (a12 : int) (a13 : int) (a14 : int) (a15 : int) (a16 : int) (a17 : int) (a18 : int) (a19 : int) = a1
|}
    );
  [%expect
    {|
    val keep : ptr[box[int]]
    val sum : box[int] @ _ -> int @ _
    val v : unit @ _ -> [`A of int @ _ -> box[int] @ 'm0; ..'a] @ _ with gc <= 'm0
    val r : unit @ _ -> {`g : int @ _ -> box[int] @ 'm0} @ _ with gc <= 'm0
    val usev : unit @ _ -> unit @ _
    val c : unit @ _ -> <int @ _ -> int @ _>{..'a} @ _
    val many : int @ 'm0 -> (int @ 'm2 -> (int @ 'm4 -> (int @ 'm6 -> (int @ 'm8 -> (int @ 'm10 -> (int @ 'm12 -> (int @ 'm14 -> (int @ 'm16 -> (int @ 'm18 -> (int @ 'm20 -> (int @ 'm22 -> (int @ 'm24 -> (int @ 'm26 -> (int @ 'm28 -> (int @ 'm30 -> (int @ 'm32 -> (int @ 'm34 -> (int @ _ -> int @ _) @ 'm35) @ 'm33) @ 'm31) @ 'm29) @ 'm27) @ 'm25) @ 'm23) @ 'm21) @ 'm19) @ 'm17) @ 'm15) @ 'm13) @ 'm11) @ 'm9) @ 'm7) @ 'm5) @ 'm3) @ 'm1 with 'm0 <= 'm35 and 'm0 <= 'm33 and 'm0 <= 'm31 and 'm0 <= 'm29 and 'm0 <= 'm27 and 'm0 <= 'm25 and 'm0 <= 'm23 and 'm0 <= 'm21 and 'm0 <= 'm19 and 'm0 <= 'm17 and 'm0 <= 'm15 and 'm0 <= 'm13 and 'm0 <= 'm11 and 'm0 <= 'm9 and 'm0 <= 'm7 and 'm0 <= 'm5 and 'm0 <= 'm3 and 'm0 <= 'm1 and 'm2 <= 'm35 and 'm2 <= 'm33 and 'm2 <= 'm31 and 'm2 <= 'm29 and 'm2 <= 'm27 and 'm2 <= 'm25 and 'm2 <= 'm23 and 'm2 <= 'm21 and 'm2 <= 'm19 and 'm2 <= 'm17 and 'm2 <= 'm15 and 'm2 <= 'm13 and 'm2 <= 'm11 and 'm2 <= 'm9 and 'm2 <= 'm7 and 'm2 <= 'm5 and 'm2 <= 'm3 and 'm4 <= 'm35 and 'm4 <= 'm33 and 'm4 <= 'm31 and 'm4 <= 'm29 and 'm4 <= 'm27 and 'm4 <= 'm25 and 'm4 <= 'm23 and 'm4 <= 'm21 and 'm4 <= 'm19 and 'm4 <= 'm17 and 'm4 <= 'm15 and 'm4 <= 'm13 and 'm4 <= 'm11 and 'm4 <= 'm9 and 'm4 <= 'm7 and 'm4 <= 'm5 and 'm6 <= 'm35 and 'm6 <= 'm33 and 'm6 <= 'm31 and 'm6 <= 'm29 and 'm6 <= 'm27 and 'm6 <= 'm25 and 'm6 <= 'm23 and 'm6 <= 'm21 and 'm6 <= 'm19 and 'm6 <= 'm17 and 'm6 <= 'm15 and 'm6 <= 'm13 and 'm6 <= 'm11 and 'm6 <= 'm9 and 'm6 <= 'm7 and 'm8 <= 'm35 and 'm8 <= 'm33 and 'm8 <= 'm31 and 'm8 <= 'm29 and 'm8 <= 'm27 and 'm8 <= 'm25 and 'm8 <= 'm23 and 'm8 <= 'm21 and 'm8 <= 'm19 and 'm8 <= 'm17 and 'm8 <= 'm15 and 'm8 <= 'm13 and 'm8 <= 'm11 and 'm8 <= 'm9 and 'm10 <= 'm35 and 'm10 <= 'm33 and 'm10 <= 'm31 and 'm10 <= 'm29 and 'm10 <= 'm27 and 'm10 <= 'm25 and 'm10 <= 'm23 and 'm10 <= 'm21 and 'm10 <= 'm19 and 'm10 <= 'm17 and 'm10 <= 'm15 and 'm10 <= 'm13 and 'm10 <= 'm11 and 'm12 <= 'm35 and 'm12 <= 'm33 and 'm12 <= 'm31 and 'm12 <= 'm29 and 'm12 <= 'm27 and 'm12 <= 'm25 and 'm12 <= 'm23 and 'm12 <= 'm21 and 'm12 <= 'm19 and 'm12 <= 'm17 and 'm12 <= 'm15 and 'm12 <= 'm13 and 'm14 <= 'm35 and 'm14 <= 'm33 and 'm14 <= 'm31 and 'm14 <= 'm29 and 'm14 <= 'm27 and 'm14 <= 'm25 and 'm14 <= 'm23 and 'm14 <= 'm21 and 'm14 <= 'm19 and 'm14 <= 'm17 and 'm14 <= 'm15 and 'm16 <= 'm35 and 'm16 <= 'm33 and 'm16 <= 'm31 and 'm16 <= 'm29 and 'm16 <= 'm27 and 'm16 <= 'm25 and 'm16 <= 'm23 and 'm16 <= 'm21 and 'm16 <= 'm19 and 'm16 <= 'm17 and 'm18 <= 'm35 and 'm18 <= 'm33 and 'm18 <= 'm31 and 'm18 <= 'm29 and 'm18 <= 'm27 and 'm18 <= 'm25 and 'm18 <= 'm23 and 'm18 <= 'm21 and 'm18 <= 'm19 and 'm20 <= 'm35 and 'm20 <= 'm33 and 'm20 <= 'm31 and 'm20 <= 'm29 and 'm20 <= 'm27 and 'm20 <= 'm25 and 'm20 <= 'm23 and 'm20 <= 'm21 and 'm22 <= 'm35 and 'm22 <= 'm33 and 'm22 <= 'm31 and 'm22 <= 'm29 and 'm22 <= 'm27 and 'm22 <= 'm25 and 'm22 <= 'm23 and 'm24 <= 'm35 and 'm24 <= 'm33 and 'm24 <= 'm31 and 'm24 <= 'm29 and 'm24 <= 'm27 and 'm24 <= 'm25 and 'm26 <= 'm35 and 'm26 <= 'm33 and 'm26 <= 'm31 and 'm26 <= 'm29 and 'm26 <= 'm27 and 'm28 <= 'm35 and 'm28 <= 'm33 and 'm28 <= 'm31 and 'm28 <= 'm29 and 'm30 <= 'm35 and 'm30 <= 'm33 and 'm30 <= 'm31 and 'm32 <= 'm35 and 'm32 <= 'm33 and 'm34 <= 'm35 and gc <= 'm35 and gc <= 'm33 and gc <= 'm31 and gc <= 'm29 and gc <= 'm27 and gc <= 'm25 and gc <= 'm23 and gc <= 'm21 and gc <= 'm19 and gc <= 'm17 and gc <= 'm15 and gc <= 'm13 and gc <= 'm11 and gc <= 'm9 and gc <= 'm7 and gc <= 'm5 and gc <= 'm3 and gc <= 'm1
    t.stag:2:27-48 Pointer in keep: heap
    t.stag:2:36-48 Box in keep: heap
    t.stag:5:22-51 Closure in v: heap, caller's region if res is local
    t.stag:5:40-50 Box in v: heap, caller's region if res.A.res is local
    t.stag:6:25-52 Closure in r: heap, caller's region if res is local
    t.stag:6:42-52 Box in r: heap, caller's region if res.g.res is local
    t.stag:8:19-43 Code in c: heap, caller's region if res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res.res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res.res.res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res.res.res.res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res.res.res.res.res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res.res.res.res.res.res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res.res.res.res.res.res.res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res.res.res.res.res.res.res.res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res.res.res.res.res.res.res.res.res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res.res.res.res.res.res.res.res.res.res.res.res.res is local
    t.stag:9:230-232 Partial_application in many: heap, caller's region if res.res.res.res.res.res.res.res.res.res.res.res.res.res.res.res.res.res is local
    |}]
;;
