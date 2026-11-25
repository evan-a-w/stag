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
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
    t.stag:3:21-31 Pointer in cell: heap if global, caller's region if local
    t.stag:5:18-30 Box in wrap: heap if global, caller's region if local
    t.stag:6:22-32 Box in pair: heap if global, caller's region if local
    t.stag:6:34-56 Box in pair: heap if global, caller's region if local
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
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
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
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
    t.stag:5:29-39 Box in kept: region
    t.stag:5:51-67 Box in kept: heap
    t.stag:6:35-45 Box in local_only: heap
    t.stag:7:38-50 Box in forced: region
    t.stag:8:46-58 Box in always_caller: caller's region
    t.stag:9:45-55 Box in in_exclave: caller's region
    t.stag:9:59-75 Box in in_exclave: heap if global, caller's region if local
    t.stag:10:33-43 Box in captured: heap
    t.stag:10:47-81 Closure in captured: heap if global, caller's region if local
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
    t.stag:3:73-116 Box in range_exclave: heap if global, caller's region if local
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
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
    ----
    This is local, because it is annotated local (at t.stag:4:40-45), then b is used (at t.stag:4:56-57), then the [region_] returns it (at t.stag:4:31-58), then the [region_] gives it (at t.stag:4:23-58), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:23-58)
    This is local, because it is annotated local (at t.stag:4:40-45), then b is used (at t.stag:4:56-57), but must be at most regional, because the [region_] returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:31-58)
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val call : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
    ----
    This is local, because it is annotated local (at t.stag:4:30-35), then b is used (at t.stag:4:50-51), but must be at most regional, because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) (at t.stag:4:50-51)
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val early : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val early_leak : unit @ _ -> box[int] @ 'm0 with gc <= 'm0
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val outer : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
    t.stag:4:72-82 Box in outer: region
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val outer_kept : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
    t.stag:4:76-88 Box in outer_kept: caller's region
    ----
    This is local, because it is annotated local (at t.stag:4:36-41), then o is used (at t.stag:4:70-71), then it is passed to a function (at t.stag:4:60-72), then a function is used at a type with other modes, then Box.make holds it, then a function is used at a type with other modes, then the function returns it (at t.stag:4:60-72), then the [region_] returns it (at t.stag:4:60-72), then the [region_] gives it (at t.stag:4:52-72), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:28-72)
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val nested : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
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
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
    ----
    This is local, because it is annotated local (at t.stag:4:78-83), then b is used (at t.stag:4:108-109), then it is assigned to c (at t.stag:4:103-109), but must be at most regional, because c outlives the [region_], which it is assigned in (at t.stag:4:103-109)
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val closure : unit @ _ -> int @ _
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
    t.stag:4:66-100 Closure in closure: region
    ----
    This is local, because it is annotated local (at t.stag:4:47-52), then b is captured by a closure (at t.stag:4:95-96), then the closure is made (at t.stag:4:63-97), then the [region_] returns it (at t.stag:4:38-98), then the [region_] gives it (at t.stag:4:30-98), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:30-98)
    This is local, because it is annotated local (at t.stag:4:47-52), then b is captured by a closure (at t.stag:4:95-96), then the closure is made (at t.stag:4:63-97), but must be at most regional, because the [region_] returns it, ending its region (return it with [exclave_] instead) (at t.stag:4:38-98)
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val in_closure : unit @ 'm0 -> (int @ _ -> int @ _) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
    ----
    val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val sum : box[int] @ _ -> int @ _
    val gives : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    t.stag:2:19-29 Box in mk: heap if global, caller's region if local
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
