open! Import

let allocations_of source = Mode_helpers.allocations source

let run ?override source ~entry =
  print_s
    [%sexp (Region_check.run ?override source ~entry : Region_check.Outcome.t)]
;;

let%expect_test "a value made in the caller's region outlives the call" =
  run
    ~entry:"use"
    {|
let mk (x : int) = Box.make x
let sum (b : box[int]) = match b with *n -> n
let use (u : unit) = let b @ local = mk 1 in Int.add (sum b) 0
|};
  [%expect {| (Returned (value 1) (on_heap 0) (in_regions 1)) |}]
;;

let reviewed =
  {|
let keep : ptr[box[int]] = Ptr.make (Box.make 0)
let sum (b : box[int]) = match b with *n -> n
|}
;;

(* Each with the placements the typechecker gives, then with those it gave
   before, which the review found wrong: the sites at [buggy], placed there in
   every instance. *)
let both source ~entry ~buggy =
  let override (allocation : Typed.Allocation.t) =
    match
      List.Assoc.find buggy (Loc.to_string allocation.loc) ~equal:String.equal
    with
    | Some placement -> { allocation with placement; follows = None }
    | None -> allocation
  in
  run (reviewed ^ source) ~entry;
  run (reviewed ^ source) ~entry ~override
;;

let%expect_test "a let's closure, used after its let" =
  let source =
    {|
let keep2 : ptr[int -> int] = Ptr.make (fun (x : int) -> x)
let f (u : unit) = let g = fun (y : int) -> y in keep2.* <- g
let f2 (u : unit) = let g = fun (y : int) -> y in g
let f3 (u : unit) = let g = (fun (y : int) -> y, fun (z : int) -> z) in g
let use1 (u : unit) = f (); keep2.* 1
let use2 (u : unit) = let h = f2 () in h 1
let use3 (u : unit) = match f3 () 1 with (a, b) -> b 2
|}
  in
  both source ~entry:"use1" ~buggy:[ "t.stag:6:27-45", Region ];
  both source ~entry:"use2" ~buggy:[ "t.stag:7:28-46", Region ];
  both source ~entry:"use3" ~buggy:[ "t.stag:8:28-68", Region ];
  [%expect
    {|
    (Returned (value 1) (on_heap 3) (in_regions 0))
    (Use_after_free
     "a call of the Closure at t.stag:6:27-45, made in the region of a call of toplevel f, after it ended")
    (Returned (value 1) (on_heap 1) (in_regions 0))
    (Use_after_free
     "a call of the Closure at t.stag:7:28-46, made in the region of a call of toplevel f2, after it ended")
    (Returned (value 2) (on_heap 2) (in_regions 0))
    (Use_after_free
     "a call of the Closure at t.stag:8:28-68, made in the region of a call of toplevel f3, after it ended")
    |}]
;;

let%expect_test "a let's allocation, following its function's argument" =
  both
    {|
let f h = let g (y : int) = exclave_ h (Box.make y) in Int.add (g 1) 0
let use (u : unit) = f (fun b -> keep.* <- b; 0)
|}
    ~entry:"use"
    ~buggy:[ "t.stag:5:39-51", Caller_region ];
  [%expect
    {|
    (Returned (value 0) (on_heap 4) (in_regions 1))
    (Use_after_free
     "a reference, from a toplevel, of the Box at t.stag:5:39-51, made in the region of a call of toplevel f, after it ended")
    |}]
;;

let%expect_test "allocations in structural records and variants" =
  let source =
    {|
let v (u : unit) = `A (fun (y : int) -> Box.make y)
let r (u : unit) = {`g = fun (y : int) -> Box.make y}
let usev (u : unit) = match v () with `A g -> keep.* <- g 1
let user (u : unit) = match r () with {`g = g} -> keep.* <- g 1
|}
  in
  both source ~entry:"usev" ~buggy:[ "t.stag:5:40-50", Caller_region ];
  both source ~entry:"user" ~buggy:[ "t.stag:6:42-52", Caller_region ];
  [%expect
    {|
    (Returned (value "()") (on_heap 3) (in_regions 1))
    (Use_after_free
     "a reference, from a toplevel, of the Box at t.stag:5:40-50, made in the region of a call of toplevel usev, after it ended")
    (Returned (value "()") (on_heap 3) (in_regions 1))
    (Use_after_free
     "a reference, from a toplevel, of the Box at t.stag:6:42-52, made in the region of a call of toplevel user, after it ended")
    |}]
;;

let%expect_test "partial applications deep in a curried type" =
  both
    {|
let keepf : ptr[int -> int] = Ptr.make (fun (x : int) -> x)
let many (a1 : int) (a2 : int) (a3 : int) (a4 : int) (a5 : int) (a6 : int) (a7 : int) (a8 : int) (a9 : int) (a10 : int) (a11 : int) (a12 : int) (a13 : int) (a14 : int) (a15 : int) (a16 : int) (a17 : int) (a18 : int) (a19 : int) = a1
let usem (u : unit) = keepf.* <- many 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18; keepf.* 19
|}
    ~entry:"usem"
    ~buggy:[ "t.stag:6:230-232", Caller_region ];
  [%expect
    {|
    (Returned (value 1) (on_heap 3) (in_regions 17))
    (Use_after_free
     "a reference, from a toplevel, of the Partial_application at t.stag:6:230-232, made in the region of a call of toplevel usem, after it ended")
    |}]
;;

let%expect_test "a let's instances each place its allocations" =
  let source =
    {|
let sum (b : box[int]) = match b with *n -> n
let keep : ptr[box[int]] = Ptr.make (Box.make 0)
let f (u : unit) =
  let g (y : int) = Box.make y in
  let b @ local = g 1 in
  keep.* <- g 2;
  sum b [@nontail]
|}
  in
  allocations_of source;
  run source ~entry:"f";
  [%expect
    {|
    val sum : box[int] @ _ -> int @ _
    val keep : ptr[box[int]]
    val f : unit @ _ -> int @ _
    t.stag:3:27-48 Pointer in keep: heap
    t.stag:3:36-48 Box in keep: heap
    t.stag:5:2-8:18 Closure in f: region
    t.stag:5:20-30 Box in f.g: heap, caller's region if res is local
    (Returned (value 1) (on_heap 3) (in_regions 2))
    |}]
;;

let%expect_test "a recursive group, its result used locally" =
  run
    ~entry:"c"
    {|
let b (n : int) = match Int.equal n 0 with true -> Box.make n | false -> a (Int.sub n 1)
let a (n : int) = match Int.equal n 0 with true -> Box.make n | false -> b (Int.sub n 1)
let c (u : unit) = let x @ local = a 3 in match x with *n -> n
|};
  [%expect {| (Returned (value 0) (on_heap 1) (in_regions 0)) |}]
;;

let%expect_test "a recursive group's argument stored globally" =
  run
    ~entry:"c2"
    {|
let keep : ptr[box[int]] = Ptr.make (Box.make 0)
let b2 (x : box[int]) (n : int) = match Int.equal n 0 with true -> keep.* <- x | false -> a2 x (Int.sub n 1)
let a2 (x : box[int]) (n : int) = b2 x n
let c2 (u : unit) = let x @ local = stack_ (Box.make 1) in a2 x 3 [@nontail]
|};
  [%expect
    {|
    (Rejected
     ("This is local, because it is allocated by [stack_] (at t.stag:5:36-55), then it is part of the value (at t.stag:5:36-55), then it is bound by an annotated let (at t.stag:5:28-33), then x is used (at t.stag:5:62-63), then it is passed to a function (at t.stag:5:59-63), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:3:77-78), then x is used (at t.stag:3:77-78), then x is passed to the function (at t.stag:3:34-108), then a function is used at a type with other modes, then it is passed to a function (at t.stag:4:34-38), then x is used (at t.stag:4:37-38), then x is passed to the function (at t.stag:4:34-40), then a function is used at a type with other modes"))
    |}]
;;
