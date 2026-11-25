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
     "a reference, from a toplevel, to the Closure at t.stag:6:27-45, made in the region of a call of toplevel f, which ends first")
    (Returned (value 1) (on_heap 1) (in_regions 0))
    (Use_after_free
     "a reference, from the result of a call of toplevel f2, of the Closure at t.stag:7:28-46, made in the region of a call of toplevel f2, after it ended")
    (Returned (value 2) (on_heap 2) (in_regions 0))
    (Use_after_free
     "a reference, from the result of a call of toplevel f3, of the Closure at t.stag:8:28-68, made in the region of a call of toplevel f3, after it ended")
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
     "a reference, from a toplevel, to the Box at t.stag:5:39-51, made in the region of a call of toplevel f, which ends first")
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
     "a reference, from a toplevel, to the Box at t.stag:5:40-50, made in the region of a call of toplevel usev, which ends first")
    (Returned (value "()") (on_heap 3) (in_regions 1))
    (Use_after_free
     "a reference, from a toplevel, to the Box at t.stag:6:42-52, made in the region of a call of toplevel user, which ends first")
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
    (Returned (value 1) (on_heap 3) (in_regions 0))
    (Use_after_free
     "a reference, from a toplevel, to the Partial_application at t.stag:6:230-232, made in the region of a call of toplevel usem, which ends first")
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

(* With every site of [kind] (in toplevel [in_]) made at [placement], whatever
   the record says. *)
let forcing ?in_ kind placement (allocation : Typed.Allocation.t) =
  if Typed.Allocation.Kind.equal allocation.kind kind
     && Option.for_all in_ ~f:(String.equal allocation.owner.toplevel.name)
  then { allocation with placement; follows = None }
  else allocation
;;

let%expect_test "a global left pointing into a region, though overwritten" =
  let source =
    {|
let zero = Box.make 0
let keepg : ptr[unit -> int] = Ptr.make (fun (u : unit) -> 0)
let stored (u : unit) = keep.* <- Box.make 3; keep.* <- zero; 0
let captured (u : unit) =
  let b = Box.make 3 in
  keepg.* <- (fun (u : unit) -> sum b);
  keepg.* <- (fun (u : unit) -> 1);
  0
|}
  in
  List.iter [ "stored"; "captured" ] ~f:(fun entry ->
    run (reviewed ^ source) ~entry;
    run (reviewed ^ source) ~entry ~override:(forcing ~in_:entry Box Region));
  [%expect
    {|
    (Returned (value 0) (on_heap 4) (in_regions 0))
    (Use_after_free
     "a reference, from a toplevel, to the Box at t.stag:7:34-44, made in the region of a call of toplevel stored, which ends first")
    (Returned (value 0) (on_heap 5) (in_regions 0))
    (Use_after_free
     "a reference, from a toplevel, to the Box at t.stag:9:10-20, made in the region of a call of toplevel captured, which ends first")
    |}]
;;

let%expect_test "a closure called in tail position, its region ended first" =
  let source =
    {|
let main (u : unit) =
  let n = sum (Box.make 3) in
  let f = fun (a : int) -> Int.add a n in
  f 2
|}
  in
  run (reviewed ^ source) ~entry:"main";
  run (reviewed ^ source) ~entry:"main" ~override:(forcing Closure Region);
  [%expect
    {|
    (Returned (value 5) (on_heap 1) (in_regions 1))
    (Use_after_free
     "a call of the Closure at t.stag:7:10-38, made in the region of a call of toplevel main, after it ended")
    |}]
;;

let%expect_test "a toplevel's initializer has a region of its own" =
  let source =
    {|
let g = let b = Box.make 1 in fun (u : unit) -> sum b
let x = let b = Box.make 1 in sum b
let main (u : unit) = Int.add (g ()) x
|}
  in
  allocations_of (reviewed ^ source);
  run (reviewed ^ source) ~entry:"main";
  run (reviewed ^ source) ~entry:"main" ~override:(forcing Box Region);
  [%expect {|
    val keep : ptr[box[int]]
    val sum : box[int] @ _ -> int @ _
    val g : unit @ '_m0 -> int @ '_m1
    val x : int
    val main : unit @ _ -> int @ _
    t.stag:2:27-48 Pointer in keep: heap
    t.stag:2:36-48 Box in keep: heap
    t.stag:5:16-26 Box in g: heap
    t.stag:5:30-53 Closure in g: heap
    t.stag:6:16-26 Box in x: region
    (Returned (value 2) (on_heap 2) (in_regions 1))
    (Use_after_free
     "a reference, from a toplevel, of the Box at t.stag:5:16-26, made in the initializer of g, after it ended")
    |}]
;;

let%expect_test "functions of functions, and saturated applications" =
  let source =
    {|
let adder (n : int) = fun (x : int) -> Box.make (Int.add n x)
let three (a : int) (b : int) (c : int) = Box.make (Int.add a (Int.add b c))
let main (u : unit) =
  let a @ local = adder 1 in
  let b @ local = a 2 in
  let p @ local = three 1 2 in
  let c @ local = p 3 in
  let d @ local = three 1 2 3 in
  Int.add (sum b) (Int.add (sum c) (sum d)) [@nontail]
|}
  in
  allocations_of (reviewed ^ source);
  run (reviewed ^ source) ~entry:"main";
  [%expect {|
    val keep : ptr[box[int]]
    val sum : box[int] @ _ -> int @ _
    val adder : int @ 'm0 -> (int @ _ -> box[int] @ 'm2) @ 'm1 with 'm0 <= 'm1 and gc <= 'm2 and gc <= 'm1
    val three : int @ 'm0 -> (int @ 'm2 -> (int @ _ -> box[int] @ 'm4) @ 'm3) @ 'm1 with 'm0 <= 'm3 and 'm0 <= 'm1 and 'm2 <= 'm3 and gc <= 'm4 and gc <= 'm3 and gc <= 'm1
    val main : unit @ _ -> int @ _
    t.stag:2:27-48 Pointer in keep: heap
    t.stag:2:36-48 Box in keep: heap
    t.stag:5:22-61 Partial_application in adder: heap, caller's region if res is local
    t.stag:5:39-61 Box in adder: heap, caller's region if res.res is local
    t.stag:6:42-76 Partial_application in three: heap, caller's region if res is local
    t.stag:6:42-76 Partial_application in three: heap, caller's region if res.res is local
    t.stag:6:42-76 Box in three: heap, caller's region if res.res.res is local
    (Returned (value 15) (on_heap 0) (in_regions 5))
    |}]
;;

let%expect_test "prims and allocators used as values" =
  let source =
    {|
let apply (f : int -> box[int]) (x : int) = f x
let boxed (x : int) = apply Box.make x
let plus (u : unit) = Int.add 1
let partly (n : int) = let g = Int.add n in g 2
let main (u : unit) = Int.add (sum (boxed 1)) (Int.add (plus () 2) (partly 3))
|}
  in
  allocations_of (reviewed ^ source);
  run (reviewed ^ source) ~entry:"main";
  run
    (reviewed ^ source)
    ~entry:"main"
    ~override:(forcing Partial_application Region);
  [%expect {|
    val keep : ptr[box[int]]
    val sum : box[int] @ _ -> int @ _
    val apply : (int -> box[int]) @ 'm0 -> (int @ _ -> box[int] @ 'm2) @ 'm1 with 'm0 <= 'm1 and nonportable gc <= 'm2 and gc <= 'm1
    val boxed : int @ _ -> box[int] @ 'm0 with nonportable gc <= 'm0
    val plus : unit @ _ -> (int @ _ -> int) @ 'm0 with gc <= 'm0
    val partly : int @ _ -> int @ _
    val main : unit @ _ -> int @ _
    t.stag:2:27-48 Pointer in keep: heap
    t.stag:2:36-48 Box in keep: heap
    t.stag:5:44-47 Partial_application in apply: heap, caller's region if res is local
    t.stag:6:28-36 Box in boxed: heap
    t.stag:7:22-31 Partial_application in plus: heap, caller's region if res is local
    t.stag:8:31-40 Partial_application in partly: heap
    (Returned (value 9) (on_heap 2) (in_regions 1))
    (Use_after_free
     "a reference, from the result of a call of toplevel plus, of the Partial_application at t.stag:7:22-31, made in the region of a call of toplevel plus, after it ended")
    |}]
;;
