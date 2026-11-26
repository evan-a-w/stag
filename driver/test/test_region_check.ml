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
  [%expect {| (Returned (value 0) (on_heap 0) (in_regions 1)) |}]
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
     ("This is local, because it is annotated local (at t.stag:5:28-33), then x is used (at t.stag:5:62-63), then it is passed to a function (at t.stag:5:59-63), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:3:77-78), then x is used (at t.stag:3:77-78), then x is passed to the function (at t.stag:3:34-108), then a function is used at a type with other modes, then it is passed to a function (at t.stag:4:34-38), then x is used (at t.stag:4:37-38), then x is passed to the function (at t.stag:4:34-40), then a function is used at a type with other modes"))
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
  [%expect
    {|
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
  [%expect
    {|
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
  [%expect
    {|
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

let%expect_test "a let shadowing a parameter" =
  run
    ~entry:"use"
    {|
let t1 (n : int) = let n = Int.add n 1 in n
let use (u : unit) = t1 1
|};
  [%expect
    {|
    (Rejected
     ("n is used before its value is defined (a value may not depend on itself). Lets are recursive, so this n is the one being defined, not the n outside it: rename one of them"))
    |}]
;;

let%expect_test "a let's value using itself" =
  let source =
    {|
let go (u : unit) =
  let g = fun (n : int) -> match Int.equal n 0 with true -> Box.make 0 | false -> g (Int.sub n 1) in
  sum (g 2)
let poly (u : unit) =
  let g = fun x -> match Int.equal 0 0 with true -> x | false -> g x in
  sum (g (Box.make 3))
let fn (u : unit) =
  let f (n : int) = match Int.equal n 0 with true -> 0 | false -> f (Int.sub n 1) in
  f 2
|}
  in
  List.iter [ "go"; "poly"; "fn" ] ~f:(fun entry ->
    run (reviewed ^ source) ~entry);
  run
    (reviewed ^ "let boxed (u : unit) = let b = Box.make (sum b) in sum b")
    ~entry:"boxed";
  [%expect
    {|
    (Returned (value 0) (on_heap 1) (in_regions 1))
    (Returned (value 3) (on_heap 1) (in_regions 1))
    (Returned (value 0) (on_heap 1) (in_regions 0))
    (Rejected
     ("b is used before its value is defined (a value may not depend on itself)"))
    |}]
;;

let%expect_test "a scheme awaiting a toplevel, refined once it is finished" =
  (* [c] is finished, and placed, while [f], which its quote uses, is still
     being checked: its scheme is refined once [f] is, by [f]'s scheme, which
     stores its argument. [h] has the type of that use. *)
  let source =
    {|
let same (q : <'a>{..'r}) (x : 'a) = x
let f b = .~(let _ = c in .< (keep.* <- b; sum b) >.)
let c (u : unit) =
  (fun h ->
    let _ = (fun (v : unit) -> same .< f >. h) in
    Int.add (h (Box.make 1)) 0)
    (fun b -> sum b)
let use (u : unit) = Int.add (c ()) (f (Box.make 2))
|}
  in
  allocations_of (reviewed ^ source);
  run (reviewed ^ source) ~entry:"use";
  [%expect
    {|
    val keep : ptr[box[int]]
    val sum : box[int] @ _ -> int @ _
    val same : <'a>{..'b} @ 'm0 -> ('a @ 'm2 -> 'a @ 'm3) @ 'm1 with 'm0 <= 'm1 and 'm2 <= 'm3 and release(locality('m2)) <= locality('m3) and gc <= 'm1
    val f : box[int] @ 'm0 -> int @ _ with 'm0 <= global gc
    val c : unit @ _ -> int @ _
    val use : unit @ _ -> int @ _
    t.stag:2:27-48 Pointer in keep: heap
    t.stag:2:36-48 Box in keep: heap
    t.stag:5:37-38 Partial_application in same: heap, caller's region if res is local
    t.stag:8:2-10:31 Closure in c: heap
    t.stag:9:12-46 Closure in c: region
    t.stag:9:36-43 Code in c: heap
    t.stag:10:15-27 Box in c: region
    t.stag:11:4-20 Closure in c: heap
    t.stag:12:39-51 Box in use: heap
    (Returned (value 3) (on_heap 5) (in_regions 2))
    |}]
;;

let%expect_test "code spliced in twice is placed once for each copy" =
  let source =
    reviewed
    ^ {|
let code = .< Box.make 7 >.
let f (u : unit) = let b = .~code in (sum b, .~code)
let use (u : unit) = match f () with (n, c) -> keep.* <- c; Int.add n (sum keep.*)
|}
  in
  allocations_of source;
  run source ~entry:"use";
  [%expect
    {|
    val keep : ptr[box[int]]
    val sum : box[int] @ _ -> int @ _
    val code : <box[int]>{..'a}
    val f : unit @ _ -> int * box[int] @ 'm0 with gc <= 'm0
    val use : unit @ _ -> int @ _
    t.stag:2:27-48 Pointer in keep: heap
    t.stag:2:36-48 Box in keep: heap
    t.stag:5:11-27 Code in code: heap
    t.stag:5:14-24 Box in f: region
    t.stag:5:14-24 Box in f: heap, caller's region if res is local
    (Returned (value 14) (on_heap 3) (in_regions 1))
    |}]
;;

let%expect_test "a toplevel value is made for each of its instances" =
  let source =
    reviewed
    ^ {|
let p = ((fun (x : int) -> Box.make x), 1)
let local_use (u : unit) = match p with (mk, n) -> let b @ local = mk n in sum b [@nontail]
let global_use (u : unit) = match p with (mk, n) -> let b @ global = mk n in sum b
let use (u : unit) = Int.add (local_use ()) (global_use ())
|}
  in
  allocations_of source;
  run source ~entry:"use";
  [%expect
    {|
    val keep : ptr[box[int]]
    val sum : box[int] @ _ -> int @ _
    val p : (int @ _ -> box[int] @ 'm0) * int with gc <= 'm0
    val local_use : unit @ _ -> int @ _
    val global_use : unit @ _ -> int @ _
    val use : unit @ _ -> int @ _
    t.stag:2:27-48 Pointer in keep: heap
    t.stag:2:36-48 Box in keep: heap
    t.stag:5:9-38 Closure in p: heap
    t.stag:5:27-37 Box in p: heap, caller's region if component 0.res is local
    (Returned (value 2) (on_heap 3) (in_regions 1))
    |}]
;;

(* What a tail call returns is made in the caller's region of the function
   making it: [via]'s Box, by [wrap], a search's part of a local argument, and a
   recursive builder's Box; each wrong if the tail call didn't end the region
   first. *)
let%expect_test "tail calls returning allocations" =
  let source =
    {|
let sum (b : box[int]) = match b with *n -> n
let wrap (x : int) = Box.make x
let via (x : int) = wrap x
let count (n : int) (x : int) =
  match Int.equal n 0 with true -> Box.make x | false -> count (Int.sub n 1) (Int.add x 1)
let pick (n : int) (b : box[int]) =
  match Int.equal n 0 with true -> b | false -> pick (Int.sub n 1) b
let use (u : unit) =
  let a @ local = via 1 in
  let c @ local = count 3 1 in
  let p @ local = pick 2 (stack_ (Box.make 7)) in
  Int.add (sum a) (Int.add (sum c) (sum p)) [@nontail]
|}
  in
  allocations_of source;
  run source ~entry:"use";
  print_s
    [%sexp
      (Region_check.run ~tail_calls_end_regions:false source ~entry:"use"
       : Region_check.Outcome.t)];
  [%expect
    {|
    val sum : box[int] @ _ -> int @ _
    val wrap : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val via : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val count : int @ 'm0 -> (int @ 'm2 -> box[int] @ 'm3) @ 'm1 with portable <= 'm0 and 'm0 <= 'm1 and portable <= 'm2 and gc <= 'm3 and gc <= 'm1
    val pick : int @ 'm0 -> (box[int] @ 'm2 -> box[int] @ 'm3) @ 'm1 with portable <= 'm0 and 'm0 <= 'm1 and portable <= 'm2 and 'm2 <= 'm3 and release(locality('m2)) <= locality('m3) and gc <= 'm1
    val use : unit @ _ -> int @ _
    t.stag:3:21-31 Box in wrap: heap, caller's region if res is local
    t.stag:6:2-90 Partial_application in count: heap, caller's region if res is local
    t.stag:6:35-45 Box in count: heap, caller's region if res.res is local
    t.stag:8:2-68 Partial_application in pick: heap, caller's region if res is local
    t.stag:12:33-45 Box in use: region
    (Returned (value 12) (on_heap 0) (in_regions 3))
    (Use_after_free
     "a reference, from the result of a call of toplevel via, of the Box at t.stag:3:21-31, made in the region of a call of toplevel via, after it ended")
    |}]
;;

let%expect_test "tail calls giving functions, and an annotated tail call" =
  let source =
    {|
let sum (b : box[int]) = match b with *n -> n
let two (a : int) (b : int) = Int.add a b
let annotated (g : int -> (int -> int) @ local) (x : int) = g x
let inferred g (x : int) = g x
let partial (x : int) = two x
let adder (a : int) = exclave_ fun (b : int) -> Int.add a b
let lb (x : int) = exclave_ Box.make x
let at_global (x : int) = (lb x : box[int] @ global)
let either (x : int) = lb x
let use (u : unit) =
  let h1 @ local = annotated two 1 in
  let h2 @ local = inferred two 2 in
  let h3 @ local = partial 3 in
  let h4 @ local = inferred adder 4 in
  let a @ local = at_global 5 in
  let b @ local = either 6 in
  Int.add (Int.add (h1 1) (Int.add (h2 1) (Int.add (h3 1) (h4 1)))) (Int.add (sum a) (sum b)) [@nontail]
|}
  in
  allocations_of source;
  run source ~entry:"use";
  [%expect
    {|
    val sum : box[int] @ _ -> int @ _
    val two : int @ 'm0 -> (int @ _ -> int @ _) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    val annotated : (int -> (int -> int) @ local) @ 'm0 -> (int @ _ -> (int -> int) @ 'm2) @ 'm1 with 'm0 <= 'm1 and local nonportable gc <= 'm2 and gc <= 'm1
    val inferred : (int @ 'm1 -> 'a @ 'm2) @ 'm0 -> (int @ _ -> 'a @ 'm4) @ 'm3 with 'm0 <= 'm3 and portable <= 'm1 and 'm2 <= 'm4 and gc <= 'm3
    val partial : int @ _ -> (int @ _ -> int @ _) @ 'm0 with gc <= 'm0
    val adder : int @ _ -> (int @ _ -> int @ _) @ 'm0 with gc <= 'm0
    val lb : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val at_global : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val either : int @ _ -> box[int] @ 'm0 with gc <= 'm0
    val use : unit @ _ -> int @ _
    t.stag:3:30-41 Partial_application in two: heap, caller's region if res is local
    t.stag:4:60-63 Partial_application in annotated: heap, caller's region if res is local
    t.stag:5:27-30 Partial_application in inferred: heap, caller's region if res is local
    t.stag:7:31-59 Closure in adder: heap, caller's region if res is local
    t.stag:8:28-38 Box in lb: heap, caller's region if res is local
    (Returned (value 25) (on_heap 1) (in_regions 5))
    |}]
;;

(* [Core.Map]'s code, built and searched in a caller's region and in a
   [region_]: its nodes are made there, and nothing is used after it ends. *)
let%expect_test "maps built in regions" =
  let map = In_channel.read_all "../../core/map.stag" in
  let uses =
    {|
let cmp (a : int) (b : int) = Int.sub a b
let build (n : int) =
  let go (i : int) (m : M.t[int, int, gc]) =
    match Int.equal i 0 with true -> m | false -> go (Int.sub i 1) (M.add cmp m i (Int.mul i i))
  in
  go n M.empty
let local_use (u : unit) =
  let m @ local = M.add cmp (M.add cmp (M.add cmp M.empty 1 10) 2 20) 3 30 in
  match M.find cmp m 2 with Some v -> v | None -> 0
let removed (u : unit) =
  let m @ local = M.remove cmp (M.add cmp (M.add cmp M.empty 1 10) 2 20) 1 in
  M.cardinal m [@nontail]
let in_region (u : unit) =
  region_ (let m = M.add cmp (M.add cmp M.empty 5 50) 6 60 in
           match M.find cmp m 6 with Some v -> v | None -> 0)
let built (u : unit) = match M.find cmp (build 5) 4 with Some v -> v | None -> 0
let sum (l : list[int]) =
  let go (l : list[int]) (acc : int) =
    match l with [] -> acc | Cons *(x, rest) -> go rest (Int.add acc x)
  in
  go l 0
let folded (u : unit) =
  let m @ local = M.add cmp (M.add cmp (M.add cmp M.empty 1 10) 2 20) 3 30 in
  let l @ local = M.fold (fun k v acc -> Cons (Box.make (Int.add k v, acc))) m [] in
  sum l [@nontail]
let updated (u : unit) =
  let m @ local = M.add cmp (M.add cmp (M.add cmp M.empty 1 10) 2 20) 3 30 in
  let m2 @ local = M.update cmp m 2 (fun o -> match o with Some v -> Some (Int.add v 1) | None -> None) in
  let m3 @ local = M.filter (fun k v -> Int.gt k 1) (M.map (fun k v -> Int.mul v 2) m2) in
  M.fold (fun k v acc -> Int.add acc v) m3 0 [@nontail]
|}
  in
  List.iter
    [ "local_use"; "removed"; "in_region"; "built"; "folded"; "updated" ]
    ~f:(fun entry ->
      run ("module M = struct\n" ^ map ^ "\nend\n" ^ uses) ~entry);
  [%expect
    {|
    (Returned (value 20) (on_heap 0) (in_regions 7))
    (Returned (value 1) (on_heap 1) (in_regions 3))
    (Returned (value 60) (on_heap 0) (in_regions 3))
    (Returned (value 16) (on_heap 16) (in_regions 0))
    (Returned (value 66) (on_heap 1) (in_regions 11))
    (Returned (value 102) (on_heap 0) (in_regions 18))
    |}]
;;

(* [Core.Lru]'s code, a cache used in a [region_]: its record is made there (its
   nodes, stored through pointers, are on the heap). *)
let%expect_test "lru caches in regions" =
  let map = In_channel.read_all "../../core/map.stag" in
  let lru = In_channel.read_all "../../core/lru.stag" in
  let uses =
    {|
let cmp (a : int) (b : int) = Int.sub a b
let in_region (u : unit) =
  region_ (
    let c = Lru.create 2 cmp in
    Lru.add c 1 10; Lru.add c 2 20; Lru.add c 3 30;
    match Lru.find c 3 with Some v -> v | None -> 0)
|}
  in
  run
    ("module Map = struct\n"
     ^ map
     ^ "\nend\nmodule Lru = struct\n"
     ^ lru
     ^ "\nend\n"
     ^ uses)
    ~entry:"in_region";
  [%expect {| (Returned (value 30) (on_heap 11) (in_regions 4)) |}]
;;
