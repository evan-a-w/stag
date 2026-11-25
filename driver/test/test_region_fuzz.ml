open! Import

(* Random functions passing allocations through closures, higher-order
   parameters, generalized [let]s, tuples, records and variants holding
   closures, cells captured by closures, partial applications, [region_],
   [exclave_] and [stack_], and stores to globals; run by [Region_check] from
   callers using what they give at [global] and at [local], and passing them
   closures that keep what they are given. None uses an allocation after its
   region ends. *)

let header =
  {|
let keep : ptr[box[int]] = Ptr.make (Box.make 0)
let keepc : ptr[int -> box[int]] = Ptr.make (fun (y : int) -> Box.make y)
let sum (b : box[int]) = match b with *n -> n
let mk2 (a : int) (b : int) = Box.make (Int.add a b)
|}
;;

module Gen = struct
  type t =
    { random : Random.State.t
    ; mutable fresh : int
    }

  let pick t choices =
    List.nth_exn choices (Random.State.int t.random (List.length choices))
  ;;

  let name t prefix =
    t.fresh <- t.fresh + 1;
    sprintf "%s%d" prefix t.fresh
  ;;

  let rec int t ~ints depth =
    match if depth <= 0 then 0 else Random.State.int t.random 4 with
    | 0 -> pick t ints
    | 1 -> "1"
    | 2 -> sprintf "(sum %s)" (box t ~ints (depth - 1) ~tail:false)
    | _ -> sprintf "(Int.add %s %s)" (int t ~ints (depth - 1)) (pick t ints)

  (* A closure of type [int -> box[int]]. *)
  and closure t ~ints depth =
    match if depth <= 0 then 0 else Random.State.int t.random 4 with
    | 0 -> "h"
    | 1 -> sprintf "(mk2 %s)" (int t ~ints (depth - 1))
    | _ ->
      let y = name t "y" in
      sprintf
        "(fun (%s : int) -> %s)"
        y
        (box t ~ints:(y :: ints) (depth - 1) ~tail:true)

  (* An expression of type [box[int]]; [tail] if in a function's tail. *)
  and box t ~ints depth ~tail = "(" ^ box_ t ~ints depth ~tail ^ ")"

  and box_ t ~ints depth ~tail =
    let sub ?(tail = tail) () = box t ~ints (depth - 1) ~tail in
    let int () = int t ~ints (depth - 1) in
    let closure () = closure t ~ints (depth - 1) in
    match
      if depth <= 0
      then Random.State.int t.random 2
      else Random.State.int t.random 17
    with
    | 0 -> sprintf "Box.make %s" (pick t ints)
    | 1 -> sprintf "stack_ (Box.make %s)" (pick t ints)
    | 2 -> if tail then sprintf "exclave_ %s" (sub ()) else sub ()
    | 3 -> sprintf "region_ (%s)" (sub ~tail:false ())
    | 4 -> sprintf "%s %s" (closure ()) (int ())
    | 5 ->
      let g = name t "g" in
      sprintf
        "(let %s = %s in let n = sum (%s %s) in %s %s)"
        g
        (closure ())
        g
        (int ())
        g
        (int ())
    | 6 ->
      let g = name t "g"
      and y = name t "y" in
      sprintf
        "(let %s (%s : int) = %s in let n = sum (%s %s) in %s %s)"
        g
        y
        (box t ~ints:(y :: ints) (depth - 1) ~tail:true)
        g
        (pick t ints)
        g
        (int ())
    | 7 ->
      let a = name t "a"
      and b = name t "b" in
      sprintf
        "(match (%s, %s) with (%s, %s) -> %s %s)"
        (closure ())
        (closure ())
        a
        b
        (pick t [ a; b ])
        (int ())
    | 8 ->
      let g = name t "g" in
      sprintf "(match `A %s with `A %s -> %s %s)" (closure ()) g g (int ())
    | 9 ->
      let g = name t "g" in
      sprintf
        "(match {`g = %s} with {`g = %s} -> %s %s)"
        (closure ())
        g
        g
        (int ())
    | 10 -> sprintf "h %s" (int ())
    | 11 ->
      let c = name t "c"
      and k = name t "k"
      and y = name t "y" in
      sprintf
        "(let mut %s = %s in let %s = fun (%s : int) -> Box.make (Int.add %s \
         %s) in %s <- 2; %s %s)"
        c
        (int ())
        k
        y
        y
        c
        c
        k
        (int ())
    | 12 -> sprintf "(keep.* <- %s; %s)" (sub ~tail:false ()) (sub ())
    | 13 -> sprintf "(keepc.* <- %s; %s)" (closure ()) (sub ())
    | 14 ->
      let p = name t "p" in
      sprintf "(let %s = mk2 %s in %s %s)" p (int ()) p (int ())
    | 15 ->
      sprintf
        "Box.make (k (fun (z : int) -> %s))"
        (box t ~ints:("z" :: ints) (depth - 1) ~tail:true)
    | _ ->
      let n = name t "n" in
      sprintf
        "(let %s @ local = %s in Box.make (sum %s))"
        n
        (sub ~tail:false ())
        n
  ;;

  let program random =
    let t = { random; fresh = 0 } in
    sprintf "let f h k (x : int) = %s\n" (box t ~ints:[ "x" ] 4 ~tail:true)
  ;;
end

(* Callers of [f], each its own entry. *)
let uses =
  [ ( "g"
    , "let g (u : unit) = let r @ global = f (fun (y : int) -> Box.make y) \
       (fun j -> sum (j 1)) 1 in sum r" )
  ; ( "l"
    , "let l (u : unit) = let r @ local = f (fun (y : int) -> Box.make y) (fun \
       j -> sum (j 1)) 1 in sum r [@nontail]" )
  ; ( "s"
    , "let s (u : unit) = let r @ local = f (fun (y : int) -> let b = Box.make \
       y in keep.* <- b; b) (fun j -> keepc.* <- j; 0) 1 in sum r [@nontail]" )
  ; ( "p"
    , "let p (u : unit) = let r = f (mk2 1) (fun j -> sum (stack_ (Box.make \
       (sum (j 2))))) 2 in keep.* <- r; sum keep.*" )
  ; ( "t"
    , "let t (u : unit) = let r @ local = f (fun (y : int) -> exclave_ \
       Box.make y) (fun j -> let b @ local = j 1 in sum b [@nontail]) 1 in sum \
       r [@nontail]" )
  ]
;;

let fuzz ?override ~seed ~runs () =
  let random = Random.State.make [| seed |] in
  let outcomes = String.Table.create () in
  let count key = Hashtbl.incr outcomes key in
  let failures = Queue.create () in
  let regions = ref 0 in
  for _ = 1 to runs do
    let body = Gen.program random in
    List.iter uses ~f:(fun (entry, use) ->
      let source = header ^ body ^ use ^ "\n" in
      match Region_check.run ?override source ~entry with
      | Rejected _ -> count "rejected"
      | Returned { in_regions; _ } ->
        regions := !regions + in_regions;
        count "ran"
      | Use_after_free message ->
        count "use after free";
        Queue.enqueue failures (source ^ message)
      | Unsupported message ->
        count "unsupported";
        Queue.enqueue failures (source ^ message))
  done;
  ( failures
  , Hashtbl.to_alist outcomes |> List.sort ~compare:[%compare: string * int]
  , !regions )
;;

let%expect_test "no allocation is used after its region ends" =
  let failures, outcomes, regions = fuzz ~seed:7 ~runs:300 () in
  Queue.iter failures ~f:print_endline;
  print_s [%message (outcomes : (string * int) list) (regions : int)];
  [%expect {| ((outcomes ((ran 920) (rejected 580))) (regions 4839)) |}]
;;

(* The same programs, with what follows a side made as if the side were always
   [local]: what the review found the placements did. *)
let%expect_test "with placements that ignore their instance, some are" =
  let override (allocation : Typed.Allocation.t) =
    match allocation.follows with
    | Some { if_local; _ } ->
      { allocation with placement = if_local; follows = None }
    | None -> allocation
  in
  let failures, outcomes, _ = fuzz ~override ~seed:7 ~runs:300 () in
  print_s [%message (outcomes : (string * int) list)];
  print_endline (Option.value_exn (Queue.peek failures));
  [%expect
    {|
    (outcomes ((ran 683) (rejected 580) ("use after free" 237)))

    let keep : ptr[box[int]] = Ptr.make (Box.make 0)
    let keepc : ptr[int -> box[int]] = Ptr.make (fun (y : int) -> Box.make y)
    let sum (b : box[int]) = match b with *n -> n
    let mk2 (a : int) (b : int) = Box.make (Int.add a b)
    let f h k (x : int) = ((keepc.* <- (mk2 x); (h x)))
    let g (u : unit) = let r @ global = f (fun (y : int) -> Box.make y) (fun j -> sum (j 1)) 1 in sum r
    a reference, from a toplevel, of the Partial_application at t.stag:5:30-52, made in the region of a call of Partial_application at t.stag:6:22-51, after it ended
    |}]
;;
