open! Import

(* Random programs passing allocations through closures, curried and partially
   applied functions (of toplevels, of closures they return, and of a curried
   parameter), higher-order parameters, toplevels calling each other and
   recursing, generalized [let]s, tuples, records and variants holding closures
   (also returned from toplevels), cells holding and captured by closures,
   [region_], [exclave_] (also of a [let]) and [stack_], tail calls returning
   allocations (also of recursive builders and searches), and stores to globals;
   run by [Region_check] from callers using what they give at [global] and at
   [local], and passing them closures that keep what they are given. None uses
   an allocation after its region ends. *)

let header =
  {|
let keep : ptr[box[int]] = Ptr.make (Box.make 0)
let keepc : ptr[int -> box[int]] = Ptr.make (fun (y : int) -> Box.make y)
let sum (b : box[int]) = match b with *n -> n
let mk2 (a : int) (b : int) = Box.make (Int.add a b)
let wrap (x : int) = Box.make x
let via (x : int) = wrap x
let count (n : int) (x : int) =
  match Int.equal n 0 with true -> Box.make x | false -> count (Int.sub n 1) (Int.add x 1)
let pick (n : int) (b : box[int]) =
  match Int.equal n 0 with true -> b | false -> pick (Int.sub n 1) b
let ev (n : int) = match Int.equal n 0 with true -> Box.make 1 | false -> od (Int.sub n 1)
let od (n : int) = match Int.equal n 0 with true -> Box.make 0 | false -> ev (Int.sub n 1)
let h (y : int) = Box.make y
let k (j : int -> box[int]) = sum (j 1)
let m (a : int) (b : int) = Box.make (Int.add a b)
|}
;;

module Gen = struct
  (* What a generated toplevel gives, given an [int]. *)
  module Shape = struct
    type t =
      | Curried (** [int -> int -> box[int]] *)
      | Returns_closure (** [int -> int -> box[int]], its body a function *)
      | Pair (** [int -> (int -> box[int]) * int] *)
      | Variant (** [int -> [`C of int -> box[int]]] *)
      | Recursive (** [int -> box[int]], recursing *)
  end

  type t =
    { random : Random.State.t
    ; mutable fresh : int
    ; mutable helpers : (string * Shape.t) list
    ; mutable reading_keepc : String.Set.t
    (** The helpers that may call what [keepc] holds. *)
    ; mutable storing : bool
    (** Whether what is made may be stored in [keepc]: it then mustn't call what
        [keepc] holds, which would loop forever. *)
    }

  let helpers t =
    if t.storing
    then
      List.filter t.helpers ~f:(fun (name, _) ->
        not (Set.mem t.reading_keepc name))
    else t.helpers
  ;;

  let storing t ~f =
    let was = t.storing in
    t.storing <- true;
    let result = f () in
    t.storing <- was;
    result
  ;;

  let pick t choices =
    List.nth_exn choices (Random.State.int t.random (List.length choices))
  ;;

  (* [rare] (a [stack_] part, which returning is rejected) one time in six, else
     one of [else_]. *)
  let rarely t rare ~else_ =
    if Random.State.int t.random 6 = 0 then rare else pick t else_
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
    match if depth <= 0 then 0 else Random.State.int t.random 6 with
    | 0 -> if t.storing then "h" else pick t [ "h"; "keepc.*" ]
    | 1 -> sprintf "(mk2 %s)" (int t ~ints (depth - 1))
    | 2 -> sprintf "(m %s)" (int t ~ints (depth - 1))
    | 3 ->
      (match
         List.filter (helpers t) ~f:(fun (_, shape) ->
           match shape with
           | Curried | Returns_closure -> true
           | Pair | Variant | Recursive -> false)
       with
       | [] -> "h"
       | helpers ->
         sprintf "(%s %s)" (fst (pick t helpers)) (int t ~ints (depth - 1)))
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
      else Random.State.int t.random 35
    with
    | 0 -> sprintf "Box.make %s" (pick t ints)
    | 1 -> sprintf "stack_ (Box.make %s)" (pick t ints)
    | 2 -> if tail then sprintf "exclave_ %s" (sub ()) else sub ()
    | 3 -> sprintf "region_ (%s)" (sub ~tail:false ())
    | 4 -> sprintf "%s %s" (closure ()) (int ())
    | 5 ->
      let g = name t "g"
      and v = name t "v" in
      sprintf
        "(let %s = %s in let %s = sum (%s %s) in %s %s)"
        g
        (closure ())
        v
        g
        (int ())
        g
        (int ())
    | 6 ->
      let g = name t "g"
      and y = name t "y"
      and v = name t "v" in
      sprintf
        "(let %s (%s : int) = %s in let %s = sum (%s %s) in %s %s)"
        g
        y
        (box t ~ints:(y :: ints) (depth - 1) ~tail:true)
        v
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
    | 13 -> sprintf "(keepc.* <- %s; %s)" (storing t ~f:closure) (sub ())
    | 14 ->
      let p = name t "p" in
      sprintf "(let %s = mk2 %s in %s %s)" p (int ()) p (int ())
    | 15 ->
      sprintf
        "Box.make (k (fun (z : int) -> %s))"
        (storing t ~f:(fun () ->
           box t ~ints:("z" :: ints) (depth - 1) ~tail:true))
    | 16 ->
      sprintf "%s %s" (pick t [ "wrap"; "via"; "count 2"; "ev"; "od" ]) (int ())
    | 17 -> sprintf "pick 2 %s" (sub ~tail:false ())
    | 18 ->
      let go = name t "go"
      and n = name t "n" in
      sprintf
        "(let %s (%s : int) = match Int.equal %s 0 with true -> %s | false -> \
         %s (Int.sub %s 1) in %s 2)"
        go
        n
        n
        (box t ~ints:(n :: ints) (depth - 1) ~tail:true)
        go
        n
        go
    | 19 -> sprintf "m %s %s" (int ()) (int ())
    | 20 ->
      let p = name t "p" in
      sprintf "(let %s = m %s in %s %s)" p (int ()) p (int ())
    | 21 | 22 | 23 ->
      (match helpers t with
       | [] -> sub ()
       | helpers ->
         let helper, (shape : Shape.t) = pick t helpers in
         (match shape with
          | Curried | Returns_closure ->
            if Random.State.bool t.random
            then sprintf "%s %s %s" helper (int ()) (int ())
            else (
              let p = name t "p" in
              sprintf "(let %s = %s %s in %s %s)" p helper (int ()) p (int ()))
          | Pair ->
            let c = name t "c"
            and n = name t "n" in
            sprintf
              "(match %s %s with (%s, %s) -> %s (Int.add %s %s))"
              helper
              (int ())
              c
              n
              c
              n
              (int ())
          | Variant ->
            let c = name t "c" in
            sprintf
              "(match %s %s with `C %s -> %s %s)"
              helper
              (int ())
              c
              c
              (int ())
          | Recursive -> sprintf "%s 2" helper))
    | 24 ->
      let c = name t "c" in
      sprintf
        "(let mut %s = (fun (y : int) -> Box.make y) in %s <- %s; %s %s)"
        c
        c
        (closure ())
        c
        (int ())
    | 25 ->
      if tail
      then (
        let b = name t "b" in
        sprintf
          "exclave_ (let %s = %s in Box.make (sum %s))"
          b
          (sub ~tail:false ())
          b)
      else sub ()
    | 26 ->
      let p = name t "p" in
      sprintf
        "(let %s = (fun (a : int) (b : int) -> Box.make (Int.add a b)) %s in \
         %s %s)"
        p
        (int ())
        p
        (int ())
    | 27 | 28 ->
      (* A tuple mixing [stack_] and other parts, bound, then taken apart. *)
      let p = name t "p"
      and a = name t "a"
      and b = name t "b" in
      sprintf
        "(let %s = ((stack_ (Box.make %s)), %s) in match %s with (%s, %s) -> \
         %s)"
        p
        (int ())
        (sub ~tail:false ())
        p
        a
        b
        (rarely
           t
           a
           ~else_:[ b; sprintf "Box.make (Int.add (sum %s) (sum %s))" a b ])
    | 29 ->
      (* Nested, through another variable, with a closure part. *)
      let p = name t "p"
      and q = name t "q"
      and f = name t "f"
      and b = name t "b" in
      sprintf
        "(let %s = ((%s, %s), stack_ (Box.make %s)) in let %s = %s in match %s \
         with ((%s, %s), _) -> %s)"
        p
        (closure ())
        (sub ~tail:false ())
        (int ())
        q
        p
        q
        f
        b
        (pick t [ b; sprintf "%s %s" f (int ()) ])
    | 30 ->
      (* Cases joined part by part. *)
      let p = name t "p"
      and a = name t "a"
      and b = name t "b" in
      sprintf
        "(let %s = match Int.equal %s 0 with true -> ((stack_ (Box.make 1)), \
         %s) | false -> (%s, stack_ (Box.make 2)) in match %s with (%s, %s) -> \
         %s)"
        p
        (int ())
        (sub ~tail:false ())
        (sub ~tail:false ())
        p
        a
        b
        (rarely
           t
           (pick t [ a; b ])
           ~else_:[ sprintf "Box.make (Int.add (sum %s) (sum %s))" a b ])
    | 31 ->
      (* A field of a record of a local and another part. *)
      let r = name t "r" in
      sprintf
        "(let %s = {`l = (stack_ (Box.make %s)); `r = %s} in %s)"
        r
        (int ())
        (sub ~tail:false ())
        (rarely
           t
           (r ^ ".`l")
           ~else_:[ r ^ ".`r"; sprintf "Box.make (sum %s.`l)" r ])
    | 32 ->
      (* A part used inside a closure, bound outside it. *)
      let p = name t "p"
      and g = name t "g"
      and a = name t "a"
      and b = name t "b" in
      sprintf
        "(let %s = ((stack_ (Box.make %s)), %s) in let %s (y : int) = match %s \
         with (%s, %s) -> %s in %s %s)"
        p
        (int ())
        (sub ~tail:false ())
        g
        p
        a
        b
        (rarely t a ~else_:[ b; sprintf "Box.make (sum %s)" a ])
        g
        (int ())
    | 33 ->
      (* Alternatives binding the same names to parts that may differ. *)
      let p = name t "p"
      and a = name t "a"
      and b = name t "b" in
      sprintf
        "(let %s = ((stack_ (Box.make %s)), %s) in match %s with (%s, %s) | %s \
         -> %s)"
        p
        (int ())
        (sub ~tail:false ())
        p
        a
        b
        (if Random.State.bool t.random
         then sprintf "(%s, %s)" b a
         else sprintf "(%s, %s)" a b)
        (rarely
           t
           a
           ~else_:[ b; sprintf "Box.make (Int.add (sum %s) (sum %s))" a b ])
    | _ ->
      let n = name t "n" in
      sprintf
        "(let %s @ local = %s in Box.make (sum %s))"
        n
        (sub ~tail:false ())
        n
  ;;

  (* A toplevel [f] may use, itself using those before it and the header's [h],
     [k] and [m]. *)
  let helper t =
    let name = name t "t" in
    let shape : Shape.t =
      pick t Shape.[ Curried; Returns_closure; Pair; Variant; Recursive ]
    in
    let box ints = box t ~ints 3 ~tail:true in
    let source =
      match shape with
      | Curried ->
        sprintf "let %s (a : int) (b : int) = %s\n" name (box [ "a"; "b" ])
      | Returns_closure ->
        sprintf
          "let %s (a : int) = fun (b : int) -> %s\n"
          name
          (box [ "a"; "b" ])
      | Pair ->
        sprintf
          "let %s (a : int) = ((fun (b : int) -> %s), a)\n"
          name
          (box [ "a"; "b" ])
      | Variant ->
        sprintf
          "let %s (a : int) = `C (fun (b : int) -> %s)\n"
          name
          (box [ "a"; "b" ])
      | Recursive ->
        sprintf
          "let %s (n : int) = match Int.equal n 0 with true -> %s | false -> \
           %s (Int.sub n 1)\n"
          name
          (box [ "n" ])
          name
    in
    if String.is_substring source ~substring:"keepc.*"
       || Set.exists t.reading_keepc ~f:(fun helper ->
         String.is_substring source ~substring:helper)
    then t.reading_keepc <- Set.add t.reading_keepc name;
    t.helpers <- (name, shape) :: t.helpers;
    source
  ;;

  let program random =
    let t =
      { random
      ; fresh = 0
      ; helpers = []
      ; reading_keepc = String.Set.empty
      ; storing = false
      }
    in
    let helpers =
      List.init (Random.State.int random 3) ~f:(fun _ -> helper t)
      |> String.concat
    in
    helpers
    ^ sprintf "let f h k m (x : int) = %s\n" (box t ~ints:[ "x" ] 4 ~tail:true)
  ;;
end

(* Callers of [f], each its own entry. *)
let uses =
  let m = "(fun (a : int) -> fun (b : int) -> Box.make (Int.add a b))" in
  [ ( "g"
    , "let g (u : unit) = let r @ global = f (fun (y : int) -> Box.make y) \
       (fun j -> sum (j 1)) "
      ^ m
      ^ " 1 in sum r" )
  ; ( "l"
    , "let l (u : unit) = let r @ local = f (fun (y : int) -> Box.make y) (fun \
       j -> sum (j 1)) mk2 1 in sum r [@nontail]" )
  ; ( "s"
    , "let s (u : unit) = let r @ local = f (fun (y : int) -> let b = Box.make \
       y in keep.* <- b; b) (fun j -> keepc.* <- j; 0) (fun (a : int) -> \
       keepc.* <- mk2 a; fun (b : int) -> Box.make b) 1 in sum r [@nontail]" )
  ; ( "p"
    , "let p (u : unit) = let r = f (mk2 1) (fun j -> sum (stack_ (Box.make \
       (sum (j 2))))) "
      ^ m
      ^ " 2 in keep.* <- r; sum keep.*" )
  ; ( "t"
    , "let t (u : unit) = let r @ local = f (fun (y : int) -> exclave_ \
       Box.make y) (fun j -> let b @ local = j 1 in sum b [@nontail]) (fun (a \
       : int) -> exclave_ fun (b : int) -> exclave_ Box.make (Int.add a b)) 1 \
       in sum r [@nontail]" )
  ]
;;

let fuzz ?override ?tail_calls_end_regions ~seed ~runs () =
  let random = Random.State.make [| seed |] in
  let outcomes = String.Table.create () in
  let count key = Hashtbl.incr outcomes key in
  let failures = Queue.create () in
  let regions = ref 0 in
  for _ = 1 to runs do
    let body = Gen.program random in
    List.iter uses ~f:(fun (entry, use) ->
      let source = header ^ body ^ use ^ "\n" in
      match
        Region_check.run ?override ?tail_calls_end_regions source ~entry
      with
      | Rejected _ -> count "rejected"
      | Returned { in_regions; _ } ->
        regions := !regions + in_regions;
        count "ran"
      | Use_after_free message ->
        count "use after free";
        Queue.enqueue failures (source ^ message)
      | Runtime_error message ->
        count "runtime error";
        print_endline (source ^ message)
      | Unsupported message ->
        count "unsupported";
        print_endline (source ^ message))
  done;
  ( failures
  , Hashtbl.to_alist outcomes |> List.sort ~compare:[%compare: string * int]
  , !regions )
;;

let%expect_test "no allocation is used after its region ends" =
  List.iter [ 7; 8; 9 ] ~f:(fun seed ->
    let failures, outcomes, regions = fuzz ~seed ~runs:300 () in
    Queue.iter failures ~f:print_endline;
    print_s
      [%message (seed : int) (outcomes : (string * int) list) (regions : int)]);
  [%expect
    {|
    ((seed 7) (outcomes ((ran 760) (rejected 740))) (regions 4152))
    ((seed 8) (outcomes ((ran 764) (rejected 736))) (regions 4466))
    ((seed 9) (outcomes ((ran 788) (rejected 712))) (regions 4483))
    |}]
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
    (outcomes ((ran 424) (rejected 740) ("use after free" 336)))

    let keep : ptr[box[int]] = Ptr.make (Box.make 0)
    let keepc : ptr[int -> box[int]] = Ptr.make (fun (y : int) -> Box.make y)
    let sum (b : box[int]) = match b with *n -> n
    let mk2 (a : int) (b : int) = Box.make (Int.add a b)
    let wrap (x : int) = Box.make x
    let via (x : int) = wrap x
    let count (n : int) (x : int) =
      match Int.equal n 0 with true -> Box.make x | false -> count (Int.sub n 1) (Int.add x 1)
    let pick (n : int) (b : box[int]) =
      match Int.equal n 0 with true -> b | false -> pick (Int.sub n 1) b
    let ev (n : int) = match Int.equal n 0 with true -> Box.make 1 | false -> od (Int.sub n 1)
    let od (n : int) = match Int.equal n 0 with true -> Box.make 0 | false -> ev (Int.sub n 1)
    let h (y : int) = Box.make y
    let k (j : int -> box[int]) = sum (j 1)
    let m (a : int) (b : int) = Box.make (Int.add a b)
    let t6 (n : int) = match Int.equal n 0 with true -> ((let p7 = ((stack_ (Box.make (Int.add n n))), ((let mut c10 = (fun (y : int) -> Box.make y) in c10 <- h; c10 n))) in match p7 with (a8, b9) | (a8, b9) -> b9)) | false -> t6 (Int.sub n 1)
    let t1 (a : int) = `C (fun (b : int) -> ((let p2 = ((stack_ (Box.make b)), ((let n5 @ local = (region_ ((Box.make b))) in Box.make (sum n5)))) in match p2 with (a3, b4) -> b4)))
    let f h k m (x : int) = ((let go11 (n12 : int) = match Int.equal n12 0 with true -> ((let p13 = ((stack_ (Box.make 1)), ((let go16 (n17 : int) = match Int.equal n17 0 with true -> (t6 2) | false -> go16 (Int.sub n17 1) in go16 2))) in match p13 with (a14, b15) -> b15)) | false -> go11 (Int.sub n12 1) in go11 2))
    let g (u : unit) = let r @ global = f (fun (y : int) -> Box.make y) (fun j -> sum (j 1)) (fun (a : int) -> fun (b : int) -> Box.make (Int.add a b)) 1 in sum r
    a reference, from the result of a call of toplevel t6, of the Box at t.stag:14:18-28, made in the region of a call of toplevel t6, after it ended
    |}]
;;

(* The same programs, run as if a tail call didn't end the region of the
   function making it: what the callee makes in its caller's region is then
   freed when that function returns. *)
let%expect_test "with tail calls not ending their region" =
  let failures, outcomes, _ =
    fuzz ~tail_calls_end_regions:false ~seed:7 ~runs:300 ()
  in
  print_s [%message (outcomes : (string * int) list)];
  Option.iter (Queue.peek failures) ~f:print_endline;
  [%expect
    {|
    (outcomes ((ran 395) (rejected 740) ("use after free" 365)))

    let keep : ptr[box[int]] = Ptr.make (Box.make 0)
    let keepc : ptr[int -> box[int]] = Ptr.make (fun (y : int) -> Box.make y)
    let sum (b : box[int]) = match b with *n -> n
    let mk2 (a : int) (b : int) = Box.make (Int.add a b)
    let wrap (x : int) = Box.make x
    let via (x : int) = wrap x
    let count (n : int) (x : int) =
      match Int.equal n 0 with true -> Box.make x | false -> count (Int.sub n 1) (Int.add x 1)
    let pick (n : int) (b : box[int]) =
      match Int.equal n 0 with true -> b | false -> pick (Int.sub n 1) b
    let ev (n : int) = match Int.equal n 0 with true -> Box.make 1 | false -> od (Int.sub n 1)
    let od (n : int) = match Int.equal n 0 with true -> Box.make 0 | false -> ev (Int.sub n 1)
    let h (y : int) = Box.make y
    let k (j : int -> box[int]) = sum (j 1)
    let m (a : int) (b : int) = Box.make (Int.add a b)
    let t1 (a : int) (b : int) = ((let p2 = (fun (a : int) (b : int) -> Box.make (Int.add a b)) 1 in p2 (sum ((let g3 = h in let v4 = sum (g3 b) in g3 a)))))
    let f h k m (x : int) = (region_ (((let p5 = ((stack_ (Box.make 1)), (pick 2 ((let p8 = t1 x in p8 x)))) in match p5 with (a6, b7) -> Box.make (Int.add (sum a6) (sum b7))))))
    let g (u : unit) = let r @ global = f (fun (y : int) -> Box.make y) (fun j -> sum (j 1)) (fun (a : int) -> fun (b : int) -> Box.make (Int.add a b)) 1 in sum r
    a reference, from the result of a call of Partial_application at t.stag:17:29-153, of the Box at t.stag:17:68-90, made in the region of a call of Partial_application at t.stag:17:29-153, after it ended
    |}]
;;
