open! Import
open Mode_helpers

(* Random [with] clauses over fixed signature shapes, with arrows, [ptr]
   modalities and a type's mode parameter: each annotation either is rejected,
   or prints a type that reads back as exactly what it says. *)

let shapes =
  [ "'a @ 'm0 -> 'a @ 'm1"
  ; "'a @ 'm0 -> ('a @ 'm1 -> 'a @ 'm2) @ 'm3"
  ; "ptr['a @@ 'm1] @ 'm0 -> 'a @ 'm2"
  ; "'a @ 'm1 -> ptr['a @@ 'm1] @ 'm0 -> 'a @ 'm2"
  ; "'a @ 'm0 -> box['m3] @ 'm2 -> 'a @ 'm4"
  ; "box['m3] @ 'm2 -> 'a @ 'm0 -> 'a @ 'm4"
  ; "('a @ 'm0 -> 'a @ 'm1) @ 'm2 -> 'a @ 'm3 -> 'a @ 'm4"
  ; "ptr['a @@ 'm0] @ 'm1 -> ('a @ 'm2 -> unit @ 'm3) @ 'm4"
  ]
;;

let axes = [| "stage"; "provenance"; "locality" |]

let atoms =
  [| [| "const"; "portable"; "nonportable" |]
   ; [| "static"; "gc"; "external" |]
   ; [| "global"; "regional"; "local" |]
  |]
;;

let vars_of shape =
  List.filter [ 0; 1; 2; 3; 4 ] ~f:(fun i ->
    String.is_substring shape ~substring:(sprintf "'m%d" i))
;;

let constraint_ random vars =
  let var () =
    sprintf "'m%d" (List.random_element_exn ~random_state:random vars)
  in
  let axis () = Random.State.int random 3 in
  let atom axis = atoms.(axis).(Random.State.int random 3) in
  let on axis v = sprintf "%s(%s)" axes.(axis) v in
  match Random.State.int random 8 with
  | 0 -> sprintf "%s <= %s" (var ()) (var ())
  | 1 ->
    let a = axis () in
    sprintf "%s <= %s" (on a (var ())) (on a (var ()))
  | 2 -> sprintf "%s <= %s" (var ()) (atom (axis ()))
  | 3 -> sprintf "%s <= %s" (atom (axis ())) (var ())
  | 4 ->
    let a = axis () in
    sprintf "meet(%s, %s) <= %s" (on a (var ())) (atom a) (on a (var ()))
  | 5 -> sprintf "release(locality(%s)) <= locality(%s)" (var ()) (var ())
  | 6 ->
    sprintf "provenance(%s) <= static_if_const(stage(%s))" (var ()) (var ())
  | _ ->
    sprintf "stage(%s) <= const_if_static(provenance(%s))" (var ()) (var ())
;;

let program random =
  let shape = List.random_element_exn ~random_state:random shapes in
  let vars = vars_of shape in
  let constraints =
    List.init
      (1 + Random.State.int random 3)
      ~f:(fun _ -> constraint_ random vars)
  in
  sprintf
    "type box['p : mode] = { v : string @@ 'p }\n\
     let diverge (u : unit) = diverge u\n\
     let s : %s with %s = fun x -> diverge ()"
    shape
    (String.concat ~sep:" and " constraints)
;;

let%expect_test "random [with] clauses read back exactly" =
  let random = Random.State.make [| 7 |] in
  let accepted = ref 0
  and rejected = ref 0
  and wrong = Queue.create () in
  for _ = 1 to 400 do
    let source = program random in
    match checked source with
    | Error _ -> incr rejected
    | Ok _ ->
      incr accepted;
      let output =
        match round_trip source ~show:`Quiet with
        | () -> [%expect.output]
        | exception exn -> [%expect.output] ^ Exn.to_string exn
      in
      if not (String.is_empty output)
      then Queue.enqueue wrong (source ^ "\n" ^ output)
  done;
  Queue.iter wrong ~f:print_endline;
  print_s
    [%message
      (!accepted : int) (!rejected : int) ~wrong:(Queue.length wrong : int)];
  [%expect {| ((!accepted 355) (!rejected 45) (wrong 0)) |}]
;;

(* Random pairs of types of one shape, differing in their modes: a value of the
   one is accepted at the other just when each mode is related as its position's
   variance says. *)
module Variance_fuzz = struct
  module Shape = struct
    type t =
      | String
      | Arrow of t * t
      | Box of t
      | Ptr of t
      | Co
      | Contra
      | Cyclic of [ `Co | `Contra | `Inv ]
  end

  (* Locality and provenance: annotations fix both, and neither has floors in
     these positions. *)
  let localities = [| "global"; "local" |]
  let provenances = [| "gc"; "external" |]

  let rec shape random depth : Shape.t =
    match if depth = 0 then 0 else Random.State.int random 6 with
    | 0 -> String
    | 1 | 2 -> Arrow (shape random (depth - 1), shape random (depth - 1))
    | 3 -> Box (shape random (depth - 1))
    | 4 -> Ptr (shape random (depth - 1))
    | _ ->
      (match Random.State.int random 5 with
       | 0 -> Co
       | 1 -> Contra
       | 2 -> Cyclic `Co
       | 3 -> Cyclic `Contra
       | _ -> Cyclic `Inv)
  ;;

  (* The atoms of each mode in [shape], outermost first, as indices. *)
  let rec slots (shape : Shape.t) =
    match shape with
    | String -> 0
    | Arrow (arg, res) -> 4 + slots arg + slots res
    | Box inner | Ptr inner -> slots inner
    | Co | Contra | Cyclic _ -> 1
  ;;

  let render (shape : Shape.t) atoms =
    let next = ref atoms in
    let take () =
      match !next with
      | atom :: rest ->
        next := rest;
        atom
      | [] -> assert false
    in
    let side () =
      let locality = localities.(take ()) in
      let provenance = provenances.(take ()) in
      sprintf "%s %s" locality provenance
    in
    let rec go (shape : Shape.t) =
      match shape with
      | String -> "string"
      | Arrow (arg, res) ->
        let arg_side = side () in
        let res_side = side () in
        let arg = go arg in
        let res = go res in
        sprintf "(%s @ %s -> %s @ %s)" arg arg_side res res_side
      | Box inner -> sprintf "box[%s]" (go inner)
      | Ptr inner -> sprintf "ptr[%s]" (go inner)
      | Co -> sprintf "co[%s]" provenances.(take ())
      | Contra -> sprintf "contra[%s]" provenances.(take ())
      | Cyclic variance ->
        let name =
          match variance with
          | `Co -> "rco"
          | `Contra -> "rcontra"
          | `Inv -> "rinv"
        in
        sprintf "%s[%s]" name provenances.(take ())
    in
    go shape
  ;;

  (* Whether [lower] may be used as [upper], comparing the atoms in [render]'s
     order. *)
  let expected (shape : Shape.t) lower upper =
    let lower = ref lower
    and upper = ref upper in
    let take () =
      match !lower, !upper with
      | l :: ls, u :: us ->
        lower := ls;
        upper := us;
        l, u
      | _ -> assert false
    in
    let related (variance : [ `Co | `Contra | `Inv ]) =
      let l, u = take () in
      match variance with
      | `Co -> l <= u
      | `Contra -> u <= l
      | `Inv -> l = u
    in
    let flip = function
      | `Co -> `Contra
      | `Contra -> `Co
      | `Inv -> `Inv
    in
    let rec go variance (shape : Shape.t) =
      match shape with
      | String -> true
      | Arrow (arg, res) ->
        let a1 = related (flip variance) in
        let a2 = related (flip variance) in
        let r1 = related variance in
        let r2 = related variance in
        let arg = go (flip variance) arg in
        let res = go variance res in
        a1 && a2 && r1 && r2 && arg && res
      | Box inner -> go variance inner
      | Ptr inner -> go `Inv inner
      | Co -> related variance
      | Contra -> related (flip variance)
      | Cyclic `Co -> related variance
      | Cyclic `Contra -> related (flip variance)
      | Cyclic `Inv -> related `Inv
    in
    go `Co shape
  ;;

  let program random =
    let shape = shape random 3 in
    let atoms () =
      List.init (slots shape) ~f:(fun _ -> Random.State.int random 2)
    in
    let lower = atoms () in
    let upper = atoms () in
    ( sprintf
        "type co['p : mode] = { v : string @@ 'p }\n\
         type contra['p : mode] = { f : co['p] -> unit }\n\
         type aco['p : mode] = co['p] * box[aco['p]]\n\
         type rco['p : mode] = { a : aco['p] }\n\
         type acontra['p : mode] = contra['p] * box[acontra['p]]\n\
         type rcontra['p : mode] = { b : acontra['p] }\n\
         type ainv['p : mode] = box[ainv['p]] -> co['p]\n\
         type rinv['p : mode] = { c : ainv['p] }\n\
         let diverge (u : unit) = diverge u\n\
         let f : %s = diverge ()\n\
         let g : %s = f"
        (render shape lower)
        (render shape upper)
    , expected shape lower upper )
  ;;
end

let%expect_test "random uses at other modes are accepted as variance says" =
  let random = Random.State.make [| 11 |] in
  let accepted = ref 0
  and rejected = ref 0
  and wrong = Queue.create () in
  for _ = 1 to 400 do
    let source, expected = Variance_fuzz.program random in
    let actual = Result.is_ok (checked source) in
    if actual then incr accepted else incr rejected;
    if Bool.( <> ) actual expected
    then Queue.enqueue wrong (sprintf "%s\n(expected %b)" source expected)
  done;
  Queue.iter wrong ~f:print_endline;
  print_s
    [%message
      (!accepted : int) (!rejected : int) ~wrong:(Queue.length wrong : int)];
  [%expect {| ((!accepted 206) (!rejected 194) (wrong 0)) |}]
;;

(* Random functions allocating in a returned position, under [exclave_],
   [region_] and [stack_]: each is accepted, its callers using the result at
   [global] and at [local], and each allocation placed, as a model of where the
   allocations can be made says. *)
module Alloc_fuzz = struct
  module Model = struct
    type t =
      { global : bool (** Its value can be on the heap. *)
      ; local : bool (** Its value can be in the caller's region. *)
      }
  end

  (* Where a returned allocation's value goes: out of the function, or out of a
     [region_], which only a global one can. *)
  module Flow = struct
    type t =
      | Result
      | Region_value
  end

  module Site = struct
    type t =
      | Placed of string
      | Returned of Flow.t
  end

  let unused random ~exclaved : string * Site.t list =
    ( (match Random.State.int random 2 with
       | 0 -> "Box.make 1"
       | _ -> "stack_ (Box.make 1)")
    , [ Placed (if exclaved then "caller's region" else "region") ] )
  ;;

  (* What [exclave_] gives: [stack_] forces it into the caller's region. *)
  let rec exclaved random depth ~flow : string * bool * Site.t list =
    match if depth = 0 then 0 else Random.State.int random 3 with
    | 0 -> "Box.make x", true, [ Returned flow ]
    | 1 -> "stack_ (Box.make x)", false, [ Placed "caller's region" ]
    | _ ->
      let unused, unused_sites = unused random ~exclaved:true in
      let body, global, sites = exclaved random (depth - 1) ~flow in
      ( sprintf "(let n = sum (%s) in %s)" unused body
      , global
      , unused_sites @ sites )
  ;;

  let rec returned random depth ~flow : string * Model.t * Site.t list =
    match if depth = 0 then 0 else Random.State.int random 4 with
    | 0 -> "Box.make x", { global = true; local = true }, [ Returned flow ]
    | 1 ->
      let body, global, sites = exclaved random (depth - 1) ~flow in
      sprintf "exclave_ %s" body, { global; local = true }, sites
    | 2 ->
      (* What it gives is in the function's own region, so must be global. *)
      let body, model, sites = returned random (depth - 1) ~flow:Region_value in
      sprintf "region_ (%s)" body, { model with local = false }, sites
    | _ ->
      let unused, unused_sites = unused random ~exclaved:false in
      let body, model, sites = returned random (depth - 1) ~flow in
      ( sprintf "(let n = sum (%s) in %s)" unused body
      , model
      , unused_sites @ sites )
  ;;

  let program random =
    let body, (model : Model.t), sites = returned random 4 ~flow:Result in
    let source =
      sprintf
        "let sum (b : box[int]) = match b with *n -> n\nlet f (x : int) = %s\n"
        body
    in
    let placements =
      List.map sites ~f:(fun (site : Site.t) ->
        let where =
          match site with
          | Placed where -> where
          | Returned Region_value -> "heap"
          | Returned Result ->
            if model.global
            then "heap, caller's region if res is local"
            else "caller's region"
        in
        "Box in f: " ^ where)
    in
    source, model, placements
  ;;
end

let%expect_test "random allocations are placed as a model says" =
  let random = Random.State.make [| 13 |] in
  let accepted = ref 0
  and rejected = ref 0
  and wrong = Queue.create () in
  for _ = 1 to 300 do
    let source, (model : Alloc_fuzz.Model.t), expected_placements =
      Alloc_fuzz.program random
    in
    let ok source = Result.is_ok (checked source) in
    let placed = placements source in
    let defined = Result.is_ok placed in
    let global =
      ok (source ^ "let g (u : unit) = let r @ global = f 1 in sum r\n")
    in
    let local =
      ok
        (source ^ "let l (u : unit) = let r @ local = f 1 in sum r [@nontail]\n")
    in
    let expected = model.global || model.local in
    if defined then incr accepted else incr rejected;
    let mismatch what actual expected =
      if Bool.( <> ) actual expected
      then
        Queue.enqueue wrong (sprintf "%s(%s: expected %b)" source what expected)
    in
    mismatch "defined" defined expected;
    mismatch "global use" global model.global;
    mismatch "local use" local expected;
    (match placed with
     | Error _ -> ()
     | Ok (_, lines) ->
       let actual = List.map lines ~f:snd in
       if not ([%equal: string list] actual expected_placements)
       then
         Queue.enqueue
           wrong
           (sprintf
              !"%s%{Sexp}"
              source
              [%message
                (actual : string list) (expected_placements : string list)]));
    if defined
    then (
      let output =
        match round_trip source ~show:`Quiet with
        | () -> [%expect.output]
        | exception exn -> [%expect.output] ^ Exn.to_string exn
      in
      if not (String.is_empty output)
      then Queue.enqueue wrong (source ^ "\n" ^ output))
  done;
  Queue.iter wrong ~f:print_endline;
  print_s
    [%message
      (!accepted : int) (!rejected : int) ~wrong:(Queue.length wrong : int)];
  [%expect {| ((!accepted 284) (!rejected 16) (wrong 0)) |}]
;;
