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
  [%expect {| ((!accepted 392) (!rejected 8) (wrong 0)) |}]
;;
