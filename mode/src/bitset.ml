open! Import

type t = int array

let bits = Int.num_bits - 1
let create size = Array.create ~len:((size + bits - 1) / bits) 0
let add t i = t.(i / bits) <- t.(i / bits) lor (1 lsl (i % bits))
let mem t i = t.(i / bits) land (1 lsl (i % bits)) <> 0

let union t other =
  let changed = ref false in
  Array.iteri other ~f:(fun i word ->
    let union = t.(i) lor word in
    if union <> t.(i)
    then (
      t.(i) <- union;
      changed := true));
  !changed
;;

let to_list t =
  Array.foldi t ~init:[] ~f:(fun i found word ->
    if word = 0
    then found
    else
      List.fold (List.range 0 bits) ~init:found ~f:(fun found bit ->
        if word land (1 lsl bit) <> 0
        then ((i * bits) + bit) :: found
        else found))
  |> List.rev
;;
