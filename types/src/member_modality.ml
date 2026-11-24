open! Import

type t =
  | Id
  | Meet of Mode.Const.t
  | Param of int
[@@deriving sexp_of, equal]

let of_atoms const =
  if Mode.Const.equal const Mode.Const.max then Id else Meet const
;;

let to_string t ~param =
  match t with
  | Id -> None
  | Param index -> Some (param index)
  | Meet const ->
    let atom (type a) (axis : a Mode.Axis.t) (point : a) =
      let module C = (val Mode.Axis.chain axis) in
      if C.equal point (Mode.Const.get Mode.Const.max axis)
      then []
      else [ Sexp.to_string (C.sexp_of_t point) |> String.lowercase ]
    in
    let atoms =
      atom Stage const.stage
      @ (if Mode.Stage.equal const.stage Const
         then []
         else atom Provenance const.provenance)
      @ atom Locality const.locality
    in
    Some (String.concat ~sep:" " atoms)
;;
