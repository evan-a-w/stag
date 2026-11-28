open! Import

type t =
  { scalar_offset : int
  ; ptr_index : int
  ; layout : Layout.t
  }
[@@deriving sexp_of]

let unit_layout =
  Layout.of_shape (Struct { repr = Default; fields = [] }) |> Or_error.ok_exn
;;

let ptr_layout = Layout.of_shape Ptr |> Or_error.ok_exn

let root_place layout =
  { Layout.Place.scalar_offset = 0; ptr_index = 0; layout }
;;
