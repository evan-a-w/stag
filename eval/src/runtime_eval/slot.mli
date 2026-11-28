open! Import

(** A value's location within a stack frame: its scalar byte offset in the
    frame's [Data_stack] area, its pointer slot index in the frame's
    [Shadow_stack] area, and its runtime [Layout.t]. *)
type t =
  { scalar_offset : int
  ; ptr_index : int
  ; layout : Layout.t
  }
[@@deriving sexp_of]

val unit_layout : Layout.t
val ptr_layout : Layout.t
val root_place : Layout.t -> Layout.Place.t
