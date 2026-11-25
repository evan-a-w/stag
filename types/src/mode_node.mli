open! Import

(** A mode variable's node on one axis, by the variable's id; points of an axis
    are by index, from [0], its least. *)
type t =
  { var : int
  ; axis : Mode.Axis.Any.t
  }
[@@deriving sexp_of]

include Hashable.S_plain with type t := t

val equal : t -> t -> bool
val compare : t -> t -> int
val of_var : Mode.Var.t -> Mode.Axis.Any.t -> t

(** The greatest point of the axis. *)
val max : Mode.Axis.Any.t -> int

(** As written in a [with] clause: [stage], [provenance], [locality]. *)
val axis_name : Mode.Axis.Any.t -> string

(** The atom of the point, as [portable]. *)
val atom : Mode.Axis.Any.t -> int -> string
