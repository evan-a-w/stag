open! Import

(** A mode: a point of each axis. *)
type t =
  { stage : Axis.Stage.t
  ; provenance : Axis.Provenance.t
  ; locality : Axis.Locality.t
  }
[@@deriving sexp, compare, equal]

val min : t
val max : t
val le : t -> t -> bool
val join : t -> t -> t
val meet : t -> t -> t
val get : t -> 'a Axis.t -> 'a
