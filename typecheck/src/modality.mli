open! Import

(** How a part of a block relates to the block: a [global_] field or constructor
    argument, or a pointer's cell, is [global] whatever the block's locality.
    What a pointer's cell holds is at least the provenance its type says, the
    [held] of [ptr[t @@ held]], whatever the pointer's own provenance. *)
type t =
  { global : bool
  ; held : Mode_arg.t list
  (** Of each pointer's cell the part is read through. *)
  }
[@@deriving sexp_of]

val id : t
val of_global : bool -> t

(** Of the cell of a [ptr[_ @@ held]]. *)
val of_cell : Mode_arg.t -> t

(** [compose outer inner]: of a part [inner] of a part [outer] of a block. *)
val compose : t -> t -> t
