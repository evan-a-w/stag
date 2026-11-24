open! Import

(** How a part of a block relates to the block: a [global_] field or constructor
    argument, or a pointer's cell, is [global] whatever the block's locality,
    and the cell of a [ptr[t @@ external]] may hold [external] values whatever
    the pointer's provenance. *)
type t =
  { global : bool
  ; external_ : bool
  }
[@@deriving sexp_of, equal]

val id : t
val of_global : bool -> t

(** Of the cell of a pointer of the type given. *)
val of_pointee : Type.Pointee.t -> t

(** [compose outer inner]: of a part [inner] of a part [outer] of a block. *)
val compose : t -> t -> t
