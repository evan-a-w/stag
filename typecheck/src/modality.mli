open! Import

(** How a part of a block relates to the block: a [global_] field or constructor
    argument is [Global], whatever the block's locality. *)
type t =
  | Id
  | Global
[@@deriving sexp_of, equal]

val of_global : bool -> t

(** [compose outer inner]: of a part [inner] of a part [outer] of a block. *)
val compose : t -> t -> t
