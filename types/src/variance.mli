open! Import

(** How a type's parameter occurs in it: whether a type with a greater argument
    there is a greater type (by the modes inside it), a lesser one, either, or
    only the same type. *)
type t =
  | Bivariant (** It doesn't occur. *)
  | Covariant
  | Contravariant
  | Invariant
[@@deriving compare, equal, sexp_of]

(** Of an occurrence at [inner] inside one at [outer]. *)
val compose : outer:t -> t -> t

(** Of a parameter occurring in both. *)
val join : t -> t -> t

(** The variances of a nominal type's parameters, in order. [None] for a type
    not known, whose parameters are taken to be invariant. *)
type of_con = string -> t list option
