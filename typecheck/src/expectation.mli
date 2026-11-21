(** How far the type an expression is expected to have constrains it. *)

open! Import

type t =
  | Required
  | Hint
  (** What it is likely to be, which may turn out to be something else. *)
  | Code_hint of t
  (** A likely code type, as a toplevel splice's code may instead be syntax, its
      quoted body being expected as [t]. *)

val is_required : t -> bool

(** The expectation of a quote's body. *)
val quoted : t -> t
