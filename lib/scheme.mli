open! Core

(** Instantiation and generalization of type schemes, whose quantified
    variables are [Generic]. *)

(** Replaces [Generic] variables by fresh ones at [rank]. The types one partial
    application [instantiate fresh ~rank] is applied to share their
    instances. *)
val instantiate : Fresh.t -> rank:Type.rank -> Type.ty -> Type.ty

(** Marks unbound variables with rank greater than [rank] as [Generic], in
    place. *)
val generalize_ty : rank:Type.rank -> Type.ty -> unit

val generalize_row : rank:Type.rank -> Type.row -> unit
