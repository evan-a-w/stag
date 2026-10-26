open! Core

(** The source of fresh ids, for variables among others: ids, which appear in
    sexps of types, depend only on what was done with the [t]. *)
type t

val create : unit -> t
val id : t -> Type.id

(** An [Unbound] variable with a fresh id. *)
val var : t -> rank:Type.rank -> (_, _) Type.Var.t

(** A record or variant with a fresh id. *)
val record : t -> Type.row -> Type.ty

val variant : t -> Type.row -> Type.ty
