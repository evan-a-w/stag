open! Import

(** The source of fresh ids, for variables among others: ids, which appear in
    sexps of types, depend only on what was done with the [t]. Unification
    relies on ids being unique, so types built with different [t]s must not
    meet. *)
type t

val create : unit -> t
val id : t -> Type.id
val set_value_parameters : t -> Value_layout.parameters -> unit
val value_parameters : t -> Value_layout.parameters

(** An [Unbound] variable with a fresh id. *)
val var : t -> rank:Type.rank -> (_, _) Type.Var.t

(** A record or variant with a fresh id. Its row's variable gets [in_record]:
    its entries must not be [assigned] already ([Unify.in_record] checks that). *)
val record : t -> Type.row -> Type.ty

val variant : t -> Type.row -> Type.ty
