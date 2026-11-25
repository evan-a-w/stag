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
val set_variances : t -> Variance.of_con -> unit
val variances : t -> Variance.of_con

(** The source of mode variables' ids. *)
val modes : t -> Mode.Id_source.t

(** An [Unbound] variable with a fresh id. *)
val var : t -> rank:Type.rank -> (_, _) Type.Var.t

(** Like [var], for a type that values are used at by variance (a scheme's
    instance, a match's result): it becomes their join, a type with modes of
    its own each is used at, rather than the first's type. *)
val join_var : t -> rank:Type.rank -> (_, _) Type.Var.t

val is_join : t -> Type.id -> bool

(** A record or variant with a fresh id. Its row's variable gets [in_record]:
    its entries must not be [assigned] already ([Unify.in_record] checks that). *)
val record : t -> Type.row -> Type.ty

val variant : t -> Type.row -> Type.ty
