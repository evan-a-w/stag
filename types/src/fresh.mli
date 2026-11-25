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

(** Runs [f], unless within another [settle]: then once what that one runs is
    done, so watchers binding variables whose watchers bind others don't nest.
    All that is queued runs, even after an error, so each adds its constraints:
    the first error of all goes to the outermost [settle]. *)
val settle
  :  t
  -> (unit -> (unit, Unification_error.t) Result.t)
  -> (unit, Unification_error.t) Result.t

(** The source of mode variables' ids. *)
val modes : t -> Mode.Id_source.t

(** An [Unbound] variable with a fresh id. *)
val var : t -> rank:Type.rank -> (_, _) Type.Var.t

(** A record or variant with a fresh id. Its row's variable gets [in_record]:
    its entries must not be [assigned] already ([Unify.in_record] checks that). *)
val record : t -> Type.row -> Type.ty

val variant : t -> Type.row -> Type.ty
