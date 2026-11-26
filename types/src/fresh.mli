open! Import

(** The source of fresh ids, for variables among others: ids, which appear in
    sexps of types, depend only on what was done with the [t]. Unification
    relies on ids being unique, so types built with different [t]s must not
    meet. *)
type t

(** What is known of the type constructors of its types. *)
module Cons : sig
  type t =
    { value_parameters : Value_layout.parameters
    ; variances : Variance.of_con
    }

  (** Of none: those of every parameter are taken to be stored by value and
      invariant. *)
  val conservative : t
end

(** Its [cons] are set by each [Type_env] created with it, and raise if read
    before. *)
val create : unit -> t

(** For types built without a [Type_env]: its [cons] are [Cons.conservative]. *)
val conservative : unit -> t

val id : t -> Type.Id.t

(** What [id] gives next, without taking it. *)
val next_id : t -> Type.Id.t

val set_cons : t -> Cons.t -> unit
val cons : t -> Cons.t

(** Runs [f], unless within another [settle] (of any [t]: an [Undo.permanent]
    section has its own): then once what that one runs is done, so watchers
    binding variables whose watchers bind others don't nest. All that is queued
    runs, even after an error, so each adds its constraints: the first error of
    all goes to the outermost [settle], and if one raises, the rest still run
    before the first exception is raised again. *)
val settle
  :  (unit -> (unit, Unification_error.t) Result.t)
  -> (unit, Unification_error.t) Result.t

(** The source of mode variables' ids. *)
val modes : t -> Mode.Id_source.t

(** An [Unbound] variable with a fresh id. *)
val var : t -> rank:Type.Rank.t -> (_, _) Type.Var.t

(** A record or variant with a fresh id. Its row's variable gets [in_record]:
    its entries must not be [assigned] already ([Unify.in_record] checks that). *)
val record : t -> Type.row -> Type.ty

val variant : t -> Type.row -> Type.ty
