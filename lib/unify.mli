open! Core

(** Unification of types, rows and presences, and row bounds. Variables
    bound are rank lowered and occurs checked, allowing cycles through
    records and variants. Cyclic types are unified coinductively. The types
    of entries with the same presence are only unified if it is [Present],
    as an entry's type doesn't matter when it is [Absent]. *)

val unify_ty
  :  Fresh.t
  -> ty1:Type.ty
  -> ty2:Type.ty
  -> (unit, Unification_error.t) Result.t

val unify_row
  :  Fresh.t
  -> row1:Type.row
  -> row2:Type.row
  -> (unit, Unification_error.t) Result.t

(** Fails with [error] if [presence] is or must be [Present]. *)
val make_absent
  :  Fresh.t
  -> Type.presence
  -> error:'error
  -> (unit, 'error) Result.t

(** Constrains [presence <= bound]: if [presence] is [Present] so is [bound].
    Between unknown presences, this adds a bound to both, whatever their
    ranks. *)
val presence_at_most
  :  Fresh.t
  -> presence:Type.presence
  -> bound:Type.presence
  -> error:Unification_error.t
  -> (unit, Unification_error.t) Result.t

(** Whether two types can't be unified whatever their variables become.
    Conservative: cyclic types may be found not to clash. *)
val clash : Type.ty -> Type.ty -> bool

(** Unifies [ty1] and [ty2] if [presence] is or becomes [Present]: while it is
    unknown, the equality is a bound of its variable, dropped if it becomes
    [Absent]. If [ty1] and [ty2] [clash], [presence] becomes [Absent]. *)
val unify_if_present
  :  Fresh.t
  -> presence:Type.presence
  -> ty1:Type.ty
  -> ty2:Type.ty
  -> (unit, Unification_error.t) Result.t

(** Lowers the ranks of the variables of [ty] to at most [rank], so that they
    are not generalized at greater ranks. *)
val lower_ty : rank:Type.rank -> Type.ty -> unit

(** Constrains every entry [row] has, or later gets, to also be an entry of
    [bound]. Names in [shadowed] are hidden by earlier entries of the row
    the constraint is about, so are not constrained. A variable [row] gets
    the bound whatever the ranks of the variables in it. *)
val constrain_row
  :  Fresh.t
  -> row:Type.row
  -> bound:Type.row
  -> shadowed:String.Set.t
  -> (unit, Unification_error.t) Result.t

(** The type of the first entry for [name] in [row], which must be present.
    [rank] is that of the variable standing for the type in the error if
    [row] lacks [name]. *)
val present_entry
  :  Fresh.t
  -> row:Type.row
  -> name:string
  -> rank:Type.rank
  -> (Type.ty, Unification_error.t) Result.t
