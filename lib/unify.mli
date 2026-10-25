open! Core

(** Unification of types, rows and presences, and row bounds. Variables
    bound are rank lowered and occurs checked. *)

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

val unify_presence
  :  presence1:Type.presence
  -> presence2:Type.presence
  -> error:'error
  -> (unit, 'error) Result.t

(** Constrains every entry [row] has, or later gets, to also be an entry of
    [bound]. Names in [shadowed] are hidden by earlier entries of the row
    the constraint is about, so are not constrained. *)
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
