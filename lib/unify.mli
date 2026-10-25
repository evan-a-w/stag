open! Core

(** Unification of types, contexts and presences, and context bounds. Variables
    bound are rank lowered and occurs checked. *)

val unify_ty
  :  Fresh.t
  -> ty1:Type.ty
  -> ty2:Type.ty
  -> (unit, Unification_error.t) Result.t

val unify_context
  :  Fresh.t
  -> context1:Type.context
  -> context2:Type.context
  -> (unit, Unification_error.t) Result.t

val unify_presence
  :  presence1:Type.presence
  -> presence2:Type.presence
  -> error:'error
  -> (unit, 'error) Result.t

(** Constrains every entry [context] has, or later gets, to also be an entry of
    [bound]. Names in [shadowed] are hidden by earlier entries of the context
    the constraint is about, so are not constrained. *)
val constrain_context
  :  Fresh.t
  -> context:Type.context
  -> bound:Type.context
  -> shadowed:String.Set.t
  -> (unit, Unification_error.t) Result.t

(** The type of the first entry for [name] in [context], which must be present.
    [rank] is that of the variable standing for the type in the error if
    [context] lacks [name]. *)
val present_entry
  :  Fresh.t
  -> context:Type.context
  -> name:string
  -> rank:Type.rank
  -> (Type.ty, Unification_error.t) Result.t
