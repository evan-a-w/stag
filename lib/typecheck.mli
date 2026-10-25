open! Core

type t

val create : unit -> t
val fresh_tvar : t -> rank:Type.rank -> Type.tvar ref
val fresh_cvar : t -> rank:Type.rank -> Type.cvar ref
val fresh_pvar : t -> rank:Type.rank -> Type.pvar ref

(** Marks unbound variables with rank greater than [rank] as [Generic]. *)
val generalize_ty : t -> rank:Type.rank -> ty:Type.ty -> Type.ty

val generalize_context
  :  t
  -> rank:Type.rank
  -> context:Type.context
  -> Type.context

module Unification_error = Unification_error

val unify_ty
  :  t
  -> ty1:Type.ty
  -> ty2:Type.ty
  -> (Type.ty, Unification_error.t) Result.t

val unify_context
  :  t
  -> context1:Type.context
  -> context2:Type.context
  -> (Type.context, Unification_error.t) Result.t

module Typecheck_error = Typecheck_error

(** Type declarations of every module are checked first. Toplevel bindings, in
    any module, may refer to each other in any order. Each strongly connected
    component is generalized once fully checked, if all its members are
    syntactic values and it is not recursive through a non-function, and
    persistent if all are syntactic functions. Returns
    the type of each binding by canonical path, in program order (depth first
    through modules). *)
val typecheck
  :  t
  -> program:Syntax.Program.t
  -> ((string * Type.ty) list, Typecheck_error.t) Result.t

(** Like [Type.to_string_hum], also printing the bounds of context variables. *)
val to_string_hum : t -> Type.ty -> string

(** Constrains every entry [context] has, or later gets, to also be an entry of
    [bound]. *)
val constrain_context
  :  t
  -> context:Type.context
  -> bound:Type.context
  -> (unit, Unification_error.t) Result.t
