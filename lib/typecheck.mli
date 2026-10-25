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

module Unification_error : sig
  module Type_mismatch_reason : sig
    type t =
      | Base_types_not_equal of string * string
      | Args_length_mismatch of int * int
      | Mismatch_arg_kind of Type.arg * Type.arg
      | Structural_mismatch
    [@@deriving sexp, compare, equal]
  end

  module Context_mismatch_reason : sig
    type t =
      | Empty_context_has_variable of string
      | Presence_mismatch of string
      (** Both contexts have the name, but one has it [Absent]. *)
      | Cyclic_context
    [@@deriving sexp, compare, equal]
  end

  type t =
    | Type_mismatch of Type.ty * Type.ty * Type_mismatch_reason.t
    | Context_mismatch of
        Type.context * Type.context * Context_mismatch_reason.t
    | Occurs_check_failed of Type.Var.t
  [@@deriving sexp, compare, equal]
end

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

module Typecheck_error : sig
  type t =
    | Unification_error of Unification_error.t
    | Not_in_scope of string
    | Duplicate_toplevel of string
    | Duplicate_substitution of string
    | Variable_used_at_wrong_level of
        { name : string
        ; var_level : Env.Level.t
        ; used_at : Env.Level.t
        }
  [@@deriving sexp, compare, equal]
end

(** Toplevel bindings may refer to each other in any order. Each strongly
    connected component is generalized once fully checked, if all its members
    are syntactic functions. Returns the type of each binding, in program
    order. *)
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
