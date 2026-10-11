open! Core

type id = int [@@deriving sexp, compare, equal]
type rank = int [@@deriving sexp, compare, equal]

type ty =
  | TCon of string * arg list
  | TArrow of ty * ty
  | TTuple of ty list
  | TCode of
      { inner : ty
      ; context : context
      }
  | TVar of tvar ref

and tvar =
  | Link of ty
  | Generic of id
  | Unbound of id * rank

and cvar =
  | CLink of context
  | CGeneric of id
  | CUnbound of id * rank

and context =
  | CEmpty
  | CExtend of string * ty * context
  | CVar of cvar ref

and arg =
  | ATy of ty
  | AContext of context
[@@deriving sexp, compare, equal]

val int : ty
val bool : ty
val string : ty
val char :ty 
val unit : ty

module Var : sig
  type t =
    | TVar of tvar ref
    | CVar of cvar ref
  [@@deriving sexp, compare, equal]
end

(** Follows [Link]s, compressing the path. *)
val canonical_ty : ty -> ty

(** Follows [CLink]s, compressing the path. *)
val canonical_context : context -> context

(** The row variable at the end of [context], if it is open. *)
val tail_var : context -> cvar ref option
