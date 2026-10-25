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
  | CExtend of string * presence * ty * context
  (** Labels are scoped: the first entry for a name shadows later ones. *)
  | CVar of cvar ref

(** Whether code uses a context entry. An [Absent] entry may be dropped, so a
    context with entries of unknown presence can still unify with one that
    lacks them. *)
and presence =
  | Present
  | Absent
  | PVar of pvar ref

and pvar =
  | PLink of presence
  | PGeneric of id
  | PUnbound of id * rank

and arg =
  | ATy of ty
  | AContext of context
[@@deriving sexp, compare, equal]

val int : ty
val bool : ty
val string : ty
val char : ty
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

(** Follows [PLink]s, compressing the path. *)
val canonical_presence : presence -> presence

(** The row variable at the end of [context], if it is open. *)
val tail_var : context -> cvar ref option

(** Renders [ty] for humans. Variables are named in order of appearance;
    weak (unbound) ones are prefixed with [_]. The [context_bounds] of the
    context variables reached are listed after the type, as
    [where ..'a <= {...}]. *)
val to_string_hum : ?context_bounds:(id -> context list) -> ty -> string
