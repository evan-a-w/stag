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

(** Renders types for humans. Variables are named in order of appearance,
    consistently across the types one printer renders; weak (unbound) ones are
    prefixed with [_]. *)
module Printer : sig
  type t

  (** [type_name] renders the canonical name of a type constructor, as is by
      default. *)
  val create
    :  ?context_bounds:(id -> context list)
    -> ?type_name:(string -> string)
    -> unit
    -> t

  val ty : t -> ty -> string

  (** The [context_bounds] of the context variables reached, as
      [" where ..'a <= {...}"], or [""] if there are none. *)
  val where : t -> string
end

(** One type rendered by a [Printer], followed by its [Printer.where]. *)
val to_string_hum : ?context_bounds:(id -> context list) -> ty -> string
