open! Core

type id = int [@@deriving sexp, compare, equal]
type rank = int [@@deriving sexp, compare, equal]

(** A unification variable standing for an ['a]. Only context variables have
    bounds: for the other kinds, ['bound] is [Nothing.t]. *)
module Var : sig
  type 'a state =
    | Unbound of rank
    | Generic
    | Link of 'a

  type ('a, 'bound) t =
    { id : id
    ; mutable state : 'a state
    ; mutable bounds : 'bound list
    }

  val create : id -> 'a state -> ('a, _) t

  (** Raises unless the variable is [Unbound]. *)
  val rank_exn : _ t -> rank

  (** As [(Unbound id rank)], [(Generic id)] or [(Link a)]: bounds are omitted,
      as they may refer back to the variable. *)
  val sexp_of_t
    :  ('a -> Sexp.t)
    -> ('bound -> Sexp.t)
    -> ('a, 'bound) t
    -> Sexp.t
end

type ty =
  | TCon of string * arg list
  | TArrow of ty * ty
  | TTuple of ty list
  | TCode of
      { inner : ty
      ; context : context
      }
  | TVar of (ty, Nothing.t) Var.t

and context =
  | CEmpty
  | CExtend of string * presence * ty * context
  (** Labels are scoped: the first entry for a name shadows later ones. *)
  | CVar of (context, bound) Var.t

(** Whether code uses a context entry. An [Absent] entry may be dropped, so a
    context with entries of unknown presence can still unify with one that
    lacks them. *)
and presence =
  | Present
  | Absent
  | PVar of (presence, Nothing.t) Var.t

and arg =
  | ATy of ty
  | AContext of context

(** An upper bound of a context variable: every entry the variable gets must
    also be an entry of [context], unless shadowed where the bound arose. *)
and bound =
  { context : context
  ; shadowed : String.Set.t
  }
[@@deriving sexp_of]

val int : ty
val bool : ty
val string : ty
val char : ty
val unit : ty

(** Follows [Link]s, compressing the path. *)
val canonical_ty : ty -> ty

val canonical_context : context -> context
val canonical_presence : presence -> presence

(** The row variable at the end of [context], if it is open. *)
val tail_var : context -> (context, bound) Var.t option

(** Rebuilds a type, replacing each variable that is not a [Link] by the result
    of the callback for its kind. Links are followed, so do not appear in the
    result. *)
module Mapper : sig
  type t =
    { ty : (ty, Nothing.t) Var.t -> ty
    ; context : (context, bound) Var.t -> context
    ; presence : (presence, Nothing.t) Var.t -> presence
    }
end

val map_ty : Mapper.t -> ty -> ty
val map_context : Mapper.t -> context -> context

(** Visits each variable that is not a [Link], stopping at the first error. *)
module Iter : sig
  type 'e t =
    { ty : (ty, Nothing.t) Var.t -> (unit, 'e) Result.t
    ; context : (context, bound) Var.t -> (unit, 'e) Result.t
    ; presence : (presence, Nothing.t) Var.t -> (unit, 'e) Result.t
    }
end

val iter_ty : 'e Iter.t -> ty -> (unit, 'e) Result.t
val iter_context : 'e Iter.t -> context -> (unit, 'e) Result.t

(** Renders types for humans. Variables are named in order of appearance,
    consistently across the types one printer renders; weak (unbound) ones are
    prefixed with [_]. *)
module Printer : sig
  type t

  (** [type_name] renders the canonical name of a type constructor, as is by
      default. *)
  val create : ?type_name:(string -> string) -> unit -> t

  val ty : t -> ty -> string

  (** The bounds of the context variables reached, as
      [" where ..'a <= {...}"], or [""] if there are none. *)
  val where : t -> string
end

(** One type rendered by a [Printer], followed by its [Printer.where]. *)
val to_string_hum : ty -> string
