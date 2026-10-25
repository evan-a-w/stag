open! Core

type id = int [@@deriving sexp, compare, equal]
type rank = int [@@deriving sexp, compare, equal]

(** A unification variable standing for an ['a]. Only row variables have
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
      ; context : row
      }
  | TVar of (ty, Nothing.t) Var.t

and row =
  | REmpty
  | RExtend of string * presence * ty * row
  (** Labels are scoped: the first entry for a name shadows later ones. *)
  | RVar of (row, bound) Var.t

(** Whether an entry is in the row (for a context, whether code uses it). An
    [Absent] entry may be dropped, so a row with entries of unknown presence
    can still unify with one that lacks them. *)
and presence =
  | Present
  | Absent
  | PVar of (presence, Nothing.t) Var.t

and arg =
  | ATy of ty
  | ARow of row

(** An upper bound of a row variable: every entry the variable gets must
    also be an entry of [row], unless shadowed where the bound arose. *)
and bound =
  { row : row
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

val canonical_row : row -> row
val canonical_presence : presence -> presence

(** The row variable at the end of [row], if it is open. *)
val tail_var : row -> (row, bound) Var.t option

(** Rebuilds a type, replacing each variable that is not a [Link] by the result
    of the callback for its kind. Links are followed, so do not appear in the
    result. *)
module Mapper : sig
  type t =
    { ty : (ty, Nothing.t) Var.t -> ty
    ; row : (row, bound) Var.t -> row
    ; presence : (presence, Nothing.t) Var.t -> presence
    }
end

val map_ty : Mapper.t -> ty -> ty
val map_row : Mapper.t -> row -> row

(** Visits each variable that is not a [Link], stopping at the first error. *)
module Iter : sig
  type 'e t =
    { ty : (ty, Nothing.t) Var.t -> (unit, 'e) Result.t
    ; row : (row, bound) Var.t -> (unit, 'e) Result.t
    ; presence : (presence, Nothing.t) Var.t -> (unit, 'e) Result.t
    }
end

val iter_ty : 'e Iter.t -> ty -> (unit, 'e) Result.t
val iter_row : 'e Iter.t -> row -> (unit, 'e) Result.t

(** Renders types for humans. Variables are named in order of appearance,
    consistently across the types one printer renders; weak (unbound) ones are
    prefixed with [_]. *)
module Printer : sig
  type t

  (** [type_name] renders the canonical name of a type constructor, as is by
      default. *)
  val create : ?type_name:(string -> string) -> unit -> t

  val ty : t -> ty -> string

  (** The bounds of the row variables reached, as
      [" where ..'a <= {...}"], or [""] if there are none. *)
  val where : t -> string
end

(** One type rendered by a [Printer], followed by its [Printer.where]. *)
val to_string_hum : ty -> string
