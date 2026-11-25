open! Import

type id = int [@@deriving sexp, compare, equal]
type rank = int [@@deriving sexp, compare, equal]

type 'a var_state =
  | Unbound of rank
  | Generic
  | Link of 'a

(** A unification variable standing for an ['a]. Only row and presence variables
    have bounds: for the other kinds, ['bound] is [Nothing.t]. [in_bounds_of]
    has the variables with a bound that has this variable (and maybe others: it
    is only added to), so that generalizing it can find them. [in_record] is for
    a row variable of a record or variant (or bound to one), whose entries are
    never [assigned]. *)
type ('a, 'bound) var =
  { id : id
  ; mutable state : 'a var_state
  ; mutable bounds : 'bound list
  ; mutable in_bounds_of : bounded list
  ; mutable kept : kept list
  ; mutable in_record : bool
  ; mutable watchers : 'a Watcher.t list
  }

and ty =
  | TCon of string * arg list
  | TArrow of ty * ty * Arrow_modes.t
  | TTuple of ty list
  | TCode of
      { inner : ty
      ; context : row
      }
  | TRecord of
      { id : id
      ; row : row
      }
  | TVariant of
      { id : id
      ; row : row (** Tags without the backquote. *)
      }
  | TVar of (ty, Nothing.t) var

and row =
  | REmpty
  | RExtend of string * presence * presence * ty * row
  (** [RExtend (name, presence, assigned, ty, rest)]. Labels are scoped: the
      first entry for a name shadows later ones. [assigned] is [Present] for an
      entry of a context the code assigns, which only a mutable variable can
      provide, and [Absent] in records and variants. *)
  | RVar of (row, bound) var

(** Whether an entry is in the row (for a context, whether code uses it). An
    [Absent] entry may be dropped, so a row with entries of unknown presence can
    still unify with one that lacks them. *)
and presence =
  | Present
  | Absent
  | PVar of (presence, presence_bound) var

(** [p <= q], as [At_most q] in the bounds of [p] and [At_least p] in those of
    [q]: if [p] is [Present] so is [q], and if [q] is [Absent] so is [p]. Bounds
    may relate variables of different ranks: a generic variable keeps its bounds
    to variables that are not generalized, which drop theirs to it (see
    [Scheme.generalize]). *)
and presence_bound =
  | At_most of presence
  | At_least of presence
  | If_present of ty * ty
  (** The types of entries with this presence that must be equal if it is
      [Present]: the type of an entry it is absent from doesn't matter. *)

and arg =
  | ATy of ty
  | ARow of row
  | AMode of Mode_arg.t

(** An upper bound of a row variable: every entry the variable gets must also be
    an entry of [row], unless shadowed where the bound arose. The variables of
    [row] may have any ranks. *)
and bound =
  { row : row
  ; shadowed : String.Set.t
  }

and bounded =
  | Row_var of (row, bound) var
  | Presence_var of (presence, presence_bound) var

(** A bound of a variable that is not generalized, with [Generic] variables:
    each of them keeps it rather than the variable, and each instance of the
    scheme adds it to the variable once (see [Scheme.generalize]). *)
and kept =
  | Row_bound of
      { id : id
      ; lower : (row, bound) var
      ; bound : bound
      }
  | If_present_kept of
      { id : id
      ; presence : (presence, presence_bound) var
      ; ty1 : ty
      ; ty2 : ty
      }

val kept_id : kept -> id

module Var : sig
  type 'a state = 'a var_state =
    | Unbound of rank
    | Generic
    | Link of 'a

  type ('a, 'bound) t = ('a, 'bound) var =
    { id : id
    ; mutable state : 'a state
    ; mutable bounds : 'bound list
    ; mutable in_bounds_of : bounded list
    ; mutable kept : kept list
    ; mutable in_record : bool
    ; mutable watchers : 'a Watcher.t list
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

(** Types are graphs: [Link]s may close cycles, each through a [TRecord] or
    [TVariant] (equi-recursive types). Records and variants are identified by
    their [id], unique among them, so may be shared. Sexps omit ids, and print a
    record or variant reached again within itself as [(Cycle_ref n)], and the
    record or variant as [(Cycle n ...)]. *)
val sexp_of_ty : ty -> Sexp.t

val sexp_of_row : row -> Sexp.t
val sexp_of_presence : presence -> Sexp.t
val sexp_of_arg : arg -> Sexp.t
val sexp_of_presence_bound : presence_bound -> Sexp.t
val sexp_of_bound : bound -> Sexp.t
val int : ty
val bool : ty
val string : ty
val char : ty
val unit : ty

(** The extensible type of exceptions, declared by [exception]. *)
val exn : ty

val exn_name : string

(** The type of what did not check, which unifies with any type, binding its
    variables to [error], so that what uses it is not in error too. *)
val error : ty

(** With the modes of an arrow in a declaration. *)
val arrow : ty -> ty -> ty

val error_name : string
val is_error : ty -> bool
val ptr_name : string

(** [ptr[pointee @@ held]], by default [ptr[pointee]], whose cell holds only
    values of provenance at most [held] ([gc] by default). *)
val ptr : ?held:Mode_arg.t -> ty -> ty

(** Of a pointer type, with what it points to. *)
val of_ptr : ty -> (ty * Mode_arg.t) option

val box_name : string

(** [ptr] or [box]: a pointer, through which types may be recursive. *)
val is_pointer_name : string -> bool

(** [box[contents]]: an immutable block holding [contents], read as a part of
    the box (at most the box's mode). *)
val box : ty -> ty

val of_box : ty -> ty option

(** Copies of types as they are now, unaffected by later unification, eg. for an
    error to show them as they were when it arose. Variables keep their ids, and
    only their state is copied, not their bounds. *)
module Snapshot : sig
  type t

  val create : unit -> t
  val ty : t -> ty -> ty
  val row : t -> row -> row
  val arg : t -> arg -> arg
end

(** Follows [Link]s, compressing the path. *)
val canonical_ty : ty -> ty

val canonical_row : row -> row
val canonical_presence : presence -> presence

(** The row variable at the end of [row], if it is open. *)
val tail_var : row -> (row, bound) Var.t option

(** The variable of a presence bound, if it is not known. *)
val presence_bound_var
  :  presence_bound
  -> (presence, presence_bound) Var.t option

(** Whether two types are the same up to [Link]s: records and variants are only
    the same if they are the same node, so this is conservative. *)
val same_ty : ty -> ty -> bool

val same_row : row -> row -> bool
val same_presence : presence -> presence -> bool

(** Whether two bounds have the same entries, in order, and shadow the same
    names. Entries of types that are not the same are still the same if
    [same_entry_ty presence ty1 ty2] says those of [presence] of types [ty1] and
    [ty2] are interchangeable in a bound. *)
val same_bound
  :  ?same_entry_ty:(presence -> ty -> ty -> bool)
  -> bound
  -> bound
  -> bool

(** Rebuilds a type, replacing each variable that is not a [Link] by the result
    of the callback for its kind. A [Link], record or variant whose variables
    all satisfy [keeps], so would be unchanged, is kept as is; others are copied
    once each, preserving sharing through them: a [Link] to a new [Link], of id
    [id ()], which closes the copy of a cycle (as does one to the copy of a
    record or variant reached again within itself). Copied records and variants
    get ids [id ()]. *)
module Mapper : sig
  type t =
    { ty : (ty, Nothing.t) Var.t -> ty
    ; row : (row, bound) Var.t -> row
    ; presence : (presence, presence_bound) Var.t -> presence
    ; keeps : 'a 'bound. ('a, 'bound) Var.t -> bool
    ; id : unit -> id
    ; mode : Mode.Var.t -> Mode.Var.t
    }
end

val map_ty : Mapper.t -> ty -> ty
val map_row : Mapper.t -> row -> row

(** Visits the variables that are not [Link]s, stopping at the first error.
    [guarded] is whether the occurrence is within a [TRecord] or [TVariant] of
    the type iterated over. An occurrence may be visited more than once, but
    each [Link], record and variant is followed at most twice. *)
module Iter : sig
  type 'e t =
    { ty : guarded:bool -> (ty, Nothing.t) Var.t -> (unit, 'e) Result.t
    ; row : guarded:bool -> (row, bound) Var.t -> (unit, 'e) Result.t
    ; presence :
        guarded:bool -> (presence, presence_bound) Var.t -> (unit, 'e) Result.t
    ; mode : Mode.Var.t -> (unit, 'e) Result.t
    }
end

(** Memoisation for comparisons of regular types whose recursion knots need not
    line up in the two unrollings being compared. *)
module Link_pairs : sig
  type 'a t

  val create : unit -> 'a t
  val ty : ty t -> ty -> ty -> bool
  val row : row t -> row -> row -> bool
end

val iter_ty : 'e Iter.t @ local -> ty -> (unit, 'e) Result.t
val iter_row : 'e Iter.t @ local -> row -> (unit, 'e) Result.t

(** Renders types for humans. Variables are named in order of appearance,
    consistently across the types one printer renders; weak (unbound) ones are
    prefixed with [_]. A cyclic type is written [(t as 'a)], with ['a] in [t]
    standing for itself, and later occurrences written ['a]; it is weak if it
    has a weak variable. *)
module Printer : sig
  type t

  (** [type_name] renders the canonical name of a type constructor: by default
      as is, except the prelude's, which are as within the prelude
      ([Prelude_path.to_string_hum]). With [modes], the mode variables of arrows
      are named, and the constraints between them listed by [where] (as
      [with 'm0 <= 'm1]); otherwise only the modes of arrows fixed to other than
      a declaration's default are, and with [name_mode_args], mode arguments not
      known are named, as [ptr[t @@ 'm0]], rather than left out. A side fixed to
      a constant is written by its atoms other than the default, as both
      declarations and annotations read an unwritten mode back. [declaration]
      prints the body of a type's declaration, whose mode variables are its
      parameters. *)
  val create
    :  ?type_name:(string -> string)
    -> ?modes:bool
    -> ?name_mode_args:bool
    -> ?declaration:bool
    -> unit
    -> t

  (** With [atomic], parenthesized unless a single word or bracketed. *)
  val ty : ?atomic:bool -> t -> ty -> string

  (** Renders a mode argument. *)
  val mode_arg : t -> Mode_arg.t -> string

  (** The head of a type's declaration, as [cell['a, 'm0 : mode]]. *)
  val declaration : t -> string -> arg list -> string

  (** Renders [row] as the context of a code type. *)
  val context : t -> row -> string

  (** Renders [ty] with the occurrences of itself within it as [name]. *)
  val recursive : t -> ty -> name:string -> string

  (** The bounds of the row and presence variables reached, as
      [" where ..'a <= {...}, 'b <= 'c"], or [""] if there are none. *)
  val where : t -> string
end

(** One type rendered by a [Printer], followed by its [Printer.where]. *)
val to_string_hum : ty -> string
