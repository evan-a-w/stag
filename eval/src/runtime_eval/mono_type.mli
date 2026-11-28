open! Import

(** Monomorphic type keys, substitutions, and mode-side instantiation tracking
    for lazy monomorphisation. *)

(** Wrapper around a concrete [Type.ty] with human-readable sexp output. *)
module Mono_ty : sig
  type t = Type.ty [@@deriving sexp_of]
end

(** Canonical key for a [Typed.Allocation.Owner.t], used to track which
    mode-polymorphic sides of an owner are instantiated as [local]. *)
module Owner_key : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  val of_owner : Typed.Allocation.Owner.t -> t
end

(** Set of [(owner, side)] pairs instantiated as [local] in a specialization. *)
module Local_sides : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  val empty : t
  val is_local : t -> owner:Owner_key.t -> side:Typed.Allocation.Side.t -> bool
end

(** Canonical sorted list of pinned mode variables and their locality values. *)
module Mode_pins : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  val empty : t
end

(** Canonical structural key for a concrete type. Equi-recursive [TRecord] /
    [TVariant] cycles are represented with de Bruijn [Mu] binders; row entries
    are normalized and sorted by label; modes and type ids are ignored. *)
module Key : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end

(** Monomorphic instantiation environment mapping generic type, row, and
    presence variables to concrete targets, alongside mode pins and
    [Local_sides.t] for resolving [Typed.Allocation.Follows]. *)
module Subst : sig
  type t [@@deriving sexp_of]

  val empty : t

  (** Least (most global) instantiation of a toplevel [scheme], pinning every
      mode variable of [scheme] to its lower bound. *)
  val least_instance
    :  owner:Typed.Allocation.Owner.t
    -> owner_sides:(Owner_key.t -> Typed.Allocation.Side.t list)
    -> scheme:Type.ty
    -> t

  val local_sides : t -> Local_sides.t
  val mode_pins : t -> Mode_pins.t

  (** Substitutes [t] into [ty] and defaults any remaining unconstrained type
      variables to [unit], open row variables to [REmpty], and unknown presence
      variables to [Absent]. Cycles are preserved safely for
      [Stag_runtime_types.layout]. *)
  val concretize : t -> fresh_id:(unit -> int) -> Type.ty -> Type.ty

  (** Structural key of an already-concrete type. *)
  val key_of_concrete_ty : Type.ty -> Key.t

  (** Structural key of [concretize t ~fresh_id ty]. *)
  val key_of_ty : t -> fresh_id:(unit -> int) -> Type.ty -> Key.t

  (** Alphabetically sorted present [(label, concrete_ty)] entries of an
      already-concrete [row], matching [Stag_runtime_types]'s field and case
      ordering. *)
  val sorted_concrete_row_entries : Type.row -> (string * Type.ty) list

  (** Expands top-level [Signature.Type_decl.Kind.Alias] constructors in [ty]. *)
  val expand_aliases
    :  Runtime_types.Resolver.t
    -> fresh_id:(unit -> int)
    -> Type.ty
    -> Type.ty

  (** True when [scheme] introduces no new [Generic] type, row, or presence
      variables beyond those already bound in [t], has no unpinned
      mode-polymorphic variables under [t]'s pins, and [owner] (if any) has no
      mode-polymorphic sides followed by allocation sites. *)
  val is_monomorphic
    :  t
    -> scheme:Type.ty
    -> owner:Typed.Allocation.Owner.t option
    -> owner_sides:(Owner_key.t -> Typed.Allocation.Side.t list)
    -> bool

  (** Matches a generalized [scheme] against a use-site [instance] type under
      [parent], returning the child substitution.

      When [is_toplevel] is [true], type/row/presence bindings and [local_sides]
      start fresh for [owner]; otherwise outer bindings and other owners'
      [local_sides] are inherited (and if [owner = None], [local_sides] is left
      unchanged). When [inherit_pins] is [true], outer mode pins are inherited
      before re-pinning [scheme]'s mode variables (used for nested generalized
      [let]s and same-SCC toplevel calls). *)
  val match_scheme
    :  resolver:Runtime_types.Resolver.t
    -> fresh_id:(unit -> int)
    -> is_toplevel:bool
    -> inherit_pins:bool
    -> owner:Typed.Allocation.Owner.t option
    -> owner_sides:(Owner_key.t -> Typed.Allocation.Side.t list)
    -> parent:t
    -> scheme:Type.ty
    -> instance:Type.ty
    -> t
end
