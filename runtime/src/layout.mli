open! Import

(** How a value of a [Shape.t] is laid out: a scalar part, of [scalar_bytes] (a
    multiple of [scalar_align]), and a run of [ptr_words] pointer slots. Each
    part of a value is contiguous within each, so a value's places are given
    from two bases: in a heap block the pointer base is the scalar base plus the
    scalar part rounded up to words, elsewhere (eg. a local, whose pointers are
    roots) the two may be apart. *)

type t [@@deriving sexp_of]

(** Errs on [repr C] with pointers, a valid range that is empty or exceeds its
    scalar, or a negative array length. *)
val of_shape : Shape.t -> t Or_error.t

val scalar_bytes : t -> int
val scalar_align : t -> int
val ptr_words : t -> int

module Path : sig
  type step =
    | Field of int (** In declaration order, whatever the repr. *)
    | Case of int
    | Index of int
  [@@deriving sexp_of]

  type t = step list [@@deriving sexp_of]
end

(** Where a part of a value is, from the value's scalar and pointer bases. *)
module Place : sig
  type nonrec t =
    { scalar_offset : int
    ; ptr_index : int
    ; layout : t
    }
  [@@deriving sexp_of]
end

(** Raises on a path that does not fit [t]. *)
val place : t -> Path.t -> Place.t

(** Pointer-free or scalar-free, so contiguous: [&] of it is allowed. *)
val addressable : t -> Path.t -> bool

(** Of an [Array], for indices known only at run time. *)
module Element : sig
  type nonrec t =
    { scalar_stride : int
    ; ptr_stride : int
    ; layout : t
    }
  [@@deriving sexp_of]
end

val element : t -> Element.t option

module Leaf : sig
  type t =
    | Scalar of
        { scalar_offset : int
        ; scalar : Shape.Scalar.t
        }
    | Ptr of { ptr_index : int } (** As an unsigned 64 bit value. *)
  [@@deriving sexp_of]
end

(** Values no value of [t] has at [leaf]: [available] of them from [start],
    wrapping around the leaf's width. *)
module Niche : sig
  type t =
    { leaf : Leaf.t
    ; start : int
    ; available : int (** Saturated at [Int.max_value]. *)
    }
  [@@deriving sexp_of]
end

(** The largest: of a union, what its discriminant leaves. *)
val niche : t -> Niche.t option

(** How a union's case is stored. A case's contents are at [place t [Case i]]. *)
module Discriminant : sig
  type t =
    | Uninhabited (** No cases: zero size, never stored. *)
    | Single (** One case. *)
    | Tag of Leaf.t (** Holds the case's index. *)
    | Niche of
        { leaf : Leaf.t
        ; untagged : int (** The case whose own value is there. *)
        ; first : int
        ; last : int
        ; start : int
        (** Case [c] in [first, last], other than [untagged], is stored as
            [start + c - first], wrapping. Any other value is [untagged]. *)
        }
  [@@deriving sexp_of]
end

(** Of a [Union]. *)
val discriminant : t -> Discriminant.t option
