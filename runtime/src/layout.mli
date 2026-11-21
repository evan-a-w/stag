open! Import

(** How a value of a [Shape.t] is laid out: a scalar part, of [scalar_bytes] (a
    multiple of [scalar_align]), and a run of [ptr_words] pointer slots. Each
    part of a value is contiguous within each, so a value's places are given
    from two bases: in a heap block the pointer base is the scalar base plus the
    scalar part rounded up to words, elsewhere (eg. a local, whose pointers are
    roots) the two may be apart.

    A union is tagged, or niche-filled (one case stored as itself, the others as
    values its type never has at one of its leaves, as Rust does), whichever is
    smaller. A case that is uninhabited and of no size is never stored, so takes
    no room. *)

type t [@@deriving sexp_of]

(** Errs on [repr C] with pointers, a [repr C] union with no cases, a valid
    range that is empty, exceeds its scalar or is of [Code], or a negative array
    length. *)
val of_shape : Shape.t -> t Or_error.t

val scalar_bytes : t -> int
val scalar_align : t -> int
val ptr_words : t -> int

(** Whether any value has the layout: not of a union all of whose cases are
    uninhabited, nor of a struct with such a field (which still has its fields'
    size), nor a non-empty array of such. *)
val inhabited : t -> bool

module Path : sig
  module Step : sig
    type t =
      | Field of int (** In declaration order, whatever the repr. *)
      | Case of int
      | Index of int
    [@@deriving sexp_of]
  end

  type t = Step.t list [@@deriving sexp_of]
end

(** Where a part of a value is, from the value's scalar and pointer bases. A
    case never stored is of no size, at the union's start. A [Default] struct
    case of a tagged union may be laid out around the tag, its fields after it
    by smallest alignment first: its layout is then not that of the struct on
    its own, so it is not [addressable] as a whole, though its fields are. *)
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

(** Pointer-free or scalar-free, so contiguous: [&] of it is allowed. Also
    within a union's case: writing a valid value of a part never changes the
    discriminant, which only uses values the part never has, and as scalars and
    pointers never share memory, even a write into a case no longer current
    can't make a pointer of an integer. *)
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

(** The niche with the most values, if any. A union's is what its discriminant
    leaves unused. *)
val niche : t -> Niche.t option

(** How a union's case is stored. A case's contents are at [place t [Case i]]. *)
module Discriminant : sig
  type t =
    | Uninhabited (** No case is ever stored: zero size. *)
    | Single of { case : int } (** The only case ever stored. *)
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

(** How to write a whole value of a case of the union at a path, from the
    value's bases: null the union's pointer slots [[ptr_index, ptr_index +
    ptr_words)], then write the case's contents (at [path @ [Case case]]), then
    the discriminant, if any. In that order, the slots the case does not use
    are null, but for a niche slot of its discriminant. *)
module Write_plan : sig
  type t =
    { ptr_index : int
    ; ptr_words : int
    ; discriminant : (Leaf.t * int) option
    (** The value to write at the leaf, modulo its width. *)
    }
  [@@deriving sexp_of]
end

(** Raises unless [path] is of a union, and [case] one of it that is stored. *)
val write_plan : t -> Path.t -> case:int -> Write_plan.t
