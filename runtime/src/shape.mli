open! Import

(** The shape of a monomorphic value, which [Layout] lays out. *)

module Scalar : sig
  type t =
    | I8
    | I16
    | I32
    | I64
    | F32
    | F64
    | Code (** A code pointer: untraced, never below 4096. *)
  [@@deriving sexp_of, equal]

  (** Also its alignment. *)
  val bytes : t -> int
end

(** [C] fixes only the order of a struct's own fields (or makes a union a C
    tagged union): the fields keep their own layouts and niches, which are still
    used outward. Whether a type is C compatible all the way down is for the FFI
    to check. *)
module Repr : sig
  type t =
    | Default
    (** A struct's fields sorted by alignment, largest first. A union's cases
        tagged or niche-filled, whichever is smaller. *)
    | C
    (** A struct's fields in declaration order, with C padding. A union as
        [struct { int32_t tag; union { ... } }]. Pointer-free only. *)
  [@@deriving sexp_of, equal]
end

type t =
  | Scalar of
      { scalar : Scalar.t
      ; valid : (int * int) option
      (** The valid values, unsigned and inclusive, if not all. Not of [Code],
          whose valid values are those from 4096. *)
      }
  | Ptr
  (** Traced. Never below 4096, but in the slots of a union its case doesn't
      use: null, or a discriminant below 4096. *)
  | Struct of
      { repr : Repr.t
      ; fields : t list
      }
  | Union of
      { repr : Repr.t
      ; cases : t list (** Empty cases make an enum. *)
      }
  | Array of
      { elt : t
      ; length : int
      }
[@@deriving sexp_of]
