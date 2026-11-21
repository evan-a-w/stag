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

module Repr : sig
  type t =
    | Default (** Fields sorted by alignment, largest first. *)
    | C (** Declaration order, C padding. Pointer-free only. *)
  [@@deriving sexp_of, equal]
end

type t =
  | Scalar of
      { scalar : Scalar.t
      ; valid : (int * int) option
      (** The valid values, unsigned and inclusive, if not all. *)
      }
  | Ptr (** Traced. Never below 4096, but in unused union slots null. *)
  | Struct of
      { repr : Repr.t
      ; fields : t list
      }
  | Union of t list (** Its cases. Empty cases make an enum. *)
  | Array of
      { elt : t
      ; length : int
      }
[@@deriving sexp_of]
