open! Import

(** A mode as written after [@]: [@ 'm] or [@ local portable]. *)

module Axis : sig
  type t =
    | Stage
    | Provenance
    | Locality
  [@@deriving sexp_of, compare, equal]

  val to_string : t -> string
end

module Atom : sig
  type t =
    | Const
    | Portable
    | Nonportable
    | Static
    | Gc
    | Unknown
    | Global
    | Regional
    | Local
  [@@deriving sexp, compare, equal, enumerate]

  val axis : t -> Axis.t
  val to_string : t -> string
  val of_string : string -> t option
end

type desc =
  | Var of string (** [@ 'm], standing for every axis. *)
  | Atoms of Atom.t list (** Non-empty, at most one of each axis. *)
[@@deriving sexp_of, compare, equal]

type t = desc Located.t [@@deriving sexp_of, compare, equal]

val to_string : t -> string

(** [lower <= upper] of a [with] clause, at least one of them a variable. *)
module Constraint : sig
  type mode := t

  type t =
    { lower : mode
    ; upper : mode
    }
  [@@deriving sexp_of, compare, equal]

  val to_string : t -> string
end
