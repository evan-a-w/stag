open! Import

(** A mode as written after [@]: [@ 'm] or [@ local portable]. *)

module Axis : sig
  type t =
    | Stage
    | Provenance
    | Locality
  [@@deriving sexp_of, compare, equal]

  val to_string : t -> string
  val of_string : string -> t option
end

module Atom : sig
  type t =
    | Const
    | Portable
    | Nonportable
    | Static
    | Gc
    | External
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
  | Var_at of Atom.t * string
  (** [@ local 'm]: a locality, and a variable for the other axes. *)
[@@deriving sexp_of, compare, equal]

type t = desc Located.t [@@deriving sexp_of, compare, equal]

val to_string : t -> string

(** ['m], or [_] for an unnamed variable. *)
val var_to_string : string -> string

(** Why a mode with a locality and a variable isn't a side of a constraint. *)
val var_at_in_constraint : string

(** A side of a [with] clause's [lower <= upper]. *)
module Term : sig
  type mode := t

  type t =
    | Mode of mode
    (** On every axis: ['m], or atoms bounding the other side on theirs. *)
    | Axis of Axis.t * mode (** A variable on one axis: [stage('m)]. *)
    | Meet of t * Atom.t (** [meet(t, gc)], on the left. *)
    | Release of t (** [release(t)], on locality, on the left. *)
    | Static_if_const of t
    (** [static_if_const(t)], from stage to provenance, on the right. *)
    | Const_if_static of t
    (** [const_if_static(t)], from provenance to stage, on the right. *)
  [@@deriving sexp_of, compare, equal]

  val to_string : t -> string
end

(** Of a [with] clause. [lower <= upper]: between modes, at least one of them a
    variable, on every axis; or between terms of variables on one axis. *)
module Constraint : sig
  type t =
    | Submode of
        { lower : Term.t
        ; upper : Term.t
        }
    | Pointer_free of string (** ['a : pointer_free]. *)
  [@@deriving sexp_of, compare, equal]

  val to_string : t -> string

  (** Why the constraint is not of one of these shapes, if it is not. *)
  val check : t -> (unit, string) Result.t
end
