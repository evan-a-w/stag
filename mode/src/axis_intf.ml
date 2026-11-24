open! Import

(** An axis is a chain: a total order with a bottom and a top. *)
module type Chain = sig
  type t [@@deriving sexp, compare, equal, enumerate]

  val min : t
  val max : t
  val le : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t

  (** The position in the chain, from [0] for [min]. *)
  val index : t -> int

  val of_index_exn : int -> t
end

module type Axis = sig
  module type Chain = Chain

  module Stage : sig
    type t =
      | Const
      | Portable
      | Nonportable

    include Chain with type t := t
  end

  module Provenance : sig
    type t =
      | Static
      | Gc
      | External

    include Chain with type t := t
  end

  module Locality : sig
    type t =
      | Global
      | Regional
      | Local

    include Chain with type t := t
  end

  type 'a t =
    | Stage : Stage.t t
    | Provenance : Provenance.t t
    | Locality : Locality.t t

  val chain : 'a t -> (module Chain with type t = 'a)

  module Any : sig
    type 'a axis := 'a t
    type t = T : 'a axis -> t

    val sexp_of_t : t -> Sexp.t
    val all : t list
    val index : t -> int
    val equal : t -> t -> bool
    val size : t -> int

    (** A point of the axis by its index. *)
    val sexp_of_point : t -> int -> Sexp.t
  end
end
