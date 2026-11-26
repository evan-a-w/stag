open! Import

module Axis : sig
  module type Chain = Axis.Chain

  module Stage = Axis.Stage
  module Provenance = Axis.Provenance
  module Locality = Axis.Locality

  type 'a t = 'a Axis.t =
    | Stage : Stage.t t
    | Provenance : Provenance.t t
    | Locality : Locality.t t

  val chain : 'a t -> (module Chain with type t = 'a)

  module Any : sig
    type 'a axis := 'a t
    type t = Axis.Any.t = T : 'a axis -> t

    val sexp_of_t : t -> Sexp.t
    val all : t list
    val equal : t -> t -> bool
    val size : t -> int
    val index : t -> int
  end
end

module Stage = Axis.Stage
module Provenance = Axis.Provenance
module Locality = Axis.Locality
module Const = Const
module Layout = Layout
module Crossing = Crossing
module Locality_map = Locality_map
module Error = Mode_error

module Id_source : sig
  type t = Id_source.t

  val create : unit -> t
end

module Reason = Reason
module Var = Var
module Morphism = Morphism
module Bitset = Bitset
module Written_morphism = Written_morphism

module Private : sig
  module Undo = Undo
  module Implied_constraints = Implied_constraints
end
