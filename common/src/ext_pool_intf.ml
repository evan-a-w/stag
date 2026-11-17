open! Import

module Definitions = struct
  [%%template
  [@@@kind_set.define
    pointer_free
    = ( immediate
      , float64
      , bits64
      , bits32
      , word
      , immediate & immediate
      , immediate & immediate & immediate
      , float64 & float64
      , bits64 & bits64 )]

  module type%template Element = sig
    type t : k
  end
  [@@kind k = pointer_free]

  (** A pool of [elt]s in unboxed arrays, outside the GC's view (every kind it
      is instantiated at is free of heap pointers), referred to by immediate
      handles, so any container of values can hold them.

      Pools and handles are branded with ['brand], fresh for each pool, so a
      handle is only usable with the pool it came from, and neither outlives the
      scope the brand is introduced in. A handle freed (or cleared) is stale:
      using it raises, until its slot has been reused 2^30 times (generations
      wrap around, 31 bits, each reuse taking two). A pool holds at most 2^32
      slots. *)
  module type%template S = sig
    type elt : k

    module Handle : sig
      type 'brand t : immediate

      (** The slot, unique among the pool's live handles. *)
      val index : _ t -> int

      val equal : 'brand t -> 'brand t -> bool
      val compare : 'brand t -> 'brand t -> int
      val hash : _ t -> int

      (** [(index generation)]. *)
      val sexp_of_t : _ t -> Sexp.t
    end

    type 'brand t : value mod non_float
    type packed = T : 'brand t -> packed [@@unboxed]

    val create : unit -> packed
    val with_ : f:('brand. 'brand t -> 'a) @ local -> 'a
    val alloc : 'brand t -> elt -> 'brand Handle.t
    val get : 'brand t -> 'brand Handle.t -> elt
    val set : 'brand t -> 'brand Handle.t -> elt -> unit
    val is_live : 'brand t -> 'brand Handle.t -> bool

    (** Its slot is reused by a later [alloc]. *)
    val free : 'brand t -> 'brand Handle.t -> unit

    (** Frees every handle at once, keeping the storage. *)
    val clear : 'brand t -> unit

    (** Live handles. *)
    val length : _ t -> int

    (** In slot order. *)
    val iter : 'brand t -> f:('brand Handle.t -> elt -> unit) @ local -> unit
  end
  [@@kind k = pointer_free]]
end

module type Ext_pool = sig
  include module type of struct
    include Definitions
  end

  [%%template:
  [@@@kind_set.define
    pointer_free
    = ( immediate
      , float64
      , bits64
      , bits32
      , word
      , immediate & immediate
      , immediate & immediate & immediate
      , float64 & float64
      , bits64 & bits64 )]

  module%template Make (E : Element [@kind k]) : S [@kind k] with type elt = E.t
  [@@kind k = pointer_free]]

  module For_testing : sig
    module type Config = sig
      val index_bits : int
      val generation_bits : int
    end

    (** As [Make], with handles of [index_bits] and [generation_bits] (by
        default 32 and 31), so that running out of slots and generations
        wrapping around can be shown. *)
    module%template Make (_ : Config) (E : Element [@kind immediate]) :
      S [@kind immediate] with type elt = E.t
  end
end
