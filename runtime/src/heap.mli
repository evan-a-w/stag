open! Import

(** The GC heap: pages of equal-size slots, one size class per page, and large
    blocks in chunks of their own. Every slot is a header and its block. Each
    region (a page or a large block's chunk) starts with a descriptor of its
    slots, and is found from any address in it by a [Page_table]. *)

module Config : sig
  type t =
    { page_bytes : int (** A power of two; regions are aligned to it. *)
    ; size_classes : int list (** Block words, increasing, from 1. *)
    }
  [@@deriving sexp_of]

  (** 64 KiB pages; classes 1-8, 10, 12, 14, 16, 20, 24, 28, 32, 40, 48, 56, 64,
      80, 96, 112, 128, 160, 192, 224, 256 words. Larger blocks are large. *)
  val default : t
end

module Sweep_stats : sig
  (** Words are of blocks, without their headers. *)
  type t =
    { freed_blocks : int
    ; freed_words : int
    ; kept_blocks : int
    ; kept_words : int
    }
  [@@deriving sexp_of]
end

type t

(** Raises unless every slot of the largest class fits in a page. *)
val create : Config.t -> t

(** A block of the size [header] gives, its header written and its words zeroed.
    Zero-size blocks are a shared immortal atom per kind, outside the heap.
    Raises on a [Free] header. *)
val alloc : t -> Header.t -> Memory.Addr.t

(** The block containing [addr], at its first field, if [addr] is within the
    words of a block in [t]: [Memory.Addr.null] for anything else, including
    headers, free slots, descriptors and zero-size atoms. *)
val block_start : t -> Memory.Addr.t -> Memory.Addr.t
[@@zero_alloc]

(** Frees every [Unmarked] block, and makes every [Marked] one [Unmarked]. *)
val sweep : t -> Sweep_stats.t

(** The live blocks, by region in creation order, then by address. *)
val iter : t -> f:(Memory.Addr.t -> unit) @ local -> unit

(** Every region in creation order, by index, with the offset from its base and
    the header of each slot: deterministic. *)
val sexp_of_t : t -> Sexp.t

module For_testing : sig
  (** The index of the region [addr] is in, and its offset from the region's
      base. *)
  val locate : t -> Memory.Addr.t -> (int * int) option

  val regions : t -> int
  val pages : t -> int
end
