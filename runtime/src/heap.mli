open! Import

(** The GC heap: pages of equal-size slots, one size class per page, and large
    blocks in runs of pages of their own. Every slot is a header and its block.
    Pages come from arenas, chunks of [arena_bytes] aligned once; a block too
    large for an arena gets a chunk of its own. Each region (a page, or a large
    block's pages) starts with a descriptor of its slots, and is found from any
    address in it by a [Page_table]. *)

module Config : sig
  type t =
    { page_bytes : int
    (** A power of two, from 64 bytes to 1 MiB; regions are aligned to it. *)
    ; arena_bytes : int (** A power of two, at least [page_bytes]. *)
    ; size_classes : int list (** Block words, increasing, from 1. *)
    }
  [@@deriving sexp_of]

  (** 64 KiB pages and 4 MiB arenas; classes 1-8, 10, 12, 14, 16, then four per
      doubling up to 1024 words. Larger blocks are large. *)
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

(** Raises on an invalid config, or one whose largest class's slot doesn't fit
    in a page. *)
val create : Config.t -> t

(** A block of the size [header] gives, its header written and its words zeroed.
    Zero-size blocks are a shared immortal atom per kind, outside the heap.
    Raises on a [Free] header. *)
val alloc : t -> Header.t -> Memory.Addr.t

(** The block containing [addr], at its first field, if [addr] is within the
    words of an allocated block of [t]: [Memory.Addr.null] for anything else,
    including headers, free slots, descriptors and zero-size atoms. *)
val block_start : t -> Memory.Addr.t -> Memory.Addr.t
[@@zero_alloc]

(** Whether [addr] is in one of [t]'s regions: its pages, descriptors and slots,
    allocated or not. *)
val owns : t -> Memory.Addr.t -> bool
[@@zero_alloc]

(** Frees every [Unmarked] block, and makes every [Marked] one [Unmarked]. The
    pages of freed large blocks go back to their arena. Pages of slots are kept,
    even when all their slots are free. *)
val sweep : t -> Sweep_stats.t

(** The allocated blocks (before a sweep, garbage included), by region in
    creation order, then by address. [f] must not allocate or sweep. *)
val iter : t -> f:(Memory.Addr.t -> unit) @ local -> unit

(** Frees all of [t]'s memory: every address it gave is then dangling. Using [t]
    afterwards raises, but for [block_start], which gives null. *)
val destroy : t -> unit

(** Every region in creation order, by index, with the offset from its base and
    the header of each slot: deterministic. *)
val sexp_of_t : t -> Sexp.t

module For_testing : sig
  (** The index of the region [addr] is in, and its offset from the region's
      base. *)
  val locate : t -> Memory.Addr.t -> (int * int) option

  val regions : t -> int
  val pages : t -> int
  val arenas : t -> int

  (** The index of the arena [addr] is in, in creation order, and of its page
      there. *)
  val arena_page : t -> Memory.Addr.t -> (int * int) option
end
