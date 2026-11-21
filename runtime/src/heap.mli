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
    ; arena_bytes : int (** A power of two, from [page_bytes] to 1 GiB. *)
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
    ; kept_footprint_words : int
    (** Of the kept blocks' slots, headers and rounding up to a class (or to
        pages, for large blocks) included. *)
    }
  [@@deriving sexp_of]
end

type t

(** Raises on an invalid config, or one whose largest class's slot doesn't fit
    in a page. *)
val create : Config.t -> t

(** A block of the size [header] gives, its header written and its words zeroed.
    Zero-size blocks are [Static_data.empty] and [Static_data.empty_bytes].
    Raises on a [Free], [Handle] or [Marked] header. *)
val alloc : t -> Header.t -> Memory.Addr.t

(** A block pointing indirectly to an OCaml value of the type identified by
    [key], with [refs] as its traced pointer words: they keep alive the Stag
    values the OCaml value refers to, and must be a superset of them (extra refs
    are harmless; missing ones dangle). [refs] are fixed for its life, which
    suits closures, whose captures are. Its scalar ID word must not be modified.
    The heap holds the value until the block is swept or the heap is destroyed.
    The OCaml value itself is never scanned by the Stag GC.

    [refs] may hold blocks or static data, not data-stack blocks. While it runs,
    the new block is marked, so a collection reentered by an OCaml allocation
    (which is forbidden) cannot leave it without its metadata. *)
val alloc_handle
  :  t
  -> refs:Memory.Addr.t array
  -> 'a Type_equal.Id.t
  -> 'a
  -> Memory.Addr.t

(** Returns a handle's OCaml value, or [None] if [addr] is not a live handle's
    base in this heap with the given type ID. A swept address may be reused;
    callers must not use a stale address. *)
val get_handle : t -> Memory.Addr.t -> 'a Type_equal.Id.t -> 'a option

(** The words a block of [header] takes in the heap, its header and rounding up
    to a class (or to pages, for a large block) included: 0 for zero-size
    blocks. *)
val footprint_words : t -> Header.t -> int

(** The block containing [addr], at its first field, if [addr] is within the
    words of an allocated block of [t]: [Memory.Addr.null] for anything else,
    including headers, free slots, descriptors and static data. *)
val block_start : t -> Memory.Addr.t -> Memory.Addr.t
[@@zero_alloc]

(** Whether [addr] is in one of [t]'s regions: its pages, descriptors and slots,
    allocated or not. *)
val owns : t -> Memory.Addr.t -> bool
[@@zero_alloc]

(** Frees every [Unmarked] block, and makes every [Marked] one [Unmarked]. The
    pages of freed large blocks, and pages of slots left with no block, go back
    to their arena, for any class or large block to take. Arenas left with no
    page in use go back to the system, but for the first. *)
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

(** Whether the metadata table and the handle blocks agree: an entry for every
    handle block, with its ID and address, and a handle block for every entry.
    Empty other than after corruption: sweep keeps them in step, and
    [check_invariants] uses this. *)
val check_handles : t -> Sexp.t list

module For_testing : sig
  val handles : t -> int

  (** The index of the region [addr] is in, and its offset from the region's
      base. *)
  val locate : t -> Memory.Addr.t -> (int * int) option

  val regions : t -> int
  val pages : t -> int
  val arenas : t -> int

  (** Of each arena, in creation order. *)
  val free_pages : t -> int list

  (** The index of the arena [addr] is in, in creation order, and of its page
      there. *)
  val arena_page : t -> Memory.Addr.t -> (int * int) option
end
