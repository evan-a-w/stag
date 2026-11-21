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

(** A block for an OCaml [value] of [kind]: its scalar ID word, which must not
    be modified, then [Handle_kind.refs kind value] as its traced pointer words,
    fixed for its life, so the GC keeps the Stag values [value] refers to alive
    as long as the block (see [Handle_kind] for what that asks of [value]). The
    heap holds [value] until the block is swept or the heap is destroyed; the
    Stag GC never scans it.

    The refs may be blocks or static data, not data-stack blocks; raises for a
    ref into the heap but no block. While it runs, the new block is marked, so a
    collection reentered by an OCaml allocation (which is forbidden) cannot free
    it before it has its metadata; such a collection would not trace its refs,
    which [Gc.alloc_handle] roots for it. *)
val alloc_handle : t -> 'a Handle_kind.t -> 'a -> Memory.Addr.t

(** [alloc_handle], with [refs] already computed as
    [Handle_kind.refs kind value], for [Gc.alloc_handle], which roots them
    first. *)
val alloc_handle_with_refs
  :  t
  -> 'a Handle_kind.t
  -> 'a
  -> Memory.Addr.t array
  -> Memory.Addr.t

(** Returns a handle's OCaml value, or [None] if [addr] is not a live handle's
    base in this heap of [kind]. A swept address may be reused, by a handle of
    any ID: callers must not use a stale address (see [get_handle_checked]). *)
val get_handle : t -> 'a Handle_kind.t -> Memory.Addr.t -> 'a option

(** Raises unless [addr] is a live handle's base. *)
val handle_ref : t -> Memory.Addr.t -> Handle_ref.t

(** As [get_handle], but [None] once the handle has been swept, even if its
    address has been reused. *)
val get_handle_checked : t -> 'a Handle_kind.t -> Handle_ref.t -> 'a option

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
    handle block, with its ID and address, and a handle block for every entry,
    holding every ref its payload has now ([Handle_kind.refs]). Empty other than
    after corruption, or a payload breaking [Handle_kind]'s rules: sweep keeps
    them in step. [Gc.check_invariants] uses this. *)
val check_handles : t -> Sexp.t list

module For_testing : sig
  val handles : t -> int

  (** IDs then count up from [id]: an ID in use makes [alloc_handle] raise. *)
  val set_next_handle_id : t -> int -> unit

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
