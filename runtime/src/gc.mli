open! Import

(** A precise, non-moving mark-sweep collector over a [Heap], from the root
    words of its [Root_source]s and the data stack's live blocks.

    Marking reads every root word and every pointer word of a marked block (its
    words [[scalar_words, size_words)]). A word is followed only if it is at
    least 4096 (below are null and niche values) and within an allocated block
    of the heap (it may point into the block): anything else, such as static
    data, zero-size atoms and foreign memory, is left alone. [Immortal] blocks
    are never traced, so must only point to other immortal blocks or outside the
    heap.

    Collections are not reentrant: nothing a collection runs (finalisers or
    signal handlers, run by the OCaml allocations of growing the mark stack, of
    the heap's metadata table, or of [alloc_handle]) may use the collector, its
    heap or its roots. *)

module Config : sig
  type t =
    { space_overhead : int
    (** Collect once the words allocated since the last collection exceed this
        percentage of the words live after it (as [Heap.footprint_words]:
        headers and rounding up to a class included)... *)
    ; min_words : int (** ...and [min_words]. *)
    }
  [@@deriving sexp_of]

  (** 120, as OCaml's, and 32768 words (256 KiB). *)
  val default : t
end

module Stats : sig
  (** Words are of blocks, without their headers. *)
  type t =
    { marked_blocks : int
    ; marked_words : int
    ; freed_blocks : int
    ; freed_words : int
    }
  [@@deriving sexp_of]
end

type t

(** Its roots are [roots]' root words, in any order, and the pointer words of
    [data_stack]'s live blocks. [static] is the program's static data, which
    [check_invariants] checks (marking needs nothing of it: it is never traced). *)
val create
  :  Config.t
  -> heap:Heap.t
  -> roots:Root_source.t list
  -> data_stack:Data_stack.t
  -> static:Static_data.t
  -> t

(** Frees the mark stack. Using [t] afterwards raises. The heap, roots and data
    stack are left as they are. *)
val destroy : t -> unit

(** As [Heap.alloc], collecting first if the policy says to. Every pointer the
    caller holds into the heap must be in a root word (or a block reachable from
    one) across the call. *)
val alloc : t -> Header.t -> Memory.Addr.t

(** Allocates a [Heap.alloc_handle] block, with the same collection and rooting
    rules as [alloc]. Its creation may collect, so its refs need not be rooted:
    they are, across that collection. Raises for a ref into the data stack. The
    returned address is not itself a root: keep it in a root or a traced field
    across calls that can collect. *)
val alloc_handle : t -> 'a Handle_kind.t -> 'a -> Memory.Addr.t

(** As [Heap.get_handle]: the payload, and any address taken from it, is valid
    only while the handle is reachable from the roots. *)
val get_handle : t -> 'a Handle_kind.t -> Memory.Addr.t -> 'a option

(** As [Heap.handle_ref]. *)
val handle_ref : t -> Memory.Addr.t -> Heap.Handle_ref.t

(** As [Heap.get_handle_checked], with [get_handle]'s validity. *)
val get_handle_checked : t -> 'a Handle_kind.t -> Heap.Handle_ref.t -> 'a option

(** Marks, then sweeps. Marking allocates nothing (on the OCaml heap) but to
    grow the mark stack. Sweeping also releases OCaml metadata for dead handles,
    which allocates. *)
val collect : t -> Stats.t

(** How many collections there have been. *)
val collections : t -> int

(** Between collections:
    - every heap block [Unmarked] or [Immortal];
    - every root word, and pointer word of an allocated, static or data-stack
      block, below 4096, outside the heap, or in an allocated block (an
      [Immortal] one, for words of [Immortal] blocks);
    - the heap's handle blocks and its metadata table agreeing, and each handle
      holding every ref its payload has ([Heap.check_handles]);
    - no pointer word of a heap block or static block, or root word of a source
      not [Root_source.holds_locals], into the data stack, nor of a static block
      into the heap;
    - no root word or data-stack word into the data stack past its live blocks;
    - every data-stack block [Scanned] or [Bytes] and [Unmarked], and each
      segment's blocks ending exactly at its end. *)
val check_invariants : t -> unit Or_error.t

module For_testing : sig
  (** In blocks. *)
  val mark_stack_capacity : t -> int

  (** The next growth of the mark stack raises. *)
  val fail_next_grow : t -> unit

  (** Marks without sweeping: what it reaches is left [Marked] until the next
      collection's sweep. *)
  val mark : t -> unit
  [@@zero_alloc]
end
