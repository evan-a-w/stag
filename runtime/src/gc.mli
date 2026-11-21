open! Import

(** A precise, non-moving mark-sweep collector over a [Heap], from the slots of
    [Roots].

    Marking reads every root slot and every pointer word of a marked block (its
    words [[scalar_words, size_words)]). A word is followed only if it is at
    least 4096 (below are null and niche values) and within an allocated block
    of the heap (it may point into the block): anything else, such as static
    data, zero-size atoms and foreign memory, is left alone. [Immortal] blocks
    are never traced, so must only point to other immortal blocks or outside the
    heap.

    Collections are not reentrant: nothing a collection runs (finalisers or
    signal handlers, run by the OCaml allocations of growing the mark stack)
    may use the collector, its heap or its roots. *)

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

(** Its roots are [roots]' slots, and the pointer words of [data_stack]'s live
    blocks. [static] is the program's static data, which [check_invariants]
    checks (marking needs nothing of it: it is never traced). *)
val create
  :  Config.t
  -> heap:Heap.t
  -> roots:Roots.t
  -> data_stack:Data_stack.t
  -> static:Static_data.t
  -> t

(** Frees the mark stack. Using [t] afterwards raises. The heap, roots and data
    stack are left as they are. *)
val destroy : t -> unit

(** As [Heap.alloc], collecting first if the policy says to. Every pointer the
    caller holds into the heap must be in a root slot (or a block reachable from
    one) across the call. *)
val alloc : t -> Header.t -> Memory.Addr.t

(** Allocates a [Heap.alloc_handle] block, with the same collection and rooting
    rules as [alloc]. The returned address is not itself a root. Keep it in a
    root or a traced field across calls that can collect. The OCaml value may
    not be used as the sole owner of any Stag heap pointers it contains. *)
val alloc_handle : t -> 'a Type_equal.Id.t -> 'a -> Memory.Addr.t

(** As [Heap.get_handle]. *)
val get_handle : t -> Memory.Addr.t -> 'a Type_equal.Id.t -> 'a option

(** Marks, then sweeps. Marking allocates nothing (on the OCaml heap) but to
    grow the mark stack. Sweeping also releases OCaml metadata for dead handles. *)
val collect : t -> Stats.t

(** How many collections there have been. *)
val collections : t -> int

(** Between collections:
    - every heap block [Unmarked] or [Immortal];
    - every root slot, and pointer word of an allocated, static or data-stack
      block, below 4096, outside the heap, or in an allocated block (an
      [Immortal] one, for words of [Immortal] blocks);
    - no pointer word of a heap block, static block or global into the data
      stack, nor of a static block into the heap;
    - no frame slot or data-stack word into the data stack past its live blocks;
    - every data-stack block [Scanned] or [Bytes] and [Unmarked], and each
      segment's blocks ending exactly at its end. *)
val check_invariants : t -> unit Or_error.t

module For_testing : sig
  (** In blocks. *)
  val mark_stack_capacity : t -> int
end
