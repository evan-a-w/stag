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
    blocks. *)
val create : Config.t -> Heap.t -> Roots.t -> Data_stack.t -> t

(** Frees the mark stack. Using [t] afterwards raises. The heap, roots and data
    stack are left as they are. *)
val destroy : t -> unit

(** As [Heap.alloc], collecting first if the policy says to. Every pointer the
    caller holds into the heap must be in a root slot (or a block reachable from
    one) across the call. *)
val alloc : t -> Header.t -> Memory.Addr.t

(** Marks, then sweeps. Marking allocates nothing (on the OCaml heap) but to
    grow the mark stack, and sweeping a constant amount but to rebuild the list
    of regions when large blocks are freed. *)
val collect : t -> Stats.t

(** How many collections there have been. *)
val collections : t -> int

(** Between collections: every block [Unmarked] or [Immortal], and every root
    slot and pointer word of an allocated or data-stack block below 4096,
    outside the heap, or in an allocated block (an [Immortal] one, for words of
    [Immortal] blocks); every data-stack block [Unmarked]; and no pointer word
    of a heap block or a block of [static] into the data stack, nor of a block
    of [static] into the heap. *)
val check_invariants : ?static:Static_data.t list -> t -> unit Or_error.t

module For_testing : sig
  (** In blocks. *)
  val mark_stack_capacity : t -> int
end
