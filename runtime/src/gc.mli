open! Import

(** A precise, non-moving mark-sweep collector over a [Heap], from the slots of
    [Roots].

    Marking reads every root slot and every pointer word of a marked block (its
    words [[scalar_words, size_words)]). A word is followed only if it is at
    least 4096 (below are null and niche values) and within an allocated block
    of the heap (it may point into the block): anything else, such as static
    data, zero-size atoms and foreign memory, is left alone. [Immortal] blocks
    are never traced, so must only point to other immortal blocks or outside the
    heap. *)

module Config : sig
  type t =
    { space_overhead : int
    (** Collect once the words allocated since the last collection exceed this
        percentage of the words live after it... *)
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

val create : Config.t -> Heap.t -> Roots.t -> t

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
    slot and pointer word of an allocated block below 4096, outside the heap, or
    in an allocated block (an [Immortal] one, for words of [Immortal] blocks). *)
val check_invariants : t -> unit Or_error.t

module For_testing : sig
  (** In blocks. *)
  val mark_stack_capacity : t -> int
end
