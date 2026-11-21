open! Import

(** A root source: a shadow stack of frames, each with pointer slots.

    A frame is [next; map; roots...], words from its base: [next] is the base of
    the frame pushed before it, or null, and [map] points to its frame map,
    [{ int32_t num_roots; int32_t num_meta; void *meta[num_meta] }]. That is
    LLVM's [shadow-stack] GC strategy's format, and the latest frame's base is
    in the word at [chain_head], so compiled code can push its own frames onto
    the chain by aliasing that word as [llvm_gc_root_chain]. Every slot is a
    word that is a pointer, null or a small value below 4096. LLVM's [num_roots]
    may count padding words too: they are ignored unless they look like pointers
    into the heap, which keeps what they point to alive (and is reported by
    [Gc.check_invariants] if it is not a block).

    Frames pushed by others (foreign frames) may be anywhere, eg. on the machine
    stack, and must be popped by whoever pushed them, before any frame pushed
    here earlier is popped. Here, [depth] counts only the frames pushed here.
    The frames pushed here share one map per slot count, with no metadata.

    Frames are bump allocated in segments, chunks of C memory chained as the
    stack grows, so a frame never moves while it is pushed: the addresses of its
    slots stay valid until it is popped.

    Growing the stack allocates on the OCaml heap, which may run finalisers or
    signal handlers: they must not use [t]. *)

type t

val create : unit -> t

(** Frees the segments and the chain head: every frame's slots are then
    dangling. Pushing afterwards raises; there are no frames left. *)
val destroy : t -> unit

(** The word holding the base of the latest frame (pushed here or not), or null.
    It stays at the same address until [destroy]. *)
val chain_head : t -> Memory.Addr.t
[@@zero_alloc]

(** Pushes a frame of [slots] null slots, giving the address of its first slot.
    Allocates (on the OCaml heap) only when it needs a new segment, or a frame
    map for a slot count it has not had (or, for 1024 slots or more, each time).
    Raises unless [0 <= slots < 2^31], as [num_roots] is an [int32_t]. *)
val push_frame : t -> slots:int -> Memory.Addr.t
[@@zero_alloc]

(** Pops the latest frame pushed here. Raises if there is none, or if it is not
    the latest on the chain (a foreign frame is still pushed). *)
val pop_frame : t -> unit
[@@zero_alloc]

(** The number of frames pushed here, and not popped. *)
val depth : t -> int

(** Walking the frames without closures. *)

(** The base of the latest frame on the chain (pushed here or not), or null. *)
val latest_frame : t -> Memory.Addr.t
[@@zero_alloc]

module Frame_map : sig
  (** Of a frame map's address. *)

  val num_roots : Memory.Addr.t -> int [@@zero_alloc]
  val num_meta : Memory.Addr.t -> int [@@zero_alloc]
end

module Frame : sig
  (** Of a frame's base. *)

  (** The base of the frame pushed before it, or null. *)
  val next : Memory.Addr.t -> Memory.Addr.t
  [@@zero_alloc]

  val map : Memory.Addr.t -> Memory.Addr.t [@@zero_alloc]

  (** Its map's [num_roots]. *)
  val slots : Memory.Addr.t -> int [@@zero_alloc]

  (** The address of slot [i]. *)
  val slot : Memory.Addr.t -> int -> Memory.Addr.t
  [@@zero_alloc]
end

module For_testing : sig
  (** With segments of [segment_bytes] (frames bigger than that get one of their
      own). *)
  val create : segment_bytes:int -> t

  (** Segments made, and the index of the one in use. *)
  val segments : t -> int

  val segment : t -> int
end

(** Every slot of every frame on the chain (pushed here or not), latest first.
    It may hold locals. *)
val root_source : t -> Root_source.t
