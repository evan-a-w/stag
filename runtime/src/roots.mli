open! Import

(** What the GC starts from: a shadow stack of frames, each with pointer slots,
    and global slots elsewhere.

    A frame is [next; map; roots...], words from its base: [next] is the base of
    the frame pushed before it, or null, and [map] points to its frame map,
    [{ int32_t num_roots; int32_t num_meta; void *meta[num_meta] }]. That is
    LLVM's [shadow-stack] GC strategy's format, so compiled code can push its
    frames onto the same chain. Every root is a word that is a pointer or null.
    The frames pushed here share one map per slot count, with no metadata.

    Frames are bump allocated in segments, chunks of C memory chained as the
    stack grows, so a frame never moves while it is pushed: the addresses of its
    slots stay valid until it is popped.

    Growing the stack allocates on the OCaml heap, which may run finalisers or
    signal handlers: they must not use [t]. *)

type t

val create : unit -> t

(** Frees the segments: every frame's slots are then dangling. [t] must be kept
    reachable until then, or its memory is freed with it. Pushing afterwards
    raises; there are no frames or globals left. *)
val destroy : t -> unit

(** Pushes a frame of [slots] null slots, giving the address of its first slot.
    Allocates (on the OCaml heap) only when it needs a new segment, or a frame
    map for a slot count it has not had (or, for 1024 slots or more, each time).
    Raises unless [0 <= slots < 2^32]. *)
val push_frame : t -> slots:int -> Memory.Addr.t
[@@zero_alloc]

(** Pops the latest frame. Raises if there is none. *)
val pop_frame : t -> unit
[@@zero_alloc]

(** The number of frames pushed. *)
val depth : t -> int

(** A slot elsewhere (eg. a mutable global in static data) holding a pointer or
    null. Raises if already added. *)
val add_global : t -> Memory.Addr.t -> unit

(** Raises if not added. *)
val remove_global : t -> Memory.Addr.t -> unit

(** Walking the roots without closures, so the GC's walk is zero-alloc. *)

(** The base of the latest frame, or null. *)
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

val globals : t -> int [@@zero_alloc]

(** The address of the [i]th global slot, in no particular order. *)
val global : t -> int -> Memory.Addr.t
[@@zero_alloc]

(** Every root slot: of every frame, latest first, then the globals. [f] must
    not push, pop, add or remove. *)
val iter_slots : t -> f:(Memory.Addr.t -> unit) @ local -> unit

module For_testing : sig
  (** With segments of [segment_bytes] (frames bigger than that get one of their
      own). *)
  val create : segment_bytes:int -> t

  (** Segments made, and the index of the one in use. *)
  val segments : t -> int

  val segment : t -> int
end
