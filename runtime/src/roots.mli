open! Import

(** What the GC starts from: a shadow stack of frames, each with pointer slots,
    and global slots elsewhere.

    A frame is [next; root count; roots...], words from its base: [next] is the
    base of the frame pushed before it, or null. That is LLVM's [shadow-stack]
    GC strategy's chain, but for its frame map pointer being the count itself,
    so compiled code can push its frames onto the same chain.

    Frames are bump allocated in segments, chunks of C memory chained as the
    stack grows, so a frame never moves while it is pushed: the addresses of its
    slots stay valid until it is popped. *)

type t

val create : unit -> t

(** Pushes a frame of [slots] null slots, giving the address of its first slot.
    Allocates (on the OCaml heap) only when it needs a new segment. *)
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

module Frame : sig
  (** Of a frame's base. *)

  (** The base of the frame pushed before it, or null. *)
  val next : Memory.Addr.t -> Memory.Addr.t
  [@@zero_alloc]

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
