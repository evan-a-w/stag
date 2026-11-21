open! Import

(** Immortal blocks, never freed (until [destroy]), for values known at compile
    time and for toplevels. Their headers are [Immortal], so the GC never marks,
    scans or frees them: they must point only to static data (or outside the
    heap). The one exception is a [Global]'s pointer slots, which are roots.

    Blocks are bump allocated in segments, chunks of C memory, and never move. *)

type t

val create : unit -> t

(** Frees every block: every address it gave is then dangling. The globals'
    slots must have been removed from their roots. Using [t] afterwards raises. *)
val destroy : t -> unit

(** A block of the size [header] gives, zeroed, its header [Immortal] whatever
    [header]'s colour. Zero-size blocks are [empty] and [empty_bytes]. Raises on
    a [Free] header. *)
val alloc : t -> Header.t -> Memory.Addr.t

(** The zero-size blocks, shared by everything (heaps included): a [Scanned] one
    and a [Bytes] one, of length 0. Both [Immortal], never freed. *)
val empty : unit -> Memory.Addr.t
[@@zero_alloc]

val empty_bytes : unit -> Memory.Addr.t [@@zero_alloc]

(** Whether [addr] is in one of [t]'s blocks, headers included. *)
val contains : t -> Memory.Addr.t -> bool

(** Every block of [t], in allocation order, globals' storage included. [f] must
    not allocate in [t]. *)
val iter : t -> f:(Memory.Addr.t -> unit) @ local -> unit

(** The storage of a mutable global of any layout: its scalar part, then its
    pointer run, each pointer slot a root. It is in a [Bytes] block, so not
    traced as an immortal block's pointers would be. *)
module Global : sig
  type t

  val layout : t -> Layout.t

  (** The base of its scalar part, [Layout.scalar_bytes] bytes. *)
  val scalars : t -> Memory.Addr.t

  (** The base of its pointer run, [Layout.ptr_words] slots. *)
  val ptrs : t -> Memory.Addr.t

  (** Of the [Bytes] block holding it. *)
  val block : t -> Memory.Addr.t
end

(** Zeroed, its pointer slots added to [roots]. *)
val add_global : t -> Roots.t -> Layout.t -> Global.t

(** Removes its slots from [roots]. Its storage is not reused, and raises if
    already removed. *)
val remove_global : Roots.t -> Global.t -> unit

(** Every block, by offset from the start of its segment, with its header:
    deterministic. *)
val sexp_of_t : t -> Sexp.t
