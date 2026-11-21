open! Import

(** Blocks of [local_] values, freed by region: [enter] gives a mark, and [exit]
    frees every block allocated since it, so regions nest.

    Blocks are laid out as heap blocks are (a header, then the block), always
    [Unmarked], bump allocated in chained segments, so they never move. They may
    point to heap blocks, static data and other data-stack blocks; heap blocks
    and static data never point here (the typechecker's job: mutable fields and
    stores through pointers are implicitly global).

    For the GC, every live block's pointer words are roots. The blocks
    themselves are never marked: pointers to them are not followed
    ([Heap.block_start] is null for them), as every live one is scanned. *)

type t

(** Segments of [segment_bytes] (64 KiB by default); a block too large for one
    gets a segment of its own. *)
val create : ?segment_bytes:int -> unit -> t

(** Frees its segments. Using it afterwards raises. *)
val destroy : t -> unit

module Mark : sig
  type t : immediate

  val equal : t -> t -> bool [@@zero_alloc]
  val sexp_of_t : t -> Sexp.t
end

val enter : t -> Mark.t [@@zero_alloc]

(** Frees every block allocated since [mark]. Raises if [mark] is later than the
    latest block, so a region exited twice or out of order raises unless others
    were entered since. *)
val exit : t -> Mark.t -> unit
[@@zero_alloc]

(** Zeroed, its header [header] made [Unmarked]. Zero-size blocks are
    [Static_data.empty] and [Static_data.empty_bytes]. Allocates (on the OCaml
    heap) only when it needs a segment it has not had. Raises for a [Free]
    header. *)
val alloc : t -> Header.t -> Memory.Addr.t
[@@zero_alloc]

(** Whether [addr] is in one of its segments, live or freed. *)
val contains : t -> Memory.Addr.t -> bool
[@@zero_alloc]

(** {2 Walking the live blocks, without closures}

    Segments [0, segments t) have live blocks: in segment [i], the blocks from
    [first_block t i] while before [segment_end t i], each after the last by
    [next_block]. *)

val segments : t -> int [@@zero_alloc]
val first_block : t -> int -> Memory.Addr.t [@@zero_alloc]
val segment_end : t -> int -> Memory.Addr.t [@@zero_alloc]
val next_block : Memory.Addr.t -> Memory.Addr.t [@@zero_alloc]

(** Of the live blocks, oldest first. [f] must not allocate here or exit. *)
val iter : t -> f:(Memory.Addr.t -> unit) @ local -> unit

(** The live blocks, by segment and offset, with their headers: deterministic. *)
val sexp_of_t : t -> Sexp.t

module For_testing : sig
  (** Segments made, live or not. *)
  val segments_made : t -> int
end
