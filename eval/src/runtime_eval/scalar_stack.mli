open! Import

(** Chunked off-heap bump stack for non-pointer (scalar) frame locals. Segments
    are never moved while live, so addresses returned by [alloc] remain valid
    until their enclosing [enter] mark is exited. *)

module Mark : sig
  type t : immediate

  val equal : t -> t -> bool [@@zero_alloc]
  val sexp_of_t : t -> Sexp.t
end

type t

(** Segments of [segment_bytes] (64 KiB by default); a frame larger than a
    segment gets a segment of its own. *)
val create : ?segment_bytes:int -> unit -> t

(** Frees all segments. Using [t] afterwards raises. *)
val destroy : t -> unit

val enter : t -> Mark.t [@@zero_alloc]

(** Zeroed, 8-byte aligned scalar storage of at least [bytes] bytes. A 0-byte
    allocation touches no memory and returns [Memory.Addr.null ()]. *)
val alloc : t -> int -> Memory.Addr.t
[@@zero_alloc]

(** Exits [mark] and any nested marks, reclaiming all allocations made since it
    was entered. Raises if [mark] is not a live mark. *)
val exit : t -> Mark.t -> unit
[@@zero_alloc]
