open! Import

(** The owner of each page-aligned address: the base of the heap region it is
    in. Open addressing on unboxed arrays, so lookups don't allocate. *)

type t

(** [page_bytes] is a power of two, at least 8. *)
val create : page_bytes:int -> t

val page_bytes : t -> int

(** Raises if [page] or [owner] is not aligned, or [page] already has an owner. *)
val add : t -> page:Memory.Addr.t -> owner:Memory.Addr.t -> unit

(** Raises if [page] has no owner. *)
val remove : t -> page:Memory.Addr.t -> unit

(** The owner of the page [addr] is in, or [Memory.Addr.null]. Any address. *)
val find : t -> Memory.Addr.t -> Memory.Addr.t
[@@zero_alloc]

val length : t -> int

(** The number of slots. *)
val capacity : t -> int

module For_testing : sig
  (** The slot a page would hash to, and the one it is in. *)
  val home : t -> page:Memory.Addr.t -> int

  val slot : t -> page:Memory.Addr.t -> int option
end
