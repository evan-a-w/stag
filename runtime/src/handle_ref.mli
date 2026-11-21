open! Import

(** A handle by its address and its ID. A swept handle's address may be reused
    by any later block, but its ID never is: [Heap.get_handle_checked] finds
    nothing for a handle that has been swept, where [Heap.get_handle] of a stale
    address may find another handle's payload. *)

type t

(** Made by [Heap.handle_ref]. *)
val create : addr:Memory.Addr.t -> id:int -> t

val addr : t -> Memory.Addr.t
val id : t -> int

(** Its ID: addresses are not deterministic. *)
val sexp_of_t : t -> Sexp.t
