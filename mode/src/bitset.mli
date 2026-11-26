open! Import

(** Sets of the integers below a size given at creation. *)
type t

val create : int -> t
val add : t -> int -> unit
val mem : t -> int -> bool

(** Adds those of the second to the first: whether that added any. *)
val union : t -> t -> bool

(** In increasing order. *)
val to_list : t -> int list
