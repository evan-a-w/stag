open! Import

(** A root source: slots elsewhere (eg. mutable globals in static data), each
    holding a pointer or null, never into the data stack. *)

type t

val create : unit -> t

(** Raises if already added. *)
val add : t -> Memory.Addr.t -> unit

(** Raises if not added. *)
val remove : t -> Memory.Addr.t -> unit

val mem : t -> Memory.Addr.t -> bool
val count : t -> int

(** Every slot, in no particular order. *)
val root_source : t -> Root_source.t
