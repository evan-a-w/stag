open! Import

(** Pluggable [Root_source.t] ([~holds_locals:false]) for exported rooted
    [Value.t]s. Deferred unregistrations can be enqueued safely from OCaml GC
    finalisers without touching [Stag_runtime]. *)

module Id : sig
  type t [@@deriving equal, sexp_of]
end

type t

val create : unit -> t

(** Frees off-heap root storage. Using [t] afterwards raises. *)
val destroy : t -> unit

val register : t -> Memory.Addr.t array -> Id.t

(** Idempotent: unregistering an already-unregistered id is a no-op. *)
val unregister : t -> Id.t -> unit

(** Safe to call from an OCaml [Gc.finalise] callback without touching
    [Stag_runtime]. Applied on the next [drain_deferred]. *)
val defer_unregister : t -> Id.t -> unit

val drain_deferred : t -> unit
val root_source : t -> Root_source.t
val live_count : t -> int
