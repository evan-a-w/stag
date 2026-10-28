open! Import

(** A batch of iterations, timed together. *)
type t =
  { iterations : int
  ; time : Time_ns.Span.t
  ; alloc : Alloc.t option (** Allocated by the batch, if measured. *)
  }
[@@deriving sexp]

(** [time / iterations], in nanoseconds. *)
val per_iteration_ns : t -> float
