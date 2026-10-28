open! Import

(** A routine measured, and its statistics. *)
type t =
  { name : string
  ; measurement : Measurement.t
  ; stats : Stats.t (** Of the per-iteration times of the samples, in ns. *)
  ; alloc : Alloc.t option (** Per iteration, over all samples, if measured. *)
  }
[@@deriving sexp_of]

val create : Config.t -> name:string -> Measurement.t -> t

(** Of each sample, in nanoseconds. *)
val per_iteration_ns : t -> float array
