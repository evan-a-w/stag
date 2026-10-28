open! Import

(** Running a set of named routines, as [stag bench] and [stag-build bench]
    do. *)

module Bench : sig
  type t =
    { name : string
    ; elements : int option (** Per iteration, for throughput. *)
    ; routine : Routine.t
    }
end

module Outcome : sig
  type t =
    { name : string
    ; entry : Report.Entry.t Or_error.t
      (** An error of the routine, which stops its measurement. *)
    }
  [@@deriving sexp_of]
end

(** Whether [name] is selected by [filters]: all are if there are none, else
    those containing any of them. *)
val selected : filters:string list -> string -> bool

(** Measures the benches [selected] in order, calling [on_outcome] after
    each, and compares them with their times in [baseline], if any. *)
val run
  :  ?alloc:(unit -> Alloc.t)
  -> ?baseline:Baseline.t
  -> ?on_outcome:(Outcome.t -> unit)
  -> ?filters:string list
  -> Config.t
  -> Clock.t
  -> Bench.t list
  -> Outcome.t list
