open! Import

(** Measuring a routine, as divan does: samples of a fixed number of
    iterations each, calibrated during warm-up.

    Warm-up runs batches of 1, 2, 4, ... iterations (as criterion does) until
    [Config.warm_up] has elapsed, including the [setup] of [Batched]
    routines, at least one batch, which estimates the time of an iteration
    and of its setup. Iterations count as at least 1ns each, as in [Plan], so
    that the warm-up ends even if the clock does not move (eg. a fake one). Then [Plan.create] decides the iterations per sample and
    the number of samples, which are each timed with one read of the clock
    before and after (and of the GC counters, if [alloc] is given). *)

(** The calibration. *)
module Plan : sig
  type t =
    { iterations : int (** Per sample. *)
    ; samples : int
    }
  [@@deriving sexp]

  (** Each sample should take [measurement_time / samples], including the
      untimed [setup] of each iteration (zero by default), and its timed part
      at least 1000 [resolution]s, so that the clock's resolution is under
      0.1% of it: that many iterations, rounded up, at least 1. If even one
      iteration (and its setup) per sample would overrun [measurement_time],
      there are fewer samples, as many as fit, but at least [min_samples]. A [per_iteration]
      under 1ns, eg. 0 from a fake clock, is taken to be 1ns. *)
  val create
    :  ?setup:Time_ns.Span.t
    -> Config.t
    -> resolution:Time_ns.Span.t
    -> per_iteration:Time_ns.Span.t
    -> t
end

type t =
  { warm_up_iterations : int
  ; warm_up_time : Time_ns.Span.t
  ; plan : Plan.t
  ; samples : Sample.t list
  }
[@@deriving sexp]

(** Stops at the routine's first error. *)
val run
  :  ?alloc:(unit -> Alloc.t)
  -> Config.t
  -> Clock.t
  -> Routine.t
  -> t Or_error.t
