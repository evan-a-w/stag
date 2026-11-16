open! Import

(** Measuring a routine, as divan does: samples of a fixed number of iterations
    each, calibrated during warm-up.

    Warm-up runs batches of 1, 2, 4, ... iterations (as criterion does) until
    their timed part has taken [Config.warm_up], or with the untimed [setup] of
    [Batched] routines 10 times that, or 2^24 iterations would be exceeded, or
    2^16 iterations have not moved the clock. The last batch estimates the time
    of an iteration and of its setup, and errs if the clock did not move during
    it. Then [Plan.create] decides the iterations per sample and the number of
    samples, which are each timed with one read of the clock before and after
    (and of the GC counters, if [alloc] is given), or for [Batched] routines,
    around each chunk of at most 64 iterations, whose inputs are made just
    before it. *)

(** The calibration. *)
module Plan : sig
  type t =
    { iterations : int (** Per sample. *)
    ; samples : int
    ; expected_time : Time_ns.Span.t (** Of the samples, with setup. *)
    }
  [@@deriving sexp]

  (** Each sample should take [measurement_time / samples], including the
      untimed [setup] of each iteration (zero by default), and its timed part at
      least 1000 [resolution]s, so that the clock's resolution is under 0.1% of
      it: that many iterations, rounded up, at least 1. If even one iteration
      (and its setup) per sample would overrun [measurement_time], there are
      fewer samples, as many as fit, but at least [min_samples]. A
      [per_iteration] under 1ns, eg. 0 from a fake clock, is taken to be 1ns. *)
  val create
    :  ?setup:Time_ns.Span.t
    -> Config.t
    -> resolution:Time_ns.Span.t
    -> per_iteration:Time_ns.Span.t
    -> t

  (** Criterion's warning, when single iterations are too slow for
      [Config.samples] of them to fit in [Config.measurement_time]. Samples
      lengthened to last 1000 clock resolutions do not warn. *)
  val warning : t -> Config.t -> string option
end

type t =
  { warm_up_iterations : int
  ; warm_up_time : Time_ns.Span.t
  ; plan : Plan.t
  ; samples : Sample.t list
  }
[@@deriving sexp]

(** Stops at the routine's first error. Errs if [Config.validate] does. *)
val run
  :  ?alloc:(unit -> Alloc.t)
  -> Config.t
  -> Clock.t
  -> Routine.t
  -> t Or_error.t
