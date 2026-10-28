open! Import

(** The options and running of [stag bench] and [stag-build bench]. *)

module Options : sig
  type t =
    { config : Measure.Config.t
    ; format : Measure.Report.Format.t
    ; alloc : bool (** Measure the GC's counters. *)
    ; baseline : string option (** To compare with. *)
    ; save_baseline : string option
    }

  (** [-quick], [-samples N], [-warm-up SPAN], [-time SPAN],
      [-noise-threshold PERCENT], [-no-alloc], [-format FORMAT] (or [-json],
      [-sexp]), [-baseline NAME] and [-save-baseline NAME]. *)
  val param : t Command.Param.t
end

(** [Clock.monotonic], or if [STAG_BENCH_FAKE_CLOCK] is a span, a fake clock
    moving by it at each read, for reproducible tests. *)
val clock : unit -> Measure.Clock.t

(** Measures the [benches] matching [filters] (see [Suite.selected]), printing
    the report to stdout: each bench as it is measured, or all at the end in a
    machine format. Baselines are files of [baseline_dir], saved with the
    results of this run merged into those already saved under the name.
    [report] shows errors, eg. of a routine, which do not stop the other
    benches. False if any failed. *)
val run
  :  ?filters:string list
  -> Options.t
  -> Measure.Clock.t
  -> baseline_dir:string
  -> report:(Diagnostic.t -> unit)
  -> Measure.Suite.Bench.t list
  -> bool
