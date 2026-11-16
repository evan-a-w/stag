open! Import

(** The options and running of [stag bench] and [stag-build bench]. *)

module Options : sig
  type t =
    { config : Measure.Config.t
    ; format : Measure.Report.Format.t
    ; alloc : bool (** Measure the GC's counters. *)
    ; baseline : string option (** To compare with. *)
    ; save_baseline : string option
    ; clock : Measure.Clock.t
    }

  (** [-quick], [-samples N], [-warm-up SPAN], [-time SPAN],
      [-noise-threshold PERCENT], [-no-alloc], [-format FORMAT] (or [-json],
      [-sexp]), [-baseline NAME] and [-save-baseline NAME]. With [fake_clock], a
      fake clock moving by it at each read. *)
  val param : fake_clock:Time_ns.Span.t option -> t Command.Param.t
end

(** Takes the hidden [-fake-clock SPAN] out of a command line, for tests: Core
    commands cannot hide flags from [-help]. Exits on a malformed [SPAN]. *)
val take_fake_clock : string list -> string list * Time_ns.Span.t option

(** Measures the [benches] matching [filters] (see [Suite.selected]), printing
    the report to stdout: each bench as it is measured, or all at the end in a
    machine format. Baselines are files of [baseline_dir], saved with the
    results of this run merged into those already saved under the name (or
    replacing them with a warning, if they are malformed). A missing baseline to
    compare with is warned of. [report] shows errors, eg. of a routine, which do
    not stop the other benches, or of the options, which do. False if any
    failed. *)
val run
  :  ?filters:string list
  -> Options.t
  -> baseline_dir:string
  -> report:(Diagnostic.t -> unit)
  -> Measure.Suite.Bench.t list
  -> bool
