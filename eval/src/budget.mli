open! Import

(** What is left of [Limits.t] in a phase of evaluation. *)
type t

(** The time limit counts from now. *)
val create : Limits.t -> t

(** Runs [f] as one step, one deeper than the steps it is nested in, or errors
    if that exceeds a limit, with a [Runtime_error] at [loc], the step's. *)
val step : t -> loc:Loc.t -> f:(unit -> 'a Or_error.t) -> 'a Or_error.t

(** A step in tail position of the step running: counted, but no deeper. *)
val tail_step : t -> loc:Loc.t -> unit Or_error.t
