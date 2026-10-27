open! Core

(** What is left of [Limits.t] in a phase of evaluation. *)
type t

(** The time limit counts from now. *)
val create : Limits.t -> t

(** Takes a step, one deeper than the steps it is nested in, or errors if that
    exceeds a limit. Each successful [enter] is followed by a [leave], once the
    step is done. *)
val enter : t -> unit Or_error.t

val leave : t -> unit
