open! Import

(** How to measure a routine, see [Measurement]. *)
type t =
  { warm_up : Time_ns.Span.t
  ; measurement_time : Time_ns.Span.t
    (** The time the samples should take, together, which decides how many
        iterations a sample runs. *)
  ; samples : int
  ; min_samples : int
    (** Fewer samples than [samples] are taken if a single iteration would
        overrun [measurement_time], but no fewer than this. *)
  ; resamples : int (** For bootstrap confidence intervals. *)
  ; confidence : float (** The level of confidence intervals, eg. [0.95]. *)
  ; noise_threshold : float
    (** A relative change smaller than this, eg. [0.02] for 2%, is noise. *)
  ; seed : int
    (** Of the bootstrap's random state, so reports are reproducible. *)
  }
[@@deriving sexp]

(** Warm-up 1s, measurement 3s, 100 samples (at least 10), 10_000 resamples,
    95% confidence, 2% noise, seed 0. *)
val default : t

(** As [default], with warm-up 100ms, measurement 500ms, 20 samples (at least
    5). *)
val quick : t
