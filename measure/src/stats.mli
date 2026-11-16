open! Import

(** Statistics of a sample of measurements, eg. the per-iteration times of a
    routine's samples. Functions of a [float array] do not modify it, and raise
    if it is empty. *)

module Estimate : sig
  (** A statistic and its confidence interval. *)
  type t =
    { point : float
    ; lower : float
    ; upper : float
    }
  [@@deriving sexp]
end

(** Tukey's fences: values beyond [1.5] interquartile ranges from the quartiles
    are mild outliers, beyond [3] severe ones, as criterion classifies them. *)
module Outliers : sig
  type t =
    { low_severe : int
    ; low_mild : int
    ; high_mild : int
    ; high_severe : int
    }
  [@@deriving sexp]

  val classify : float array -> t
  val total : t -> int
end

type t =
  { count : int
  ; mean : Estimate.t (** With its bootstrap confidence interval. *)
  ; median : float
  ; min : float
  ; max : float
  ; stddev : float
  ; mad : float
  ; p90 : float
  ; p99 : float
  ; outliers : Outliers.t
  }
[@@deriving sexp]

val mean : float array -> float

(** The sample standard deviation (dividing by [n - 1]), 0 for one value. *)
val stddev : float array -> float

(** [percentile data p] for [p] in [0, 1], interpolating linearly between the
    closest ranks: [(n - 1) * p] in the sorted data (numpy's default). *)
val percentile : float array -> float -> float

val median : float array -> float

(** The median absolute deviation from the median, scaled by 1.4826 to estimate
    the standard deviation of normal data, as criterion does. *)
val mad : float array -> float

(** [f data], with the [confidence] interval of the percentile bootstrap: [f] of
    [resamples] resamples of [data] with replacement, drawn from [random]. *)
val bootstrap
  :  random:Random.State.t
  -> resamples:int
  -> confidence:float
  -> float array
  -> f:(float array -> float)
  -> Estimate.t

(** As [bootstrap], of [f a b], resampling [a] and [b] independently. [f] is
    given resamples of the same lengths as [a] and [b]. *)
val bootstrap2
  :  random:Random.State.t
  -> resamples:int
  -> confidence:float
  -> float array
  -> float array
  -> f:(float array -> float array -> float)
  -> Estimate.t

(** Bootstrapping with [Config.resamples], [confidence] and [seed]. *)
val summarize : Config.t -> float array -> t
