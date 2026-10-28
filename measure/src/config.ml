open! Import

type t =
  { warm_up : Time_ns.Span.t
  ; measurement_time : Time_ns.Span.t
  ; samples : int
  ; min_samples : int
  ; resamples : int
  ; confidence : float
  ; noise_threshold : float
  ; seed : int
  }
[@@deriving sexp]

let default =
  { warm_up = Time_ns.Span.second
  ; measurement_time = Time_ns.Span.of_int_sec 3
  ; samples = 100
  ; min_samples = 10
  ; resamples = 10_000
  ; confidence = 0.95
  ; noise_threshold = 0.02
  ; seed = 0
  }
;;

let quick =
  { default with
    warm_up = Time_ns.Span.of_int_ms 100
  ; measurement_time = Time_ns.Span.of_int_ms 500
  ; samples = 20
  ; min_samples = 5
  }
;;
