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

let validate t =
  let check ok message = if ok then Ok () else Or_error.error_string message in
  let non_negative span = Time_ns.Span.( >= ) span Time_ns.Span.zero in
  Or_error.combine_errors_unit
    [ check (t.samples >= 1) "the number of samples must be at least 1"
    ; check
        (t.min_samples >= 1 && t.min_samples <= t.samples)
        "the minimum number of samples must be from 1 to the number of samples"
    ; check (non_negative t.warm_up) "the warm-up time must not be negative"
    ; check
        (non_negative t.measurement_time)
        "the measurement time must not be negative"
    ; check (t.resamples >= 0) "the number of resamples must not be negative"
    ; check
        (Float.( > ) t.confidence 0. && Float.( < ) t.confidence 1.)
        "the confidence level must be between 0 and 1"
    ; check
        (Float.is_finite t.noise_threshold && Float.( >= ) t.noise_threshold 0.)
        "the noise threshold must be a number, not negative"
    ]
;;
