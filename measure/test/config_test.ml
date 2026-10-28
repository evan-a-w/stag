open! Import

let%expect_test "validate" =
  let validate config =
    print_s [%sexp (Config.validate config : unit Or_error.t)]
  in
  validate Config.default;
  validate Config.quick;
  validate { Config.default with samples = 1; min_samples = 1 };
  validate
    { Config.default with
      warm_up = Time_ns.Span.zero
    ; measurement_time = Time_ns.Span.zero
    ; resamples = 0
    ; noise_threshold = 0.
    };
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Ok ())
    (Ok ())
    |}];
  validate { Config.default with samples = 0; min_samples = 0 };
  validate { Config.default with min_samples = 101 };
  validate
    { Config.default with
      warm_up = Time_ns.Span.of_int_sec (-1)
    ; measurement_time = Time_ns.Span.of_int_sec (-1)
    };
  validate { Config.default with resamples = -1 };
  validate { Config.default with confidence = 1. };
  validate { Config.default with noise_threshold = -0.01 };
  validate { Config.default with noise_threshold = Float.nan };
  [%expect {|
    (Error
     ("the number of samples must be at least 1"
      "the minimum number of samples must be from 1 to the number of samples"))
    (Error
     "the minimum number of samples must be from 1 to the number of samples")
    (Error
     ("the warm-up time must not be negative"
      "the measurement time must not be negative"))
    (Error "the number of resamples must not be negative")
    (Error "the confidence level must be between 0 and 1")
    (Error "the noise threshold must be a number, not negative")
    (Error "the noise threshold must be a number, not negative")
    |}]
;;
