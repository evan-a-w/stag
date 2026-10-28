open! Import

let%expect_test "time" =
  List.iter
    [ 0.
    ; 0.5
    ; 0.0123456
    ; 0.001
    ; 1.7e-17
    ; -1e-4
    ; 1.
    ; 12.
    ; 123.456
    ; 999.4
    ; 999.94
    ; 999.96
    ; 1000.
    ; 1234.5678
    ; 99_995.
    ; 999_999.9
    ; 1_234_567.
    ; 12_345_678.
    ; 999_950_000.
    ; 1e9
    ; 59.99e9
    ; 1.2345e13
    ; -1234.
    ; Float.nan
    ; Float.infinity
    ]
    ~f:(fun ns -> printf "%-15s %s\n" (Float.to_string ns) (Report.time ns));
  [%expect
    {|
    0.              0.000 ns
    0.5             0.5000 ns
    0.0123456       0.01235 ns
    0.001           0.001000 ns
    1.7e-17         0.000 ns
    -0.0001         0.000 ns
    1.              1.000 ns
    12.             12.00 ns
    123.456         123.5 ns
    999.4           999.4 ns
    999.94          999.9 ns
    999.96          1.000 us
    1000.           1.000 us
    1234.5678       1.235 us
    99995.          100.0 us
    999999.9        1.000 ms
    1234567.        1.235 ms
    12345678.       12.35 ms
    999950000.      1.000 s
    1000000000.     1.000 s
    59990000000.    59.99 s
    12345000000000. 12340 s
    -1234.          -1.234 us
    nan             nan ns
    inf             inf ns
    |}]
;;

let%expect_test "Format.to_string" =
  List.iter Report.Format.all ~f:(fun format ->
    print_endline (Report.Format.to_string format));
  [%expect
    {|
    human
    sexp
    json
    |}]
;;

let config = { Config.default with resamples = 1000 }

(* Samples of 12 iterations, of per-iteration times in us. *)
let measurement ?(alloc = false) per_iteration_us : Measurement.t =
  { warm_up_iterations = 1023
  ; warm_up_time = Time_ns.Span.of_int_ms 1001
  ; plan =
      { iterations = 12
      ; samples = List.length per_iteration_us
      ; expected_time = Time_ns.Span.zero
      }
  ; samples =
      List.map per_iteration_us ~f:(fun us ->
        { Sample.iterations = 12
        ; time =
            Time_ns.Span.of_int_ns (12 * Float.iround_nearest_exn (us *. 1000.))
        ; alloc =
            Option.some_if
              alloc
              { Alloc.minor_words = 12. *. 12_350.
              ; promoted_words = 12. *. 2.
              ; major_words = 0.
              ; minor_collections = 0.12
              ; major_collections = 0.
              }
        })
  }
;;

(* 20 samples around 1.24ms, with one high mild and one high severe outlier. *)
let fib_times =
  List.init 18 ~f:(fun i -> 1230. +. Float.of_int (i * 7 % 19))
  @ [ 1270.; 1310. ]
;;

let fib ?alloc () =
  Bench_result.create config ~name:"fib/20" (measurement ?alloc fib_times)
;;

let%expect_test "human" =
  let result = fib () in
  print_s [%sexp (Bench_result.per_iteration_ns result : float array)];
  print_s [%sexp (result.stats : Stats.t)];
  print_string (Report.human config { result; change = Not_compared });
  [%expect
    {|
    (1230000 1237000 1244000 1232000 1239000 1246000 1234000 1241000 1248000
     1236000 1243000 1231000 1238000 1245000 1233000 1240000 1247000 1235000
     1270000 1310000)
    ((count 20) (mean ((point 1243950) (lower 1237900) (upper 1252400)))
     (median 1239500) (min 1230000) (max 1310000) (stddev 17878.169575446504)
     (mad 8154.2999999999993) (p90 1250200) (p99 1302400)
     (outliers ((low_severe 0) (low_mild 0) (high_mild 1) (high_severe 1))))
    fib/20
      time:      [1.238 ms 1.244 ms 1.252 ms] (mean, 95% CI)
      median:    1.240 ms   stddev: 17.88 us   MADN: 8.154 us
      range:     [1.230 ms 1.310 ms]   p90: 1.250 ms   p99: 1.302 ms
      samples:   20 x 12 iterations (warm-up: 1023 iterations in 1.001 s)
      outliers:  2 of 20 (10.00%): 1 high mild, 1 high severe
    |}]
;;

let%expect_test "human, with allocation and change" =
  let result = fib ~alloc:true () in
  let comparison =
    Comparison.compare
      config
      ~baseline:
        (Array.map (Bench_result.per_iteration_ns result) ~f:(fun ns ->
           ns *. 0.95))
      ~current:(Bench_result.per_iteration_ns result)
  in
  print_string (Report.human config { result; change = Compared comparison });
  [%expect
    {|
    fib/20
      time:      [1.238 ms 1.244 ms 1.252 ms] (mean, 95% CI)
      median:    1.240 ms   stddev: 17.88 us   MADN: 8.154 us
      range:     [1.230 ms 1.310 ms]   p90: 1.250 ms   p99: 1.302 ms
      alloc:     12.35k minor words, 0 major words, 0.01000 minor GCs, 0 major GCs per iteration
      samples:   20 x 12 iterations (warm-up: 1023 iterations in 1.001 s)
      outliers:  2 of 20 (10.00%): 1 high mild, 1 high severe
      change:    [+4.365% +5.263% +6.177%] (95% CI) Performance has regressed.
    |}]
;;

let%expect_test "human, without outliers, with one iteration per sample" =
  let result =
    Bench_result.create
      config
      ~name:"slow"
      { warm_up_iterations = 1
      ; warm_up_time = Time_ns.Span.of_int_sec 2
      ; plan =
          { iterations = 1; samples = 3; expected_time = Time_ns.Span.zero }
      ; samples =
          List.map [ 2; 3; 4 ] ~f:(fun s ->
            { Sample.iterations = 1
            ; time = Time_ns.Span.of_int_sec s
            ; alloc = None
            })
      }
  in
  let comparison =
    Comparison.compare
      config
      ~baseline:(Bench_result.per_iteration_ns result)
      ~current:(Bench_result.per_iteration_ns result)
  in
  print_string (Report.human config { result; change = Compared comparison });
  [%expect
    {|
    slow
      time:      [2.000 s 3.000 s 4.000 s] (mean, 95% CI)
      median:    3.000 s   stddev: 1.000 s   MADN: 1.483 s
      range:     [2.000 s 4.000 s]   p90: 3.800 s   p99: 3.980 s
      samples:   3 x 1 iteration (warm-up: 1 iteration in 2.000 s)
      outliers:  none
      change:    [-33.333% +0.000% +57.143%] (95% CI) No change in performance detected.
    |}]
;;

let entries () : Report.Entry.t list =
  let tiny =
    Bench_result.create
      { config with resamples = 10 }
      ~name:"tiny \"quoted\"\n"
      { warm_up_iterations = 3
      ; warm_up_time = Time_ns.Span.of_int_ns 30
      ; plan =
          { iterations = 4; samples = 2; expected_time = Time_ns.Span.zero }
      ; samples =
          [ { iterations = 4
            ; time = Time_ns.Span.of_int_ns 10
            ; alloc = Some Alloc.zero
            }
          ; { iterations = 4
            ; time = Time_ns.Span.of_int_ns 14
            ; alloc = Some Alloc.zero
            }
          ]
      }
  in
  let one name =
    Bench_result.create
      { config with resamples = 10 }
      ~name
      (measurement [ 1. ])
  in
  [ { result = tiny
    ; change =
        Compared
          { change = { point = 0.5; lower = 0.25; upper = 0.75 }
          ; verdict = Regressed
          }
    }
  ; { result = one "one"; change = Not_compared }
  ; { result = one "new"; change = Not_in_baseline }
  ]
;;

let%expect_test "machine" =
  print_string (Report.machine Json (entries ()));
  [%expect
    {|
    [
      {
        "name": "tiny \"quoted\"\n",
        "samples": 2,
        "iterations": 4,
        "mean": {
          "point": 3,
          "lower": 2.5,
          "upper": 3.5
        },
        "median": 3,
        "min": 2.5,
        "max": 3.5,
        "stddev": 0.7071067811865476,
        "mad": 0.7413,
        "p90": 3.4,
        "p99": 3.49,
        "outliers": {
          "low_severe": 0,
          "low_mild": 0,
          "high_mild": 0,
          "high_severe": 0
        },
        "alloc": {
          "minor_words": 0,
          "promoted_words": 0,
          "major_words": 0,
          "minor_collections": 0,
          "major_collections": 0
        },
        "change": {
          "point": 0.5,
          "lower": 0.25,
          "upper": 0.75,
          "verdict": "regressed"
        }
      },
      {
        "name": "one",
        "samples": 1,
        "iterations": 12,
        "mean": {
          "point": 1000,
          "lower": 1000,
          "upper": 1000
        },
        "median": 1000,
        "min": 1000,
        "max": 1000,
        "stddev": 0,
        "mad": 0,
        "p90": 1000,
        "p99": 1000,
        "outliers": {
          "low_severe": 0,
          "low_mild": 0,
          "high_mild": 0,
          "high_severe": 0
        },
        "alloc": null,
        "change": null
      },
      {
        "name": "new",
        "samples": 1,
        "iterations": 12,
        "mean": {
          "point": 1000,
          "lower": 1000,
          "upper": 1000
        },
        "median": 1000,
        "min": 1000,
        "max": 1000,
        "stddev": 0,
        "mad": 0,
        "p90": 1000,
        "p99": 1000,
        "outliers": {
          "low_severe": 0,
          "low_mild": 0,
          "high_mild": 0,
          "high_severe": 0
        },
        "alloc": null,
        "change": {
          "verdict": "not_in_baseline"
        }
      }
    ]
    |}];
  print_string (Report.machine Json []);
  [%expect {| [] |}];
  print_string (Report.machine Sexp (entries ()));
  [%expect
    {|
    (((result
       ((name "tiny \"quoted\"\n")
        (measurement
         ((warm_up_iterations 3) (warm_up_time 30ns)
          (plan ((iterations 4) (samples 2) (expected_time 0s)))
          (samples
           (((iterations 4) (time 10ns)
             (alloc
              (((minor_words 0) (promoted_words 0) (major_words 0)
                (minor_collections 0) (major_collections 0)))))
            ((iterations 4) (time 14ns)
             (alloc
              (((minor_words 0) (promoted_words 0) (major_words 0)
                (minor_collections 0) (major_collections 0)))))))))
        (stats
         ((count 2) (mean ((point 3) (lower 2.5) (upper 3.5))) (median 3)
          (min 2.5) (max 3.5) (stddev 0.70710678118654757) (mad 0.7413) (p90 3.4)
          (p99 3.49)
          (outliers ((low_severe 0) (low_mild 0) (high_mild 0) (high_severe 0)))))
        (alloc
         (((minor_words 0) (promoted_words 0) (major_words 0)
           (minor_collections 0) (major_collections 0))))))
      (change
       (Compared
        ((change ((point 0.5) (lower 0.25) (upper 0.75))) (verdict Regressed)))))
     ((result
       ((name one)
        (measurement
         ((warm_up_iterations 1023) (warm_up_time 1.001s)
          (plan ((iterations 12) (samples 1) (expected_time 0s)))
          (samples (((iterations 12) (time 12us) (alloc ()))))))
        (stats
         ((count 1) (mean ((point 1000) (lower 1000) (upper 1000))) (median 1000)
          (min 1000) (max 1000) (stddev 0) (mad 0) (p90 1000) (p99 1000)
          (outliers ((low_severe 0) (low_mild 0) (high_mild 0) (high_severe 0)))))
        (alloc ())))
      (change Not_compared))
     ((result
       ((name new)
        (measurement
         ((warm_up_iterations 1023) (warm_up_time 1.001s)
          (plan ((iterations 12) (samples 1) (expected_time 0s)))
          (samples (((iterations 12) (time 12us) (alloc ()))))))
        (stats
         ((count 1) (mean ((point 1000) (lower 1000) (upper 1000))) (median 1000)
          (min 1000) (max 1000) (stddev 0) (mad 0) (p90 1000) (p99 1000)
          (outliers ((low_severe 0) (low_mild 0) (high_mild 0) (high_severe 0)))))
        (alloc ())))
      (change Not_in_baseline)))
    |}];
  print_s
    [%sexp
      (Or_error.try_with (fun () -> Report.machine Human [])
       : string Or_error.t)];
  [%expect {| (Error "Report.machine: human is not a machine format") |}]
;;
