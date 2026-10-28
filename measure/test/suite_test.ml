open! Import

let%expect_test "selected" =
  let names = [ "fib"; "fib/10"; "sort/100"; "Sort" ] in
  List.iter
    [ []; [ "fib" ]; [ "/1" ]; [ "sort"; "fib/" ]; [ "nothing" ]; [ "" ] ]
    ~f:(fun filters ->
      print_s
        [%message
          (filters : string list)
            ~selected:
              (List.filter names ~f:(Suite.selected ~filters) : string list)]);
  [%expect
    {|
    ((filters ()) (selected (fib fib/10 sort/100 Sort)))
    ((filters (fib)) (selected (fib fib/10)))
    ((filters (/1)) (selected (fib/10 sort/100)))
    ((filters (sort fib/)) (selected (fib/10 sort/100)))
    ((filters (nothing)) (selected ()))
    ((filters ("")) (selected (fib fib/10 sort/100 Sort)))
    |}]
;;

let%expect_test
    "run: in order, filtered, errors do not stop the others, and comparison \
     with the baseline"
  =
  let fake = Clock.Fake.create () in
  let config =
    { Config.quick with
      samples = 3
    ; min_samples = 3
    ; warm_up = Time_ns.Span.of_int_us 1
    ; measurement_time = Time_ns.Span.of_int_us 3
    }
  in
  let bench ?elements name ns : Suite.Bench.t =
    { name
    ; elements
    ; routine =
        Plain
          (fun () ->
            Clock.Fake.advance fake (Time_ns.Span.of_int_ns ns);
            Ok ())
    }
  in
  let failing : Suite.Bench.t =
    { name = "fails"
    ; elements = None
    ; routine = Plain (fun () -> Or_error.error_string "boom")
    }
  in
  let run ?baseline benches =
    Suite.run
      ?baseline
      ~filters:[ "a"; "fails" ]
      ~on_outcome:(fun outcome -> printf "measured %s\n" outcome.name)
      config
      (Clock.Fake.clock fake)
      benches
  in
  let first =
    run [ bench "a" 10; failing; bench "b" 10; bench ~elements:4 "ab" 20 ]
  in
  let baseline =
    Baseline.update
      Baseline.empty
      (List.filter_map first ~f:(fun outcome ->
         Result.ok outcome.entry
         |> Option.map ~f:(fun entry -> entry.Report.Entry.result)))
  in
  let second = run ~baseline [ bench "a" 20; bench "ab" 20; bench "new a" 1 ] in
  List.iter (first @ second) ~f:(fun { name; entry } ->
    match entry with
    | Error error -> print_s [%message name (error : Error.t)]
    | Ok entry -> print_string (Report.human config entry));
  [%expect
    {|
    measured a
    measured fails
    measured ab
    measured a
    measured ab
    measured new a
    a
      time:      [10.00 ns 10.00 ns 10.00 ns] (mean, 95% CI)
      median:    10.00 ns   stddev: 0.000 ns   MAD: 0.000 ns
      range:     [10.00 ns 10.00 ns]   p50: 10.00 ns   p90: 10.00 ns   p99: 10.00 ns
      samples:   3 x 100 iterations (warm-up: 127 iterations in 1.270 us)
      outliers:  none
    (fails (error boom))
    ab
      time:      [20.00 ns 20.00 ns 20.00 ns] (mean, 95% CI)
      median:    20.00 ns   stddev: 0.000 ns   MAD: 0.000 ns
      range:     [20.00 ns 20.00 ns]   p50: 20.00 ns   p90: 20.00 ns   p99: 20.00 ns
      thrpt:     [200.0 Melem/s 200.0 Melem/s 200.0 Melem/s]
      samples:   3 x 50 iterations (warm-up: 63 iterations in 1.260 us)
      outliers:  none
    a
      time:      [20.00 ns 20.00 ns 20.00 ns] (mean, 95% CI)
      median:    20.00 ns   stddev: 0.000 ns   MAD: 0.000 ns
      range:     [20.00 ns 20.00 ns]   p50: 20.00 ns   p90: 20.00 ns   p99: 20.00 ns
      samples:   3 x 50 iterations (warm-up: 63 iterations in 1.260 us)
      outliers:  none
      change:    [+100.000% +100.000% +100.000%] (95% CI) Performance has regressed.
    ab
      time:      [20.00 ns 20.00 ns 20.00 ns] (mean, 95% CI)
      median:    20.00 ns   stddev: 0.000 ns   MAD: 0.000 ns
      range:     [20.00 ns 20.00 ns]   p50: 20.00 ns   p90: 20.00 ns   p99: 20.00 ns
      samples:   3 x 50 iterations (warm-up: 63 iterations in 1.260 us)
      outliers:  none
      change:    [+0.000% +0.000% +0.000%] (95% CI) No change in performance detected.
    new a
      time:      [1.000 ns 1.000 ns 1.000 ns] (mean, 95% CI)
      median:    1.000 ns   stddev: 0.000 ns   MAD: 0.000 ns
      range:     [1.000 ns 1.000 ns]   p50: 1.000 ns   p90: 1.000 ns   p99: 1.000 ns
      samples:   3 x 1000 iterations (warm-up: 1023 iterations in 1.023 us)
      outliers:  none
    |}]
;;
