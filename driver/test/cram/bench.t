Benchmarks are toplevel let%bench declarations, ignored by check and run.

  $ cat > fib.stag <<'EOF'
  > let fib n = match Int.lt n 2 with
  >   | true -> n
  >   | false -> Int.add (fib (Int.sub n 1)) (fib (Int.sub n 2))
  > let%bench "fib" = fib 5
  > let%bench "fib" n in [1; 3] = fib n
  > let main = print_endline "main"
  > EOF
  $ stag check fib.stag
  module Fib : sig
    val fib : int -> int
    val main : unit
  end
  $ stag run fib.stag
  main

A fake clock, moving 100ns at each read, makes the report reproducible (and
the iterations too fast to see); a short warm-up and measurement time keep it
fast, as the fake time passes much slower than real time. The program's output
is discarded.

  $ alias bench='stag bench -warm-up 1us -time 1us -fake-clock 100ns'
  $ bench fib.stag -samples 5 -no-alloc -filter fib/
  fib/1
    time:      [0.1000 ns 0.1000 ns 0.1000 ns] (mean, 95% CI)
    median:    0.1000 ns   stddev: 0.000 ns   MADN: 0.000 ns
    range:     [0.1000 ns 0.1000 ns]   p90: 0.1000 ns   p99: 0.1000 ns
    samples:   5 x 1000 iterations (warm-up: 1023 iterations in 1.000 us)
    outliers:  none
  fib/3
    time:      [0.1000 ns 0.1000 ns 0.1000 ns] (mean, 95% CI)
    median:    0.1000 ns   stddev: 0.000 ns   MADN: 0.000 ns
    range:     [0.1000 ns 0.1000 ns]   p90: 0.1000 ns   p99: 0.1000 ns
    samples:   5 x 1000 iterations (warm-up: 1023 iterations in 1.000 us)
    outliers:  none

Allocation is measured with the GC's counters (numbers left out here).

  $ bench fib.stag -samples 5 -filter fib/1 | grep alloc | sed -E 's/[0-9][0-9.]*k?/N/g'
    alloc:     N minor words, N major words, N minor GCs, N major GCs per iteration

Machine readable formats.

  $ bench fib.stag -samples 3 -no-alloc -filter fib/1 -sexp
  (((result
     ((name fib/1)
      (measurement
       ((warm_up_iterations 1023) (warm_up_time 1us)
        (plan ((iterations 1000) (samples 3) (expected_time 3us)))
        (samples
         (((iterations 1000) (time 100ns) (alloc ()))
          ((iterations 1000) (time 100ns) (alloc ()))
          ((iterations 1000) (time 100ns) (alloc ()))))))
      (stats
       ((count 3)
        (mean
         ((point 0.10000000000000002) (lower 0.10000000000000002)
          (upper 0.10000000000000002)))
        (median 0.1) (min 0.1) (max 0.1) (stddev 1.6996749443881478E-17)
        (mad 0) (p90 0.1) (p99 0.1)
        (outliers ((low_severe 0) (low_mild 0) (high_mild 0) (high_severe 0)))))
      (alloc ())))
    (change Not_compared)))
  $ bench fib.stag -samples 3 -no-alloc -filter fib/1 -json
  [
    {
      "name": "fib/1",
      "samples": 3,
      "iterations": 1000,
      "mean": {
        "point": 0.10000000000000002,
        "lower": 0.10000000000000002,
        "upper": 0.10000000000000002
      },
      "median": 0.1,
      "min": 0.1,
      "max": 0.1,
      "stddev": 1.6996749443881478e-17,
      "mad": 0,
      "p90": 0.1,
      "p99": 0.1,
      "outliers": {
        "low_severe": 0,
        "low_mild": 0,
        "high_mild": 0,
        "high_severe": 0
      },
      "alloc": null,
      "change": null
    }
  ]

Baselines are saved in _stag/bench, and compared with.

  $ bench fib.stag -samples 5 -no-alloc -save-baseline before > /dev/null
  $ ls _stag/bench
  before.sexp
  $ stag bench -warm-up 1us -time 1us -fake-clock 200ns fib.stag -samples 5 -no-alloc -baseline before -filter fib/3
  fib/3
    time:      [2.597 ns 2.597 ns 2.597 ns] (mean, 95% CI)
    median:    2.597 ns   stddev: 0.000 ns   MADN: 0.000 ns
    range:     [2.597 ns 2.597 ns]   p90: 2.597 ns   p99: 2.597 ns
    samples:   5 x 77 iterations (warm-up: 31 iterations in 1.000 us)
    outliers:  none
    change:    [+2497.403% +2497.403% +2497.403%] (95% CI) Performance has regressed.
  $ bench fib.stag -samples 5 -no-alloc -baseline before -filter fib/3
  fib/3
    time:      [0.1000 ns 0.1000 ns 0.1000 ns] (mean, 95% CI)
    median:    0.1000 ns   stddev: 0.000 ns   MADN: 0.000 ns
    range:     [0.1000 ns 0.1000 ns]   p90: 0.1000 ns   p99: 0.1000 ns
    samples:   5 x 1000 iterations (warm-up: 1023 iterations in 1.000 us)
    outliers:  none
    change:    [+0.000% +0.000% +0.000%] (95% CI) No change in performance detected.
  $ bench fib.stag -samples 3 -no-alloc -filter fib/1 -baseline missing
  Warning: no baseline missing to compare with
  fib/1
    time:      [0.1000 ns 0.1000 ns 0.1000 ns] (mean, 95% CI)
    median:    0.1000 ns   stddev: 0.000 ns   MADN: 0.000 ns
    range:     [0.1000 ns 0.1000 ns]   p90: 0.1000 ns   p99: 0.1000 ns
    samples:   3 x 1000 iterations (warm-up: 1023 iterations in 1.000 us)
    outliers:  none
  $ bench fib.stag -save-baseline ../x
  Error parsing command line:
  
    failed to parse -save-baseline value "../x".
    ("baseline name starts with '.'" ../x)
  
  For usage information, run
  
    stag bench -help
  
  [1]

A failing benchmark is reported, and the others still run.

  $ cat > fail.stag <<'EOF'
  > let%bench "div" = Int.div 1 0
  > let%bench "ok" = ()
  > EOF
  $ bench fail.stag -samples 5 -no-alloc
  File "fail.stag", line 1, characters 18-29:
  1 | let%bench "div" = Int.div 1 0
                        ^^^^^^^^^^^
  Error: Benchmark div failed: Division by zero
  ok
    time:      [0.1000 ns 0.1000 ns 0.1000 ns] (mean, 95% CI)
    median:    0.1000 ns   stddev: 0.000 ns   MADN: 0.000 ns
    range:     [0.1000 ns 0.1000 ns]   p90: 0.1000 ns   p99: 0.1000 ns
    samples:   5 x 1000 iterations (warm-up: 1023 iterations in 1.000 us)
    outliers:  none
  [1]

Errors in benchmarks are type errors of the program.

  $ cat > bad.stag <<'EOF'
  > let%bench "bad" n in [1; "a"] = n
  > EOF
  $ bench bad.stag
  File "bad.stag", line 1, characters 25-28:
  1 | let%bench "bad" n in [1; "a"] = n
                               ^^^
  Error: This expression has type string but an expression was expected of type int
  [1]
  $ stag check bad.stag
  File "bad.stag", line 1, characters 25-28:
  1 | let%bench "bad" n in [1; "a"] = n
                               ^^^
  Error: This expression has type string but an expression was expected of type int
  [1]

Benchmarking the interpreter itself: parsing, typechecking and evaluating the
program.

  $ bench fib.stag -samples 3 -no-alloc -phases
  parse
    time:      [0.1000 ns 0.1000 ns 0.1000 ns] (mean, 95% CI)
    median:    0.1000 ns   stddev: 0.000 ns   MADN: 0.000 ns
    range:     [0.1000 ns 0.1000 ns]   p90: 0.1000 ns   p99: 0.1000 ns
    samples:   3 x 1000 iterations (warm-up: 1023 iterations in 1.000 us)
    outliers:  none
  typecheck
    time:      [0.1000 ns 0.1000 ns 0.1000 ns] (mean, 95% CI)
    median:    0.1000 ns   stddev: 0.000 ns   MADN: 0.000 ns
    range:     [0.1000 ns 0.1000 ns]   p90: 0.1000 ns   p99: 0.1000 ns
    samples:   3 x 1000 iterations (warm-up: 1023 iterations in 1.000 us)
    outliers:  none
  eval
    time:      [1.600 ns 1.600 ns 1.600 ns] (mean, 95% CI)
    median:    1.600 ns   stddev: 0.000 ns   MADN: 0.000 ns
    range:     [1.600 ns 1.600 ns]   p90: 1.600 ns   p99: 1.600 ns
    samples:   3 x 500 iterations (warm-up: 511 iterations in 1.300 us)
    outliers:  none
