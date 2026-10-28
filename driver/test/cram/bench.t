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

A malformed baseline is an error when comparing, and replaced with a warning
when saving.

  $ echo '(' > _stag/bench/broken.sexp
  $ bench fib.stag -samples 3 -no-alloc -filter fib/1 -baseline broken
  Error: (("delete the baseline or save it again" _stag/bench/broken.sexp)
   ("malformed baseline" _stag/bench/broken.sexp
    (exn
     (Failure
      "Sexplib.Sexp.gen_load_sexp: EOF in _stag/bench/broken.sexp while in state Parsing_list"))))
  [1]
  $ bench fib.stag -samples 3 -no-alloc -filter fib/1 -save-baseline broken > /dev/null
  Warning: overwriting a malformed baseline: (("delete the baseline or save it again" _stag/bench/broken.sexp)
   ("malformed baseline" _stag/bench/broken.sexp
    (exn
     (Failure
      "Sexplib.Sexp.gen_load_sexp: EOF in _stag/bench/broken.sexp while in state Parsing_list"))))
  $ bench fib.stag -samples 3 -no-alloc -filter fib/1 -baseline broken | grep change
    change:    [+0.000% +0.000% +0.000%] (95% CI) No change in performance detected.

Benchmarks not in the baseline are noted.

  $ bench fib.stag -samples 3 -no-alloc -filter fib/3 -baseline broken
  fib/3
    time:      [0.1000 ns 0.1000 ns 0.1000 ns] (mean, 95% CI)
    median:    0.1000 ns   stddev: 0.000 ns   MADN: 0.000 ns
    range:     [0.1000 ns 0.1000 ns]   p90: 0.1000 ns   p99: 0.1000 ns
    samples:   3 x 1000 iterations (warm-up: 1023 iterations in 1.000 us)
    outliers:  none
    change:    not in the baseline

Invalid options are errors.

  $ bench fib.stag -samples 0
  Error: ("the number of samples must be at least 1"
   "the minimum number of samples must be from 1 to the number of samples")
  [1]
  $ stag bench fib.stag -warm-up -1s
  Error: "the warm-up time must not be negative"
  [1]
  $ stag bench fib.stag -time -1s
  Error: "the measurement time must not be negative"
  [1]
  $ bench fib.stag -noise-threshold -1
  Error: "the noise threshold must be a number, not negative"
  [1]
  $ bench fib.stag -noise-threshold nan
  Error: "the noise threshold must be a number, not negative"
  [1]
  $ stag bench fib.stag -fake-clock
  Error: -fake-clock needs a SPAN
  [2]
  $ stag bench fib.stag -fake-clock soon
  Error: -fake-clock: invalid span "soon"
  [2]

The fake clock is hidden from -help.

  $ stag bench -help | grep -c fake
  0
  [1]

A clock that does not move ends the warm-up after a bounded number of
iterations, and fails the benchmark.

  $ stag bench -warm-up 1s -time 1s -fake-clock 0ns fib.stag -samples 3 -no-alloc -filter fib/1
  Error: Benchmark fib/1 failed: ("the clock did not move while timing a batch of iterations, so it cannot time them"
   (iterations 65536))
  [1]

Benchmarks are not limited by -fuel, which applies to evaluating the program.

  $ cat > long.stag <<'EOF'
  > let count n = match Int.equal n 0 with true -> 0 | false -> count (Int.sub n 1)
  > let%bench "count" = count 1000
  > EOF
  $ bench long.stag -samples 1 -no-alloc -fuel 100 | head -1
  count
  $ cat > long_main.stag <<'EOF'
  > let count n = match Int.equal n 0 with true -> 0 | false -> count (Int.sub n 1)
  > let main = count 1000
  > EOF
  $ bench long_main.stag -fuel 100
  File "long_main.stag", line 1, characters 60-79:
  1 | let count n = match Int.equal n 0 with true -> 0 | false -> count (Int.sub n 1)
                                                                  ^^^^^^^^^^^^^^^^^^^
  Error: Out of fuel: evaluation took more than 100 steps
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

A failing argument fails only its benchmark; -filter selects benchmarks
before their arguments are evaluated, so the arguments of the others are not.

  $ cat > args.stag <<'EOF'
  > let%bench "div" n in [Int.div 1 0; 2] = n
  > let%bench "same" n in [1; Int.sub 2 1] = n
  > let%bench "long" s in ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"] = s
  > let%bench "ok" = ()
  > EOF
  $ bench args.stag -samples 1 -no-alloc | grep -E '^[a-zF]|Error'
  File "args.stag", line 1, characters 22-33:
  1 | let%bench "div" n in [Int.div 1 0; 2] = n
                            ^^^^^^^^^^^
  Error: Benchmark div failed: Division by zero
  File "args.stag", line 2, characters 10-16:
  2 | let%bench "same" n in [1; Int.sub 2 1] = n
                ^^^^^^
  Error: Benchmark same failed: Two benchmarks are named "same/1", the other at args.stag:2:10-16
  div/2
  same/1
  long/"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa...
  ok
  $ bench args.stag -samples 1 -no-alloc -filter ok
  ok
    time:      [0.1000 ns 0.1000 ns 0.1000 ns] (mean, 95% CI)
    median:    0.1000 ns   stddev: 0.000 ns   MADN: 0.000 ns
    range:     [0.1000 ns 0.1000 ns]   p90: 0.1000 ns   p99: 0.1000 ns
    samples:   1 x 1000 iterations (warm-up: 1023 iterations in 1.000 us)
    outliers:  none

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
