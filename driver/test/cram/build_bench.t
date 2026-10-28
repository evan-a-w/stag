stag-build bench runs the let%bench benchmarks of every target, each in its
own program, named TARGET/NAME.

  $ touch stag-project
  $ mkdir lib main tests
  $ echo '(library (name lib))' > lib/stag
  $ cat > lib/sum.stag <<'EOF'
  > let sum n = match Int.equal n 0 with
  >   | true -> 0
  >   | false -> Int.add n (sum (Int.sub n 1))
  > let%bench "sum" n in [1; 10] = sum n
  > EOF
  $ echo '(executable (name main) (libraries lib))' > main/stag
  $ cat > main/main.stag <<'EOF'
  > let main = print_int (Lib.Sum.sum 3)
  > let%bench "main" = Lib.Sum.sum 2
  > EOF
  $ echo '(test (name tests) (libraries lib))' > tests/stag
  $ cat > tests/t.stag <<'EOF'
  > let%expect_test "sum" = print_int (Lib.Sum.sum 4); [%expect {| 10 |}]
  > let%bench "string" = Int.to_string (Lib.Sum.sum 3)
  > EOF
  $ stag-build build
  $ stag-build exec main
  6
  $ stag-build test

A fake clock (see stag bench) makes the output reproducible.

  $ alias bench='stag-build bench -warm-up 1us -time 1us -samples 3 -no-alloc -fake-clock 100ns'
  $ bench | grep -E '^[a-z]|time:'
  lib/sum/1
    time:      [0.1000 ns 0.1000 ns 0.1000 ns] (mean, 95% CI)
  lib/sum/10
    time:      [0.1000 ns 0.1000 ns 0.1000 ns] (mean, 95% CI)
  main/main
    time:      [0.1000 ns 0.1000 ns 0.1000 ns] (mean, 95% CI)
  tests/string
    time:      [0.1000 ns 0.1000 ns 0.1000 ns] (mean, 95% CI)

FILTERs select by substring.

  $ bench sum/1 string | grep -E '^[a-z]'
  lib/sum/1
  lib/sum/10
  tests/string
  $ bench nothing

Baselines are under _stag/bench of the project root.

  $ bench -save-baseline base > /dev/null
  $ cd lib && bench -baseline base main | grep change; cd ..
    change:    [+0.000% +0.000% +0.000%] (95% CI) No change in performance detected.
  $ ls _stag/bench
  base.sexp

Benchmarking the interpreter's phases on each target's program.

  $ bench -phases | grep -E '^[a-z]'
  lib/parse
  lib/typecheck
  lib/eval
  main/parse
  main/typecheck
  main/eval
  tests/parse
  tests/typecheck
  tests/eval
  $ bench -phases lib/ | grep -E '^[a-z]'
  lib/parse
  lib/typecheck
  lib/eval

A target with errors is reported, and the others still run.

  $ cat > tests/t.stag <<'EOF'
  > let%bench "bad" = Int.add 1 "a"
  > EOF
  $ bench | grep -E '^[a-z]|Error'
  File "tests/t.stag", line 1, characters 28-31:
  1 | let%bench "bad" = Int.add 1 "a"
                                  ^^^
  Error: This expression has type string but an expression was expected of type int
  lib/sum/1
  lib/sum/10
  main/main
  $ bench -phases | grep -E '^[a-z]|Error'
  File "tests/t.stag", line 1, characters 28-31:
  1 | let%bench "bad" = Int.add 1 "a"
                                  ^^^
  Error: This expression has type string but an expression was expected of type int
  lib/parse
  lib/typecheck
  lib/eval
  main/parse
  main/typecheck
  main/eval

The interpreter benchmarks of bench/ build, and their expect tests pass.

  $ unset STAG_BENCH_FAKE_CLOCK
  $ cp -RL ../../../bench interpreter && chmod -R u+w interpreter && cd interpreter
  $ stag-build build
  $ stag-build test
