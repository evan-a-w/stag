stag-build limits each phase as stag does: compile time, evaluation and each
test.

  $ touch stag-project
  $ mkdir lib app
  $ echo '(library (name lib))' > lib/stag
  $ cat > lib/a.stag <<'X'
  > let loop n = loop (Int.add n 1)
  > 
  > let%expect_test "loops" =
  >   loop 0;
  >   [%expect {| |}]
  > X
  $ echo '(executable (name app) (libraries lib))' > app/stag
  $ echo 'let x = Lib.A.loop 0' > app/main.stag
  $ cat > app/splice.stag <<'X'
  > let y = .~(Lib.A.loop 0)
  > X
  $ stag-build build -fuel 1000
  File "app/splice.stag", line 1, characters 8-24:
  1 | let y = .~(Lib.A.loop 0)
              ^^^^^^^^^^^^^^^^
  Error: This toplevel splice failed at compile time: Out of fuel: evaluation took more than 1000 steps
  File "lib/a.stag", line 1, characters 19-28:
  1 | let loop n = loop (Int.add n 1)
                         ^^^^^^^^^
  Note: The error was raised here
  [1]
  $ rm app/splice.stag
  $ stag-build exec app -fuel 1000
  File "lib/a.stag", line 1, characters 19-28:
  1 | let loop n = loop (Int.add n 1)
                         ^^^^^^^^^
  Error: Out of fuel: evaluation took more than 1000 steps
  [1]
  $ stag-build test -fuel 1000
  File "lib/a.stag", line 1, characters 18-31:
  1 | let loop n = loop (Int.add n 1)
                        ^^^^^^^^^^^^^
  Error: Expect test "loops" failed: Out of fuel: evaluation took more than 1000 steps
  lib: 1 of 1 test(s) failed
  [1]
