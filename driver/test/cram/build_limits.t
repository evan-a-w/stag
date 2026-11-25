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
  $ stag-build build -time-limit 100ms
  $ rm app/splice.stag
  $ stag-build exec app -fuel 1000
  $ stag-build test -fuel 1000
