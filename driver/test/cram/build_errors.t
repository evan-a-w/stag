stag-build reports the errors of every file of a target, and doesn't run
anything with errors.

  $ touch stag-project
  $ mkdir lib
  $ echo '(library (name lib))' > lib/stag
  $ cat > lib/a.stag <<'X'
  > let x : int = "x"
  > let y = B.z
  > 
  > let%expect_test "a" =
  >   print_endline "ran";
  >   [%expect {| |}]
  > X
  $ cat > lib/b.stag <<'X'
  > let z = Int.add 1 true
  > let w = (
  > X
  $ stag-build build
  File "lib/a.stag", line 1, characters 14-17:
  1 | let x : int = "x"
                    ^^^
  Error: This expression has type string but an expression was expected of type int
  File "lib/b.stag", line 1, characters 18-22:
  1 | let z = Int.add 1 true
                        ^^^^
  Error: This expression has type bool but an expression was expected of type int
  File "lib/b.stag", line 3, characters 0-0:
  3 | 
      ^
  Error: Expected an expression, found end of file
  [1]
  $ stag-build test
  File "lib/a.stag", line 1, characters 14-17:
  1 | let x : int = "x"
                    ^^^
  Error: This expression has type string but an expression was expected of type int
  File "lib/b.stag", line 1, characters 18-22:
  1 | let z = Int.add 1 true
                        ^^^^
  Error: This expression has type bool but an expression was expected of type int
  File "lib/b.stag", line 3, characters 0-0:
  3 | 
      ^
  Error: Expected an expression, found end of file
  [1]
  $ ls -A
  lib
  stag-project

Past 50 errors, only how many more are reported.

  $ for i in $(seq 1 55); do echo "let x$i : int = \"$i\""; done > lib/b.stag
  $ stag-build build 2>&1 | grep -c '^Error'
  50
  $ stag-build build 2>&1 | tail -2
  Error: This expression has type string but an expression was expected of type int
  ... and 7 more errors
