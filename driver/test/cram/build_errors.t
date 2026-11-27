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

Warnings are reported once per target, both when checking succeeds and when it fails.

  $ rm -rf lib
  $ mkdir lib bin
  $ echo '(library (name lib))' > lib/stag
  $ echo '(executable (name app) (libraries lib))' > bin/stag
  $ cat > lib/a.stag <<'X'
  > let f b = match b with true -> 1 | false -> 2 | _ -> 3
  > X
  $ cat > bin/main.stag <<'X'
  > let g b = match b with true -> 1 | false -> 2 | true -> 3
  > let x = Lib.A.f true
  > X
  $ stag-build build
  File "lib/a.stag", line 1, characters 48-49:
  1 | let f b = match b with true -> 1 | false -> 2 | _ -> 3
                                                      ^
  Warning: This case is unused [unused-arm]: the cases before it match every value it does
  File "bin/main.stag", line 1, characters 48-52:
  1 | let g b = match b with true -> 1 | false -> 2 | true -> 3
                                                      ^^^^
  Warning: This case is unused [unused-arm]: the cases before it match every value it does
  $ cat >> bin/main.stag <<'X'
  > let bad : int = "oops"
  > X
  $ stag-build build
  File "lib/a.stag", line 1, characters 48-49:
  1 | let f b = match b with true -> 1 | false -> 2 | _ -> 3
                                                      ^
  Warning: This case is unused [unused-arm]: the cases before it match every value it does
  File "bin/main.stag", line 1, characters 48-52:
  1 | let g b = match b with true -> 1 | false -> 2 | true -> 3
                                                      ^^^^
  Warning: This case is unused [unused-arm]: the cases before it match every value it does
  File "bin/main.stag", line 3, characters 16-22:
  3 | let bad : int = "oops"
                      ^^^^^^
  Error: This expression has type string but an expression was expected of type int
  [1]
