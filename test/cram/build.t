stag-build builds projects of libraries, executables and tests. The example
project:

  $ cp -RL ../../examples/calc calc && chmod -R u+w calc && cd calc
  $ find . -type f | sort
  ./bin/main.stag
  ./bin/stag
  ./fmt/show.stag
  ./fmt/stag
  ./math/arith.stag
  ./math/power.stag
  ./math/stag
  ./stag-project
  ./test/calc_test.stag
  ./test/stag
  $ cat stag-project math/stag fmt/stag bin/stag test/stag
  (library (name math))
  (library
   (name fmt)
   (libraries math))
  (executable
   (name calc)
   (libraries math fmt))
  (test
   (name calc_test)
   (libraries fmt))

Building typechecks everything, writing nothing.

  $ stag-build build
  $ ls -A
  bin
  fmt
  math
  stag-project
  test

Executables are evaluated with exec.

  $ stag-build exec calc
  3^4 = 81
  16

Tests run the expect tests of libraries and tests.

  $ stag-build test
  $ cat > test/calc_test.stag <<'X'
  > let%expect_test "show" =
  >   print_endline (Fmt.Show.power 2 8);
  >   [%expect {| 2^8 = 255 |}]
  > 
  > let%expect_test "more" =
  >   print_endline (Fmt.Show.power 10 3)
  > X
  $ cat >> math/power.stag <<'X'
  > 
  > let%expect_test "fourth" =
  >   print_int (fourth 3);
  >   [%expect {| |}]
  > X
  $ stag-build test
  --- math/power.stag
  +++ _stag/math/power.stag.corrected
  @@ -13,4 +13,4 @@
   
   let%expect_test "fourth" =
     print_int (fourth 3);
  -  [%expect {| |}]
  +  [%expect {| 81 |}]
  math: 1 of 2 test(s) failed
  --- test/calc_test.stag
  +++ _stag/test/calc_test.stag.corrected
  @@ -1,6 +1,7 @@
   let%expect_test "show" =
     print_endline (Fmt.Show.power 2 8);
  -  [%expect {| 2^8 = 255 |}]
  +  [%expect {| 2^8 = 256 |}]
   
   let%expect_test "more" =
  -  print_endline (Fmt.Show.power 10 3)
  +  print_endline (Fmt.Show.power 10 3);
  +  [%expect {| 10^3 = 1000 |}]
  calc_test: 2 of 2 test(s) failed
  [1]
  $ find _stag -type f | sort
  _stag/math/power.stag.corrected
  _stag/math/power.stag.digest
  _stag/test/calc_test.stag.corrected
  _stag/test/calc_test.stag.digest

Testing only some stanzas, from a subdirectory:

  $ (cd test && stag-build test calc_test)
  --- test/calc_test.stag
  +++ _stag/test/calc_test.stag.corrected
  @@ -1,6 +1,7 @@
   let%expect_test "show" =
     print_endline (Fmt.Show.power 2 8);
  -  [%expect {| 2^8 = 255 |}]
  +  [%expect {| 2^8 = 256 |}]
   
   let%expect_test "more" =
  -  print_endline (Fmt.Show.power 10 3)
  +  print_endline (Fmt.Show.power 10 3);
  +  [%expect {| 10^3 = 1000 |}]
  calc_test: 2 of 2 test(s) failed
  [1]
  $ stag-build test nope calc
  Error: Unknown test or library nope
  Error: calc is an executable, whose tests are not run
  [1]
  $ stag-build test math fmt
  --- math/power.stag
  +++ _stag/math/power.stag.corrected
  @@ -13,4 +13,4 @@
   
   let%expect_test "fourth" =
     print_int (fourth 3);
  -  [%expect {| |}]
  +  [%expect {| 81 |}]
  math: 1 of 2 test(s) failed
  [1]

Promoting moves the corrections over the sources.

  $ stag-build promote
  Promoting _stag/math/power.stag.corrected to math/power.stag.
  Promoting _stag/test/calc_test.stag.corrected to test/calc_test.stag.
  $ cat test/calc_test.stag
  let%expect_test "show" =
    print_endline (Fmt.Show.power 2 8);
    [%expect {| 2^8 = 256 |}]
  
  let%expect_test "more" =
    print_endline (Fmt.Show.power 10 3);
    [%expect {| 10^3 = 1000 |}]
  $ find _stag -type f
  $ stag-build test

A correction is removed once its tests pass.

  $ cat > test/calc_test.stag <<'X'
  > let%expect_test "show" = print_endline "x"
  > X
  $ stag-build test calc_test
  --- test/calc_test.stag
  +++ _stag/test/calc_test.stag.corrected
  @@ -1,1 +1,2 @@
  -let%expect_test "show" = print_endline "x"
  +let%expect_test "show" = print_endline "x";
  +[%expect {| x |}]
  calc_test: 1 of 1 test(s) failed
  [1]
  $ find _stag -type f
  _stag/test/calc_test.stag.corrected
  _stag/test/calc_test.stag.digest
  $ cat > test/calc_test.stag <<'X'
  > let%expect_test "show" = print_endline "x"; [%expect {| x |}]
  > X
  $ stag-build test calc_test
  $ find _stag -type f

exec errors:

  $ stag-build exec nope
  Error: Unknown executable nope
  [1]
  $ stag-build exec math
  Error: math is a library, not an executable
  [1]
  $ stag-build exec calc_test
  Error: calc_test is a test, not an executable
  [1]
  $ stag-build exec calc -- a b
  Error: executables cannot take arguments yet
  [1]
  $ stag-build exec calc --
  3^4 = 81
  16

Type errors are reported once: targets depending on a library with errors are
not checked.

  $ cp math/arith.stag arith.bak
  $ echo 'let oops = Int.add 1 "two"' >> math/arith.stag
  $ stag-build build
  File "math/arith.stag", line 3, characters 21-26:
  3 | let oops = Int.add 1 "two"
                           ^^^^^
  Error: This expression has type string but an expression was expected of type int
  [1]
  $ stag-build exec calc
  File "math/arith.stag", line 3, characters 21-26:
  3 | let oops = Int.add 1 "two"
                           ^^^^^
  Error: This expression has type string but an expression was expected of type int
  [1]
  $ stag-build test
  File "math/arith.stag", line 3, characters 21-26:
  3 | let oops = Int.add 1 "two"
                           ^^^^^
  Error: This expression has type string but an expression was expected of type int
  [1]
  $ mv arith.bak math/arith.stag

Runtime errors:

  $ cat > bin/main.stag <<'X'
  > let main = print_endline "dividing"; print_int (Int.div 1 0)
  > X
  $ stag-build exec calc
  dividing
  File "bin/main.stag", line 1, characters 47-60:
  1 | let main = print_endline "dividing"; print_int (Int.div 1 0)
                                                     ^^^^^^^^^^^^^
  Error: Division by zero
  [1]
  $ cat > test/calc_test.stag <<'X'
  > let%expect_test "div" = print_endline "before"; print_int (Int.div 1 0)
  > let%expect_test "ok" = print_endline "fine"; [%expect {| fine |}]
  > X
  $ stag-build test calc_test
  File "test/calc_test.stag", line 1, characters 58-71:
  1 | let%expect_test "div" = print_endline "before"; print_int (Int.div 1 0)
                                                                ^^^^^^^^^^^^^
  Error: Expect test "div" failed: Division by zero
  Output since the last [%expect]:
    before
  calc_test: 1 of 2 test(s) failed
  [1]

Errors in stag files:

  $ cat > bin/stag <<'X'
  > (executable (name calc) (libraries math fmt nope)
  > X
  $ stag-build build
  File "bin/stag", line 2, characters 0-0:
  2 | 
      ^
  Error: Unclosed parentheses at end of input
  [1]
  $ cat > bin/stag <<'X'
  > (executable (name calc) (libraries math fmt nope))
  > X
  $ stag-build build
  File "bin/stag", line 1, characters 44-48:
  1 | (executable (name calc) (libraries math fmt nope))
                                                  ^^^^
  Error: Unknown library nope
  [1]
  $ cat > bin/stag <<'X'
  > (executable (name calc) (libraries math fmt))
  > X
  $ cat > math/stag <<'X'
  > (library (name math) (libraries fmt))
  > X
  $ stag-build build
  File "fmt/stag", line 3, characters 12-16:
  3 |  (libraries math))
                  ^^^^
  Error: Dependency cycle between libraries: math -> fmt -> math
  [1]
  $ echo '(library (name fmt))' > math/stag
  $ stag-build build
  File "math/stag", line 1, characters 15-18:
  1 | (library (name fmt))
                     ^^^
  Error: The name fmt is already used by the library in fmt/stag
  File "bin/stag", line 1, characters 35-39:
  1 | (executable (name calc) (libraries math fmt))
                                         ^^^^
  Error: Unknown library math
  File "fmt/stag", line 3, characters 12-16:
  3 |  (libraries math))
                  ^^^^
  Error: Unknown library math
  [1]

Outside a project:

  $ cd .. && mkdir elsewhere && cd elsewhere && stag-build build
  Error: No stag-project file in the current directory or any of its ancestors
  [1]
