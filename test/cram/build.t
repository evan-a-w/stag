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

A stag-project that is not a regular file doesn't make a root:

  $ mkdir stag-project && stag-build build
  Error: No stag-project file in the current directory or any of its ancestors
  [1]
  $ cd .. && rm -r elsewhere

Expect test failures other than mismatches are reported:

  $ mkdir failures && cd failures && touch stag-project && mkdir t
  $ echo '(test (name t))' > t/stag
  $ cat > t/t.stag <<'X'
  > let%expect_test "twice" =
  >   let go n = match Int.equal n 0 with
  >     | true -> ()
  >     | false -> print_int n; [%expect {| 1 |}]; go (Int.sub n 1)
  >   in
  >   go 2
  > 
  > let%expect_test "error" =
  >   print_endline "before";
  >   [%expect {| before |}];
  >   print_endline "after";
  >   print_int (Int.div 1 0)
  > X
  $ stag-build test
  File "t/t.stag", line 4, characters 37-44:
  4 |     | false -> print_int n; [%expect {| 1 |}]; go (Int.sub n 1)
                                           ^^^^^^^
  Error: Expect test "twice": this [%expect] was reached with different outputs:
  Output 1:
    2
  Output 2:
    1
  File "t/t.stag", line 12, characters 12-25:
  12 |   print_int (Int.div 1 0)
                   ^^^^^^^^^^^^^
  Error: Expect test "error" failed: Division by zero
  Output since the last [%expect]:
    after
  t: 2 of 2 test(s) failed
  [1]
  $ cd ..

Testing a stanza runs only its own tests, not those of its libraries (this one
would loop):

  $ mkdir only && cd only && touch stag-project && mkdir lib dep
  $ echo '(library (name lib))' > lib/stag
  $ cat > lib/lib.stag <<'X'
  > let one = 1
  > let%expect_test "loops" = let loop x = loop x in loop ()
  > X
  $ echo '(test (name dep) (libraries lib))' > dep/stag
  $ cat > dep/dep.stag <<'X'
  > let%expect_test "one" = print_int Lib.Lib.one; [%expect {| 2 |}]
  > X
  $ stag-build test dep
  --- dep/dep.stag
  +++ _stag/dep/dep.stag.corrected
  @@ -1,1 +1,1 @@
  -let%expect_test "one" = print_int Lib.Lib.one; [%expect {| 2 |}]
  +let%expect_test "one" = print_int Lib.Lib.one; [%expect {| 1 |}]
  dep: 1 of 1 test(s) failed
  [1]
  $ cd ..

The files of a library are evaluated in dependency order, as those of a
program are by stag run:

  $ mkdir order && cd order && touch stag-project && mkdir lib bin
  $ echo '(library (name lib))' > lib/stag
  $ cat > lib/a.stag <<'X'
  > let x = print_endline "A.x"; B.y
  > X
  $ cat > lib/b.stag <<'X'
  > let first = print_endline "B.first"
  > let y = print_endline "B.y"; 1
  > X
  $ echo 'let z = print_endline "C.z"' > lib/c.stag
  $ echo '(executable (name main) (libraries lib))' > bin/stag
  $ echo 'let main = print_endline "Main"' > bin/main.stag
  $ stag-build exec main
  B.first
  B.y
  A.x
  C.z
  Main
  $ stag run lib/a.stag lib/b.stag lib/c.stag
  B.first
  B.y
  A.x
  C.z

A toplevel used before its turn is evaluated then:

  $ cat > lib/b.stag <<'X'
  > let first = y
  > let mid = print_endline "B.mid"
  > let y = print_endline "B.y"; 1
  > X
  $ stag-build exec main
  B.y
  B.mid
  A.x
  C.z
  Main
  $ stag run lib/a.stag lib/b.stag lib/c.stag
  B.y
  B.mid
  A.x
  C.z

Expect tests in executables are never run, so they are warned about:

  $ cat >> bin/main.stag <<'X'
  > let%expect_test "never" = print_endline "never"
  > X
  $ stag-build build
  File "bin/main.stag", line 2, characters 16-23:
  2 | let%expect_test "never" = print_endline "never"
                      ^^^^^^^
  Warning: This expect test is in the executable main, whose tests are never run
  $ stag-build exec main
  File "bin/main.stag", line 2, characters 16-23:
  2 | let%expect_test "never" = print_endline "never"
                      ^^^^^^^
  Warning: This expect test is in the executable main, whose tests are never run
  B.first
  B.y
  A.x
  C.z
  Main
  $ cd ..
