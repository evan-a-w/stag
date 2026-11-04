stag-build builds projects of libraries, executables and tests. The example
project:

  $ cp -RL ../../../examples/calc calc && chmod -R u+w calc && cd calc
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
  @@ -14,4 +14,4 @@
   
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
  @@ -14,4 +14,4 @@
   
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
  $ find _stag -type f | sort
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
  Error: Uncaught exception Division_by_zero
  [1]
  $ cat > test/calc_test.stag <<'X'
  > let%expect_test "div" = print_endline "before"; print_int (Int.div 1 0)
  > let%expect_test "ok" = print_endline "fine"; [%expect {| fine |}]
  > X
  $ stag-build test calc_test
  File "test/calc_test.stag", line 1, characters 58-71:
  1 | let%expect_test "div" = print_endline "before"; print_int (Int.div 1 0)
                                                                ^^^^^^^^^^^^^
  Error: Expect test "div" failed: Uncaught exception Division_by_zero
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
  Error: Expect test "error" failed: Uncaught exception Division_by_zero
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
  B.y
  B.mid
  A.x
  C.z
  Main
  $ cd ..

Symlinks to directories are not followed, those to files are:

  $ mkdir links && cd links && touch stag-project && mkdir a b
  $ echo '(library (name a))' > a/stag
  $ echo 'let x = 1' > a/x.stag
  $ echo '(library (name b))' > b/stag
  $ echo 'let y = 2' > y.stag.src
  $ ln -s ../y.stag.src b/y.stag
  $ ln -s .. a/loop
  $ ln -s ../b a/blink
  $ ln -s nowhere a/dangling
  $ stag-build build
  $ echo '(executable (name main) (libraries a b))' > stag
  $ echo 'let main = print_int (Int.add A.X.x B.Y.y)' > main.stag
  $ stag-build exec main
  3

Unreadable files and directories are errors:

  $ ln -s nowhere a/dangling.stag
  $ mkdir c && chmod 000 c
  $ stag-build build
  Error: a/dangling.stag: No such file or directory
  Error: c: Permission denied
  [1]
  $ chmod 755 c && rm a/dangling.stag
  $ cd ..

Promoting refuses corrections it can't apply safely, leaving them:

  $ mkdir promote && cd promote && touch stag-project && mkdir -p t/sub
  $ echo '(test (name t))' > t/stag
  $ echo 'let%expect_test "a" = print_int 1' > t/a.stag
  $ echo 'let%expect_test "b" = print_int 2' > t/b.stag
  $ stag-build test > /dev/null 2>&1
  [1]
  $ find _stag | sort
  _stag
  _stag/t
  _stag/t/a.stag.corrected
  _stag/t/a.stag.digest
  _stag/t/b.stag.corrected
  _stag/t/b.stag.digest
  $ echo 'let%expect_test "a" = print_int 10' > t/a.stag
  $ rm t/b.stag
  $ mkdir -p _stag/t/sub && echo x > _stag/t/sub/stag.corrected
  $ echo x > _stag/stag-project.corrected
  $ stag-build promote
  Error: Not promoting _stag/stag-project.corrected: stag-project is not a source of the project
  Error: Not promoting _stag/t/a.stag.corrected: t/a.stag has changed since its tests ran; run stag-build test again
  Error: Not promoting _stag/t/b.stag.corrected: t/b.stag does not exist
  Error: Not promoting _stag/t/sub/stag.corrected: t/sub/stag does not exist
  [1]
  $ cat t/a.stag
  let%expect_test "a" = print_int 10
  $ find _stag -type f | sort
  _stag/stag-project.corrected
  _stag/t/a.stag.corrected
  _stag/t/a.stag.digest
  _stag/t/b.stag.corrected
  _stag/t/b.stag.digest
  _stag/t/sub/stag.corrected
  $ rm -r _stag/stag-project.corrected _stag/t/sub _stag/t/b.*

Testing again replaces the correction, which then promotes, removing the
directories left empty:

  $ stag-build test > /dev/null 2>&1
  [1]
  $ stag-build promote
  Promoting _stag/t/a.stag.corrected to t/a.stag.
  $ cat t/a.stag
  let%expect_test "a" = print_int 10;
  [%expect {| 10 |}]
  $ find _stag
  _stag

A target that no longer checks has its corrections removed, as they are stale:

  $ echo 'let%expect_test "a" = print_int 1' > t/a.stag
  $ stag-build test > /dev/null 2>&1
  [1]
  $ find _stag -type f | sort
  _stag/t/a.stag.corrected
  _stag/t/a.stag.digest
  $ echo 'let%expect_test "a" = print_int "1"' > t/a.stag
  $ stag-build test 2>&1 | tail -1
  Error: This expression has type string but an expression was expected of type int
  $ find _stag
  _stag

A _stag that is not a directory is an error:

  $ rm -r _stag && touch _stag
  $ stag-build test
  Error: _stag is not a directory
  [1]
  $ stag-build promote
  Error: _stag is not a directory
  [1]
  $ cd ..

Formatting rewrites the sources that are not formatted, printing their diffs,
and with --check only prints them, failing if there are any. The example is
formatted:

  $ cp -RL ../../../examples/calc calc_fmt && chmod -R u+w calc_fmt && cd calc_fmt
  $ stag-build fmt --check
  $ stag-build fmt
  $ echo 'let   triple x =   Int.mul 3 x' >> math/arith.stag
  $ cp math/arith.stag arith.before
  $ stag-build fmt --check
  --- math/arith.stag
  +++ math/arith.stag
  @@ -1,3 +1,3 @@
   let double x = Int.mul 2 x
   let square x = Int.mul x x
  -let   triple x =   Int.mul 3 x
  +let triple x = Int.mul 3 x
  [1]
  $ diff arith.before math/arith.stag && rm arith.before
  $ stag-build fmt
  --- math/arith.stag
  +++ math/arith.stag
  @@ -1,3 +1,3 @@
   let double x = Int.mul 2 x
   let square x = Int.mul x x
  -let   triple x =   Int.mul 3 x
  +let triple x = Int.mul 3 x
  $ stag-build fmt --check
  $ cat math/arith.stag
  let double x = Int.mul 2 x
  let square x = Int.mul x x
  let triple x = Int.mul 3 x

A parse error is reported, the other sources still formatted:

  $ echo 'let x =' > fmt/broken.stag
  $ echo 'let   y = 1' > math/messy.stag
  $ stag-build fmt -check
  --- math/messy.stag
  +++ math/messy.stag
  @@ -1,1 +1,1 @@
  -let   y = 1
  +let y = 1
  File "fmt/broken.stag", line 2, characters 0-0:
  2 | 
      ^
  Error: Expected an expression, found end of file
  [1]
  $ stag-build fmt
  --- math/messy.stag
  +++ math/messy.stag
  @@ -1,1 +1,1 @@
  -let   y = 1
  +let y = 1
  File "fmt/broken.stag", line 2, characters 0-0:
  2 | 
      ^
  Error: Expected an expression, found end of file
  [1]
  $ cat math/messy.stag fmt/broken.stag
  let y = 1
  let x =

A source that cannot be rewritten is reported, not shown as rewritten:

  $ rm fmt/broken.stag
  $ echo 'let   y = 1' > math/messy.stag && chmod a-w math/messy.stag
  $ stag-build fmt
  Error: math/messy.stag: Permission denied
  [1]
  $ chmod u+w math/messy.stag && cat math/messy.stag
  let   y = 1
  $ cd ..
