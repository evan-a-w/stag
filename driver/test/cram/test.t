Expect tests.

  $ cat > power.stag <<'EOF'
  > let pow x n =
  >   let go acc n = match Int.equal n 0 with
  >     | true -> acc
  >     | false -> go (Int.mul acc x) (Int.sub n 1)
  >   in
  >   go 1 n
  > 
  > let%expect_test "power" =
  >   print_int (pow 3 2);
  >   [%expect {| 9 |}]
  > 
  > let%expect_test "wrong" =
  >   print_int (pow 2 10);
  >   [%expect {| 1000 |}];
  >   print_endline "a";
  >   print_endline "b"
  > EOF
  $ cat > ok.stag <<'EOF'
  > let%expect_test "fine" = print_string "x"; [%expect {| x |}]
  > EOF

Running prints a diff and writes the corrected file.

  $ stag test power.stag ok.stag
  --- power.stag
  +++ power.stag.corrected
  @@ -11,6 +11,10 @@
   
   let%expect_test "wrong" =
     print_int (pow 2 10);
  -  [%expect {| 1000 |}];
  +  [%expect {| 1024 |}];
     print_endline "a";
  -  print_endline "b"
  +  print_endline "b";
  +  [%expect {|
  +    a
  +    b
  +    |}]
  1 of 3 test(s) failed
  [1]
  $ ls
  ok.stag
  power.stag
  power.stag.corrected
  $ cat power.stag.corrected
  let pow x n =
    let go acc n = match Int.equal n 0 with
      | true -> acc
      | false -> go (Int.mul acc x) (Int.sub n 1)
    in
    go 1 n
  
  let%expect_test "power" =
    print_int (pow 3 2);
    [%expect {| 9 |}]
  
  let%expect_test "wrong" =
    print_int (pow 2 10);
    [%expect {| 1024 |}];
    print_endline "a";
    print_endline "b";
    [%expect {|
      a
      b
      |}]

Tests do not run with run.

  $ stag run power.stag ok.stag

Promoting corrects the file in place, after which the tests pass.

  $ stag test --promote power.stag ok.stag
  --- power.stag
  +++ power.stag.corrected
  @@ -11,6 +11,10 @@
   
   let%expect_test "wrong" =
     print_int (pow 2 10);
  -  [%expect {| 1000 |}];
  +  [%expect {| 1024 |}];
     print_endline "a";
  -  print_endline "b"
  +  print_endline "b";
  +  [%expect {|
  +    a
  +    b
  +    |}]
  1 of 3 test(s) failed (expectations promoted)
  $ stag test power.stag ok.stag
  $ ls
  ok.stag
  power.stag

Runtime errors fail the test.

  $ cat > err.stag <<'EOF'
  > let%expect_test "boom" =
  >   print_int (Int.div 1 0)
  > EOF
  $ stag test err.stag
  File "err.stag", line 2, characters 12-25:
  2 |   print_int (Int.div 1 0)
                  ^^^^^^^^^^^^^
  Error: Expect test "boom" failed: Uncaught exception Division_by_zero
  1 of 1 test(s) failed
  [1]

The output since the last [%expect] is shown with the error, and printed output
comes before it.

  $ cat > err_output.stag <<'EOF'
  > let%expect_test "partial" =
  >   print_string "seen";
  >   [%expect {| seen |}];
  >   print_endline "before";
  >   print_endline "  the error";
  >   match 2 with 1 -> ()
  > EOF
  $ stag test err_output.stag 2>&1
  File "err_output.stag", line 6, characters 2-22:
  6 |   match 2 with 1 -> ()
        ^^^^^^^^^^^^^^^^^^^^
  Error: Expect test "partial" failed: Match failure: no case matches the value 2
  Output since the last [%expect]:
    before
      the error
  1 of 1 test(s) failed
  [1]

An [%expect] reached with different outputs is an error, not corrected.

  $ cat > twice.stag <<'EOF'
  > let%expect_test "l" = let f x = print_int x; [%expect {| 1 |}] in f 1; f 2
  > EOF
  $ stag test twice.stag
  File "twice.stag", line 1, characters 54-61:
  1 | let%expect_test "l" = let f x = print_int x; [%expect {| 1 |}] in f 1; f 2
                                                            ^^^^^^^
  Error: Expect test "l": this [%expect] was reached with different outputs:
  Output 1:
    1
  Output 2:
    2
  1 of 1 test(s) failed
  [1]
  $ ls twice.stag*
  twice.stag

-promote removes a stale FILE.corrected.

  $ cat > stale.stag <<'EOF'
  > let%expect_test "s" = print_int 1
  > EOF
  $ stag test stale.stag > /dev/null 2>&1
  [1]
  $ ls stale.stag*
  stale.stag
  stale.stag.corrected
  $ stag test -promote stale.stag > /dev/null 2>&1
  $ ls stale.stag*
  stale.stag
  $ cat stale.stag
  let%expect_test "s" = print_int 1;
  [%expect {| 1 |}]

A file given more than once is read once.

  $ stag test ok.stag ok.stag ./ok.stag
  $ stag check ok.stag ok.stag
  module Ok : sig end

[%expect] outside a test.

  $ printf 'let x = [%%expect]\n' > outside.stag
  $ stag test outside.stag
  File "outside.stag", line 1, characters 8-17:
  1 | let x = [%expect]
              ^^^^^^^^^
  Error: [%expect] is only allowed in the body of a let%expect_test where it runs with the test: not in a quote's code nor in a toplevel splice
  [1]

-print-corrected prints the corrected file, or the file as it is if its tests
pass, writing nothing. Mismatches alone don't fail.

  $ cat > print.stag <<'EOF'
  > let%expect_test "p" = print_int 1; [%expect {| 2 |}]
  > EOF
  $ stag test -print-corrected print.stag
  let%expect_test "p" = print_int 1; [%expect {| 1 |}]
  1 of 1 test(s) failed
  $ ls print.stag*
  print.stag
  $ stag test -print-corrected ok.stag
  let%expect_test "fine" = print_string "x"; [%expect {| x |}]

Errors still fail, after printing the corrections of other tests.

  $ cat >> print.stag <<'EOF'
  > let%expect_test "boom" = print_int (Int.div 1 0)
  > EOF
  $ stag test -print-corrected print.stag
  File "print.stag", line 2, characters 35-48:
  2 | let%expect_test "boom" = print_int (Int.div 1 0)
                                         ^^^^^^^^^^^^^
  Error: Expect test "boom" failed: Uncaught exception Division_by_zero
  let%expect_test "p" = print_int 1; [%expect {| 1 |}]
  let%expect_test "boom" = print_int (Int.div 1 0)
  2 of 2 test(s) failed
  [1]
  $ stag test -print-corrected power.stag ok.stag
  Error: -print-corrected takes one FILE
  [2]
  $ stag test -print-corrected -promote ok.stag
  Error parsing command line:
  
    Cannot pass more than one of these: 
      -promote
      -print-corrected
  
  For usage information, run
  
    stag test -help
  
  [1]

Limits apply to tests, evaluation and compile time.

  $ cat > loop.stag <<'EOF'
  > let loop n = loop (Int.add n 1)
  > let%expect_test "loop" = loop 0
  > let%expect_test "fine" = print_int 1; [%expect {| 1 |}]
  > EOF
  $ stag test -max-depth 1000 loop.stag
  File "loop.stag", line 1, characters 19-26:
  1 | let loop n = loop (Int.add n 1)
                         ^^^^^^^
  Error: Expect test "loop" failed: Stack depth exceeded: evaluation nested more than 1000 steps deep
  1 of 2 test(s) failed
  [1]
  $ stag test -fuel 1000 loop.stag
  File "loop.stag", line 1, characters 19-28:
  1 | let loop n = loop (Int.add n 1)
                         ^^^^^^^^^
  Error: Expect test "loop" failed: Out of fuel: evaluation took more than 1000 steps
  1 of 2 test(s) failed
  [1]
Where the time limit stops evaluation varies.

  $ stag test -time-limit 100ms loop.stag 2>&1 | grep Error
  Error: Expect test "loop" failed: Time limit exceeded: evaluation took more than 100ms
  $ printf 'let loop n = loop n\nlet x = loop 0\n' > loop_run.stag
  $ stag run -fuel 1000 loop_run.stag
  File "loop_run.stag", line 1, characters 13-17:
  1 | let loop n = loop n
                   ^^^^
  Error: Out of fuel: evaluation took more than 1000 steps
  [1]
  $ printf 'let loop n = loop n\nlet x = .~(loop 0)\n' > loop_check.stag
  $ stag check -fuel 1000 loop_check.stag
  File "loop_check.stag", line 2, characters 8-18:
  2 | let x = .~(loop 0)
              ^^^^^^^^^^
  Error: This toplevel splice failed at compile time: Out of fuel: evaluation took more than 1000 steps
  File "loop_check.stag", line 1, characters 13-17:
  1 | let loop n = loop n
                   ^^^^
  Note: The error was raised here
  [1]
