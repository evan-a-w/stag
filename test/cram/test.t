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
  Error: Expect test "boom" failed: Division by zero
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
