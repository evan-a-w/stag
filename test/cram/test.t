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
  Error: Division by zero
  1 of 1 test(s) failed
  [1]

[%expect] outside a test.

  $ printf 'let x = [%%expect]\n' > outside.stag
  $ stag test outside.stag
  File "outside.stag", line 1, characters 8-17:
  1 | let x = [%expect]
              ^^^^^^^^^
  Error: [%expect] is only allowed in the body of a let%expect_test, outside quotes and toplevel splices
  [1]
