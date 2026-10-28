stag run and stag test report all errors, and don't evaluate a program with
errors.

  $ cat > a.stag <<'X'
  > let shown = print_endline "evaluated"
  > let x : int = "x"
  > let y = Int.add x true
  > 
  > let%expect_test "t" =
  >   print_endline "ran";
  >   [%expect {| |}]
  > X
  $ stag run a.stag
  File "a.stag", line 2, characters 14-17:
  2 | let x : int = "x"
                    ^^^
  Error: This expression has type string but an expression was expected of type int
  File "a.stag", line 3, characters 18-22:
  3 | let y = Int.add x true
                        ^^^^
  Error: This expression has type bool but an expression was expected of type int
  [1]
  $ stag test a.stag
  File "a.stag", line 2, characters 14-17:
  2 | let x : int = "x"
                    ^^^
  Error: This expression has type string but an expression was expected of type int
  File "a.stag", line 3, characters 18-22:
  3 | let y = Int.add x true
                        ^^^^
  Error: This expression has type bool but an expression was expected of type int
  [1]
  $ ls
  a.stag
  $ for i in $(seq 1 55); do echo "let x$i : int = \"$i\""; done > b.stag
  $ stag check b.stag 2>&1 | grep -c '^Error'
  50
  $ stag check b.stag 2>&1 | tail -2
  Error: This expression has type string but an expression was expected of type int
  ... and 5 more errors
