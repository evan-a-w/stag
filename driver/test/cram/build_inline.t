stag-build corrects inline expansions like expect tests, in libraries and tests.

  $ touch stag-project && mkdir gen t
  $ echo '(library (name gen))' > gen/stag
  $ cat > gen/gen.stag <<'EOF'
  > let value name n = Cons (Ptr.make ( (Stag.Toplevel.let_ name Nil (Stag.Expr.int n), Nil)))
  > 
  > include%inline .~(value "base" 10)
  > end%inline
  > EOF
  $ echo '(test (name t) (libraries gen))' > t/stag
  $ cat > t/t.stag <<'EOF'
  > module%inline M = .~(Gen.Gen.value "x" 1)
  > end%inline
  > 
  > let%expect_test "x" = print_int (Int.add M.x Gen.Gen.base)
  > EOF
  $ stag-build test
  File "gen/gen.stag", line 3, characters 0-34:
  3 | include%inline .~(value "base" 10)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This inline expansion is out of date
  --- gen/gen.stag
  +++ _stag/gen/gen.stag.corrected
  @@ -1,4 +1,5 @@
   let value name n = Cons (Ptr.make ( (Stag.Toplevel.let_ name Nil (Stag.Expr.int n), Nil)))
   
   include%inline .~(value "base" 10)
  +  let base = 10
   end%inline
  gen: 1 inline expansion(s) out of date
  File "t/t.stag", line 1, characters 0-41:
  1 | module%inline M = .~(Gen.Gen.value "x" 1)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This inline expansion is out of date
  --- t/t.stag
  +++ _stag/t/t.stag.corrected
  @@ -1,4 +1,6 @@
   module%inline M = .~(Gen.Gen.value "x" 1)
  +  let x = 1
   end%inline
   
  -let%expect_test "x" = print_int (Int.add M.x Gen.Gen.base)
  +let%expect_test "x" = print_int (Int.add M.x Gen.Gen.base);
  +[%expect {| 11 |}]
  t: 1 of 1 test(s) failed
  t: 1 inline expansion(s) out of date
  [1]
  $ find _stag -name '*.corrected' | sort
  _stag/gen/gen.stag.corrected
  _stag/t/t.stag.corrected
  $ stag-build promote
  Promoting _stag/gen/gen.stag.corrected to gen/gen.stag.
  Promoting _stag/t/t.stag.corrected to t/t.stag.
  $ cat gen/gen.stag t/t.stag
  let value name n = Cons (Ptr.make ( (Stag.Toplevel.let_ name Nil (Stag.Expr.int n), Nil)))
  
  include%inline .~(value "base" 10)
    let base = 10
  end%inline
  module%inline M = .~(Gen.Gen.value "x" 1)
    let x = 1
  end%inline
  
  let%expect_test "x" = print_int (Int.add M.x Gen.Gen.base);
  [%expect {| 11 |}]
  $ stag-build test

A changed generator makes the expansions using it stale.

  $ sed -i.bak 's/Stag.Expr.int n/Stag.Expr.int (Int.mul 2 n)/' gen/gen.stag && rm gen/gen.stag.bak
  $ stag-build test
  File "gen/gen.stag", line 3, characters 0-34:
  3 | include%inline .~(value "base" 10)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This inline expansion is out of date
  --- gen/gen.stag
  +++ _stag/gen/gen.stag.corrected
  @@ -1,5 +1,5 @@
   let value name n = Cons (Ptr.make ( (Stag.Toplevel.let_ name Nil (Stag.Expr.int (Int.mul 2 n)), Nil)))
   
   include%inline .~(value "base" 10)
  -  let base = 10
  +  let base = 20
   end%inline
  gen: 1 inline expansion(s) out of date
  File "t/t.stag", line 1, characters 0-41:
  1 | module%inline M = .~(Gen.Gen.value "x" 1)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This inline expansion is out of date
  --- t/t.stag
  +++ _stag/t/t.stag.corrected
  @@ -1,6 +1,6 @@
   module%inline M = .~(Gen.Gen.value "x" 1)
  -  let x = 1
  +  let x = 2
   end%inline
   
   let%expect_test "x" = print_int (Int.add M.x Gen.Gen.base);
  -[%expect {| 11 |}]
  +[%expect {| 22 |}]
  t: 1 of 1 test(s) failed
  t: 1 inline expansion(s) out of date
  [1]
  $ stag-build promote
  Promoting _stag/gen/gen.stag.corrected to gen/gen.stag.
  Promoting _stag/t/t.stag.corrected to t/t.stag.
  $ stag-build test

Stale expansions are reported even if running the program fails, and checking
and building only warn of them.

  $ cat > t/t.stag <<'EOF'
  > module%inline M = .~(Gen.Gen.value "x" 1)
  > end%inline
  > let crash = raise (Failure "boom")
  > EOF
  $ stag-build test
  File "t/t.stag", line 1, characters 0-41:
  1 | module%inline M = .~(Gen.Gen.value "x" 1)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This inline expansion is out of date
  File "t/t.stag", line 3, characters 12-34:
  3 | let crash = raise (Failure "boom")
                  ^^^^^^^^^^^^^^^^^^^^^^
  Error: Uncaught exception Failure "boom"
  --- t/t.stag
  +++ _stag/t/t.stag.corrected
  @@ -1,3 +1,4 @@
   module%inline M = .~(Gen.Gen.value "x" 1)
  +  let x = 2
   end%inline
   let crash = raise (Failure "boom")
  t: 1 inline expansion(s) out of date
  [1]
  $ stag-build build
  File "t/t.stag", line 1, characters 0-41:
  1 | module%inline M = .~(Gen.Gen.value "x" 1)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: This inline expansion is out of date
