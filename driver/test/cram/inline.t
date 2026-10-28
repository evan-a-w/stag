Inline expansions show what a declaration splice generates in the source.

  $ cat > gen.stag <<'EOF'
  > let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
  > 
  > include%inline .~(value "x" (Stag.Expr.int 1))
  > end%inline
  > 
  > module M = struct
  >   include%inline .~(value "y" (Stag.Expr.int 2))
  >   end%inline
  > end
  > 
  > let%expect_test "x" = print_int (Int.add x M.y); [%expect {| |}]
  > EOF

A fresh expansion is filled in, with the expect tests' corrections.

  $ stag test gen.stag
  File "gen.stag", line 3, characters 0-46:
  3 | include%inline .~(value "x" (Stag.Expr.int 1))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: this inline expansion is out of date: the declarations written differ from those its splice generates
  File "gen.stag", line 7, characters 2-48:
  7 |   include%inline .~(value "y" (Stag.Expr.int 2))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: this inline expansion is out of date: the declarations written differ from those its splice generates
  --- gen.stag
  +++ gen.stag.corrected
  @@ -1,11 +1,13 @@
   let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
   
   include%inline .~(value "x" (Stag.Expr.int 1))
  +  let x = 1
   end%inline
   
   module M = struct
     include%inline .~(value "y" (Stag.Expr.int 2))
  +    let y = 2
     end%inline
   end
   
  -let%expect_test "x" = print_int (Int.add x M.y); [%expect {| |}]
  +let%expect_test "x" = print_int (Int.add x M.y); [%expect {| 3 |}]
  2 inline expansion(s) out of date
  1 of 1 test(s) failed
  [1]
  $ stag test --promote gen.stag
  File "gen.stag", line 3, characters 0-46:
  3 | include%inline .~(value "x" (Stag.Expr.int 1))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: this inline expansion is out of date: the declarations written differ from those its splice generates
  File "gen.stag", line 7, characters 2-48:
  7 |   include%inline .~(value "y" (Stag.Expr.int 2))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: this inline expansion is out of date: the declarations written differ from those its splice generates
  --- gen.stag
  +++ gen.stag.corrected
  @@ -1,11 +1,13 @@
   let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
   
   include%inline .~(value "x" (Stag.Expr.int 1))
  +  let x = 1
   end%inline
   
   module M = struct
     include%inline .~(value "y" (Stag.Expr.int 2))
  +    let y = 2
     end%inline
   end
   
  -let%expect_test "x" = print_int (Int.add x M.y); [%expect {| |}]
  +let%expect_test "x" = print_int (Int.add x M.y); [%expect {| 3 |}]
  2 inline expansion(s) out of date (promoted)
  1 of 1 test(s) failed (expectations promoted)
  $ cat gen.stag
  let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
  
  include%inline .~(value "x" (Stag.Expr.int 1))
    let x = 1
  end%inline
  
  module M = struct
    include%inline .~(value "y" (Stag.Expr.int 2))
      let y = 2
    end%inline
  end
  
  let%expect_test "x" = print_int (Int.add x M.y); [%expect {| 3 |}]
  $ stag test gen.stag

Editing the expansion makes it stale.

  $ sed -i.bak 's/let y = 2/let y = 3 (* edited *)/' gen.stag
  $ stag test gen.stag
  File "gen.stag", line 8, characters 2-48:
  8 |   include%inline .~(value "y" (Stag.Expr.int 2))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: this inline expansion is out of date: the declarations written differ from those its splice generates
  --- gen.stag
  +++ gen.stag.corrected
  @@ -6,7 +6,7 @@
   
   module M = struct
     include%inline .~(value "y" (Stag.Expr.int 2))
  -    let y = 3 (* edited *)
  +    let y = 2
     end%inline
   end
   
  1 inline expansion(s) out of date
  [1]
  $ cat gen.stag.corrected
  let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
  
  include%inline .~(value "x" (Stag.Expr.int 1))
    let x = 1
  end%inline
  
  module M = struct
    include%inline .~(value "y" (Stag.Expr.int 2))
      let y = 2
    end%inline
  end
  
  let%expect_test "x" = print_int (Int.add x M.y); [%expect {| 3 |}]
  $ stag test -print-corrected gen.stag | diff - gen.stag.corrected
  File "gen.stag", line 8, characters 2-48:
  8 |   include%inline .~(value "y" (Stag.Expr.int 2))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: this inline expansion is out of date: the declarations written differ from those its splice generates
  1 inline expansion(s) out of date

Checking and running a program only warn of stale expansions.

  $ stag check gen.stag
  File "gen.stag", line 8, characters 2-48:
  8 |   include%inline .~(value "y" (Stag.Expr.int 2))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: this inline expansion is out of date: the declarations written differ from those its splice generates
  module Gen : sig
    val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
    val x : int
    module M : sig
      val y : int
    end
  end
  $ stag run gen.stag
  File "gen.stag", line 8, characters 2-48:
  8 |   include%inline .~(value "y" (Stag.Expr.int 2))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: this inline expansion is out of date: the declarations written differ from those its splice generates

Formatting formats the expansion.

  $ cat > fmt.stag <<'EOF'
  > include%inline .~e
  >       let x =   1 (* one *)
  >     end%inline
  > EOF
  $ stag fmt fmt.stag
  include%inline .~e
    let x = 1 (* one *)
  end%inline

The expansion's tokens.

  $ stag dump-tokens fmt.stag
  (inline 0 14)
  (punct 15 17)
  (lident 17 18)
  (keyword 25 28)
  (lident 29 30)
  (punct 31 32)
  (int 35 36)
  (comment 37 46)
  (inline 51 61)

The expansion is parsed, so syntax errors in it are reported.

  $ cat > bad.stag <<'EOF'
  > include%inline .~e
  >   let x =
  > end%inline
  > EOF
  $ stag test bad.stag
  File "bad.stag", line 3, characters 0-10:
  3 | end%inline
      ^^^^^^^^^^
  Error: Expected an expression, found `end%inline`
  [1]
