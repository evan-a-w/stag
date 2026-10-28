Formatting one file prints it formatted.

  $ cat > messy.stag <<'EOF'
  > let   double x =   Int.mul 2 x
  > let f x = match x with | 0 -> "zero" | _ -> "other"
  > EOF
  $ stag fmt messy.stag
  let double x = Int.mul 2 x
  
  let f x =
    match x with
    | 0 -> "zero"
    | _ -> "other"
  $ stag fmt messy.stag > tidy.stag
  $ stag fmt tidy.stag | diff - tidy.stag

An empty file formats to nothing.

  $ touch empty.stag
  $ stag fmt empty.stag

Checking lists the files that are not formatted, writing nothing.

  $ stag fmt --check tidy.stag empty.stag
  $ stag fmt --check messy.stag tidy.stag empty.stag
  messy.stag
  [1]
  $ stag fmt -check messy.stag
  messy.stag
  [1]
  $ cat messy.stag
  let   double x =   Int.mul 2 x
  let f x = match x with | 0 -> "zero" | _ -> "other"

Several files need a flag.

  $ stag fmt messy.stag tidy.stag
  Error: several FILEs need -inplace or -check
  [2]
  $ stag fmt --check --inplace messy.stag 2>&1 | head -4
  Error parsing command line:
  
    Cannot pass more than one of these: 
      -inplace

Formatting in place rewrites only the files that change.

  $ cp messy.stag messy2.stag
  $ touch -t 200001010000 old messy.stag messy2.stag tidy.stag
  $ stag fmt --inplace messy.stag messy2.stag tidy.stag
  $ diff messy.stag tidy.stag && diff messy2.stag tidy.stag
  $ find messy.stag messy2.stag tidy.stag -newer old
  messy.stag
  messy2.stag
  $ stag fmt -inplace messy.stag
  $ stag fmt --check messy.stag messy2.stag

Parse errors are reported, and the other files still processed.

  $ cat > broken.stag <<'EOF'
  > let x =
  > let y = 1
  > EOF
  $ echo 'let   z = 1' > messy3.stag
  $ stag fmt broken.stag
  File "broken.stag", line 3, characters 0-0:
  3 | 
      ^
  Error: Expected `in` for the `let` at 2:0, found end of file
  [1]
  $ stag fmt --check broken.stag messy3.stag
  File "broken.stag", line 3, characters 0-0:
  3 | 
      ^
  Error: Expected `in` for the `let` at 2:0, found end of file
  messy3.stag
  [1]
  $ stag fmt --inplace broken.stag messy3.stag
  File "broken.stag", line 3, characters 0-0:
  3 | 
      ^
  Error: Expected `in` for the `let` at 2:0, found end of file
  [1]
  $ cat messy3.stag broken.stag
  let z = 1
  let x =
  let y = 1

A missing file is an error.

  $ stag fmt missing.stag
  Error: missing.stag: No such file or directory
  [2]
  $ stag fmt --check tidy.stag missing.stag
  Error: missing.stag: No such file or directory
  [2]
