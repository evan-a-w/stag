Programs are sets of files, each a module named after its file.

  $ cat > util.stag <<'EOF'
  > let double x = Int.mul 2 x
  > let greet name = print_endline (String.append "Hello, " name)
  > EOF
  $ cat > main.stag <<'EOF'
  > let n = Util.double 21
  > let main = Util.greet "world"; print_int n; print_endline ""
  > EOF

  $ stag check main.stag util.stag
  module Main : sig
    val n : int
    val main : unit
  end
  module Util : sig
    val double : int -> int
    val greet : string -> unit
  end

Files are evaluated in dependency order.

  $ stag run main.stag util.stag
  Hello, world
  42

Output from toplevel splices is printed when checking.

  $ cat > splice.stag <<'EOF'
  > let x = .~(print_endline "compiling"; .<1>.)
  > EOF
  $ stag check splice.stag
  compiling
  module Splice : sig
    val x : int
  end

Errors are reported like OCaml's.

  $ cat > bad_type.stag <<'EOF'
  > let f x = Int.add x 1
  > let y =
  >   f "one"
  > EOF
  $ stag check bad_type.stag
  File "bad_type.stag", line 3, characters 4-9:
  3 |   f "one"
          ^^^^^
  Error: This expression has type string but an expression was expected of type int
  [1]
  $ cat > unbound.stag <<'EOF'
  > let y = Util.triple 1
  > EOF
  $ stag check unbound.stag util.stag
  File "unbound.stag", line 1, characters 8-19:
  1 | let y = Util.triple 1
              ^^^^^^^^^^^
  Error: Unbound value Util.triple
  [1]
  $ cat > syntax.stag <<'EOF'
  > let x = (1, 2
  > EOF
  $ stag check syntax.stag
  File "syntax.stag", line 2, characters 0-0:
  2 | 
      ^
  Error: Expected `)` to close this `(` (opened at 1:8), found end of file
  [1]
  $ printf 'let x = 4611686018427387904\n' > big.stag
  $ stag check big.stag
  File "big.stag", line 1, characters 8-27:
  1 | let x = 4611686018427387904
              ^^^^^^^^^^^^^^^^^^^
  Error: Integer literal 4611686018427387904 exceeds the range of int
  [1]
  $ printf 'let s = "abc\n' > lex.stag
  $ stag check lex.stag
  File "lex.stag", line 1, characters 8-9:
  1 | let s = "abc
              ^
  Error: Unterminated string literal
  [1]
  $ printf 'let x = 1\n' > bad-name.stag
  $ stag check bad-name.stag
  File "bad-name.stag":
  Error: bad-name.stag is not a valid module file name: it should be a letter followed by letters, digits, _ and ', then .stag
  [1]
  $ cat > multi.stag <<'EOF'
  > let r =
  >   .~(let x = 1 in
  >      Int.div x 0;
  >      .<1>.)
  > EOF
  $ stag check multi.stag
  File "multi.stag", lines 2-4, characters 2-11:
  2 |   .~(let x = 1 in
  3 |      Int.div x 0;
  4 |      .<1>.)
  Error: This toplevel splice failed at compile time: Uncaught exception Division_by_zero
  File "multi.stag", line 3, characters 5-16:
  3 |      Int.div x 0;
           ^^^^^^^^^^^
  Note: The error was raised here
  [1]
  $ cat > crash.stag <<'EOF'
  > let s = String.sub "abc" 1 5
  > EOF
  $ stag run crash.stag
  File "crash.stag", line 1, characters 8-28:
  1 | let s = String.sub "abc" 1 5
              ^^^^^^^^^^^^^^^^^^^^
  Error: Uncaught exception Invalid_argument "String.sub: invalid substring (position 1, length 5) of a string of length 3"
  [1]
  $ stag check missing.stag
  Error: missing.stag: No such file or directory
  [2]

Runtime errors are located, and output before them is printed first.

  $ cat > match.stag <<'EOF'
  > let a = print_endline "first"
  > let f x = match x with
  >   | 1 -> ()
  > let b = a; f 2
  > EOF
  $ stag run match.stag 2>&1
  first
  File "match.stag", lines 2-3, characters 10-11:
  2 | let f x = match x with
  3 |   | 1 -> ()
  Error: Match failure: no case matches the value 2
  [1]

Tabs before an error keep the caret aligned.

  $ printf 'let x =\n\tInt.add 1 true\n' > tab.stag
  $ stag check tab.stag 2>&1 | sed 's/\t/<tab>/g'
  File "tab.stag", line 2, characters 11-15:
  2 | <tab>Int.add 1 true
      <tab>          ^^^^
  Error: This expression has type bool but an expression was expected of type int

A splice's code using a variable bound at another level is explained.

  $ printf 'let w = .~(let z = 1 in .< z >.)\n' > level.stag
  $ stag check level.stag
  module Level : sig
    val w : int
  end
