Tail calls take no OCaml stack: with the stack capped at 64k words, millions
of tail calls run, while a non-tail recursion far shallower overflows.

  $ export OCAMLRUNPARAM=l=64k

  $ cat > tail.stag <<'EOF'
  > let tail n acc =
  >   match Int.equal n 0 with
  >   | true -> acc
  >   | false -> tail (Int.sub n 1) (Int.add acc 1)
  > let main = print_int (tail 3000000 0); print_endline ""
  > EOF
  $ stag run tail.stag
  3000000

  $ cat > non_tail.stag <<'EOF'
  > let non_tail n =
  >   match Int.equal n 0 with
  >   | true -> 0
  >   | false -> Int.add 1 (non_tail (Int.sub n 1))
  > let main = print_int (non_tail 200000); print_endline ""
  > EOF
  $ stag run non_tail.stag 2>&1 | grep overflow
    Stack overflow

It runs with the default stack.

  $ OCAMLRUNPARAM= stag run non_tail.stag
  200000
