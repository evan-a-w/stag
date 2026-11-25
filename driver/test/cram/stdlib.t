The sources of the prelude in a stag checkout, marked by its stag-stdlib file,
are checked as the prelude: Core's modules see each other, and prims.

Copies of a checkout in its own _build directory are not checkouts, so the
checkout is made outside it.

  $ ROOT=$PWD && cd "$TMPDIR" && mkdir stag && cp -RL "$ROOT"/../../../stag-stdlib "$ROOT"/../../../core "$ROOT"/../../../prelude stag/ && chmod -R u+w stag && cd stag

  $ stag check core/option.stag | head -4
  module Core : sig
    module Option : sig
      val some : 'a -> option['a]
      val is_some : option['a] -> bool
  $ stag check prelude/prelude.stag | head -4
  type list['a] = Nil | Cons of ptr['a * list['a]]
  type option['a] = None | Some of 'a
  type result['a, 'b] = Ok of 'a | Error of 'b
  exception Division_by_zero
  $ cd core && stag check option.stag ../prelude/prelude.stag | grep -c . && cd ..
  154

Names in the prelude's sources are as within the prelude, and a file given
twice is checked once.

  $ stag check core/sexp.stag ./core/sexp.stag core/../core/sexp.stag | grep -E 'of_string_many :|^module' 
  module Core : sig
      val of_string_many : string -> result[list[t], Parse_error.t]

Programs in a checkout see the prelude of its sources, not the embedded one,
and may be checked along with them.

  $ mkdir tests && echo 'let x = Core.List.length_plus_one Nil' > tests/t.stag
  $ cat >> core/list.stag <<'X'
  > let length_plus_one l = Int.succ (length l)
  > X
  $ stag check tests/t.stag
  module T : sig
    val x : int
  end
  $ stag run tests/t.stag
  $ stag check core/int.stag tests/t.stag | grep -E '^module|^val'
  module Core : sig
  module T : sig
  $ echo 'let x = Core.List.length_plus_one Nil' > ../outside.stag && stag check ../outside.stag
  File "../outside.stag", line 1, characters 8-33:
  1 | let x = Core.List.length_plus_one Nil
              ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound value Core.List.length_plus_one
  [1]
  $ cp ../outside.stag tests/inside.stag && stag check ../outside.stag tests/inside.stag
  File "../outside.stag", line 1, characters 8-33:
  1 | let x = Core.List.length_plus_one Nil
              ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound value Core.List.length_plus_one
  File "tests/inside.stag", line 1, characters 8-33:
  1 | let x = Core.List.length_plus_one Nil
              ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound value Core.List.length_plus_one
  [1]

Core's files must be modules, and the prelude has no declaration splices.

  $ echo 'let x = 1' > core/bad-name.stag && stag check tests/t.stag
  File "core/bad-name.stag":
  Error: bad-name.stag is not a valid module file name: it should be a letter followed by letters, digits, _ and ', then .stag
  [1]
  $ rm core/bad-name.stag && echo 'let x =' > core/broken.stag && stag check core/int.stag
  File "core/broken.stag", line 2, characters 0-0:
  2 | 
      ^
  Error: Expected an expression, found end of file
  [1]
  $ echo 'include%inline .~(Nil)' > core/broken.stag && echo 'end%inline' >> core/broken.stag && stag check core/int.stag
  File "core/broken.stag", line 1, characters 0-22:
  1 | include%inline .~(Nil)
      ^^^^^^^^^^^^^^^^^^^^^^
  Error: This declaration splice failed at compile time: the prelude cannot have declaration splices
  [1]
  $ rm core/broken.stag && stag check core/int.stag | head -1
  module Core : sig

The preludes of two checkouts are not checked together.

  $ cp -R . ../stag2 && stag check core/int.stag ../stag2/core/int.stag
  Error: core/int.stag and ../stag2/core/int.stag are sources of the preludes of different checkouts, which are not checked together
  [1]

Errors are located in their files.

  $ sed -i.bak 's/Int.succ acc/Int.succc acc/' core/list.stag && rm core/list.stag.bak
  $ stag check core/option.stag
  File "core/list.stag", line 16, characters 32-41:
  16 |     | Cons * (_, rest) -> loop (Int.succc acc) rest
                                       ^^^^^^^^^
  Error: Unbound value Int.succc
  [1]
  $ cat >> prelude/prelude.stag <<'X'
  > let oops = Prim.Nope.nope
  > X
  $ stag check core/list.stag
  File "core/list.stag", line 16, characters 32-41:
  16 |     | Cons * (_, rest) -> loop (Int.succc acc) rest
                                       ^^^^^^^^^
  Error: Unbound value Int.succc
  File "prelude/prelude.stag", line 385, characters 11-25:
  385 | let oops = Prim.Nope.nope
                   ^^^^^^^^^^^^^^
  Error: Unbound value Prim.Nope.nope
  [1]

Prims are only the prelude's.

  $ cd .. && echo 'let x = Prim.String.length' > a.stag && stag check a.stag
  File "a.stag", line 1, characters 8-12:
  1 | let x = Prim.String.length
              ^^^^
  Error: Unbound module Prim
  [1]
  $ stag check a.stag stag/core/list.stag
  File "stag/core/list.stag", line 16, characters 32-41:
  16 |     | Cons * (_, rest) -> loop (Int.succc acc) rest
                                       ^^^^^^^^^
  Error: Unbound value Int.succc
  File "stag/prelude/prelude.stag", line 385, characters 11-25:
  385 | let oops = Prim.Nope.nope
                   ^^^^^^^^^^^^^^
  Error: Unbound value Prim.Nope.nope
  [1]

Without the marker, they are programs.

  $ cp "$ROOT"/../../../core/list.stag stag/core/ && rm stag/stag-stdlib && stag check stag/core/list.stag
  File "stag/core/list.stag", line 16, characters 32-40:
  16 |     | Cons * (_, rest) -> loop (Int.succ acc) rest
                                       ^^^^^^^^
  Error: Unbound value Int.succ
  File "stag/core/list.stag", line 60, characters 27-35:
  60 |       | false -> nth rest (Int.pred n)
                                  ^^^^^^^^
  Error: Unbound value Int.pred
  File "stag/core/list.stag", line 80, characters 20-28:
  80 |     | true -> loop (Int.succ i) (Cons (Ptr.make ( (f i, acc))))
                           ^^^^^^^^
  Error: Unbound value Int.succ
  File "stag/core/list.stag", line 104, characters 32-40:
  104 |     | Cons * (x, rest) -> loop (Int.succ i) rest (Cons (Ptr.make ( (f i x, acc))))
                                        ^^^^^^^^
  Error: Unbound value Int.succ
  File "stag/core/list.stag", line 121, characters 12-20:
  121 |       loop (Int.succ i) rest
                    ^^^^^^^^
  Error: Unbound value Int.succ
  File "stag/core/list.stag", line 157, characters 14-22:
  157 |     | true -> Int.succ n
                      ^^^^^^^^
  Error: Unbound value Int.succ
  File "stag/core/list.stag", line 225, characters 43-51:
  225 |     | Cons * (x, rest), true -> loop rest (Int.pred n) (Cons (Ptr.make ( (x, acc))))
                                                   ^^^^^^^^
  Error: Unbound value Int.pred
  File "stag/core/list.stag", line 233, characters 36-44:
  233 |   | Cons * (_, rest), true -> drop (Int.pred n) rest
                                            ^^^^^^^^
  Error: Unbound value Int.pred
  [1]
