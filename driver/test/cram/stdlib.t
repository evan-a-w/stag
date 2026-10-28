The sources of the prelude in a stag checkout, marked by its stag-stdlib file,
are checked as the prelude: Core's modules see each other, and prims.

  $ mkdir stag && cp -RL ../../../stag-stdlib ../../../core ../../../prelude stag/ && chmod -R u+w stag && cd stag

  $ stag check core/option.stag | head -4
  module Core : sig
    module Option : sig
      val some : 'a -> option['a]
      val is_some : option['a] -> bool
  $ stag check prelude/prelude.stag | head -4
  type list['a] = Nil | Cons of 'a * .list['a]
  type option['a] = None | Some of 'a
  type result['a, 'b] = Ok of 'a | Error of 'b
  exception Division_by_zero
  $ cd core && stag check option.stag ../prelude/prelude.stag | grep -c . && cd ..
  116

Errors are located in their files.

  $ sed -i.bak 's/Int.succ acc/Int.succc acc/' core/list.stag && rm core/list.stag.bak
  $ stag check core/option.stag
  File "core/list.stag", line 16, characters 30-39:
  16 |     | Cons (_, rest) -> loop (Int.succc acc) rest
                                     ^^^^^^^^^
  Error: Unbound value Int.succc
  [1]
  $ cat >> prelude/prelude.stag <<'X'
  > let oops = Prim.Nope.nope
  > X
  $ stag check core/list.stag
  File "core/list.stag", line 16, characters 30-39:
  16 |     | Cons (_, rest) -> loop (Int.succc acc) rest
                                     ^^^^^^^^^
  Error: Unbound value Int.succc
  File "prelude/prelude.stag", line 249, characters 11-25:
  249 | let oops = Prim.Nope.nope
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
  Error: sources of the prelude are checked only with each other
  [2]

Without the marker, they are programs.

  $ cp ../../../core/list.stag stag/core/ && rm stag/stag-stdlib && stag check stag/core/list.stag
  File "stag/core/list.stag", line 16, characters 30-38:
  16 |     | Cons (_, rest) -> loop (Int.succ acc) rest
                                     ^^^^^^^^
  Error: Unbound value Int.succ
  File "stag/core/list.stag", line 60, characters 27-35:
  60 |       | false -> nth rest (Int.pred n)
                                  ^^^^^^^^
  Error: Unbound value Int.pred
  File "stag/core/list.stag", line 80, characters 20-28:
  80 |     | true -> loop (Int.succ i) (Cons (f i, acc))
                           ^^^^^^^^
  Error: Unbound value Int.succ
  File "stag/core/list.stag", line 104, characters 30-38:
  104 |     | Cons (x, rest) -> loop (Int.succ i) rest (Cons (f i x, acc))
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
  File "stag/core/list.stag", line 225, characters 41-49:
  225 |     | Cons (x, rest), true -> loop rest (Int.pred n) (Cons (x, acc))
                                                 ^^^^^^^^
  Error: Unbound value Int.pred
  File "stag/core/list.stag", line 233, characters 34-42:
  233 |   | Cons (_, rest), true -> drop (Int.pred n) rest
                                          ^^^^^^^^
  Error: Unbound value Int.pred
  [1]
