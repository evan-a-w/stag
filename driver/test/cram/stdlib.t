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
  type list['a] = Nil | Cons of 'a * list['a]
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
  File "stag/prelude/prelude.stag", line 249, characters 11-25:
  249 | let oops = Prim.Nope.nope
                   ^^^^^^^^^^^^^^
  Error: Unbound value Prim.Nope.nope
  [1]

Without the marker, they are programs.

  $ cp "$ROOT"/../../../core/list.stag stag/core/ && rm stag/stag-stdlib && stag check stag/core/list.stag
  File "stag/core/list.stag", line 16, characters 30-38:
  16 |     | Cons (_, rest) -> loop (Int.succ acc) rest
                                     ^^^^^^^^
  Error: Unbound value Int.succ
  [1]
