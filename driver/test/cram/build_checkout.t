A project in a stag checkout (made outside the _build directory) sees the prelude of its sources. If it has an
error, it is reported once, relative to the project, with its source, and no
target is checked.

  $ cd "$(mktemp -d)" && mkdir stag && cd stag && touch stag-stdlib && mkdir prelude core
  $ echo 'let x =' > prelude/prelude.stag
  $ mkdir proj && cd proj && touch stag-project
  $ mkdir lib app
  $ echo '(library (name lib))' > lib/stag
  $ echo 'let v = 1' > lib/v.stag
  $ echo '(executable (name app) (libraries lib))' > app/stag
  $ echo 'let x = Lib.V.v' > app/main.stag
  $ stag-build build
  File "../prelude/prelude.stag", line 2, characters 0-0:
  2 | 
      ^
  Error: Expected an expression, found end of file
  [1]
