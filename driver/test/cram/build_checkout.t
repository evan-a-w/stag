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

From a directory in the project, paths are still relative to the project.

  $ cd app && stag-build build
  File "../prelude/prelude.stag", line 2, characters 0-0:
  2 | 
      ^
  Error: Expected an expression, found end of file
  [1]
  $ cd ../..

A project outside the checkout whose files are all symlinks into it sees the
checkout's prelude too, named relative to the project.

  $ mkdir ../outside && cd ../outside && touch stag-project
  $ mkdir lib app
  $ echo '(library (name lib))' > lib/stag
  $ ln -s ../../stag/proj/lib/v.stag lib/v.stag
  $ echo '(executable (name app) (libraries lib))' > app/stag
  $ ln -s ../../stag/proj/app/main.stag app/main.stag
  $ stag-build build
  File "../stag/prelude/prelude.stag", line 2, characters 0-0:
  2 | 
      ^
  Error: Expected an expression, found end of file
  [1]

With the prelude fixed, both build.

  $ echo 'let x = 1' > ../stag/prelude/prelude.stag
  $ stag-build build
  $ cd ../stag/proj && stag-build build
