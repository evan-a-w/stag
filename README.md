# stag

A staged ML: OCaml-like syntax, polymorphic records and variants, and typed
quotes (`.< e >.`) and splices (`.~e`), including compile time splices. See
[syntax.md](syntax.md).

```
stag check FILE...            # typecheck, print the signature
stag run FILE...              # typecheck and evaluate
stag test [-promote] FILE...  # run let%expect_test tests, writing FILE.corrected
```

A program is a set of files, each a module named after the file
(`foo_bar.stag` is `Foo_bar`). Errors are reported as OCaml's compiler does.

## stag-build

A dune-like build tool for projects (see [examples/calc](examples/calc)). The
project root is the nearest directory, from the current one up, with a
`stag-project` file (empty for now). Each directory under it (except those
starting with `.` or `_`) may have a `stag` file with one stanza, which owns
the `.stag` files of that directory:

```
(library (name foo) (libraries bar baz))   ; libraries is optional
(executable (name main) (libraries foo))
(test (name foo_test) (libraries foo))
```

Names are unique in the project. Library `foo` is the module `Foo`, with its
files as submodules (`Foo.A`), which refer to each other directly (`A.x`).
The files of an executable or test are modules of the program themselves. A
program is its libraries, closed transitively, then its own files. Every
library of that closure is visible by name, not only those listed (so the
files of an executable or test can't be named after one), and they shadow
prims (a library `int` would hide `Int.*`). Evaluation is in dependency order,
as for `stag run`.

```
stag-build build             # typecheck every stanza
stag-build exec NAME         # typecheck and evaluate an executable
stag-build test [NAME...]    # run the expect tests of libraries and tests
stag-build promote           # accept the corrections of failed tests
```

Expect tests run in library and test stanzas (not executables), each in its own
program. The corrections of failing files are written to
`_stag/PATH.corrected` (removed once they pass), and the diffs printed.
Executables cannot take arguments yet (`exec NAME -- ARGS` is rejected).
Nothing else is written: programs are interpreted.

## Library

The library (`lib/`) provides the pieces for other tools: `Source_file` and
`Driver.check_files` (files to a checked program; `Driver.check_modules` for
modules already parsed), `Driver.eval`, `Expect_test_runner`, `Diagnostic`
(rendering errors) and `Text_diff`.

Tests: `dune build @runtest` (expect tests in `test/` and
`stag_build/test/`, CLI cram tests in `test/cram/`).
