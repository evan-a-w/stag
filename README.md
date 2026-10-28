# stag

A staged ML: OCaml-like syntax, polymorphic records and variants, and typed
quotes (`.< e >.`) and splices (`.~e`), including compile time splices. See
[syntax.md](syntax.md).

```
stag check FILE...            # typecheck, print the signature
stag run FILE...              # typecheck and evaluate
stag test [-promote] FILE...  # run let%expect_test tests, writing FILE.corrected
```

`stag test -print-corrected FILE` prints the corrected file instead, writing
nothing. `-fuel N`, `-max-depth N` and `-time-limit SPAN` bound each phase of
evaluation (compile time, evaluation, each test); they are unlimited by default.

A program is a set of files, each a module named after the file
(`foo_bar.stag` is `Foo_bar`). Errors are reported as OCaml's compiler does.

## stag-build

A dune-like build tool for projects (see [examples/calc](examples/calc)). The
project root is the nearest directory, from the current one up, with a
`stag-project` file (empty for now). Each directory under it (except those
starting with `.` or `_`, and symlinks to directories) may have a `stag` file
with one stanza, which owns the `.stag` files of that directory:

```
(library (name foo) (libraries bar baz))   ; libraries is optional
(executable (name main) (libraries foo))
(test (name foo_test) (libraries foo))
```

Names are unique in the project. Library `foo` is the module `Foo`, with its
files as submodules (`Foo.A`), which refer to each other directly (`A.x`).
The files of an executable or test are modules of the program themselves. A
program is its libraries, closed transitively, then its own files. Every
library of that closure is visible by name, not only those listed (so no file
of the program can be named after one), and they shadow prims (a library `int`
would hide `Int.*`). Evaluation is as for `stag run` (see
[syntax.md](syntax.md#files)): libraries and files in dependency order, and the
files of each library ordered among themselves likewise.

```
stag-build build             # typecheck every stanza
stag-build exec NAME         # typecheck and evaluate an executable
stag-build test [NAME...]    # run the expect tests of libraries and tests
stag-build promote           # accept the corrections of failed tests
```

Each target is checked once per command, and not if a library it depends on
has errors. Output at compile time is discarded. Expect tests run in library
and test stanzas, each in its own program, running only the tests of its own
files; those in executables are never run, and warned about. The corrections
of failing files are written to `_stag/PATH.corrected`, with the digest of the
version they correct in `_stag/PATH.digest` (both removed once the tests pass,
or if the target no longer checks), and the diffs printed. `promote` refuses,
leaving it, a correction whose source is not in the project or has changed
since. Executables cannot take arguments yet (`exec NAME -- ARGS` is
rejected). Nothing else is written: programs are interpreted.

## Library

The compiler is split into libraries, each in `X/src` (library `stag_X`, tests
in `X/test`): `common`, `types`, `syntax`, `ir`, `eval`, `typecheck` and
`driver` (whose `bin` is the `stag` executable), each depending only on those
before it. They provide the pieces for other tools: `Source_file` and
`Driver.check_files` (files to a checked program; `Driver.check_modules` for
modules already parsed), `Driver.eval`, `Expect_test_runner`, `Diagnostic`
(rendering errors) and `Text_diff`. `Prelude` is the stag module `Stag`
([eval/src/prelude/stag.stag](eval/src/prelude/stag.stag)), the syntax as
data, which `Syntax_value` converts to and from `Syntax`, for declarations
generated at compile time (not yet in scope of programs). The core library,
the stag module `Core`, is the files of [core/](core) (each a submodule,
`core/sexp.stag` is `Core.Sexp`), embedded by the library `stag_core_source`
and added to the prelude by `Prelude`.

Tests: `dune build @runtest`, then `dune promote` to accept changes.

- Runtime behaviour: stag expect tests in `tests/*.stag` (any new `.stag` file
  there is picked up automatically), run by `stag test -print-corrected`
  with a time and depth limit.
- Types, type errors and compile-time evaluation: OCaml expect tests on stag
  source strings, using `driver/test/source_helpers.mli`.
- Type internals and the parser: OCaml unit tests in `X/test/`.
- The CLI: cram tests in `driver/test/cram/`.
