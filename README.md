# stag

A staged ML: OCaml-like syntax, polymorphic records and variants, and typed
quotes (`.< e >.`) and splices (`.~e`), including compile time splices. See
[syntax.md](syntax.md).

```
stag check FILE...            # typecheck, print the signature
stag run FILE...              # typecheck and evaluate
stag test [-promote] FILE...  # run let%expect_test tests and inline expansions, writing FILE.corrected
stag bench FILE...            # run let%bench benchmarks (see Benchmarking)
stag fmt FILE                 # print FILE formatted
stag fmt -inplace FILE...     # format the files in place
stag fmt -check FILE...       # list the unformatted files on stdout, failing if any
```

`stag fmt` prints to 80 columns, keeping comments where they were (by the
tokens around them) and blank lines between toplevels.

`stag test -print-corrected FILE` prints the corrected file instead, writing
nothing. `-fuel N`, `-max-depth N` and `-time-limit SPAN` bound each phase of
evaluation (compile time, evaluation, each test); they are unlimited by default.

A program is a set of files, each a module named after the file
(`foo_bar.stag` is `Foo_bar`). Errors are reported as OCaml's compiler does:
all of them, by location (the first 50, then how many more), each once, and
not those that only follow from another (uses of what failed to check, names a
failed splice might have defined). A program with errors is not evaluated, nor
are its tests run.

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
stag-build bench [FILTER...] # run the benchmarks of every target
stag-build fmt [-check]      # format every source in place (or only check),
                             # printing the diffs
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

## Benchmarking

`let%bench "name" = e` and `let%bench "name" x in [a; b] = e` (see
[syntax.md](syntax.md#benchmarks)) are measured by `stag bench FILE...`
(`-filter SUBSTRING`, repeatable) and `stag-build bench [FILTER...]` (each
target in its own program, as tests are, its benches named `TARGET/NAME`),
modelled on criterion, divan and core_bench:

```
fib/15
  time:      [1.797 ms 1.837 ms 1.878 ms] (mean, 95% CI)
  median:    1.841 ms   stddev: 94.96 us   MADN: 70.69 us
  range:     [1.670 ms 2.071 ms]   p90: 1.926 ms   p99: 2.043 ms
  alloc:     1.407M minor words, 1.773k major words, 5.375 minor GCs, 0.01071 major GCs per iteration
  samples:   20 x 14 iterations (warm-up: 63 iterations in 120.3 ms)
  outliers:  1 of 20 (5.00%): 1 high mild
  change:    [+1.104% +3.212% +5.020%] (95% CI) Performance has regressed.
```

- Each bench starts after a `Gc.compact`. Warm-up runs batches of 1, 2, 4,
  ... iterations for `-warm-up SPAN` (default 1s, and at most 2^24
  iterations), estimating the time of an iteration from the last batch. Then,
  as divan does, `-samples N` (default 100) samples of a fixed number of
  iterations each, chosen so the samples take `-time SPAN` (default 3s) in
  all, and each at least 1000 times the clock's resolution; iterations slower
  than that make fewer samples (at least 10, or 5 with `-quick`), with a
  warning. `-quick` is 100ms, 500ms and 20 samples. The clock is
  `CLOCK_MONOTONIC`. Benches are not limited by `-fuel` etc., which apply to
  evaluating the program.
- Statistics are of the per-iteration time of each sample: the mean with a
  bootstrap confidence interval (10000 resamples, fixed seed), median,
  stddev, MADN (the median absolute deviation scaled by 1.4826, to estimate
  the standard deviation of normal data), min, max, p90/p99, and outliers classified by Tukey's
  fences (mild beyond 1.5 IQR, severe beyond 3). With `-no-alloc`, the GC's
  counters (minor and major words, collections per iteration) are not read.
- `-save-baseline NAME` saves the per-iteration times of the run in
  `_stag/bench/NAME.sexp` (of the project root, or `-baseline-dir` for
  `stag bench`), merged with those already saved; `-baseline NAME` compares
  with them: the bootstrapped change of the mean, regressed or improved, as
  criterion has it, if its whole interval is beyond `-noise-threshold
  PERCENT` (default 2), no change if it includes 0, else within noise. A
  baseline mean of 0 cannot be compared with, benches not in the baseline are
  noted, and a missing baseline is warned of. Baselines are written to a
  temporary file renamed over the old; a malformed one is an error to compare
  with and replaced, with a warning, by `-save-baseline`.
- `-format sexp|json` (`-sexp`, `-json`) prints all results at the end, for
  machines; the default is the human report, bench by bench.
- `-phases` benchmarks the interpreter itself on the program (of each
  target, for `stag-build`) instead: `parse`, `typecheck` (of the parsed
  program, including compile-time splices) and `eval` (of a freshly checked
  program, the checks untimed and made at most 64 at a time). [bench/](bench) is a project of interpreter
  benchmarks (calls, list sorting, strings, pattern matching, staged and
  unstaged power): `cd bench && stag-build bench`.
- The hidden `-fake-clock SPAN` replaces the clock with a fake one moving by
  `SPAN` at each read, for reproducible output in tests.

The engine is the library `stag_measure` ([measure/src](measure/src)),
independent of stag, with an injectable `Clock`.

## Library

The compiler is split into libraries, each in `X/src` (library `stag_X`, tests
in `X/test`): `common`, `types`, `syntax`, `ir`, `eval`, `typecheck` and
`driver` (whose `bin` is the `stag` executable), each depending only on those
before it, and `measure`, depending on none. They provide the pieces for other tools: `Source_file` and
`Driver.check_files` (files to a checked program; `Driver.check_modules` for
modules already parsed), `Driver.eval`, `Expect_test_runner`, `Bench_runner`
(a program's benches) and `Phase_bench`, `Diagnostic` (rendering errors),
`Text_diff`, `Printer` (syntax to source) and `Formatter` (source to formatted
source, keeping comments). `Prelude` is the prelude
([prelude/prelude.stag](prelude/prelude.stag)), whose
module `Stag` is the syntax as data, which `Syntax_value` converts to and from
`Syntax`, for declarations generated at compile time. The core library, the
stag module `Core`, is the files of [core/](core) (each a submodule,
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

## Editor support

[editors/vscode](editors/vscode) is a VS Code extension: highlighting,
brackets and indentation for `.stag` files, and a client for the language
server `stag lsp` (formatting falls back to `stag fmt` if it cannot start).
`stag.serverPath` sets the stag executable (default: `stag` on PATH).

`stag lsp` speaks the Language Server Protocol over stdin and stdout
(`lsp/`). It supports:

- Diagnostics, on open, change and save. A document is checked as `stag
  check` or `stag-build` would: in its project (the nearest `stag-project`
  above it), with the open documents' unsaved contents, or standalone
  otherwise. Errors in other files of the project are published for those
  files, and documents that depend on a changed one are checked again, as
  are all documents when files change on disk (if the client watches
  files). Checking is limited (`-max-depth`, `-time-limit`, `-fuel`; by
  default a nesting of 100000 and 5s, and `none` lifts a limit), so a
  compile-time splice cannot hang the server. Checks wait until the client
  has sent nothing for 150ms, so a burst of changes is checked once, at its
  latest version; formatting, semantic tokens and symbols don't wait for
  them.
- Formatting, by `stag fmt` (no edits if the document doesn't parse).
- Semantic tokens: keywords, comments, strings, numbers, operators, staging
  operators (as macros), and names by their syntactic role (types in type
  positions, constructors, fields, modules, variables, parameters, type
  parameters, with a declaration modifier). Values the typechecker resolved
  to functions are tokens of type function.
- Hover (the type of the name or expression under the cursor) and go to
  definition (of variables, toplevel values across files, types,
  constructors and record fields), from the typed tree of the document's
  check, which, with errors, has what did check: inside an expression that
  failed to check, its parts that did.
- Only documents of files (`file:`) and new ones (`untitled:`) are checked:
  others, eg. a diff's old version, could not be checked as in their
  project.
- The outline (`textDocument/documentSymbol`): modules, types and their
  constructors and fields, exceptions, lets, expect tests and benches.

```sh
cd editors/vscode
npm ci && npm run package    # node from nix: nix shell nixpkgs#nodejs_22 -c ...
code --install-extension stag-*.vsix
./test.sh                    # grammar tests and tsc; --integration runs VS Code
```

Its tests need npm packages (and VS Code for `--integration`), so they are not
part of `dune build @runtest`.
