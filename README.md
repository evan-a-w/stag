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

The library (`lib/`) provides the pieces for other tools: `Source_file` and
`Driver.check_files` (files to a checked program), `Driver.eval`,
`Expect_test_runner`, `Diagnostic` (rendering errors) and `Text_diff`.

Tests: `dune build @runtest` (expect tests in `test/`, CLI cram tests in
`test/cram/`).
