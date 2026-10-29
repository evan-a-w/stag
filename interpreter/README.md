# Rust bindings

Rust versions of stag's OCaml types (`Typed`, `Type`, `Syntax`, ...),
converted once from OCaml values, and built back into OCaml values, for
Rust code linked into stag.

## Layout

- `ppx/`: `[@@deriving rust]`, which registers type declarations with
  `ppx/runtime` (`Ppx_rust_runtime`). Next to each declared `u` it defines a
  witness `rust_u`, by which other declarations refer to `u`, so OCaml
  resolves and checks every name. Every type referred to must derive `rust`
  too (and export its witness in the `.mli`). Attributes:
  - `[@@rust.shared]`: each OCaml value of the type is converted once, into
    the arena, and referred to by a `Ref` (as for types with mutable fields).
    Use it for values shared in OCaml, eg. `Type.ty`.
  - `[@rust.skip]` on a field: left out (eg. functions). Values built back
    have `()` there, so the declaration needs an OCaml function rebuilding
    them, registered with `Ppx_rust_runtime.register_rebuild rust_u f`
    (eg. `Prim.t` is looked up by name); the generator rejects declarations
    without one.
  `ppx/test/errors.t` shows what is rejected.
- `gen/`: `Rust_bindgen`, which generates Rust from the registered
  declarations; `gen/bin` prints it for stag's libraries. `gen/example`
  holds declarations exercising what stag's own types don't, which
  `gen/test` checks, and whose Rust the `stag_ast_example` crate compiles.
- `rust/`: the cargo workspace.
  - `stag_ast`: the generated types (`src/generated.rs`, promoted by dune)
    and the conversion: `convert::<T>(value)` returns the converted value and
    its `Arena`, and `to_ocaml(&x, &arena, gc_stress)` builds the OCaml
    value back. `typed::Expr` is OCaml's `Typed.Expr.t`, `type_::Ty` is
    `Type.ty`, `typed::expr::Desc` is `Typed.Expr.Desc.t`, etc.
  - `stag_rust`: the one static library linked into OCaml. Rust code called
    from OCaml (eg. an interpreter) goes here, as modules, so that std and
    the dependencies are linked once. `debug.rs` has externals for tests,
    `interp.rs` the interpreter.
  - `stag_ast_example`: the generated Rust of `gen/example`, for tests.
- `interp_value/`: `Interp_value.t`, the values the interpreter returns to
  OCaml (its own values are Rust's).
- `stag_rust/`: the OCaml library linking the static library, declaring its
  externals (`Stag_rust.Interp` for the interpreter). `stag_rust/test` converts typechecked programs and prints what
  Rust sees, and round-trips them (OCaml to Rust to OCaml), checking that
  the result is equal and shares and is cyclic alike, also under GC stress.

## Conversion

- Values of types with mutable fields and of `[@@rust.shared]` types live in
  the `Arena`, once per OCaml value (by address), and are referred to by
  `Ref<T>`s (`arena[r]`). So sharing and cycles are kept, and conversion is
  linear in the size of the OCaml value.
- No OCaml code may run during a conversion (the GC would move values):
  convert within the external, before anything allocates on the OCaml heap.
- Values not of the type converted to may crash. Where it can tell (eg. an
  unexpected tag), conversion returns an `Error`.
- Core `Map`s and `Set`s become `HashMap`s and `HashSet`s (`Map`, `Set`).
- OCaml strings are bytes: they become `Str`s, not necessarily UTF-8.

## Building OCaml values

- `to_ocaml` must be called from OCaml (eg. in an external). The value it
  returns is unrooted: return it, or root it before anything allocates.
- Each `Ref` becomes one OCaml block, so OCaml values share as Rust ones do,
  and cycles (through mutable records) are rebuilt: a stored value's block is
  allocated and recorded before its fields are built.
- Rooting: any allocation may move every young OCaml value, so Rust holds
  none across one. The builder has one global root, an OCaml array: first a
  block per store of the `Arena`, of the values built so far by `Ref`, then
  a stack of the blocks being filled. Blocks are allocated before their
  fields (initialised to `()`), pushed, and each field is written
  (`caml_modify`) as soon as it is built, so the stack is as deep as the
  value, not as wide.
- Core maps and sets are built by OCaml callbacks that `[@@deriving rust]`
  registers for each key type, with its comparator (`of_alist_exn`,
  `of_list`), so they are real maps and sets, of which Rust builds the list.
- `GcStress` (for tests), before every allocation:
  - `Minor` collects the minor heap, then fills it with garbage, so that a
    young value held unrooted across an allocation is overwritten. Set the
    minor heap small when using it.
  - `Compact` also compacts the major heap, and moves the builder's root
    blocks (its stack and stores) to new blocks, overwriting the old ones'
    fields, which catches major values held unrooted. It is slow (quadratic),
    so for small values.
  - Neither runs pending actions (signal handlers, finalisers), so no OCaml
    exception can cross Rust frames.

## Writing the interpreter

`Stag_rust.Interp` implements `Stag_eval.Interpreter_intf.S`, as
`Treewalk` does, so the typechecker and driver can use either. Its `t` holds
an OCaml custom block owning the Rust `Interp` (`rust/stag_rust/src/interp.rs`),
which lives as long as it:

- Each operation is an external that converts its OCaml arguments with
  `convert_into` into the interp's `Arena`, then calls a method of `Interp`.
  Values are shared within one conversion only, so types are read with
  `arena[expr.ty]` but are not shared between calls.
- `add_ident_info` and `add_scc` store what they convert; the evaluating
  methods are not implemented.
- Results are `Interp_value.Outcome.t`s (errors as values, so no OCaml
  exception crosses Rust frames), built with `to_ocaml` and the interp's
  arena. Values are `Interp_value.t`.
- `splice`'s and `run`'s `entry` and `reflect` stay in OCaml for now.

Rules for externals:

- Convert every OCaml argument before anything allocates on the OCaml heap
  (`to_ocaml`, `ocaml_string::of_str`, callbacks): allocating may move them.
- Return what `to_ocaml` built at once: it is not rooted.
- To fail, raise with `caml_failwith_value` only after dropping what Rust
  owns, as it does not unwind; prefer returning errors as values.
- Panics must not unwind into OCaml: `convert` and `to_ocaml` catch theirs.

`stag_rust/test/test_interp.ml` drives `Stag_rust.Interp` as the driver
does: `dune build @interpreter/stag_rust/test/runtest`, then `dune promote`
to accept changes.

## Building

`dune build` regenerates `generated.rs` and builds the static library with
`cargo build --offline --locked`. Cargo runs offline, so fetch the
dependencies once:

```sh
cd interpreter/rust && cargo fetch --locked
```

With the generated files up to date, cargo also works on its own from
`interpreter/rust` (`cargo test`, `cargo clippy`).

Rust tests are expect tests (the `expect-test` crate: `expect![[...]]`).
`dune build @runtest` runs them with `UPDATE_EXPECT` on copies of the test
files using `expect!` and diffs the results against the originals, so
`dune promote` accepts changes, as for OCaml expect tests. Those files are
listed in `rust/dune.inc`, generated by `rust/gen_dune_inc.sh`: after adding
one, the first `dune build @runtest` shows a diff of `dune.inc`, which
`dune promote` accepts. `UPDATE_EXPECT=1 cargo test` also updates them in
place.
