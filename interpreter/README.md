# Rust bindings

Rust versions of stag's OCaml types (`Typed`, `Type`, `Syntax`, ...),
converted once from OCaml values, for Rust code linked into stag.

## Layout

- `ppx/`: `[@@deriving rust]`, which registers type declarations with
  `ppx/runtime` (`Ppx_rust_runtime`). Next to each declared `u` it defines a
  witness `rust_u`, by which other declarations refer to `u`, so OCaml
  resolves and checks every name. Every type referred to must derive `rust`
  too (and export its witness in the `.mli`). Attributes:
  - `[@@rust.shared]`: each OCaml value of the type is converted once, into
    the arena, and referred to by a `Ref` (as for types with mutable fields).
    Use it for values shared in OCaml, eg. `Type.ty`.
  - `[@rust.skip]` on a field: left out (eg. functions).
  `ppx/test/errors.t` shows what is rejected.
- `gen/`: `Rust_bindgen`, which generates Rust from the registered
  declarations; `gen/bin` prints it for stag's libraries. `gen/example`
  holds declarations exercising what stag's own types don't, which
  `gen/test` checks, and whose Rust the `stag_ast_example` crate compiles.
- `rust/`: the cargo workspace.
  - `stag_ast`: the generated types (`src/generated.rs`, promoted by dune)
    and the conversion: `convert::<T>(value)` returns the converted value and
    its `Arena`. `typed::Expr` is OCaml's `Typed.Expr.t`, `type_::Ty` is
    `Type.ty`, `typed::expr::Desc` is `Typed.Expr.Desc.t`, etc.
  - `stag_rust`: the one static library linked into OCaml. Rust code called
    from OCaml (eg. an interpreter) goes here, as modules, so that std and
    the dependencies are linked once. `debug.rs` has externals for tests.
  - `stag_ast_example`: the generated Rust of `gen/example`, for tests.
- `stag_rust/`: the OCaml library linking the static library, declaring its
  externals. `stag_rust/test` converts typechecked programs and prints what
  Rust sees.

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

## Building

`dune build` regenerates `generated.rs` and builds the static library with
`cargo build --offline --locked`. `dune build @runtest` also runs
`cargo test`. Cargo runs offline, so fetch the dependencies once:

```sh
cd interpreter/rust && cargo fetch --locked
```

With the generated files up to date, cargo also works on its own from
`interpreter/rust` (`cargo test`, `cargo clippy`).
