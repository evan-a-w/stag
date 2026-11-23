# Modes

Design notes for adding modes, in the style of OxCaml. Being implemented (see
[Implementation plan](#implementation-plan)). This file covers the stage axis;
the provenance and locality axes, the solver, mode polymorphism and
monomorphisation are in [semantics.md](semantics.md#modes).

## Motivation

Before modes, a single syntactic test, "is this binding a function?"
(`not (List.is_empty args)`), decides two unrelated things:

1. **Generalization** (value restriction): `let f x = ...` is generalized,
   `let y = id id` is not.
2. **Crossing levels**: generalized functions were persistent, usable at any
   later level. Anything else became a free variable of the enclosing quote.

Modes make both of these precise and separate, and also let toplevels be
required to be constant (no arbitrary code run to initialize them) without
dedicated syntax.

## The mode axis

A single ordered axis:

```
const  <  portable  <  nonportable
```

- **const**: statically known, computed without running code. Required for
  toplevels (when enabled, see below) and for values used by compile-time code.
  Also the condition for generalization: nothing was computed, so nothing
  holds a fresh reference. This replaces the syntactic value restriction.
- **portable**: does not depend on the current stage's runtime, so may cross
  levels (the analogue of OxCaml's `portable`, crossing stages rather than
  domains). Replaces `persistent`.
- **nonportable**: everything else, e.g. function arguments by default.

`const` implies `portable`: what is known at compile time exists at every
stage. This is the stage axis of [semantics.md](semantics.md), alongside
provenance and locality; `const` also implies `static` provenance there.

This is about what values *capture*, not about effects. Effect purity (calling
a function performs no side effects) would be a separate property of arrows,
needing a small effect system; deferred until the language has effects.

Mode crossing by kind, as in OxCaml: plain data like `int` can always be
lifted into code, so crosses `portable` regardless of mode, but never `const`.
Applied when the type is known where the constraint arises.

## Expression modes

| expression                 | mode                                        |
|----------------------------|---------------------------------------------|
| literal                    | const                                       |
| variable                   | the binding's mode                          |
| function (`let f x = ...`) | at least the join of the modes it captures |
| application `f x`          | nonportable (the arrow's return mode, from D) |
| `e.*`, assignment          | nonportable                                 |
| match                      | join of the scrutinee and the arms; its pattern variables have the scrutinee's mode (exception cases: nonportable) |
| splice in a quote          | nonportable (the code's mode is not known)  |
| toplevel splice            | the mode of its expansion: `.~(.<1>.)` is const |
| value computed at compile time, embedded in generated code | portable |
| `let x = e1 in e2`         | join of `e1` and `e2`                       |
| quote `<e>`                | join of the values it captures from outer levels (its free variables are resolved at the splice, so don't count) |

Application is never `const` as arrow return modes are never `const` (for
now; arrows carry argument and return modes, `'a @ 'm -> 'b @ 'n`). A
Rust-style `const fn` could later be an arrow whose result is `const` when its
arguments are.

## Locks

`Scope` keeps a stack of locks, and each entry the locks enclosing its
binding. A use crosses the locks between the two (Fitch-style, as OxCaml's
`Env` does). A splice in a quote pushes a splice lock, which cancels its quote
and the locks between them: code built in the splice is used where the splice
is, so a variable bound inside the quote crosses the closures around the
splice, while one bound outside the quote crosses none of them.

- **Closure lock** (entering a function body): the captured variable's mode
  must be at most the closure's mode.
- **Quote lock**: the variable must be `portable`, and the code is at least
  as nonportable as it. Otherwise it becomes a free variable of the quote.
  Whether it is `portable` is decided by its lower bound where it is used,
  then required: a later rise is an error. A variable used in its own
  definition (a function in its body, a toplevel being checked) is not
  decided on, so is always a free variable there, and computing with it is
  nonportable. Toplevels are referred to by symbol, so cross closure locks,
  and mutable toplevels quote locks, freely.
- **Toplevel splice lock** (splice outside any quote, i.e. code run at compile
  time): the variable must be `const`, since only `const` values exist at
  compile time. Not implemented yet: compile-time code may use any checked
  toplevel but a mutable one (the "runtime-only toplevel" check).

A splice of code whose context is not known yet may use any variable visible
there, so is taken to use them all, crossing their locks.

## Toplevels

A compiler flag, e.g. `Typecheck.create ~const_toplevels:bool`:

- **On (compiler)**: every toplevel's initializer must be `const`.
- **Off (interpreter, the default)**: any toplevel is allowed.
  Generalization is still decided by mode: functions always, other values when
  `const` (then required), and a recursive group only if all its members are
  functions. Toplevels are order independent, so initialization needs an
  order: cycles through non-functions (e.g. `let x = f 1` and `let f y = x`)
  are reported at check time (see syntax.md, Evaluation).

## Toplevel splices

A splice outside any quote runs at compile time, so may only use `const`
values (the toplevel splice lock).

For `let cube = ~(power 3 <2>)` to be `const` in compiler mode, the *generated*
code must be `const` too. That needs code types to record the mode of the
expression they contain, `<t @ m>{ctx}`: `<1>` is `<int @ const>`,
`<add 1 2>` is not, and splicing yields the code's mode. Deferred: until then a
toplevel splice has the mode of its expansion, checked after it is generated.

## Behaviour changes

- Toplevel constants like `n` resolve directly inside quotes (they are
  `const`), instead of becoming free variables filled in at the splice.
- Local closures capturing arguments, mutable locals or computed values no
  longer cross levels: they are free variables of the quote. (Ints too, until
  mode crossing by kind.)
- Constant data is generalized, local or not; values computed by applying
  or splicing are not.

## Mutable variables

A `let mut` variable has two modes: the variable's own (its identity, shared
by the code that uses it) and that of the value it holds.

- **The variable** is always `nonportable`, never `const` (`const` values are
  deeply immutable, and lifting copies them). So it crosses neither a quote
  lock nor a toplevel splice lock, and capturing it makes a closure
  `nonportable`. That join propagates through closures and data, and with
  arguments `nonportable` by default (and mode crossing by kind for plain
  data), it replaces the hand-written check that decided persistence (a closure
  that may reach a mutable local, through what it captures or parameters
  whose type may hold a function, was not persistent). Not `const`, it is
  never generalized either.
- **The value held** has a mode variable `m` fixed at the binding, as
  OxCaml's `let mutable`: `x <- e` requires `mode e ≤ m`, and reading `x` is
  at `m`. Locality: a value local to an inner region (each function body,
  closures included, is one) must not be stored in a variable of an outer
  one, so a variable captured by a closure holds `global` values, unless the
  closure is `local`. An uncaptured variable may hold values local to its own
  region.
- **Representation** (see semantics.md): uncaptured, a stack slot; captured
  only by `local` closures, on the data stack; captured by a `gc` closure, a
  heap box, whose stores have a write barrier unless the value is `static`.

Mutable globals are `static`, referred to by symbol (so code may assign them:
`.< counter <- 1 >.` captures nothing), and hold `global` values (a global
is a GC root). They are not `const`, so compile-time code can't use them: the
toplevel splice lock, applied to the toplevels a splice reaches, replaces the
hand-written "runtime-only toplevel" check. Under `const_toplevels`, their
initializer must be `const` (static data), not the global.

Code that assigns a free variable has it as a `mut` entry of its context
(`<unit>{x : mut int}`), provided at the splice only by a mutable variable of
the splice's level. Modes don't change this: the entry is a variable of the
level the code is spliced at, never one crossing a lock, so the provider's
value mode is checked against `m` as for any assignment there.

Later: `&x` of a mutable variable, and writes through pointers, need a
writable/read-only distinction on pointers; and with mutation, effect purity
(eg. a `const fn` may not assign a global) starts to matter.

## Implementation plan

Each step is a branch stacked on the previous one.

- **A. Syntax**: mode annotations (`(x @ local)`, `'a @ 'm -> 'b @ 'n`),
  `local_`, `exclave_`, `global_` fields, the non-tail call marker. Parsed
  and printed, ignored by the typechecker.
- **B. Solver**: the `Mode` library (`mode/`): the three axes, mode
  variables with bounds, edges with morphisms (`const ⇒ static`, crossing),
  reasons and errors, atomic updates, generalisation and instantiation,
  defaulting. Standalone, expect tests.
- **C. Stage axis in the typechecker**: `Env.Entry` gets a `mode` in place of
  `persistent`, `expr_type` returns a type and a mode; closure and quote
  locks (the toplevel splice lock is left for later); generalisation decided by `const`; the
  `const_toplevels` flag. Removes `may_hold_closure` and `is_allowed_at`.
- **D. Arrow modes and polymorphism**: arrows carry argument, return and
  closure modes; schemes keep mode variables and their constraints; mode
  annotations; printing that hides unconstrained and default modes.
- **E. Locality**: regions, `local_`, `exclave_`, the tail call rule,
  `global_` fields, `Unsafe.assume_global`, mutable variables captured by
  closures holding `global` values, mode crossing by kind.
- **F. Provenance**: allocation is `gc`, literals and `const` values
  `static`, stores, mutable globals.
- **G. `External`**: C functions with a C compatible ABI (rejecting
  signatures it can't express), producing `unknown` values.

## Later

- `<t @ m>{ctx}` code modes.
- Function pointer arrows (`=>`).
- A dynamic mode checker in the interpreter, as a test oracle.
- Presence ordering constraints (`Absent <= Present`) are kept as bounds
  between presence variables, so are solved independently of the order they
  arise in, as are the type equalities of entries the code may use, which
  wait for the entry to be `Present`. They move into the mode solver, as a
  two point axis whose equalities are triggers (see
  [semantics.md](semantics.md#solver)).
- Effect purity, `const fn`, mode crossing by kind.
