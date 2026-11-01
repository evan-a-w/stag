# Modes

Design notes for adding modes, in the style of OxCaml. Not implemented yet.

## Motivation

Today a single syntactic test, "is this binding a function?"
(`not (List.is_empty args)`), decides two unrelated things:

1. **Generalization** (value restriction): `let f x = ...` is generalized,
   `let y = id id` is not.
2. **Crossing levels**: `Env.Entry.persistent` lets generalized functions be
   used at any later level. Anything else becomes a free variable of the
   enclosing quote.

Modes make both of these precise and separate, and also let toplevels be
required to be constant (no arbitrary code run to initialize them) without
dedicated syntax.

## How level crossing works today

For `let n = 3` at toplevel:

- `finish_scc` marks the SCC persistent only if every member has arguments, so
  `n` gets `{ level = 0; quote_id = None; persistent = false }` and is not
  generalized.
- In `<n + 1>` (level 1), `var_type` finds the toplevel entry, but
  `Env.Entry.is_allowed_at` fails (non-persistent entries are only usable at
  their own level, in their own quote). Since `entry.level <= scope.level`,
  `n` becomes a **free variable of the quote** with a fresh type unrelated to
  `int`: `not_persistent : <'_a>{n : '_a; ..'_b}`. The splice site provides
  `n` later.
- Toplevel functions such as `add` are persistent, so `<add 1 2>` refers to
  the toplevel directly. Local generalized functions are persistent too, even
  when they capture level-0 arguments:

  ```ocaml
  let f n =
    let g x = x + n in  (* g captures a level-0 runtime value *)
    < g 1 >             (* but is allowed to cross into level 1 *)
  ```

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

Later refinement: mode crossing by kind, as in OxCaml. Plain data like `int`
can always be lifted into code, so could cross `portable` regardless of mode,
but never `const`.

## Expression modes

| expression                 | mode                                        |
|----------------------------|---------------------------------------------|
| literal                    | const                                       |
| variable                   | the binding's mode                          |
| function (`let f x = ...`) | at least the join of the modes it captures |
| application `f x`          | nonportable (for now)                       |
| `let x = e1 in e2`         | join of `e1` and `e2`                       |
| quote `<e>`                | join of the values it captures from outer levels (its free variables are resolved at the splice, so don't count) |

Application is never `const` as arrow return modes are never `const`. A
Rust-style `const fn` could later be an arrow whose result is `const` when its
arguments are.

## Locks

`Scope` keeps a stack of locks; each entry records the lock depth it was bound
at. Looking up a variable walks the locks it crosses (Fitch-style, as OxCaml's
`Env` does):

- **Closure lock** (entering a function body): the captured variable's mode
  must be at most the closure's mode.
- **Quote lock**: the variable must be `portable`. Otherwise it becomes a free
  variable of the quote, as today.
- **Toplevel splice lock** (splice outside any quote, i.e. code run at compile
  time): the variable must be `const`, since only `const` values exist at
  compile time.

This replaces `level` / `quote_id` / `persistent` checks in
`Env.Entry.is_allowed_at`.

## Toplevels

A compiler flag, e.g. `Typecheck.create ~const_toplevels:bool`:

- **On (compiler)**: every toplevel must be `const`.
- **Off (interpreter)**: any toplevel is allowed. Generalization is still
  decided by mode. Toplevels are order independent, so initialization needs an
  order: cycles through non-functions (e.g. `let x = f 1` and `let f y = x`)
  are reported at check time (see syntax.md, Evaluation).

## Toplevel splices

A splice outside any quote runs at compile time, so may only use `const`
values (the toplevel splice lock).

For `let cube = ~(power 3 <2>)` to be `const` in compiler mode, the *generated*
code must be `const` too. That needs code types to record the mode of the
expression they contain, `<t @ m>{ctx}`: `<1>` is `<int @ const>`,
`<add 1 2>` is not, and splicing yields the code's mode. Deferred: until then a
toplevel splice is not `const`.

## Behaviour changes

- Toplevel constants like `n` resolve directly inside quotes (they are
  `const`), instead of becoming free variables filled in at the splice.
- Local closures capturing arguments no longer cross levels.

## Mutable variables

A `let mut` variable has two modes: the variable's own (its identity, shared
by the code that uses it) and that of the value it holds.

- **The variable** is always `nonportable`, never `const` (`const` values are
  deeply immutable, and lifting copies them). So it crosses neither a quote
  lock nor a toplevel splice lock, and capturing it makes a closure
  `nonportable`. That join propagates through closures and data, and with
  arguments `nonportable` by default (and mode crossing by kind for plain
  data), it replaces the hand-written check that decides persistence today
  (a closure that may reach a mutable local, through what it captures or
  parameters whose type may hold a function, is not persistent; see
  syntax.md). Not `const`, it is never generalized either.
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

1. `Mode` module: the lattice, and mode variables with lower and upper bounds
   (`submode`, errors). Standalone, with expect tests.
2. `Env.Entry` gets a `mode` in place of `persistent`; `expr_type` returns a
   type and a mode.
3. Closure, quote and toplevel splice locks.
4. Generalization decided by `const`; the `const_toplevels` flag.
5. Function arguments default to `nonportable`; mode annotations on arrows
   later. No mode polymorphism initially: leftover mode variables are
   defaulted at generalization.

## Later

- `<t @ m>{ctx}` code modes.
- Mode annotations and mode polymorphism.
- Presence ordering constraints (`Absent <= Present`) are kept as bounds
  between presence variables, so are solved independently of the order they
  arise in, as are the type equalities of entries the code may use, which
  wait for the entry to be `Present`. They move into the mode solver, as a
  two point axis whose equalities are triggers (see
  [semantics.md](semantics.md#solver)).
- Effect purity, `const fn`, mode crossing by kind.
