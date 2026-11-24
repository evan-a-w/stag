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

`const` means the same value on every evaluation. So a function's parameter
is at least `portable` in its body, whatever its callers pass: it may differ
between calls, even if each is `const`. What is built from it (a closure
capturing it, code embedding it) is then never `const`, so never `static`,
while what the body builds from constants alone stays `const`.
A function's result is likewise never `const` (calling it runs code), whether
written on an annotation's arrow or as a definition's result (`let g x : t @ const`
is an error). What is read from a block, through a field's modality or a cell,
is met with the block's mode, so it is `const` only if the block is: a
parameter's `@@ const` field isn't. `const` implies `static`: written with `gc`
or `external`, in either order, it is an error.

`const` implies `portable`: what is known at compile time exists at every
stage. This is the stage axis of [semantics.md](semantics.md), alongside
provenance and locality; `const` also implies `static` provenance there.

This is about what values *capture*, not about effects. Effect purity (calling
a function performs no side effects) would be a separate property of arrows,
needing a small effect system; deferred until the language has effects.

Mode crossing by layout: a value of a pointer-free by-value type (`int`,
`bool`, `char`, `unit`, and records, tuples and variants of pointer-free
types) holds nothing a mode could restrict. It crosses locality and
provenance entirely, and stage up to `portable` (it can always be lifted into
code, but `const` stays a promise about how it was computed). Applied when the
type is known where the constraint arises; a function's parameter type is
unified with what the context expects before its body is checked, so whether
a use crosses does not depend on the order things are checked in. Records,
tuples and variants are values, not allocations: what is local in a local
`(p, 1)` is `p`.

## Expression modes

| expression                 | mode                                        |
|----------------------------|---------------------------------------------|
| literal                    | const                                       |
| variable                   | the binding's mode                          |
| function (`let f x = ...`) | at least the join of the modes it captures |
| application `f x`          | the arrow's result mode, at least portable |
| `e.*`, assignment          | nonportable                                 |
| match                      | join of the scrutinee and the arms; its pattern variables have the scrutinee's mode (exception cases: nonportable) |
| splice in a quote          | nonportable (the code's mode is not known)  |
| toplevel splice            | the mode of its expansion: `.~(.<1>.)` is const |
| value computed at compile time, embedded in generated code | portable |
| `let x = e1 in e2`         | join of `e1` and `e2`                       |
| quote `<e>`                | join of the values it captures from outer levels (its free variables are resolved at the splice, so don't count) |

See [Arrow modes](#arrow-modes).

## Arrow modes

Arrows carry the modes of their argument and result, `'a @ 'm -> 'b @ 'n`; a
closure's own mode is the mode of the arrow value, like any value's.

- **Functions.** Each parameter has the arrow's argument mode, joined with
  `portable` (see [The mode axis](#the-mode-axis)), and the body's value is at
  most its result mode. The closure of a partial application holds
  the closure and the arguments given so far.
- **Application.** The argument is at most the arrow's argument mode, and the
  result has the arrow's result mode, joined with `portable`: calling runs
  code, so an application is never `const` (a Rust-style `const fn` could
  later be an arrow whose result is `const` when its arguments are).
- **Unification** equates the modes of two arrows: there is no subtyping
  (variance) of arrow modes for now, a known limitation.
- **Polymorphism.** Mode variables are generalised with the type variables of
  a scheme, with the constraints between them, and copied per instance, so
  `let apply f x = f x` works for portable and nonportable `f` alike.
- **Annotations.** A side written without a mode is the default,
  `nonportable gc global`, in an annotation as in a declaration (and for
  builtins), as OxCaml's legacy modes: `(f : int -> int)` makes `f`'s arrow
  the default's, and `'m` keeps a side polymorphic (one variable across a
  definition's annotations; in a declaration a mode variable is an error).
  Axes not written are the default's, so `int @ local -> int` is
  `int @ nonportable gc local -> int @ nonportable gc global`. An arrow's
  `curry`, never written, is free in an annotation. As in OxCaml, the result
  of a curried arrow written without a mode is as local as the arguments
  before it (`ptr[int] @ local -> int -> int` is
  `ptr[int] @ local -> (int -> int) @ local`), as it is the closure of a
  partial application holding them. In a declaration its
  `curry` is fixed `regional`, as a declaration can't tell a partial application from a function value
  returned, so a tail call through it given a `local` argument must be marked
  `[@nontail]`.
- **Decisions made while checking a function.** Whether a variable is
  embedded in a quote, and whether a `let` is `const` enough to generalise,
  can't wait for the function's uses. They take the lower bound the mode would
  have with every mode chosen by those uses raised to its upper bound: the
  modes of the parameters of the functions being checked, the modes within
  their types (eg. the result of a function parameter), and those of the types
  of toplevels not finished yet. So `let f x = .<x>.` makes `x` a free
  variable of the quote, whatever `f` is called with, and `(x @ portable)`
  opts into embedding it, restricting callers to portable arguments.

### Printing

By default, types show only fixed modes other than the default, so most
signatures print as without modes. With modes shown
(`Signature.to_string_hum ~modes:true`), mode variables are named `'m0`,
`'m1`, ... and their constraints follow the type:

```
val apply : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> ('a @ 'm4 -> 'b @ 'm5) @ 'm3
  with 'm0 <= 'm3 and portable <= 'm1 and 'm2 <= 'm5
  and release(locality('m2)) <= locality('m5) and 'm2 <= regional
  and stage('m4) <= stage('m1) and provenance('m4) <= provenance('m1)
  and meet(locality('m4), regional) <= locality('m1) and gc <= 'm3
```

The `with` clause is also syntax for annotations (see syntax.md), and the
printed constraints are exact: read back, they imply just what the
variables' constraints do, no more and no less (up to the floor every
annotation gives a result: `portable`). So printed types can be trusted on
their own, by interfaces and separately checked code.

- variables equal on every axis by their constraints print as one;
- a variable in no relation and fixed on every axis prints as its atoms
  other than the default (`string @ external`), or not at all if it is the
  default, as an unwritten mode reads back as the default; any other is
  named, even if unconstrained (`'a @ 'm0 -> unit @ 'm1`);
- constraints and bounds the others imply, as read back, are left out;
- only the item's own variables are named: constraints through other
  variables (of other members of a recursive group, or a weak variable's
  current neighbours) are projected onto its own;
- a weak (not generalized) variable is named `'_m0`, as weak type variables,
  and shown with its current constraints;
- a variable only on `ptr` cells and mode arguments prints no locality, which
  no position reads; relations through it still print.

A mutable global whose reads are `external` shows it
(`val g : string @ external`), parenthesizing an arrow or tuple type
(`val g : (int -> int) @ external`).

### Printed constraints

The solver's constraints between two variables' axes are each
`a <= m(b)`, `m` a monotone map from `b`'s axis to `a`'s with a left
adjoint `m*` (`m*(x) <= y` iff `x <= m(y)`). Composing the solver's maps
gives only these, up to ones relating nothing (`m*` always the least point),
each written in one form:

| Written | Map | `m*` (least to greatest point) | Where it arises |
| --- | --- | --- | --- |
| `'a <= 'b` | identity, every axis | | subsumption |
| `stage('a) <= stage('b)` (and `locality`, `provenance`) | identity, one axis | | a relation holding on only some axes |
| `meet(stage('a), portable) <= stage('b)` | `meet_with portable` | const, portable, portable | crossing: a value whose type crosses to `portable` |
| `meet(provenance('a), gc) <= provenance('b)` | `meet_with gc` | static, gc, gc | a field `@@ gc` read from a record at `'a` |
| `meet(locality('a), regional) <= locality('b)` | `borrow` | global, regional, regional | a value seen from a nested region: a parameter, a capture |
| `release(locality('a)) <= locality('b)` | `release` | global, local, local | a regional value seen from the enclosing region: a result, `exclave_` |
| `provenance('a) <= static_if_const(stage('b))` | `const_is_static` (stage to provenance: const ↦ static, else external) | static ↦ const, else portable | a `const` value is `static` |
| `stage('a) <= const_if_static(provenance('b))` | `runtime_is_gc` (provenance to stage: static ↦ const, else nonportable) | const ↦ static, else gc | a value allocated at run time is `gc` |

`meet` and `release` apply `m*` on the left of `<=`; `static_if_const` and
`const_if_static` apply `m` itself on the right, naming the map rather than
its adjoint. `meet(t, c)` takes any atom `c` of `t`'s axis (meeting with the
greatest is the identity, with the least relates nothing); the printer only
uses the middle one. Functions nest: `meet(release(locality('m)), regional)`
is `borrow` after `release`, which is `borrow` again, as the printer would
write it; a constraint's left functions compose with its right ones, so
`f(a) <= g(b)` is `a <= (f ∘ g)(b)` with `f`'s map.

Each variable's own `provenance <= static_if_const(stage)` (`const` is
`static`) is never printed, and neither is a result's `portable` lower bound,
which every annotation's result has.

The printer finds the exact set as follows (`types/src/mode_closure.ml`,
`mode_model.ml`, `mode_printing.ml`):

1. For each counted variable's axis `x` and each point `p` of `x` within its
   bounds, it asks the solver how far raising `x` to `p` raises every other
   axis `y` (following the constraints that hold, a generic variable's being
   those its scheme keeps). This is what the constraints imply between them:
   `y >= g(x)` for a monotone `g`, including `y = x` (`g` may be above the
   identity, as `release` is on locality).
2. Variables equal on every axis (each at least the other, with the same
   bounds) are grouped as one.
3. The written forms per pair of axes form a chain (nothing < `meet` <
   identity < `release`), so for each pair the greatest written `w` with
   `w* <= g`, raising `y` above its lower bound somewhere, is the candidate:
   with every variable's bounds, they imply exactly what the solver does.
4. Candidates are dropped, latest first, when the rest still imply them: an
   edge `x -> y` with map `w` is redundant iff propagating from `x` at each
   point of its domain without it still raises `y` to `w*(p)`. Pairs related
   on every axis (by the plain map) print as `'a <= 'b` and are reduced in the
   same pass, then bounds. The final set is checked against the solver (both
   bounds and every `g`): a printer that would print something inexact fails
   with an internal error instead.

A mode argument (`ptr[t @@ 'm]`, `cell[t, 'm]`) relates only stage and
provenance, and a locality atom in one is an error. Its variable is a mode
variable like any other: a `with` clause may bound or relate its locality, and
a plain `'m <= 'n` relates every axis. But if it is only in mode arguments, no
position reads its locality, so the printer eliminates that node: the solver's
closure composes the relations through it, which print between the nodes that
are read. A plain relation still relates the eliminated node when read back,
so the reduction keeps it as a hidden node, and a pair is printed plainly only
where the localities it relates through it are related (else per axis). Such
a variable prints as atoms only if fixed on both its axes (`ptr['a @@ const static]`), as a mode
argument's unwritten axes read back as the default; one also on an arrow, and
free there on locality, keeps its name in both places.

An arrow's side fixed to a constant (by a declaration, as a record field's
`string -> string`) prints only its atoms other than the default, as
annotations and declarations alike read unwritten axes as the default.

`release(locality('a)) <= locality('b)` is not a default between a
parameter and its result: most curried and higher-order functions don't have
it (the outer arrow of `first x y = x` only has `'m0 <= 'm1`, and `apply f x =
f x` relates `x` to nothing), so it would need an opt-out wherever they're
printed.

So, for example, reading a field `x : string @@ gc`:

```
type r2 = { x : string @@ gc; y : string }
let fx (r : r2) = r.x
val fx : r2 @ 'm0 -> string @ 'm1 with stage('m0) <= stage('m1)
  and meet(provenance('m0), gc) <= provenance('m1)
  and release(locality('m0)) <= locality('m1)
```

The result is `'m0` met with `gc` on provenance, and a regional record gives
a local result (`release`): it was regional in `fx`'s region, which ends.

### Annotations on uses

`(e @ m)` (or `(e : t @ m)`, on any expression) is the check-only form: it
bounds the mode of `e`, so of a variable `x` at this use, and `x` keeps its own
mode. A binding's
annotation, `let x @ m = e`, is instead the variable's mode (as in OxCaml): `e`
must be at most `m`, and uses of `x` see `m` on the axes it names (`e`'s mode
on the others), though generalisation is still decided by `e`'s own mode.
Parameters `(x @ m)` and `let mut x @ m` are likewise exact. So
`let n @ portable = 1` gives a `portable` `n`, while `let n = (one @ portable)`
leaves `n` as `const` as `one`.

For example, `let cap x = (x @ portable)` restricts `cap`'s argument to portable
values, and with `@ 'm` the use is `'m`, shared with the definition's other
annotations (`(x : t @ 'm with 'm <= portable)`).

Known limitation: annotation variable names are scoped per toplevel and per
toplevel splice's expansion, not per piece of generated code. Code values
spliced together into one expansion share them, so two pieces each naming
`'m` (or `'a`) constrain one variable.

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
  then required: a later rise is an error. A (non-mutable) local of a type
  that crosses stage (plain data, as `int`) is `portable` whatever its
  stage, so is used directly. A variable used in its own
  definition (a local function in its body, a toplevel being checked) is not
  decided on, so is always a free variable of a quote there. A local function
  in its own body is nonportable to compute with, as is a local closure
  capturing it (other than itself); so is a toplevel non-function being
  checked. A toplevel function captures nothing, so its mode is known while
  it is checked. Toplevels are referred to by symbol, so are not captured by
  closures, and mutable toplevels cross quote locks freely.
- **Toplevel splice lock** (splice outside any quote, i.e. code run at compile
  time): the variable must be `const`, since only `const` values exist at
  compile time. Not implemented yet: compile-time code may use any checked
  toplevel but a mutable one (the "runtime-only toplevel" check).

A splice of code whose context is not known yet may use any variable visible
there, so is taken to use them all, crossing their locks. It may also use any
free variable of the enclosing quote, so the closures around it are
nonportable, as is a closure around a splice providing such a variable.

This is a known loss of precision: every code value has an open context
(`.<1>.` included), so a closure in a quote containing a splice is never
`const`. In `.< fun a -> let p = fun u -> .~(.<1>.) in (p 1, p true) >.`, `p`
is not generalized. It goes once code types carry their mode (`<t @ m>`, see
Toplevel splices). Annotating the spliced code with a closed context
(`.~(c : <int>{})`) avoids it.

## Toplevels

A compiler flag, e.g. `Typecheck.create ~const_toplevels:bool`:

- **On (compiler)**: every toplevel's initializer must be `const`.
- **Off (interpreter, the default)**: any toplevel is allowed.
  Generalization is still decided by mode: functions always, other values when
  `const` (then required), and a recursive group only if all its members are
  functions. Toplevels are order independent, so initialization needs an
  order: cycles through non-functions (e.g. `let x = f 1` and `let f y = x`)
  are reported at check time (see syntax.md, Evaluation).

## Toplevels in quotes

A toplevel used in a quote is checked first, to know its mode: `const` ones
are used directly, others are free variables of the quote. One whose checking
is underway (a recursive reference) is a free variable, and computing with it
elsewhere is nonportable: `let z = r` in a toplevel `r` depends on is not
generalized, whichever order they are checked in. So the order of
checking matters: with

```ocaml
let q = .<later>.
let later = .~(let z = q in .<1>.)
```

checking `q` checks `later`, whose splice needs `q`, which isn't checked yet:
an error. With the two swapped, `later` is checked first, so `q` is checked
during its splice, and `later` is a free variable of `q`'s quote.

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
  longer cross levels: they are free variables of the quote, unless their type
  is pointer-free.
- Constant data is generalized, local or not; values computed by applying
  or splicing are not.
- An arrow written in an annotation without modes has the default modes
  (`nonportable gc global`), as in a declaration, rather than fresh
  variables: `(f : int -> int)` makes `f` take and return global values.
  Write `'m` to keep a side polymorphic.

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
  at `m`. Its initializer is checked likewise, and `let mut x @ m`
  (or a mutable toplevel's `@ m`) bounds `m`. Reading is `nonportable` for
  now, as the variable is. Locality: a value local to an inner region (each function body,
  closures included, is one) must not be stored in a variable of an outer
  one. Assigning a variable from inside a closure (which may be called from a
  shorter region) needs a `global` value; reading it there sees what is
  `local` to the variable's region as `regional`, so a closure that reads a
  variable holding local values is itself local. An uncaptured variable may
  hold values local to its own region.
- **Representation** (see semantics.md): uncaptured, a stack slot; captured
  only by `local` closures, on the data stack; captured by a `gc` closure, a
  heap box, whose stores have a write barrier unless the value is `static`.

Mutable globals are referred to by their symbol, `static` whatever they hold
(so code may assign them:
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
  `stack_`, `exclave_`, `global_` fields (now `@@ global`), the non-tail call marker. Parsed
  and printed, ignored by the typechecker.
- **B. Solver**: the `Mode` library (`mode/`): the three axes, mode
  variables with bounds, edges with morphisms (`const ⇒ static`, crossing),
  reasons and errors, atomic updates, generalisation and instantiation,
  defaulting. Standalone, expect tests.
- **C. Stage axis in the typechecker**: `Env.Entry` gets a `mode` in place of
  `persistent`, `expr_type` returns a type and a mode; closure and quote
  locks (the toplevel splice lock is left for later); generalisation decided
  by `const`; the `const_toplevels` flag. Removes `may_hold_closure` and `is_allowed_at`.
- **D. Arrow modes and polymorphism**: arrows carry argument and return
  modes (a closure's mode is the mode of the arrow value); schemes keep mode variables and their constraints; mode
  annotations; printing that hides unconstrained and default modes.
- **E. Locality**: regions, inferred allocation locality, `stack_`,
  `exclave_`, the tail call rule, `global_` fields (now `@@ global`), `Unsafe.assume_global`,
  mutable variables assigned from closures holding `global` values, mode
  crossing by kind on locality, exact binding annotations.
- **F. Provenance**: `unknown` renamed `external`; deep provenance through
  blocks, pointers and closures, not through what is only evaluated; what is
  allocated at run time is `gc` unless `const`, literals and toplevel
  constants `static`; declared arrows default to `gc`; stores through
  pointers take `gc` values (through `gc` pointers, until H), mutable variables infer their held provenance;
  provenance annotations and bounds in `with`; the Go-style GC design.
- **G. `external`**: C functions at ordinary curried types, their modes
  trusted as written (unwritten ones a declaration's defaults, so they may
  produce `external` values only where their types say so; the external's own
  mode is unconstrained: a code pointer, `static` and `const` like any
  toplevel function), called through libffi by the interpreter. Only
  immediates (`int`, `bool`, `char`, `unit`) for now: other signatures,
  pointers included, are rejected.
- **H. Pointee modality**: `ptr[t @@ m]`, `m` a provenance or a mode
  variable in the type, unified exactly and generalized like arrow modes:
  stores through it take at most `m`, reads are at least `m`. `ptr[t]` is
  `@@ gc`. `Ptr.make` infers it; weak toplevel cells are fixed at the end of
  their definition (`gc` by default). Stores needn't check the pointer
  stored through (every `ptr` is to stag's memory; C's will be `cptr[t]`).
  In `with`, `'m <= 'n` is on every axis and `provenance('m) <= provenance('n)`
  on one (likewise `stage`, `locality`).
  Then one modality syntax, `t @@ m` on record fields, constructor
  arguments and `ptr`, on every axis (`global_` fields are gone): meet on
  values (a field read from a block at `b` is at most `b ⊓ m`), exact
  through storage (a cell's reads are `m`, and no longer join the
  pointer's own provenance), composing along `p.*.f`. `ptr`'s covers stage
  and provenance; its locality is always `global`. Types take mode
  parameters, `type cell['a, 'p : mode]`, applied as `cell[string, external]`
  (the declaration says which arguments are modes); invariant for now. Mode
  parameters (and so `ptr`'s cells) are on stage and provenance only, and a
  modality's locality is only `global`. `ptr` is declared
  `ptr['a, 'p : mode]`, with `ptr[t @@ m]` as sugar. A cell's reads are at
  least `portable` (never `const`). Record updates keep the mode parameters
  of the fields they copy.

## Later

- `<t @ m>{ctx}` code modes.
- `cptr[t]`: pointers into C's memory, which the GC doesn't scan, holding
  only C-compatible data (pointer-free values and `cptr`s), for `external`
  signatures.
- Pointers in `external` signatures, and callbacks (`@ static` closures
  passed to C).
- A dynamic mode checker in the interpreter, as a test oracle.
- Presence ordering constraints (`Absent <= Present`) are kept as bounds
  between presence variables, so are solved independently of the order they
  arise in, as are the type equalities of entries the code may use, which
  wait for the entry to be `Present`. They move into the mode solver, as a
  two point axis whose equalities are triggers (see
  [semantics.md](semantics.md#solver)).
- Effect purity, `const fn`.
