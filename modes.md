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
closure's own mode is the mode of the arrow value, like any value's. Only the
stage axis is constrained so far.

- **Functions.** Each parameter has the arrow's argument mode, and the body's
  value is at most its result mode. The closure of a partial application holds
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
- **Annotations.** In an annotation, a side written without a mode is a fresh
  variable, so annotating restricts no mode; `'m` names one variable across a
  definition's annotations. In a declaration (and for builtins), it is the
  default, `nonportable external global`, and a mode variable is an error. As
  in OxCaml, the result of a curried arrow written without a mode is as local
  as the arguments before it (`ptr[int] @ local -> int -> int` is
  `ptr[int] @ local -> (int -> int) @ local`), as it is the closure of a
  partial application holding them. Its `curry` is still fixed `regional`,
  as a declaration can't tell a partial application from a function value
  returned, so a tail call through it given a `local` argument must be marked
  `[@nontail]`. Provenance written in a declaration isn't checked yet.
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
`'m1`, ... and their constraints follow the type, simplified:

```
val apply : ('a @ 'm1 -> 'b @ 'm2) @ 'm0 -> ('a @ 'm4 -> 'b @ 'm5) @ 'm3
  with 'm0 <= 'm3 and 'm2 <= 'm5 and 'm4 <= 'm1 and portable <= 'm5
```

- variables equal by their constraints (a cycle) print as one;
- a variable fixed to a constant prints as it (`int @ const -> int @ const`);
- constraints and bounds implied by the others are left out;
- a generic variable named once and in no constraint is left out, which
  reads back as a fresh variable;
- only the item's own variables are named: constraints through other
  variables (of other members of a recursive group, or a weak variable's
  current neighbours) are projected onto its own;
- bounds every mode meets (`const <= 'm`, `'m <= nonportable`) are left out;
- a weak (not generalized) variable is named `'_m0`, as weak type variables,
  and shown with its current constraints;
- relations through a crossing morphism are not printed.

The `with` clause is also syntax for annotations (see syntax.md), so printed
types read back as themselves. As only the stage axis is printed, a constraint
between variables constrains only the stage axis, until the others are
checked.

### Annotations on uses

`(x @ m)` (or `(x : t @ m)`) is the check-only form: it bounds the mode of `x`
at this use, so of the variable, and `x` keeps its own mode. A binding's
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
  then required: a later rise is an error. A variable used in its own
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
  `stack_`, `exclave_`, `global_` fields, the non-tail call marker. Parsed
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
  `exclave_`, the tail call rule, `global_` fields, `Unsafe.assume_global`,
  mutable variables assigned from closures holding `global` values, mode
  crossing by kind on locality, exact binding annotations.
- **F. Provenance**: allocation is `gc`, literals and `const` values
  `static`, stores, mutable globals.
- **G. `External`**: C functions with a C compatible ABI (rejecting
  signatures it can't express), producing `external` values.

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
- Effect purity, `const fn`.
