# Semantics

Design notes for the runtime semantics of stag and its compilation to LLVM.
Mostly not implemented yet; modes are being added first (see
[modes.md](modes.md), whose `const < portable < nonportable` axis is the stage
axis here, and its implementation plan).

## Overview

One language at every level: meta level (compile-time splices, generators) and
residual program alike. It has GC'd values, closures and modes, but a program
that uses none of them is essentially C: by-value structs, fixed width
integers, static functions, static data. Features are implemented
incrementally; initially the compiled subset is the C-like one, and the rest
is rejected by the backend (the interpreter supports everything).

## Values and layout

- Records, tuples, nominal and polymorphic variants are by value (C structs and
  tagged unions). Polymorphic records and variants are monomorphised, so their
  layout is static.
- Pointers are of two types: `ptr[t]`, a mutable cell, and `box[t]`, an
  immutable one (`Box.make v`), which share a representation. Where the
  pointee lives (static data, GC heap, data stack) is given by the pointer's
  modes, not its type. `ptr[t @@ external]`, a pointer to a cell that may hold
  `external` values, is a distinct type (see [Modes](#modes)). C's memory
  will have a type of its own.
- No implicit boxing: a recursive type must recurse through `ptr[t]` or
  `box[t]` (see
  [Recursive types](#recursive-types)).
- Mutation: mutable fields and stores through pointers. A store into a GC
  object has a write barrier, unless the stored value is at most `static`.
- Mutable variables (`let mut`, see syntax.md): a local is a stack slot,
  boxed only when a closure captures it; a toplevel is a static global, with
  no compile-time instance.
- Integers: `i8 i16 i32 i64` (and unsigned variants), wrapping arithmetic.
  `int` is `i64`. Division by zero and overflowing shifts are checked runtime
  errors, never UB.
- Strings: literals are `static`, built strings are `gc`.

## Recursive types

Nothing is boxed implicitly: a type whose by-value layout would be infinite
is an error, and recursion is written through `box[t]` (or `ptr[t]`, to
mutate it):

```ocaml
type list['a] = Nil | Cons of box['a * list['a]]
```

Checked per declaration SCC, at declaration time: a nominal type is an error
if it reaches itself through by-value occurrences, i.e. constructor arguments
and fields, and type arguments of types that contain that parameter by value
(each declaration records which parameters it contains by value, so
`type t = T of wrap[t]` with `type wrap['a] = W of 'a` is an error, but
`type t = T of wrap[ptr[t]]` is not). Cyclic structural types
(`` [`Nil | `Cons of int * 'a] as 'a ``) are likewise errors unless every
cycle goes through a `ptr`; this is checked where unification closes the
cycle (the occurs check). A function type is not a `ptr`, so it does not
break a structural cycle: `let self x = x x` is rejected, as is the alias
`type f = int -> f` (a nominal declaration may still recurse through a
function type, as a closure is not stored by value). An alias is cyclic
unless its expansion reaches itself through a `ptr` and some other type
constructor: `type al = ptr[al]` is rejected, `type f = int -> ptr[f]` is not.

Where the pointee lives is the pointer's mode, as for any `ptr`: a `const`
list is static data, an allocated one in the heap, a `stack_` one on the data
stack.

The existing tests and examples using recursive variants (`list`, `nat`) are
rewritten with `ptr`.

## Pointers

All explicit. A `ptr[t]` is never null (`option[ptr[t]]` is represented as a
nullable pointer, unobservably).

```ocaml
Ptr.make e          (* allocate: heap, or data stack under stack_ *)
Box.make e          (* allocate an immutable box[t]: no stores; covariant *)
&x                  (* address of a variable: local *)
*e   e.*            (* read *)
e.*.x               (* field of the pointee *)
*e <- v   e.* <- v  (* write *)
e.*.x <- v          (* write a field of the pointee *)
```

- A store `e.*.f.g <- v` evaluates the pointer expression `e` first, then
  `v`, and only then reads the cell's contents, replaces the field path `f.g`
  in them, and writes the result back. So stores made while evaluating `v`
  are kept in the fields it doesn't replace: with `q = Ptr.make {x = 0; y = 0}`,
  `q.*.x <- (q.* <- {x = 1; y = 9}; 3)` leaves `{x = 3; y = 9}`.

- Postfix `e.*` is at the field access level, so it chains (`p.*.next.*`).
- Prefix `*e` is at the constructor level (`C e`), so it extends over an
  application, as in C: `*f x` is `*(f x)`, `*r.x` is `*(r.x)`. It cannot be
  an unparenthesized argument (`f *p` is not `f (*p)`; write `f p.*`), which
  leaves `a *b` free for infix multiplication later.
- `(*` always starts a comment, so `(*p)` does not parse as a deref: write
  `( *p)` or `p.*`. Outside comments `(f p.*)` lexes as expected, but inside
  one `*)` ends it, so commenting out such code ends the comment early (as
  OCaml's `( *)`).

In patterns, `*p` matches a pointer whose pointee matches `p`, read when the
match is: `*Cons (x, tl)` is `*(Cons (x, tl))`, binding `tl` to the tail
pointer. It is irrefutable iff `p` is.

```ocaml
let length l =
  match l with
  | Nil -> 0
  | Cons (_, tl) -> Int.add 1 (length tl.*)

let sum p =
  match p with
  | *Nil -> 0
  | *Cons (x, *Nil) -> x
  | *Cons (x, tl) -> Int.add x (sum tl)
```

## Functions

There is one arrow type, `a -> b`. Every function value is a closure, boxed,
at least the provenance of what it captures. A `static` closure is `const`
and a constant global; others are allocated (heap or data stack); one
capturing an `external` value is `external`. The closure's mode is the mode
of the arrow value itself.

Calling a function known statically (a toplevel, an `external`) with all its
arguments is a direct call, with no closure involved: a compilation detail,
not part of the type. Partially applying it, or passing it as a value, makes
a closure, as for any function.

C functions are declared with `external` (see
[syntax.md](syntax.md#external-functions)), at ordinary curried types: an
`external f : a -> b -> r` is C's `r f(a, b)`, and `unit -> r` is `r f(void)`.
Its modes are trusted as written, unwritten ones a declaration's defaults
(`nonportable gc global`). The external itself is a code pointer, so its own
mode is unconstrained: like any toplevel function, it is `static` and `const`.
Only immediates cross the boundary for now (`int` as `int64_t`, `bool`,
`char`, and `unit` as `void`): pointers are not supported yet, and signatures
that can't be expressed are rejected. A stag function passed to C as a
callback must be `@ static`: a
`static` closure carries a C ABI entry point, as it needs no environment.
Callbacks are not implemented yet.

Tail calls are guaranteed in tail position (LLVM `tailcc` + `musttail`).

## Modes

Modes are a product of independent axes, each a small lattice:

- **Stage** (binding time, see [modes.md](modes.md)):
  `const ≤ portable ≤ nonportable`. Whether a value is known at compile time
  (`const`), and whether code of the next stage may refer to it (`portable`).
  `const` values are deeply immutable, since lifting copies them.
- **Provenance** (memory): `static ≤ gc ≤ external`. What a value may reach.
  Deep: a value's provenance is at least that of everything reachable from it.
  - `static`: immortal data emitted with the program (literals, constants,
    `const` values, closures capturing nothing allocated at run time). Never
    scanned by the GC, as it can only reach `static` data.
  - `gc`: may reach the GC heap (and `static` data). Everything allocated at
    run time is at least `gc` (closures, partial applications, code,
    `Ptr.make`, raised exceptions), data stack blocks included (only their
    locality differs).
  - `external`: may reach memory the GC doesn't manage (C's `malloc`, mapped
    memory). The GC ignores such pointers (it finds objects by address, see
    [GC](#gc)), but they may dangle, and C may not expect them to be retained.
- **Locality**: `global ≤ regional ≤ local`, as OxCaml's regionality. See
  [Locality](#locality).

The axes are independent except for two links between stage and provenance:
`const` implies `static` (a value known at compile time is emitted as static
data), and what is allocated at run time (a closure, a partial application,
code, `Ptr.make`) is at least `gc` unless it is `const` (emitted statically).
In the solver these are monotone morphisms between the axes, as OxCaml's
solver relates axes: `prov ≤ const_is_static(stage)` with
`const_is_static(const) = static` and `external` otherwise, in every
variable, and `stage ≤ runtime_is_gc(prov)` with
`runtime_is_gc(static) = const` and `nonportable` otherwise, in each
allocation's. They cannot be one axis, because of
mode crossing by kind: pointer-free data (ints) crosses provenance and
locality entirely, and crosses `portable` (it can be lifted into code), but
never `const`, as a runtime int is not known at compile time. Crossing is
applied when the type is known where the constraint arises. A raised
exception's payload is at least `gc` outright, as it is allocated whatever
its stage.

A function's parameter is at least `portable` in its body: `const` means the
same value on every evaluation, and a parameter may differ between calls. So
what a function builds from its parameters is never `const`, so never
`static`. Likewise an application's result is at least `portable`, so an
arrow's result is too: annotations floor it (`int -> int @ const` is
rejected), and printing leaves that bound out.

Provenance follows reachability, not evaluation:

- A block (a record, tuple or constructor, by value), a pointer and a closure
  are at least what they hold or capture; a part read from them is at most
  their provenance. A field with a modality still counts: it is reachable.
- What is only evaluated (a `let`'s bound expression, a sequence's first
  part, a scrutinee) doesn't count towards the result's provenance.
- Literals and toplevel constants are `static`; declared functions and
  `external`s take and return `gc` values unless their types say otherwise
  (`string @ external -> ...`), and an `external`'s result is trusted as
  declared. Prims only read their arguments, so take any (`local`,
  `external`), and return new `gc` values; a partial application of one holds
  the arguments so far, so is as local as they are.
- A heap block holding an `external` value is itself `external`. Stores must
  not raise a cell's provenance, as the cell's other aliases rely on it, and
  a pointer's mode may have been raised by subsumption: what a cell may be
  given is in the pointer's type instead: `ptr[t @@ m]`, whose `m` is
  unified exactly, so all aliases agree on it. A store through it
  (`p.* <- v`) takes values of at most `m`, and a read through it is at
  least `m` (unless the type crosses), and at least `portable`: a cell may
  change between evaluations, so what is read from it is never `const`.
  `ptr[t]` is `ptr[t @@ nonportable gc]` (`ptr` is a type with a mode
  parameter, `ptr['a, 'p : mode]`, and `ptr[t @@ m]` is `ptr[t, m]`), and `m`
  may be a variable, so code can be polymorphic in it. `Ptr.make` infers
  `m` from its argument and the cell's uses; a toplevel cell that is not
  generalized has its `m` fixed by its definition, `gc` unless that needs
  more. A pointer's own provenance is that of its cell (`gc`), not of what
  the cell holds: the type says that, and can't be forgotten. A mutable variable (`let mut`, a mutable global) has
  a held mode of its own, inferred from what is assigned to it: an
  `external` assignment makes it `external` (its reads then are), unless it
  is annotated `@ gc`, which rejects it. `@ external` opts in.
- Every `ptr` points to stag's memory (static data, the GC heap or the data
  stack), whatever its mode, so a store needn't check the pointer stored
  through: no `external` signature takes or returns pointers yet, and C's
  memory will have its own type (`cptr[t]`), whose stores will be limited to
  C-compatible data.
- A closure is `static` only if it is `const`: what it captures is then
  known at compile time, so it is emitted statically. A callback passed to C
  must be `@ static` (C may retain it), once the FFI has callbacks.

`external` is produced by annotations (`let x @ external = e`, a declared
function type's `@ external` result), including those of `external`
declarations of C functions: an external's modes are trusted as written, not
forced to `external`.

In compiled code, a value crossing from compile time into a quote must be
`const` (lifted, as static data) or a runtime toplevel, referred to by its
symbol (the symbol is `static`, whatever the toplevel holds, so mutable
globals keep their identity).

### Locality

As OxCaml. `local` values are allocated on a **data stack**, separate from the
machine (function) stack, and managed by the runtime as a bump allocator:

- Each function body is a region: on entry it saves the data stack pointer,
  on exit restores it, freeing everything allocated locally in the call.
- Allocation is inferred, as in OxCaml: each allocation (closure, quote,
  `Ptr.make`) has a mode variable, at least its parts' (a block holding a
  value local to the region is itself local) and at most what its uses allow.
  Records, tuples and constructors are values, as local as their parts.
  Nothing needs a keyword to be local: compiled code allocates in the current
  region whatever may be `local`, the interpreter on its heap. `stack_ e`
  forces `e`'s allocations to be `local`, so it is an error if they escape,
  and an error if `e` allocates nothing (a record, tuple or constructor must
  hold an allocation). Pointer-free data (`int`, `bool`, `char`, `unit`, and
  records, tuples and variants of them) crosses locality: a copy of it is as
  good anywhere.
- A function's result is at most `regional` (it outlives the function's
  region); a toplevel, what is stored through a pointer and what a
  `@@ global` field holds must be `global`. A closure is at least the localities of what
  it captures, as seen from its body (what is `local` to the enclosing body is
  `regional` inside it).
- `exclave_ e` ends the current region early and evaluates `e` in the caller's,
  so a function can return a local value (allocate in the caller's region).
- `&x` of a variable puts `x` on the data stack, and is a `local` `ptr`.
- `local` values cannot escape their region: not returned (except via
  `exclave_`), not stored into `global` blocks (including GC objects), not
  captured by `global` closures. A `local` argument (`(x @ local)`) is
  borrowed: the callee may not retain it.
- `regional` is local to an *outer* region: inside a function body, its
  `local` parameters (and what is read from them) are `regional`, as they
  live in the caller's region. `stack_` allocations of the current body are
  `local`. Returning needs `global` (`regional` or `local` under `exclave_`).
- Modalities, `t @@ m` on a record field or constructor argument, on any
  axis, but only `global` on locality: `regional` and `local` are relative
  to a region, so a block returned to its caller's region with `exclave_`
  would keep its parts `regional` there though they were allocated in the
  callee's: building one needs the part at most `m`, and reading it from a block
  at mode `b` gives at most `b ⊓ m` (`@@ global`: always `global`, even
  inside a local block; `@@ portable`: embeddable even when the block isn't;
  `@@ gc`: GC-owned even in an `external` block). A `const` modality is also
  `static`. Through a pointer the modality is exact instead: a store takes
  at most `m`, and a read gives `m` whatever the pointer's mode, as the
  cell's aliases may store anything that fits. Modalities compose along a
  path (`p.*.f`: the cell's, then the field's). A type's mode parameters
  (`type cell['a, 'p : mode]`) are on stage and provenance only (a locality
  atom in a mode argument is an error); their variance is inferred from where
  they occur (see Variance). A record
  update `{ r with ... }` may change a mode parameter only if no field it
  keeps refers to it, by its type or its modality.
- Records have no mutable fields: mutation is through a `ptr` (`p.* <- v`,
  `p.*.x <- v`), whose pointee is storage rather than a value. Such a store
  is implicitly `@@ global`, so an older region's block never points into a
  newer region. `Unsafe.assume_global e` (name tentative) bypasses this, the
  programmer guaranteeing that `e` outlives what it is stored into. The GC
  stays safe either way: it never follows pointers into the data stack, so
  only the program's own reads can dangle. Which pointers may be written
  through (immutability as a permission) is left to modes and modalities.
- Tail calls end the current region before the call, so arguments of a call
  in tail position must be at most `regional`: passing a `local` value is an
  error, fixed by marking the call non-tail (see syntax.md), which keeps the
  region alive until it returns. A call inside `exclave_` is always fine.
  A saturated call `f x y` is the call of `f x`'s result after the region has
  ended, so that result must be at most `regional` too, unless it is the
  closure of a partial application, which a saturated call of a known
  function never makes. Each arrow records which (its `curry` mode, not
  printed): the arrows of a multi-parameter function's partial applications
  leave it free, its last arrow (a function value it returns) bounds it by
  `regional`. A function whose body is directly a `fun` (`let f a = fun b ->
  e`, `fun a -> fun b -> e`) is one function of both's parameters, as in
  OxCaml: a compiler must merge them into one arity rather than make the
  inner closure, which is then a partial application too. A tail call of an
  unknown function bounds its intermediate results by their arrows' `curry`,
  so it may be passed curried functions, but not ones returning a `local`
  function value.
- In a quote, `exclave_` and tail position are decided where the code is
  spliced, as the region they end is the one of the function it ends up in.
  An explicit substitution (`.~c.{x = e}`) is only evaluated there: `e`'s
  mode is checked again where the code it is substituted into is finally
  spliced.
- The interpreter has no regions: `stack_` allocates on its heap and
  `exclave_ e` is `e`. The rules are checked all the same.

A local value of fixed size that reaches neither an `exclave_` nor a caller
(it dies with its own region) may be put on the machine stack instead (an
`alloca` in the entry block, or a promoted register): the data stack is only
needed for `exclave_` results and for local allocations of dynamic size or in
loops. Either way, local blocks have the provenance `gc` (the GC scans them as
roots: the data stack is a root region, with Boehm registered with
`GC_add_roots`) and only their locality differs. Stores into local blocks need
no write barrier.

### Solver

Per axis, a graph: mode variables with a lower and an upper bound, and edges
`a ≤ b`. Adding an edge propagates lower bounds forward (`lower b ⊔= lower
a`) and upper bounds backward (`upper a ⊓= upper b`), along edges
transitively; a variable whose lower bound exceeds its upper is an error.
Each bound only moves monotonically through a lattice of height ≤ 4, so
propagation is linear in the edges overall. Equality (unifying the modes of
two arrow types) is two edges. Every edge carries the reason it was added, to
explain errors ("`x` is `gc` because it is allocated here, and must be
`static` because it is captured by this static closure").

The `const ⇒ static` link is a morphism edge between the stage and provenance
graphs, `prov ≤ const_is_static(stage)`. It propagates upper bounds through
it (`upper prov ⊓= const_is_static (upper stage)`) and lower bounds through
its left adjoint `l` (`lower stage ⊔= l (lower prov)`, with
`l(static) = const`, `l(gc) = l(external) = portable`: a heap value is not
`const`). An allocation adds the edge `stage ≤ runtime_is_gc(prov)` the other
way: upper bounds give `upper stage ⊓= runtime_is_gc (upper prov)` (a `static`
allocation is `const`), and its left adjoint `l'`, with `l'(const) = static`
and `l'(portable) = l'(nonportable) = gc`, gives
`lower prov ⊔= l' (lower stage)` (an allocation that isn't `const` is at
least `gc`). Morphisms must have adjoints for this to work, as in OxCaml.

The presence variables of rows (`Absent ≤ Present`, today `Type.presence`
with `At_most`/`At_least` bounds) are another axis of the same solver, a
two point lattice. Their `If_present` equalities (entry types that must be
equal once the entry is `Present`) are triggers: callbacks run when a
variable's lower bound reaches a given point, here unifying the types. Their
generalisation (bounds kept between generic and outer variables) is the same
as for modes.

Mode crossing by kind is applied when the type is known at the constraint;
constraints on values of a type variable later found to be `int` are not
retracted (as OxCaml; annotations recover precision). A crossing is a
morphism on the edge, e.g. `a ≤ r(b)` with `r` the right adjoint of
`x ↦ x ⊓ portable` on stage; on provenance and locality the edge is dropped.

Each operation is atomic: a constraint that fails leaves the graph as it was.

### Mode polymorphism

From the start: at generalisation, the scheme keeps the mode variables
occurring in the type (those not of an outer rank), with the constraints
between them and with outer variables implied by the graph (paths through the
other, internal, variables, composing their morphisms), and their current
bounds. Instantiation copies the variables, bounds and constraints fresh.
Internal variables stay in the graph: the bounds they imply are already on
the kept ones. At instantiation for monomorphisation, an unconstrained
variable defaults to its lower bound.

Syntax: `'a @ 'm -> 'b @ 'n`, where a mode variable ranges over all axes,
or concrete modes, `int @ local -> int`. Printing hides modes that are
unconstrained or at their default; the full constraints are shown on
request.

## Monomorphisation

Full monomorphisation over types, and over the code-relevant modes, for
compiled code. Polymorphic recursion is rejected in compiled code
(instantiation that does not reach a fixpoint). A mode variable is
code-relevant when the generated code depends on it: locality at an
allocation site (data stack or heap), provenance of a stored value (a
`static` one skips half of the write barrier), transitively through callees.
Others (the stage axis always, locality of a parameter that is only read) are erased to their upper bound,
which is always sound since the body was checked for every mode within its
bounds. A function is compiled once per distinct assignment of its relevant
variables, so instances differing only in irrelevant modes share code.

A scheme only copies the mode variables it keeps, not the internal ones of
the body (such as an allocation site's locality). So the monomorphiser keeps
each definition's body mode constraints and re-solves them per instance,
rather than relying on scheme instantiation.

## Pipeline

```
Typed  (after compile-time splices, run by the interpreter)
  → monomorphise (types and modes)
  → closure conversion / lambda lifting
  → Core IR: first-order, ANF, explicit layouts and address spaces
  → textual LLVM IR (.ll) → clang (from nixpkgs)
```

Lowering of modes to LLVM: `static` is `ptr` (globals, constant if `const`),
`gc` and `external` are `ptr addrspace(1)` (GC references).

Runtime quotes (code values at level 0 in a compiled program) are not compiled
initially.

## Interpreters

- **Treewalk** (on `Typed`): uses the OCaml GC, ignores modes at runtime.
  Later, optionally: tag pointers with their actual provenance and locality
  and check the static modes dynamically, as a test oracle for mode
  soundness.
- **Core IR interpreter**: the reference semantics of the compiled subset.
  Expect tests run under Treewalk, the Core IR interpreter and compiled LLVM,
  and must agree.

## GC

As Go's: non-moving, non-generational, mark and sweep, finding objects by
address rather than by header.

- The heap is made of arenas of size-classed spans. A pointer's object is
  found by its address: the arena index maps it to its span, which gives the
  object's start and size, and its pointer bitmap (spans of pointer-free
  objects are `noscan`, and are never scanned). A pointer outside the arenas
  (into `static` data, the data stack, C's memory) is ignored, so no header is
  needed on `static` or `external` data, and `external` values are traced
  safely wherever they are stored.
- `static` data is never scanned: by deep provenance it only reaches `static`
  data. Mutable globals are roots (they may be assigned `gc` values), as are
  the machine stacks and the data stack.
- Marking is incremental first, then concurrent with the program. While (and
  only while) marking, stores run Go's hybrid write barrier, shading the
  overwritten pointer and the stored one. No barrier is needed on stores into
  the data stack or other stack slots, on initialising stores (into an object
  allocated since marking began, which is allocated black), or on stores of
  pointer-free values. A barrier is needed whatever the stored value's
  provenance, as the overwritten pointer must be shaded; only shading the
  stored one may be skipped when it is `static`.
- As cgo's rule: C may not retain a GC pointer beyond the call it was passed
  to (a callback passed to C must be `static`).

Steps:

1. Boehm (conservative, non-moving): modes only serve as optimisations. It
   also finds objects by address and tolerates pointers outside its heap.
2. The precise collector above, stop the world, with LLVM's `shadow-stack`
   strategy or stack maps for roots.
3. Incremental, then concurrent marking, with the write barrier.

## Plan

1. Modes: see the implementation plan in [modes.md](modes.md). Later, move
   presence variables onto the solver (with triggers).
2. Language: fixed width integers, pointer types, mutation, C callbacks
   (`@ static` closures), pointers in `external`s, closure mode restriction,
   recursion only through pointers (rewriting the existing recursive
   variants).
3. Treewalk: the new constructs; optional dynamic provenance checks.
4. Monomorphiser.
5. Closure conversion and lowering to Core IR; Core IR interpreter;
   differential tests against Treewalk.
6. LLVM emitter (textual), Boehm, clang via nix; expect tests on the emitted
   IR and on the output of the compiled programs.
7. Later: precise GC, runtime quotes, by-value structs in the FFI.
