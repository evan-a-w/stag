# Semantics

Design notes for the runtime semantics of stag and its compilation to LLVM. Not
implemented yet. Extends [modes.md](modes.md), whose `const < portable <
nonportable` axis is the stage axis here (see [Modes](#modes)).

## Overview

One language at every level: meta level (compile-time splices, generators) and
residual program alike. It has GC'd values, closures and modes, but a program
that uses none of them is essentially C: by-value structs, fixed width
integers, function pointers, static data. Features are implemented
incrementally; initially the compiled subset is the C-like one, and the rest
is rejected by the backend (the interpreter supports everything).

## Values and layout

- Records, tuples, nominal and polymorphic variants are by value (C structs and
  tagged unions). Polymorphic records and variants are monomorphised, so their
  layout is static.
- Pointers are one type, `ptr[t]`; where the pointee lives (static data, GC
  heap, data stack, foreign memory) is given by the pointer's modes, not its
  type.
- No implicit boxing: a recursive type must recurse through `ptr[t]` (see
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
is an error, and recursion is written through `ptr[t]`:

```ocaml
type list['a] = Nil | Cons of 'a * ptr[list['a]]
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
list is static data, an allocated one in the heap, a `local_` one on the data
stack.

The existing tests and examples using recursive variants (`list`, `nat`) are
rewritten with `ptr`.

## Pointers

All explicit. A `ptr[t]` is never null (`option[ptr[t]]` is represented as a
nullable pointer, unobservably).

```ocaml
Ptr.make e          (* allocate: heap, data stack under local_, static if const *)
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

Two arrow types:

- **Closure** `a -> b` (the existing arrow): boxed, with mode `static` or `gc`
  only. A `static` closure captures only `static` values and is a constant
  global; a `gc` closure is heap allocated and captures `global` values
  (capturing `unknown` ones is unsafe, as storing them).
- **Function pointer** `a => b`: a code pointer, capturing nothing (it may
  refer to `const`/`static` toplevels directly). No allocation, C ABI
  compatible. Same precedence and associativity as `->` (`int * int => int`
  is `(int * int) => int`).

Function pointers are unary and uncurried: several arguments are a tuple,
flattened by the calling convention into separate parameters (`unit` into
none), so `int * int => int` is C's `int64_t (*)(int64_t, int64_t)`. Defining
and calling look the same as for closures:

```ocaml
let add (x, y) = Int.add x y      (* add : int * int => int, or a closure *)
let apply (f : int * int => int) = f (1, 2)
```

Whether an arrow is a closure or a function pointer is inferred: arrows carry
a kind variable (`closure | fn_ptr`), generalised like a mode variable and
monomorphised. A function capturing nothing may be either, so `add` above can
be passed where a closure is expected and where a function pointer is (two
instances are compiled); one capturing anything not `static` is a closure.
An unconstrained kind variable defaults to `fn_ptr` when allowed. `->` and
`=>` in annotations force the kind.

Tail calls are guaranteed in tail position (LLVM `tailcc` + `musttail`).

## Modes

Modes are a product of independent axes, each a small lattice:

- **Stage** (binding time, see [modes.md](modes.md)):
  `const ≤ portable ≤ nonportable`. Whether a value is known at compile time
  (`const`), and whether code of the next stage may refer to it (`portable`).
  `const` values are deeply immutable, since lifting copies them.
- **Provenance** (memory): `static ≤ gc ≤ unknown`. Where pointers point.
  - `static`: immortal data with GC headers marked immortal, so tracing it is
    harmless. Deep: everything reachable from a `static` value is `static`.
  - `gc`: in the GC heap, or `static`.
  - `unknown`: anything (FFI, malloc). The safety escape hatch: it may be
    stored into GC objects and captured by closures, unmarked, but that is
    unsafe (the programmer guarantees it outlives them). Since an `unknown`
    may point into the GC heap, it is traced, wherever it is stored: the GC
    tolerates pointers outside its heap, in roots and in GC objects alike
    (Boehm does; a precise GC checks headers or the heap range on such
    fields). Stores of `unknown` keep their write barrier.
- **Locality**: `global ≤ local`, as OxCaml. See [Locality](#locality).

The axes are independent except for one link: `const` implies `static` (a
value known at compile time is emitted as static data). In the solver this is
a monotone morphism bounding provenance by stage, `prov ≤ f(stage)` with
`f(const) = static` and `f(_) = unknown`, as OxCaml's solver relates axes.
They cannot be one axis, because of mode crossing by kind: pointer-free data
(ints) crosses provenance and locality entirely, and crosses `portable` (it
can be lifted into code), but never `const`, as a runtime int is not known at
compile time.

In compiled code, a value crossing from compile time into a quote must be
`const` (lifted, as static data) or a runtime toplevel (`static`, referred to
by its symbol, so mutable globals keep their identity).

### Locality

As OxCaml. `local` values are allocated on a **data stack**, separate from the
machine (function) stack, and managed by the runtime as a bump allocator:

- Each function body is a region: on entry it saves the data stack pointer,
  on exit restores it, freeing everything allocated locally in the call.
- `local_ e` allocates `e`'s blocks in the current region (closures included,
  as a third closure mode beside `static` and `gc`). Allocation is otherwise
  `global` (GC heap), unless inferred local because the value does not escape.
- `exclave_ e` ends the current region early and evaluates `e` in the caller's,
  so a function can return a local value (allocate in the caller's region).
- `&x` of a variable puts `x` on the data stack, and is a `local` `ptr`.
- `local` values cannot escape their region: not returned (except via
  `exclave_`), not stored into `global` blocks (including GC objects), not
  captured by `global` closures. A `local` argument (`(x @ local)`) is
  borrowed: the callee may not retain it.
- `global_` on record fields and constructor arguments: always `global`, even
  inside a local block.
- Records have no mutable fields: mutation is through a `ptr` (`p.* <- v`,
  `p.*.x <- v`), whose pointee is storage rather than a value. Such a store
  is implicitly `global_`, so an older region's block never points into a
  newer region. `Unsafe.assume_global e` (name tentative) bypasses this, the
  programmer guaranteeing that `e` outlives what it is stored into. The GC
  stays safe either way: it never follows pointers into the data stack, so
  only the program's own reads can dangle. Which pointers may be written
  through (immutability as a permission) is left to modes and modalities.
- Tail calls end the current region before the call, so a local value of the
  current region cannot be passed to a tail call (OxCaml's rule); such a call
  is not a tail call (or is an error under an explicit `[@tail]`).

Data stack blocks have GC headers, so their provenance is `gc` (traceable)
and only their locality differs. The data stack is a GC root region (with
Boehm, registered with `GC_add_roots`). Stores into local blocks need no
write barrier.

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
graphs, `prov ≤ f(stage)`. It propagates upper bounds through `f`
(`upper prov ⊓= f (upper stage)`) and lower bounds through its left adjoint
`g` (`lower stage ⊔= g (lower prov)`, with `g(static) = const`,
`g(gc) = g(unknown) = portable`: a heap value is not `const`). Morphisms
must have adjoints for this to work, as in OxCaml.

The presence variables of rows (`Absent ≤ Present`, today `Type.presence`
with `At_most`/`At_least` bounds) are another axis of the same solver, a
two point lattice. Their `If_present` equalities (entry types that must be
equal once the entry is `Present`) are triggers: callbacks run when a
variable's lower bound reaches a given point, here unifying the types. Their
generalisation (bounds kept between generic and outer variables) is the same
as for modes.

Mode crossing by kind is applied when the type is known at the constraint;
constraints on values of a type variable later found to be `int` are not
retracted (as OxCaml; annotations recover precision).

### Mode polymorphism

At generalisation, the scheme keeps the mode variables occurring in the type,
with the constraints between them, and with outer variables, implied by the
graph (its transitive closure restricted to those variables). Other variables
are fixed to their lower bound, which is always consistent. Instantiation
copies the variables and constraints fresh. At instantiation for
monomorphisation, an unconstrained variable defaults to its lower bound.

## Monomorphisation

Full monomorphisation over types and modes for compiled code. Polymorphic
recursion is rejected in compiled code (instantiation that does not reach a
fixpoint). A mode variable the body does not depend on may instead be erased
to its upper bound (always sound, since the body was checked for every mode
within its bounds), bounding code size.

## Pipeline

```
Typed  (after compile-time splices, run by the interpreter)
  → monomorphise (types and modes)
  → closure conversion / lambda lifting
  → Core IR: first-order, ANF, explicit layouts and address spaces
  → textual LLVM IR (.ll) → clang (from nixpkgs)
```

Lowering of modes to LLVM: `static` is `ptr` (globals, constant if `const`),
`gc` and `unknown` are `ptr addrspace(1)` (GC references).

Runtime quotes (code values at level 0 in a compiled program) are not compiled
initially.

## Interpreters

- **Treewalk** (on `Typed`): uses the OCaml GC, ignores modes at runtime.
  Optionally tags pointers with their actual provenance and checks the static
  modes dynamically, as a test oracle for mode soundness.
- **Core IR interpreter**: the reference semantics of the compiled subset.
  Expect tests run under Treewalk, the Core IR interpreter and compiled LLVM,
  and must agree.

## GC

1. Boehm (conservative, non-moving): modes only serve as optimisations.
2. Precise non-moving GC with LLVM's `shadow-stack` strategy.
3. Statepoints (`RewriteStatepointsForGC`), possibly moving: `unknown` roots
   and fields then need a heap check before relocation.

## Plan

1. `Mode`: the axes, the product, mode variables with bounds and constraints,
   morphisms, triggers, solver and errors. Standalone, expect tests. Then move
   presence variables onto it.
2. Typechecker: modes on entries and expressions, locks (closure, quote,
   toplevel splice), modes on arrows, mode polymorphism in schemes.
3. Language: fixed width integers, pointer types, mutation, function pointer
   type, closure mode restriction, recursion only through pointers (rewriting
   the existing recursive variants).
4. Treewalk: the new constructs; optional dynamic provenance checks.
5. Monomorphiser.
6. Closure conversion and lowering to Core IR; Core IR interpreter;
   differential tests against Treewalk.
7. LLVM emitter (textual), Boehm, clang via nix; expect tests on the emitted
   IR and on the output of the compiled programs.
8. Later: precise GC, runtime quotes, C FFI with by-value structs.
