# Proofs

Design notes for proving properties of stag programs. Not implemented yet.
Three approaches are compared, followed by a recommendation. Surface syntax is
illustrative only: stag has no parser yet, and no primitives (the `add`, `eq`
and `cond` used in tests are stubs).

## Goals

- Implementations stay ordinary ML, with no proof terms, ghost arguments or
  invariants inline.
- Proofs live in a companion file per module: `list.stag` is the
  implementation, and `list.proof.stag` (or a Lean file, see B) holds theorems
  about it.
- The proof file sees the whole module, including items a future signature
  would hide. Specification helpers (`sorted`, `count`, `perm`) are either
  stag code in the proof file or logic-only definitions. The latter are
  needed for quantifiers, `Prop`, and equality on `'a`, none of which stag
  has. Later, signatures could include theorem statements, so that abstract
  types come with laws.
- Proofs are Lean-like: theorem statements, then tactic scripts or terms.
- Later: proofs about the code that generators produce.

```ocaml
(* list.stag *)
type 'a list = Nil | Cons of 'a * 'a list

let append xs ys =
  match xs with
  | Nil -> ys
  | Cons (x, xs) -> Cons (x, append xs ys)

(* list.proof.stag *)
open List

theorem append_nil (xs : 'a list) : append xs Nil = xs := by
  induction xs <;> simp [append]

theorem append_assoc (xs ys zs : 'a list) :
    append (append xs ys) zs = append xs (append ys zs) := by
  induction xs <;> simp [append, *]
```

## Shared prerequisites

These are needed whichever approach is chosen. Most of them are also useful
without proofs.

### 1. Primitives and the meaning of `int`

`int` is a nominal type with no operations, and `Lit.Int` stores a string, so
literals are unbounded. Before anything can be proved, we need:

- Primitive operations with defined semantics: arithmetic, comparison, and
  equality where it is meaningful.
- A lazy conditional. The tests' `cond` is a strict function, so the staged
  `power` test would loop forever under call-by-value. `match` on `bool` would
  serve.
- A decision on `int`: mathematical integers, or machine integers at runtime.
  The logic wants mathematical integers. With machine integers, either the
  logic models wrapping, which is painful to reason about, or arithmetic is
  checked.
- Likewise for `char` and `string`: bytes, as in OCaml, or Unicode, as in
  Lean.

### 2. Semantics

Stag has no evaluator, so proofs currently have nothing to be about. We need a
reference evaluator for the pure fragment. Its rules follow the typechecker:

- **Quotes.** `<e>` evaluates the splices that run when the quote is built:
  those whose quote depth, net of splices, returns to the quote's own level.
  In `<<~(~c)>>`, the inner `~c` runs when the outer quote is built. The result
  is code whose free variables are named by its context.
- **Explicit splices.** `~{x = e} c` provides exactly the listed entries.
  The code's context must be closed apart from those entries, so missing
  entries are an error and nothing passes outward. Unused extras are allowed.
  `e` is code at the splice's level and has to be inserted as
  `let x = e in c`. Textual substitution would let binders inside `c` capture
  `e`, as in `~{x = y} <let y = 2 in x>`. The evaluator also has to choose
  between by-value and by-name insertion; the choice matters once divergence
  or effects exist.
- **Implicit splices.** `~c` inside a quote provides each entry from the
  binder of that name in the *current* quote, if one is visible. Binders of
  outer levels never capture, names bound at a later level are forced
  `Absent`, and toplevels never capture inside a quote. Remaining entries
  become entries of the enclosing quote's context.
- **Toplevel splices.** A splice outside any quote is checked at level −1. It
  is compile-time macro expansion of possibly open code. Its entries are
  provided by level 0 locals, or else by the toplevel with that canonical
  name. Entries that cannot be provided are forced `Absent`, so code that is
  known to use them is rejected. At the moment no toplevel can be used inside
  a toplevel splice at all, not even a persistent one, so the
  `~(power 3 <2>)` example in `modes.md` is aspirational. Reflection happens
  after expansion, so the logic never sees level −1.
- **Non-persistent toplevels in quotes.** In `<n + 1>`, the name `n` is a free
  variable meaning "whatever `n` is where the code is finally spliced". A
  local `n` at a toplevel splice captures it. A theorem about a generator that
  produces such code has to quantify over that environment.
- **Persistent values.** Persistent closures, including local ones that
  capture level 0 values, are embedded in code as runtime values, not
  referred to by name.

The semantics follow today's level-crossing rules. `modes.md` will change
them (constants will resolve directly, and closures that capture arguments
will no longer cross levels), so the evaluator should be written against
whichever rules hold by then.

The evaluator is the ground truth. It resolves splices from the actual free
names of code values, not from types. Property tests ("well-typed programs
don't get stuck") are the main evidence that the context typing is sound.
That evidence matters, because presence is approximated, and the outcome
depends on the order in which constraints arise (see the "presence
approximation" test).

### 3. Exhaustiveness

Non-exhaustive matches get stuck. An exhaustiveness check is needed anyway;
`types.md` lists it as missing.

### 4. Termination

A logic can only use a definition as an equation if the definition is total.

- **Call graphs must be level-aware.** The SCCs that `Typecheck` computes
  internally, and does not expose, count every reference as an edge,
  including references inside quotes. In `let f x = <f x>`, the quote is code
  that runs later, not a call. Only same-level references, and references
  inside splices that run now, are calls.
- **Structural recursion** on variants is accepted automatically.
- **Other recursion needs a measure.** To keep the implementation clean, the
  measure and its proof go in the proof file:

  ```ocaml
  (* sort.proof.stag *)
  termination merge_sort by length xs
    decreasing := by simp [split_length]
  ```

- **Partial domains.** Recursion on `int` such as `power n` is not structural,
  and it diverges for negative `n`. There are three options: a `nat` type,
  domain preconditions (as in Isabelle's `function` with `domintros`), or
  partial-correctness reasoning. Stag at least needs `nat`, or theorems with
  preconditions.
- **Generated code.** An implicit splice inside `let r x = ~c` can capture `r`
  itself, so whether generated code terminates cannot be read off the
  generator. It needs a separate argument about the code.

### 5. Reflected and opaque definitions

- A definition is **reflected** when it is total and pure (stag has no effects
  yet). Reflected means it becomes a transparent logical definition, with
  equations and an induction principle.
- Every other definition is **opaque**: a constant with its type and no
  equations. A reflected definition may call an opaque one. Its equations are
  then consistent, but they say nothing about termination: in effect,
  partial-correctness semantics for whatever depends on opaque code.
- Equations for a definition that may not terminate are never emitted as
  axioms. `f x = f x + 1` would prove `False`.
- Recursive non-function bindings, such as `let x = Cons (1, x)`, are accepted
  by the typechecker but have no inductive model. They are rejected in proof
  mode, or made opaque.
- Effects, once added, are opaque at first, and later possibly modelled in a
  monad. Purity would be a separate property of arrows; `modes.md` defers it.

### 6. Types the logic can accept

Stag accepts declarations that HOL and Lean do not:

- Non-strictly-positive types, such as `type t = T of (t -> int)`.
- Uninhabited types, such as `type t = T of t`. HOL requires every type to be
  non-empty. Lean's `opaque` and `partial` definitions need `Inhabited` or
  `Nonempty`.

A positivity and inhabitation check classifies each type. A type that fails
it becomes opaque, and so does everything that matches on it.

### 7. A reflected core IR

Checked stag is lowered to a small, explicitly typed, pure core:

- **Explicit polymorphism.** Type abstraction is explicit, and each use site
  records its instantiation. `Typecheck` computes this information but then
  discards it. Schemes are not plain HM: they quantify over context and
  presence variables, with bounds (`where ..'a <= {..'b}`). The IR therefore
  needs bounded row quantification, at least on code types.
- **Weak variables are defaulted.** Toplevels such as `cube : <int>{..'_a}`
  keep variables that are never generalized, and the IR needs ground types.
  An empty context and `unit` are natural defaults.
- **Lambda lifting.** Local recursive or generalized `let`s become toplevel
  `fix` groups, with stable names that proofs can refer to. HOL terms have no
  let-polymorphism, and Isabelle's `fun` works only at toplevel.
- **Explicit recursion.** There is one `fix` group per SCC of real calls, each
  annotated with how it was shown to terminate (structurally, by a measure,
  or not at all, in which case it is opaque).
- **Case trees.** Matches compile to case trees, which are exhaustive by (3).
- **Canonical names.** Modules flatten to canonical names (`M.N.f`), which
  `Module_scope` already produces.
- **Explicit quotes and splices.** Implicit splices carry their *may-capture*
  set: the binders of the current quote visible at the splice, plus "forward
  the rest outward". Which entries the code actually uses is not known
  statically.

Every backend consumes this IR. It is a standalone, testable module, and its
pretty-printed output makes good expect test output.

## Approach A: native proof kernel

stag gets its own logic and proof checker. Proof files are stag modules with
`theorem`, `lemma` and `termination` items.

### Kernel choice

- **HOL-style (LCF)**: classical higher-order logic. Its type system is HM
  with type constructors, which matches stag's value types once rows are set
  aside. Recursive definitions come with their equations and induction
  principles derived from the recursion scheme, as in Isabelle's `fun`.
  Proofs are tactic scripts that build `thm` values, and the syntax can still
  look like Lean. The kernel is a few hundred lines, but only if code types
  are encoded (for example contexts as an untyped `Env`). Putting rows and
  presence into the kernel's types makes it considerably larger.
- **Dependent type theory (Lean-like CIC)**: more expressive, with proofs as
  terms, dependent pattern matching and type classes. The kernel is much
  larger (universes, inductive families, definitional equality,
  elaboration). stag programs are not dependently typed, so most of that power
  would go unused by the implementation side.

HOL fits better. The only thing lost compared with Lean is proofs as
programs, which the goal of separate files already rules out.

### Implications

- **Readability**: one language. Names in proofs are exactly the stag names,
  with the same module resolution, and specification helpers are plain stag.
- **Staging**: the code model can be designed into the logic (see Staging),
  and that is the main advantage of this approach.
- **Cost**: high. On top of the prerequisites, it needs a parser, the kernel,
  a term elaborator, and a tactic engine with usable automation (`simp`,
  `induction`, `cases`, linear arithmetic, congruence closure). Without good
  automation, proofs get long and unreadable, so most of the effort goes
  there, not into the kernel. Realistically this is months of work.
- **Trust**: the kernel, plus the reflection from the IR, meaning that the
  generated equations agree with the evaluator.
- **Tooling**: an interactive goal view has to be built, unless an LSP is
  added.

## Approach B: export to Lean

A backend translates the core IR to Lean 4. Proofs are real Lean, in files
that import the generated ones.

```
list.stag                implementation
List/Lemmas.lean         handwritten: lemmas needed by termination proofs
_build/.../List.lean     generated: imports List/Lemmas, defines List.append ...
List/Proofs.lean         handwritten: import List; theorem append_nil ...
```

### Translation

- **Types.** Variants become `inductive` and records become `structure`.
  Types in a recursive group go in a `mutual` block, where records may have to
  become single-constructor inductives. Aliases are already expanded.
- **Modules** become namespaces. Lean has a single namespace, whereas stag
  keeps types, values and modules apart (a module may declare both a type `t`
  and a value `t`), and Lean places constructors under the type's namespace
  rather than the module's. So names need a deterministic mangling, which
  must also avoid Lean keywords (`fun`, `at`, `by`, `have`, `end`, ...).
  Proofs refer to the mangled names, so the mangling must be stable and
  predictable.
- **Polymorphism** becomes implicit type arguments
  (`def append {α : Type} : List α → List α → List α`).
- **SCCs** of real calls become `mutual` blocks.
- **Builtins.** `int` becomes `Int`. `char` and `string` map to Lean's
  Unicode `Char` and `String` only if stag adopts Unicode semantics; otherwise
  they map to byte types.
- **Termination.** Lean's structural recursion often succeeds without help.
  Otherwise Lean needs `termination_by` and `decreasing_by` at the definition
  site, which is in the generated file, so the generator copies them from a
  hints file. The lemmas those proofs use cannot live in `Proofs.lean`,
  because `Proofs.lean` imports the generated file. They go in a lemma file
  that the generated file imports, as in the layout above.
- **Non-terminating definitions.** Lean's `partial_fixpoint` is not a drop-in
  replacement. It requires tail recursion with a `Nonempty` result, or a
  monadic result in an order-enriched monad such as `Option`, which changes
  the type and therefore every call site. It gives an unfolding equation and
  partial-correctness induction only. The simple default is an `opaque`
  constant. A polymorphic `opaque` (`'a -> 'b`) needs `Nonempty` or
  `Inhabited` instance arguments, which also changes its signature.
- **Code types** are opaque at first. That makes every function that builds a
  quote opaque, and so is everything whose equations depend on those
  functions: in practice, all staged code is out of reach until a code model
  exists.

### Implications

- **Readability**: the proofs are idiomatic Lean, using mathlib, `simp`,
  `omega`, `grind` and the infoview, all of which is mature. Readers need to
  know Lean as well as stag, and the mangled names are a seam between the two.
- **Cost**: the lowest of the three. The core IR and a printer are enough to
  prove things about simple functional code.
- **Trust**: the translation becomes part of the TCB. It can silently diverge
  from stag's semantics, for example in evaluation order (once effects
  exist), integer or string semantics, or matching. Differential testing of
  the generated Lean (`#eval`) against the stag evaluator reduces that risk.
- **Workflow**: when an implementation changes, the Lean file is regenerated
  and the proofs are re-checked. Errors are reported in Lean terms, against
  generated code, far from the stag source. The project also depends on a Lean
  toolchain, which is acceptable via nix.
- **Staging**: awkward (see below).
- **Stepping stone**: the IR, the termination information and the reflection
  rules are exactly what A needs, so B does not waste effort even if A is
  built later.

## Approach C: refinement types with SMT

Liquid Haskell-style. A spec file gives refinement signatures, and the
resulting verification conditions are discharged by an SMT solver (z3 or
cvc5).

```ocaml
(* sort.spec.stag *)
val sort : l:int list -> { r : int list | sorted r && perm l r }
```

### Implications

- **Readability**: the specs are short and sit beside the signature. Most
  proofs need no text at all. Harder proofs are lemmas written as ordinary
  stag functions with refinement types, using refinement reflection and PLE
  (Proof by Logical Evaluation), so even the proofs are in the regular
  language. Reflected functions must pass the same termination check.
- **Fit**: refinement checking works as a separate pass after HM, on the IR.
  Refinements on context entries would act as preconditions on a quote's free
  variables (`<int>{x : {v : int | v > 0}}`), which is a neat way to state
  what a generator assumes.
- **Limits**: SMT is a large trusted component and unpredictable: proofs can
  start timing out after small changes. Quantifiers and induction are weak, so
  anything beyond decidable arithmetic and data properties falls back to
  manual lemmas, written in a style that is not Lean-like. Expressiveness is
  capped by what the SMT encoding supports.
- **Cost**: moderate. It needs a VC generator, an SMT encoding of datatypes
  and measures, and refinement inference (liquid inference using qualifiers).

C fits better as an automation tactic inside A (an `smt` tactic, like
Isabelle's sledgehammer, where the solver's answer is replayed or trusted)
than as the whole system.

## Staging

The eventual goal is to prove properties of generated code, assuming the
primitives from prerequisite 1 and a `nat` type:

```ocaml
let rec power (n : nat) x =
  match n with
  | Zero -> <1>
  | Succ n -> < ~x * ~(power n x) >

theorem power_correct (n : nat) (v : int) :
    ⟦power n <x>⟧ {x = v} = v ^ n
```

Here `⟦c⟧ env` is the denotation of the code `c` in an environment `env` that
provides its context, and `^` is logic-only.

### Models of code

- **Shallow**: `<t>{ctx}` means `Env -> t`, where `Env` maps names to values.
  - A quote denotes a function of the environment.
  - An explicit splice `~{x = e} c` denotes `fun env -> ⟦c⟧ {x = ⟦e⟧ env}`.
    The environment is fresh, because explicit splices close the context.
  - An implicit splice denotes `fun env -> ⟦c⟧ (env' ++ env)`, where `env'`
    holds the current quote's binders that are in its may-capture set, and the
    rest of `env` is forwarded outward.
  - Persistent values embedded in code are constants of the denotation.

  Because contexts are label-indexed and splicing is name-based, an
  environment keyed by name fits stag more closely than de Bruijn indices or
  typed HOAS. Entries whose presence is `Absent` are simply never read. Row
  variables become polymorphism over the rest of the environment, or an
  untyped `Value` universe with a typing invariant (simplest in HOL). That
  invariant holds only if the context typing is sound, which is what the
  property tests in prerequisite 2 back up. This model supports functional
  correctness only.
- **Deep**: code values are syntax trees, `⟦_⟧` is defined over them, and the
  model is proved adequate with respect to the evaluator. This is needed for
  properties of the code itself, such as size, operation count ("`power 3`
  generates exactly three multiplications") or the absence of residual
  splices. Persistent values appear as opaque embedded constants, so those
  syntactic properties only cover the parts that are syntax. The deep model
  also needs binder handling: names are enough, together with the
  `let`-insertion rule for explicit splices.

Equality of code in theorems is extensional (`⟦c1⟧ = ⟦c2⟧`). Syntactic
equality is available only in the deep model, through explicit functions on
syntax. Termination of generated code is a property of the code, proved
separately (see prerequisite 4).

### Per approach

- **A**: `⟦_⟧`, `Env` and the code type are built into the logic, so both
  models are possible, starting with shallow. Using the untyped `Env` keeps
  the kernel small.
- **B**: shallow code becomes `Env → t` with `Env := String → Value` and a
  `Value` inductive, which makes it untyped and loses Lean's type checking of
  generated code. A typed deep embedding (intrinsically typed syntax indexed
  by contexts) struggles with row variables and presence variables. This
  works, but is clumsy.
- **C**: refinements on context entries and on results give pre- and
  postconditions for generators, but relating code to its denotation needs
  `⟦_⟧` in the SMT encoding, which quantifier-heavy proofs make fragile.

## Comparison

|                       | A: native (HOL)          | B: Lean export             | C: refinements + SMT |
|-----------------------|--------------------------|----------------------------|----------------------|
| Effort                | high                     | low                        | moderate             |
| Trusted base          | small kernel, reflection | Lean, translation          | SMT solver, VC gen   |
| Expressiveness        | full HOL                 | full Lean, mathlib         | decidable fragments  |
| Proof readability     | Lean-like, stag names    | idiomatic Lean, mangled names | specs only, mostly |
| Automation            | must be built            | excellent                  | excellent, brittle   |
| Staged code           | natural                  | clumsy                     | limited              |
| External dependencies | none                     | Lean toolchain             | SMT solver           |

## Recommendation

Build the shared foundation first, use Lean export to get value early, and
only build a native kernel if proofs about staged code become central.

1. Primitives (including a lazy conditional and `nat`), and decide what `int`,
   `char` and `string` mean.
2. Reference evaluator for the pure fragment, following the splice rules
   above, with expect tests and "well-typed programs don't get stuck"
   property tests.
3. Exhaustiveness checking, plus the positivity and inhabitation check.
4. Termination checking over a level-aware call graph: structural recursion
   first, with measures from the proof file later. This gives the reflected
   and opaque classification.
5. The core IR, with explicit types and instantiations, defaulted weak
   variables, lambda-lifted `fix` groups, case trees, canonical names and
   may-capture sets. Pretty-printed in expect tests.
6. Lean backend: types, definitions, namespaces with mangling, the lemma,
   generated and proof file layout, and code types opaque. Differential tests
   of the generated Lean against the evaluator.
7. Re-evaluate. If staging proofs matter by then, build A with a HOL-style
   kernel over the same IR: shallow code model first, then deep, with SMT as
   an optional tactic.

## Open questions

- Unbounded `int` everywhere, or machine integers at runtime? Bytes or
  Unicode for `char` and `string`?
- The proof file's name and syntax: `foo.proof.stag` with stag syntax plus
  `theorem`, or Lean files for B.
- Once signatures exist, should proofs see hidden items? (Proposed: yes, since
  proofs are about the implementation. Theorem statements in signatures are
  what clients see.)
- Proof maintenance: detect stale proofs by hashing the IR of the definitions
  each theorem depends on, so that only affected proofs are re-checked.
- Should the evaluator wait for the level-crossing changes in `modes.md`?
- How effects, once added, enter the logic: opaque only, or monadic models.
