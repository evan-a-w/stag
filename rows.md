# Polymorphic records and variants

Structural records and variants, typed by rows shared with code contexts.

## Rows

`Type.context` becomes `Type.row`, used by three type formers:

```ocaml
| TCode of { inner : ty; context : row }   (* <t>{x : int; ..'r} *)
| TRecord of row                            (* {x : int; ..'r} *)
| TVariant of row                           (* [`A of int | `B; ..'r] *)
```

Rows keep their current semantics everywhere: labels are scoped (Leijen),
so the first entry for a label shadows later ones, entries have a presence,
and row variables may have bounds. Unification, rewriting and bounds are
shared. There is one row kind: a row variable or parameter (`..r`) may be
used in any of the three positions. Record and variant operations only create
`Present` entries and never bounds; presence variables and bounds reach those
rows only through a row variable shared with a context.

`Syntax.Type_expr.context` becomes `row`, `Type_decl.Param.Context` and
`Type_expr.arg`'s `Context` become `Row`, `Type.AContext` becomes `ARow`, and
errors are renamed to match. `Type_expr.t` gains `Record of row` and
`Variant of row`; a variant entry is a tag and its single argument type
(`unit` for a nullary tag). A label appears at most once in an annotation
(`Duplicate_row_entry`), as before for contexts.

Polymorphic labels are bare strings: no module resolution, no declarations,
and they never become free variables of quotes. Aliases cover naming them:
`type point = {x : int; y : int}`. Nominal records and constructors are
unchanged.

## Records

| expression | type |
|---|---|
| `{x = a; y = b}` | `{x : 'a; y : 'b}`: closed; `{}` is the empty record |
| `e.x` | `{x : 'a; ..'r} -> 'a` (the first `x`) |
| `{x = v; ... \| e}` extension | `{..'r} -> {x : 'a; ...; ..'r}`: shadows, no constraint |
| `e \ x` restriction | `{x : 'a; ..'r} -> {..'r}`: removes the first `x` |
| `{e with x = v; ...}` update | `{x : 'a; ..'r} -> {x : 'b; ..'r}`: requires `x`, may change its type |

Update is `{x = v | e \ x}`, so a shadowed `x` is untouched. Labels must be
distinct within one literal, extension or update (`Duplicate_row_entry`);
shadowing is only by nesting extensions.

## Variants

- `` `A e `` : `` [`A of 'a; ..'r] ``; `` `A `` alone has argument `unit`.
- A variant pattern `` `A p `` at type `t` unifies `t` with
  `` [`A of t_p; ..'r] `` (rewriting `t` to have `` `A `` first).
- A match whose cases have no top level catch-all (a `Var` or `Any`
  pattern) but some top level variant pattern closes the scrutinee's row to
  the cases' tags: the scrutinee is unified with `` [`A of ta | `B of tb] ``
  (each tag once). Unifying with a row built from the tags, rather than
  closing the tail of the scrutinee's row, matters: the scrutinee may already
  have entries of its own, so `` match `C with `A -> e `` is an error rather
  than `` [`A | `C] ``. Nested variant patterns stay open. An empty match,
  `match e with`, requires `e : []` and has any type, unless the scrutinee's type is
  already known (after checking it) to be a declared variant type with no
  constructors, such as `type void = |` (or an alias of one, as aliases are
  expanded), which is then kept: `let absurd (x : void) = match x with` is
  `void -> 'a`. Builtins are abstract, so `match (1 : int) with` is an error.
  This is order dependent: in `let f x = match x with`, `x` is not yet known
  to be `void`, so `f : [] -> 'a` and applying `f` to a `void` is an error.
- Residuals: a top level `Var` pattern gets the scrutinee's type with, for
  each tag handled by an earlier case `` `A p `` with `p` irrefutable, the
  first entry for that tag removed (once per tag). Irrefutable: `Var`, `Any`,
  tuples, and record patterns of irrefutable patterns (nominal variants with
  one constructor are not special cased, nor is the literal `()`); a nullary
  `` `A `` (no argument pattern) is handled irrefutably. A refutable `` `A p `` leaves
  `` `A `` in the residual.

```ocaml
let handle_a e k = match e with `A n -> n | other -> k other
(* [`A of int; ..'r] -> ([..'r] -> int) -> int *)
```

- Closing is stricter than residuals: closing gives each tag one entry, so
  on `` v : [`A of int | `A of bool] `` (e.g. `type t[{..'r}] = [`A of int;
  ..'r]` at `t[{A : bool}]`), `` match v with `A n -> e `` is rejected, while
  `` match v with `A n -> e | other -> (match other with `A b -> e') `` is
  accepted: the residual removes the first `` `A `` and the inner match
  closes the rest.

No embedding operation, so an inner duplicate tag is never inhabited.
No exhaustiveness checking beyond what closing gives.

## Record patterns

- `{x = p; y = q}`: closed.
- `{x = p; _}`: open, `{x : t_p; ..'r}`.
- `{x = p | rest}`: `rest : {..'r}`, the restriction of the matched record
  (a shadowed `x` is visible in `rest`).

These are `Pattern.Poly_record { fields; rest }` with `rest` a
`Pattern.Rest.t`: `Closed`, `Open` or `Bind name`.

Labels are distinct within one pattern.

## Recursion

Structural types are equi-recursive, with OCaml's default semantics for
polymorphic variants and objects:

```ocaml
let len l = match l with `Nil -> 0 | `Cons (_, t) -> add 1 (len t)
(* ([`Cons of 'a * 'b | `Nil] as 'b) -> int *)
```

- Types are graphs. Only variables are mutable, so every cycle passes
  through a `Link`, and it must also pass through a `TRecord` or `TVariant`
  (it is guarded). The occurs check (`Unify.adjuster`, via `Type.iter_ty`'s
  `guarded`) only rejects a variable reached outside any record or variant
  of the type it is bound to: `'a = 'a -> int`, `'a = 'a * int`,
  `'a = 'a list`, `'a = <'a>{}`, `'a = <int>{x : 'a}` and a row variable in
  its own row's spine (or a context or type argument there) are still
  `Occurs_check_failed`, while `'a = {x : 'a list}`,
  `` 'a = [`A of 'a -> int] `` and `r = x : {..r}` are cycles.
- Unification is coinductive: it keeps the pairs of (physical) records and
  variants it has unified or is unifying (in a hash set), and assumes a pair
  equal when it is reached again. Unification only creates rows and variables, never
  records or variants, so the pairs are finitely many and it terminates.
  Rewriting and row unification follow row spines, which are acyclic.
- Traversals terminate, preserve sharing and take linear time.
  `Type.iter_ty` visits each `Link` at most once unguarded and once
  guarded. `Type.map_ty` (instantiation) memoizes on `Link`s and on records
  and variants, closing the copy of a cycle with a new `Link`, and keeps the
  parts with nothing to instantiate as they are: whether a `Link` reaches a
  variable to instantiate is computed once per `Link`, by strongly connected
  components, as a cycle has something to instantiate if any part of it
  does. Generalization marks variables in place. Records and variants are
  identified (`Type.Node`) physically or by their (physical) canonical row,
  as `TRecord (RVar r)` may be built more than once for one `r`, and tables
  of them hash their first few immutable nodes, with variables by id.
- Known limitation: equal cycles built separately stay distinct, as
  unification does not merge equal nodes, so they may print differently:
  after unifying two separately built cyclic types, one may print unrolled,
  e.g. `` [`Cons of int * ([`Cons of int * 'a | `Nil] as 'a) | `Nil] ``, as
  may the aliases of a mutually recursive clique (see below).
- Printing: `` ([`Cons of int * 'a | `Nil] as 'a) ``, always parenthesized.
  The name is allocated where it first appears (usually within the body),
  like other variables of the printer, and is weak (`'_a`) if the type has a
  weak variable. Later occurrences of the same node print as its name. Sexps
  print `(Cycle n ...)` around a record or variant reached again within
  itself, and `(Cycle_ref n)` there.

### Recursive aliases

An alias may refer to itself, or to aliases referring back to it, through
records and variants:

```ocaml
type 'a l = [`Nil | `Cons of 'a * 'a l]
type 'a a = {b : 'a b; v : 'a}   type 'b b = [`End | `More of 'b a]
type tree = [`Leaf of int | `Node of tree list]
```

- An expansion of an alias ties the knot: a reference back to an alias
  being expanded becomes a variable linked to the expansion's body once
  elaborated. Within one elaboration (an annotation, or a declaration with
  its own parameters), each alias is expanded once per (physical)
  arguments, and further references share the result (or the knot), so a
  mutually recursive clique of n aliases takes n expansions, not one per
  path. Separate elaborations have their own cycles.
- The resulting cycle must be guarded, which is judged on the expansion
  (not where the reference appears, as the alias's body may place its
  arguments in a record or variant): it is `Cyclic_type_alias` if the knot
  is reached from the expansion outside any record or variant
  (`type t = t`, `type t = t * int`, `type t = t list`, `type t = t id`,
  `type t = <int>{x : t}`), while `` type 'a f = [`A of 'a] `` and
  `type g = g f` give `` type g = [`A of g] ``, as in OCaml.
- Recursion must be regular: the reference back must have the arguments of
  the expansion in progress, i.e. (as declarations are elaborated with their
  own parameters) the alias's own parameters in order, checked physically.
  Otherwise it is `Non_regular_recursive_alias` (`` type 'a t = [`A of ('a *
  'a) t] ``, `` [`A of int t] ``, swapped parameters, or a different row
  argument), so expansion terminates.
- `Signature` keeps the expanded body of an alias, a cyclic type, and prints
  occurrences of the whole body within it as the alias:
  `` type l['a] = [`Cons of 'a * l['a] | `Nil] ``, and `type ints = int l` is
  `` type ints = [`Cons of int * ints | `Nil] ``. Other aliases of a clique
  are not printed by name, but each appears once, in the `as` form:
  `` type a0 = [`T0 of a0 | `T1 of ([`T0 of a0 | `T1 of 'a] as 'a)] ``. Uses of an alias elsewhere,
  as in values and other declarations, are expanded, so print in the `as`
  form: `` val nil : ([`Cons of bool * 'a | `Nil] as 'a) ``.

## Values

Record literals, extension, restriction, update, field access and variant
injection of values are values (as for nominal records and constructors).

## Printing

`{x : int; ..'a}`, `` [`A of int | `B; ..'a] `` (a `unit` argument prints as
a nullary tag), `{}` and `[]` when closed and empty, `[..'a]` when open and
empty. Absent and unknown presence entries print as in contexts (`-x`,
`x?'p : t`; `` -`A ``, `` `A?'p of t `` in variants). Row arguments of type
constructors and bounds print in the record/context form `{...}`.
