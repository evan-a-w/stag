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
  `match e with`, has type `[] -> 'a`.
- Residuals: a top level `Var` pattern gets the scrutinee's type with, for
  each tag handled by an earlier case `` `A p `` with `p` irrefutable, the
  first entry for that tag removed (once per tag). Irrefutable: `Var`, `Any`,
  tuples, and record patterns of irrefutable patterns (nominal records with
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

Structural types can't be recursive: there are no equi-recursive types, so
the occurs check rejects `{x : 'a; ..'r}` with `'a` that record (and
likewise for variants and rows, `Cyclic_row`), and an alias whose body
refers to itself, through records and variants too, is
`Cyclic_type_alias`. Recursion goes through a nominal wrapper:
`` type t = T of [`Nil | `Cons of int * t] ``.

## Values

Record literals, extension, restriction, update, field access and variant
injection of values are values (as for nominal records and constructors).

## Printing

`{x : int; ..'a}`, `` [`A of int | `B; ..'a] `` (a `unit` argument prints as
a nullary tag), `{}` and `[]` when closed and empty, `[..'a]` when open and
empty. Absent and unknown presence entries print as in contexts (`-x`,
`x?'p : t`; `` -`A ``, `` `A?'p of t `` in variants). Row arguments of type
constructors and bounds print in the record/context form `{...}`.
