# User-defined types and modules

Design for user-defined sum/product types, type aliases and (simple) modules.

## Surface types: `Syntax.Type_expr`

Annotations and declarations are written as surface types, with variables by
name, then elaborated to `Type.ty`. This replaces `Type.ty` in all annotations
(`Syntax.Binding.t`, `Let`, `Toplevel.Let`).

```ocaml
module Type_expr : sig
  type t =
    | Var of string                              (* 'a *)
    | Con of Path.t * arg list                   (* int, M.t, 'a list *)
    | Arrow of t * t
    | Tuple of t list
    | Code of { inner : t; context : context }   (* <t>{...} *)
  and context =
    | Empty                                      (* closed *)
    | Extend of string * t * context             (* x : t; ... (Present) *)
    | Var of string                              (* ..r *)
  and arg = Ty of t | Context of context
end
```

Elaboration:

- In annotations, type and context variables are unification variables scoped
  over the enclosing toplevel definition (as in OCaml): the same name within a
  toplevel is the same variable, created fresh on first use at the current
  rank.
- In declarations, variables must be declared parameters
  (`Unbound_type_variable`) of the right kind, `Ty` vs `Context`
  (`Type_variable_kind_mismatch`).
- A label appears at most once in one context (`Duplicate_context_entry`).
- Builtins `int`, `bool`, `string`, `char`, `unit` are nominal types without
  constructors declared in the root module (so `Type.int` is `TCon ("int", [])`
  as before): redeclaring one at the root is `Duplicate_type`, while `M.unit`
  is a distinct type that shadows the builtin within `M`.
- Arity and kind of arguments are checked (`Type_arity_mismatch`,
  `Type_arg_kind_mismatch`).
- Aliases are expanded during elaboration, so never appear in `Type.ty`.

## Declarations: `Syntax.Type_decl`

```ocaml
module Type_decl : sig
  module Param : sig type t = Ty of string | Context of string end
  module Kind : sig
    type t =
      | Variant of (string * Type_expr.t list) list  (* Cons of 'a * 'a list *)
      | Record of (string * Type_expr.t) list
      | Alias of Type_expr.t
  end
  type t = { name : string; params : Param.t list; kind : Kind.t }
end
```

- `Toplevel.t` gains `Type of Type_decl.t`.
- Declarations are order independent and may be (mutually) recursive, like
  `let` toplevels. They are all checked before any `let`.
- Variant and record types are nominal: `Type.TCon (canonical name, args)`.
  The canonical name is the full module path joined with `.` (e.g. `"M.t"`;
  just `"t"` at the root).
- Aliases are structural (expanded). Cyclic aliases are an error
  (`Cyclic_type_alias`); recursion through a variant or record is fine.
- Constructor and field names are unique within a module
  (`Duplicate_constructor`, `Duplicate_field`); type names likewise
  (`Duplicate_type`). Parameters must be distinct.
- Constructors take a list of arguments (arity checked, no intermediate
  tuple).
- Context parameters let data hold open code, e.g.
  `type ..r stmt = Stmt of <unit>{..r} * ..r stmt list`.

## Expressions and patterns

```ocaml
(* Expr gains *)
| Tuple of t list
| Construct of Path.t * t list        (* not a function: a value *)
| Record of (Path.t * t) list         (* all fields, exactly once *)
| Field of t * Path.t
| Match of t * (Pattern.t * t) list

module Pattern : sig
  type t =
    | Any
    | Var of string
    | Lit of Lit.t
    | Tuple of t list
    | Construct of Path.t * t list
    | Record of (Path.t * t) list     (* any subset of the fields *)
end
```

- Records and fields are resolved by field name (unique within a module, so no
  type-directed disambiguation). Construction must give every field exactly
  once (`Missing_field`, `Duplicate_field`); fields must belong to the same
  type (`Fields_of_different_types`).
- Each use of a constructor/field instantiates the declaration's parameters
  fresh at the current rank.
- Patterns bind variables; a variable bound twice in one pattern is
  `Duplicate_pattern_variable`. Bindings go through `Env.with_bindings` like
  `let`, so in quotes they are quote-local and captured by splices like any
  other binder.
- Constructors and fields are not in `Env`, exist at every stage, and so never
  become free variables of quotes.
- No exhaustiveness or redundancy checking yet.

## Generalization: syntactic values

Generalization is decided by whether the bound expression is a syntactic value
(the relaxed value restriction), instead of "has arguments":

- values: literals, variables, functions (`let` with arguments), quotes whose
  level 0 splices (those run when the quote is built: inside no deeper quote)
  splice values, `Construct`/`Tuple`/`Record` of values, `Field` of a value,
  `let x = v1 in v2` where `v1` does not refer to `x`,
  `match v with p -> v | ...`
- not values: applications, splices

Local `let`s are recursive, so like OCaml's `let rec`, a recursive non-function
is not generalized: `let x = e1 in e2` is generalized only if `e1` does not
refer to `x`. This is over-approximated: a splice with implicit substitutions
may refer to any name in scope.

Level crossing (`Env.Entry.persistent`) is unchanged: still only functions.
Modes will replace both checks (see `modes.md`). For an SCC of toplevels,
generalize iff all members are values and, if any member is not a function,
the SCC is not recursive (it has more than one member, or one referring to
itself); persistent iff all are functions.

## Modules

No functors or signatures yet.

```ocaml
(* Toplevel gains *)
| Module of string * Toplevel.t list
```

- `Path.t` is a non-empty list of names, e.g. `["M"; "N"; "x"]`. Variables,
  types, constructors and fields are referred to by paths; local variables are
  single names.
- Resolution is lexical. A single name `x` is looked up in: local bindings,
  then the members of the current module, then of each enclosing module out
  to the root. A path `M.rest` resolves `M` the same way (as a module), then
  looks `rest` up in `M`'s members only.
- Members of all modules are order independent, as toplevels are now; checking
  on demand and SCC detection work across modules (keyed by canonical path).
- Module names are unique within a module (`Duplicate_module`).
- Resolution is implemented in `Module_scope`, which returns canonical names;
  types, constructors, fields and toplevels are stored by canonical name.
  `Signature`'s printer mirrors the lookup rules to choose relative names.
- Duplicate members (`Duplicate_toplevel`, `Duplicate_type`,
  `Duplicate_constructor`, `Duplicate_field`, `Duplicate_module`) are per
  module and named canonically. A module and a member of another kind may
  share a name.
- `typecheck` returns the program's `Signature.t`: its items (values with
  their schemes, checked type declarations and modules) mirroring the program,
  with unqualified names. Builtin types are not items.
- `Signature.to_string_hum` names each type constructor relative to the module
  of the item it is printed in: by the shortest suffix of its canonical name
  that resolves back to it under the lexical rules above (`M.N.t` is `t` in
  `M.N`, `N.t` in `M`, `M.N.t` at the root). If none does, e.g. a root `t`
  (or builtin `int`) inside a module declaring its own `t` (`int`), or `N.t`
  inside a module with its own `N`, it is the canonical name after a leading
  `.` standing for the root: `.t`, `.int`, `.N.t`. The printer reconstructs
  the scopes from the signature itself. Errors and `Typecheck.to_string_hum`
  keep canonical names.
- Paths in errors are as written: `Unbound_module` (the prefix up to the
  missing module), `Unbound_type`, `Unbound_constructor`, `Unbound_field`,
  `Not_in_scope` for values, and the type in type argument errors. Other
  names in errors are canonical.
- A qualified path whose last name is missing is an error even inside a
  quote: only single names may become free variables.

### Toplevels in quotes

Toplevels are still not in `Env`, so implicit splices never capture them.
Persistent toplevels are usable in quotes by any path. A non-persistent
toplevel used in a quote becomes a free variable of the quote labelled by its
**canonical** name, however it was written: inside `M`, `n` and `M.n` give the
same entry `M.n`, which a binder `n` of an enclosing quote does not capture.
Labelling by the path as written would make the entry depend on where the code
was built, so splicing it elsewhere would find a different value or none.

At a level 0 splice, an entry is provided by the local of that name, else the
toplevel of that canonical name (as before, for root toplevels). Locals are
single names, so only the toplevel provides an entry such as `M.n`. The
explicit substitution shorthand `{l}` stands for `{l = l}`, with the right
`l` read as a path and resolved lexically.
