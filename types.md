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
- In declarations, variables must be declared parameters of the right kind
  (`Ty` vs `Context`), otherwise `Unbound_type_variable`.
- Builtins `int`, `bool`, `string`, `char`, `unit` stay as they are
  (`Type.int` etc.), resolved when no user type of that name is in scope.
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

- values: literals, variables, functions (`let` with arguments), quotes,
  `Construct`/`Tuple`/`Record` of values, `Field` of a value,
  `let x = v1 in v2`, `match v with p -> v | ...`
- not values: applications, splices

Level crossing (`Env.Entry.persistent`) is unchanged: still only functions.
Modes will replace both checks (see `modes.md`). For an SCC of toplevels,
generalize iff all members are values; persistent iff all are functions.

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
- `typecheck` returns every `let` with its canonical path, in program order.
- Errors name canonical paths: `Unbound_module`, `Unbound_type`,
  `Unbound_constructor`, `Unbound_field`, and `Not_in_scope` for values.
