# Syntax

The concrete syntax of stag. The parser is a hand written recursive descent
parser (no parser generators), producing `Syntax` with a `Loc.t` on every
node, for good errors.

## Lexical

- Comments `(* ... *)`, nesting, as OCaml.
- Literals: integers of any size (digits, kept as a string), `true`, `false`,
  chars `'c'`, strings `"s"` with OCaml escapes (`\n \t \\ \" \' \ddd`), `()`.
- `lowercase_ident`: values, types, fields, type params. `Capitalized_ident`:
  constructors and modules. `` `Tag ``: polymorphic variant tags and
  polymorphic record fields (both are structural labels). `'a`: type
  variables.
- Paths: `M.N.x`, `M.t`, `M.C`. A path is a sequence of capitalized names
  followed by the final name; `r.x` with lowercase `r` is a field access.
- Keywords: `let in match with type of module struct end`, `true false`.
- `@` and `@@` are reserved (for modes, see below).
- Staging tokens: `.<` `>.` `.~`.

## Types

As printed by `Type.to_string_hum`, so printed types parse back:

```
int    list['a]    M.t    with_x[{..'a}]        (* args postfix in brackets *)
'a -> 'b * 'c                                    (* -> right assoc, * n-ary *)
{`x : int; `y : bool; ..'r}   {..'r}   {}       (* polymorphic record *)
[`A of int | `B; ..'r]   []                      (* polymorphic variant *)
<'a>{x : int; ..'r}                              (* code type and context *)
```

A type argument in brackets is a type, or a row `{...}` (record-style row,
used for row parameters). Weak variables (`'_a`) and `where` constraints are
printed but never written.

## Expressions

```ocaml
let f x (y : int) : t = e in body   (* local lets are always recursive, no `rec` *)
e1; e2                              (* sugar for let _ = e1 in e2 *)
match e with | p -> e | ...         (* leading | optional *)
f x y                               (* application, left assoc *)
(x : t)                             (* annotated variable: Expr.Var's annotation *)
(a, b, c)   ()   (e)
C   C e   C (a, b)                  (* constructor args are n-ary: C (a, b) has 2 *)
{x = 1; M.y = 2}   r.x   r.M.x      (* nominal records *)
{`x = 1; `y = true}   r.`x          (* polymorphic records *)
{`x = v; `y = w | r}                (* extend *)
r \ `x                              (* restrict *)
{r with `x = v}                     (* update *)
`A   `A e                           (* polymorphic variant *)
.< e >.                             (* quote *)
.~e   .~(f x)                       (* splice: all in scope (subs = None) *)
.~e{}                               (* explicit, nothing provided (Some []) *)
.~e{x = e1; y}                      (* explicit; `y` alone is a variable: (y, None) *)
```

Splice binds tighter than application (`.~f x` is `(.~f) x`). A splice outside
any quote is a toplevel (compile-time) splice.

`match` arms extend as far as possible, as in OCaml (wrap nested matches in
parentheses).

## Patterns

```ocaml
_   x   1   'c'   "s"   true   ()   (p, q)
C   C p   C (p, q)
{x = p; M.y = q}                    (* nominal *)
`A   `A p
{`x = p}   {`x = p; ..}   {`x = p; ..rest}   (* closed, open, bind rest *)
```

## Declarations

```ocaml
let f x y = e                          (* toplevels are order independent *)
type nat = Z | S of nat
type list['a] = Nil | Cons of 'a * list['a]
type with_x[{..'r}] = {`x : int; ..'r} (* alias *)
type point = {x : int; y : int}        (* nominal record *)
type t                                 (* abstract *)
module M = struct ... end
```

## Files

Files are modules, as in OCaml: `foo_bar.stag` is the module `Foo_bar`,
containing the file's declarations. A program is a set of files, and since
toplevels in any module may refer to each other in any order, files may refer
to each other in any order too.

## Later

### Declaration quotes

A quote of declarations is quoted module code:

```ocaml
let make_ops zero = .< struct
    type t = A | B of int
    let f x = match x with A -> .~zero | B y -> y
  end >.
(* make_ops : <int>{..} -> <sig type t = A | B of int; val f : t -> int end>{..} *)

module Ops = .~(make_ops .<0>.)     (* splice as a module *)
include .~(make_ops .<0>.)          (* splice into the current module *)
let module M = .~e in body          (* inside expressions, including quotes *)
```

Open questions: contexts with type, constructor and module entries;
generativity (splicing twice gives distinct types); type code (`<type>`).

### Modes

OxCaml style:

```ocaml
let n @ const = 3
let f (x @ portable) = ...
(e : t @ m)
t @ m -> t' @ m'          (* @ binds looser than *, tighter than -> *)
val n : int @@ const      (* modalities on signature items and fields *)
<t @ m>{ctx}              (* code modes *)
```

### Other

`fun`, `if`, infix operators (`let (+) x y = ...`, OCaml precedences),
interface files.
