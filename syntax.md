# Syntax

The concrete syntax of stag. The parser is a hand written recursive descent
parser (no parser generators), producing `Syntax` with a `Loc.t` on every
node, for good errors.

## Lexical

- Comments `(* ... *)`, nesting, as OCaml.
- Literals: integers (OCaml's native `int`; a literal out of its range is a
  lexing error; digits may be separated by `_`, as in `1_000`, but not lead
  with one; a `-` directly before a digit makes it negative, as in `-1`, in
  expressions and patterns), `true`, `false`, chars `'c'`, strings `"s"`,
  quoted strings `{|raw|}` and `{id|raw|id}` (no escapes; `id` is lowercase
  letters and `_`), `()`. `-` is otherwise not a token: there is no binary
  minus, so `f -1` and `x-1` are `f (-1)` and `x (-1)` (write `Int.sub`).
  Printed values parenthesize negative arguments, `Some (-1)`, as OCaml does,
  so they will still parse once infix operators follow OCaml.
- Escapes, in chars and strings: `\n \t \r \b`, `\ ` (space), `\\ \" \'`,
  `\ddd` (3 decimal digits, at most 255) and `\xhh` (2 hex digits). In
  strings, a `\` at the end of a line skips the newline and the spaces and
  tabs that start the next line.
- `lowercase_ident`: values, types, fields, type params. `Capitalized_ident`:
  constructors and modules. `` `Tag ``: polymorphic variant tags and
  polymorphic record fields (both are structural labels). `'a`: type
  variables.
- Paths: `M.N.x`, `M.t`, `M.C`. A path is a sequence of capitalized names
  followed by the final name; `r.x` with lowercase `r` is a field access.
- Keywords: `let rec in match with type of module struct end`, `true false`.
  `rec` is reserved, as lets are always recursive.
- `@` and `@@` are reserved (for modes, see below).
- Staging tokens: `.<` `>.` `.~`, and `.{` (a splice's substitutions).
- Expect test tokens: `let%expect_test`, `[%expect`.

## Types

As printed by `Type.to_string_hum`, so printed types parse back:

```
int    list['a]    M.t    with_x[{..'a}]        (* args postfix in brackets *)
'a -> 'b * 'c                                    (* -> right assoc, * n-ary *)
{`x : int; `y : bool; ..'r}   {..'r}   {}       (* polymorphic record *)
[`A of int | `B; ..'r]   [..'r]   []             (* polymorphic variant *)
<'a>{x : int; ..'r}                              (* code type and context *)
```

A type argument in brackets is a type, or a row `{...}` (record-style row,
used for row parameters). A `{...}` argument always parses as a row; given to
a type parameter (rather than a row parameter) it is the record type of that
row (so `list[{}]` is a list of empty records). Rows are of polymorphic
records, so their labels are written with backticks (`` with_x[{`y : int}] ``),
everywhere except in the context of a code type, whose entries are variables,
written without (`<int>{x : int}`). Type arguments are written once, after the
name (`list[int]`, not `int list` or `list[a][b]`). A constructor's argument of
function type is parenthesized: `C of (int -> int)`. Weak variables
(`'_a`), presence variables (`` `x?'a ``), absent labels (`-x`), `as` cycles and
`where` constraints are printed but never written.

## Expressions

```ocaml
let f x (y : int) : t = e in body   (* lets are always recursive: no `rec` *)
e1; e2                              (* sugar for let _ = e1 in e2 *)
match e with | p -> e | ...         (* leading | optional; arms are e1; e2 level *)
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
.~e.{}                              (* explicit, nothing provided (Some []) *)
.~e.{x = e1; y}                     (* explicit; `y` alone is a variable: (y, None) *)
type t   type list[int]   type (a * b)  (* a type as a value, see Reflection *)
```

Splice binds tighter than application (`.~f x` is `(.~f) x`), and looser than
field access (`.~r.x` is `.~(r.x)`). The substitutions `.{...}` follow the
spliced expression (`.~r.x.{y}` is `.~(r.x)` providing `y`), and are only
written after a splice; `.~f {x = 1}` applies `.~f` to a record. A splice
outside any quote is a toplevel (compile-time) splice.

Precedence, loosest first:

| Construct                                  | Associativity      |
| ------------------------------------------ | ------------------ |
| `let ... in`, `match` (extend right)       |                    |
| `e1; e2`                                   | right              |
| `e1, e2`                                   | n-ary              |
| `C e`, `` `A e ``, `type t`               | right (`C D x`)    |
| application `f x`                          | left               |
| splice `.~e`, `.~e.{...}`                  | prefix             |
| `e.x`, `e.M.x`, `` e.`x ``, `` e \ `x ``   | left, postfix      |

`let` and `match` may also start a tuple component (`a, let x = 1 in x, y` is
`(a, let x = 1 in (x, y))`). Restriction is postfix like field access, so
`` f r \ `x `` is `` f (r \ `x) ``. Record field values, extended records (after `|`)
and substitutions are at the tuple level (`{x = 1, 2}`), and a `let` or `match`
there ends at the next `;` rather than sequencing (`{x = let a = 1 in a; y = 2}`
has two fields; parenthesize to sequence); the record of an update
`{e with ...}` is at the splice level. Trailing `;` is allowed in `{...}`.
`C (a, b)` and `C ((a, b))` both have two arguments. A parenthesized
expression's location includes its parentheses.

`type` takes an atomic type: a name, possibly applied (`type M.t[int]`), or a
parenthesized type (`type (int -> t)`); as an argument it is parenthesized
(`f (type t)`).

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
type void = |                          (* empty variant *)
type t                                 (* abstract *)
module M = struct ... end
module M = .~e                         (* declaration splices, see below *)
include .~e
```

## Primitives

Built-in functions, implemented by the interpreter, available in every program
at any level (in quotes and at compile time too, like persistent toplevels):

```ocaml
print_string : string -> unit      print_endline : string -> unit
print_int : int -> unit            print_char : char -> unit
Int.add sub mul div rem : int -> int -> int     Int.neg : int -> int
Int.equal lt le gt ge : int -> int -> bool      Int.compare : int -> int -> int
Int.to_string : int -> string      Int.of_string : string -> int
String.append : string -> string -> string      String.length : string -> int
String.get : string -> int -> char              String.sub : string -> int -> int -> string
String.equal : string -> string -> bool         String.compare : string -> string -> int
Char.equal : char -> char -> bool  Char.code : char -> int
Char.of_code : int -> char         Char.to_string : char -> string
Bool.not : bool -> bool            Bool.equal : bool -> bool -> bool
```

They are outside the program's root module, so the program's own definitions
shadow them: a toplevel `print_int` hides the prim, and a module `Int` hides
all of `Int.*`. Division by zero, out of bounds indices, unparsable
`Int.of_string` and invalid `Char.of_code` are runtime errors at the
application.

## Prelude

A prelude, written in stag, is in scope in every program, outside its root
module (inside the prims), so the program's definitions shadow it as they do
prims:

```ocaml
type list['a] = Nil | Cons of 'a * list['a]
type option['a] = None | Some of 'a
type result['a, 'e] = Ok of 'a | Error of 'e
module Stag = struct ... end      (* the syntax as data, see below *)
module Core = struct ... end      (* the core library, see below *)
```

Its toplevels are checked only when used. Its names have no syntax of their
own: once shadowed (eg. by a file `stag.stag`), the prelude's are not
reachable, and an unbound path through the shadowing module says so.
Messages and signatures write the prelude's names unqualified (`option[int]`),
except where the program shadows them, as `prelude.option[int]`; its
locations are in the file `<prelude>`. In a message, a prelude type or module
is shadowed if a type or module of its name is declared in a module enclosing
the error, other than by an include (so that it does not depend on which includes
checking had run when it stopped); a spliced module's declarations count.

## Core library

The prelude's module `Core` is a small standard library, written in stag in the
top-level directory `core/`: each file is a submodule named after it
(`core/list.stag` is `Core.List`), and the files refer to each other as
siblings (`List.map` within `core/` is `Core.List.map`). Like the rest of the
prelude it is checked only when used, so a program that does not use it is
unaffected, and a program's (or stag-build library's) own `Core` shadows it.
Its locations are in the files `<core>/list.stag` etc.

Its names and argument orders are those of OCaml's Stdlib where it has the
function (functions first, `String.sub s pos len`), with a few from Base
(`Option.first_some`, `String.strip`). Partial functions return options
(`List.hd`, `List.nth`, `Int.of_string`, `List.map2` on lists of different
lengths). Functions on lists run in constant stack. Each file's helpers are in
its submodule `Internal`, not meant for use outside `core/`.

- `Core.List`, `Core.Option`, `Core.Result`: the usual combinators on the
  prelude's types (`map`, `fold_left`, `filter_map`, `find`, `zip`, `assoc`, a
  stable `sort`, `Option.value`, `Result.bind`, `Result.all`, ...).
- `Core.Int`, `Core.Char`, `Core.Bool`, `Core.String`: the prims of their
  name, re-exported, and helpers on them (`String.concat sep parts`,
  `split_on_char`, `strip`, `escaped`, `Char.is_digit`, ...).
- `Core.Sexp`: `type t = Atom of string | List of list[t]`, printed and parsed
  exactly as by OCaml's Sexplib and Parsexp (checked against them in
  `driver/test/test_core_sexp.ml`). `to_string` is Sexplib's machine format
  (`(a"b c"(d)e()f)`), and `to_string_hum` its human one, packing lists onto
  lines of 78 columns as `Format` does. `of_string` (one sexp) and
  `of_string_many` accept what Parsexp's `Single` and `Many` do, comments
  included, and fail with its messages, as `result[_, Parse_error.t]`: a
  message with the offset, line (from 1) and column (from 0) of the error.
- Conversions for derived code: `sexp_of_X` and `X_of_sexp` for `unit`,
  `bool`, `int`, `char`, `string`, `list`, `option`, `result` and pairs and
  triples (`sexp_of_pair`, `triple_of_sexp`), the container ones taking those
  of their elements. `X_of_sexp` returns `result[X, Of_sexp_error.t]`: a
  message and the sexp that failed, which for containers is the innermost
  failing element.

Since a file of `core/` shadows the prims of its name (`Core.String` hides
`String.length`), the prelude (only) can name any prim `Prim.<prim>`
(`Prim.String.length`), a path that nothing shadows. This is for values only;
prims have no types or modules.

## Declaration splices

Declarations are generated as data: the syntax is defined in stag, in the
prelude's module `Stag` (`Stag.Type_expr.t`, `Stag.Expr.t`, `Stag.Toplevel.t`,
etc., mirroring `Syntax`, with helpers such as `Stag.Expr.var`,
`Stag.Toplevel.let_`), and a declaration splice of a list of declarations is
run at compile time, converted to syntax and checked where it is spliced, as a
ppx's output would be. Shapes may be computed (constructors from data), which
typed quotes cannot express.

```ocaml
module Ops = .~(make_ops names)     (* e : list[Stag.Toplevel.t] *)
include .~(make_ops names)          (* into the current module, in its place *)
```

Declaration splices may be in any module, and may generate declaration
splices. `e` is checked in the module the splice is written in, at compile
time (as a toplevel splice). Names in generated declarations are as written
(`"M.x"` is a path, `"A"` a constructor or tag), resolved where they are
spliced, except that types, constructors and fields may be named by a
`Stag.Path.Ref` (see Reflection), which names the same declaration wherever
it is spliced, and every node is located at the splice. Values that are not syntax
(eg. a tuple of one component, an uppercase let name) are errors at the
splice; negative int literals are allowed. Errors in generated declarations
are reported at the splice, naming the declaration and showing it. Generated expect tests
run, but are not corrected.

Written declarations are visible throughout their module, in any order. What
an include generates is visible only after the include: to the declarations
after it in its module (and in the modules declared after it there), and from
outside the module. From there it shadows outer names, as in OCaml; code
before it, including its own expression, never sees it. A name is looked up
among the module's written declarations, then what the includes before it
generate, then outward, from the module's place in
the enclosing one, then in the prelude and prims. Splices run on demand,
once: an include when a lookup needs what it generates, a spliced module
(whose name is known statically) when a path enters it, others when checking
reaches them, in order. A lookup that needs what a running splice generates
is an error, eg. a generator written after the include that uses it: write it
before the include, or in another module. A name not found past a running
include is unbound, noting that the include may define it; one found further
out (or a prim) is an error, as the include may shadow it. A name an include
generates, used before it, is unbound, noting the include. A failed splice fails wherever it
is reached. A name both spliced and written in a module, or generated by two
includes of a module, is a duplicate. Spliced types are named by the splice's
path, so splicing twice gives distinct types. Signatures list an include's
declarations in its place.

## Reflection

Types are values, for deriving: `type τ` has type `Stag.Type.t[τ]`, where `τ`
is checked as an annotation where it is written (so unbound names and arity
errors are reported there; a parameterised type is written applied, eg.
`type tree['a]`). The parameter is the reflected type, its variables standing
for any type: each `type` expression has its own type variables, so
`let f (x : 'a) = type 'a` has type `'a -> Stag.Type.t['b]`.
`Stag.Type.t['a]` and the refs `Stag.Type_ref.t`, `Stag.Constructor_ref.t`
and `Stag.Field_ref.t` are abstract: only reflection makes them.

```ocaml
Stag.Type.expr : Stag.Type.t['a] -> Stag.Type_expr.t
Stag.Type.decl : Stag.Type_ref.t -> option[Stag.Type.Decl.t]
Stag.Type_ref.name : Stag.Type_ref.t -> string   (* also Constructor_ref, Field_ref *)

module Stag.Type.Kind : sig
  type t =
    | Variant of list[Constructor_ref.t * list[Type_expr.t]]
    | Record of list[Field_ref.t * Type_expr.t]
    | Alias of Type_expr.t
end
module Stag.Type.Decl : sig
  type t = { params : list[Type_decl.Param.t]; kind : Kind.t }
end
```

A ref is a declared type, constructor or field, by identity, and `name` is
the name it is declared with. `expr` gives the type with each type it names
as a `Stag.Path.Ref` (`Stag.Type_expr.Con (Ref list, ...)`); `decl` gives a
type's declaration (its constructors and fields as refs, its types as for
`expr`), or `None` if it is abstract, as are the builtin types. A type
variable is given as `Stag.Type_expr.Var "a"`, deliberately unhygienic: where
it is spliced, it is the type parameter (or annotation variable) of that
name there. `Stag.Path.t['r]` (`Name of string | Ref of 'r`) names types
(`Path.t[Type_ref.t]`), constructors (`Path.t[Constructor_ref.t]`) and fields
(`Path.t[Field_ref.t]`) in generated syntax: a `Ref` names its declaration
wherever it is spliced, even where its name is shadowed or not in scope.

```ocaml
type t = A | B of int * t
include .~(Show.derive (type t))     (* t's to_string, in the same module *)
```

Reflecting a declaration checks it, on demand: a declaration that depends on
what the splice reflecting it generates is an error, as for other lookups
that need a running splice. Reflection values are ordinary values, so they
can exist at run time too, eg. a `type τ` in a runtime function, and `expr`
and the refs' `name` work there; only `decl`, which queries the typechecker,
is a runtime error at run time.

## Files

Files are modules, as in OCaml: `foo_bar.stag` is the module `Foo_bar`,
containing the file's declarations. A file name must be a letter followed by
letters, digits, `_` and `'`, then `.stag`. A program is a set of files, and
since toplevels in any module may refer to each other in any order, files may
refer to each other in any order too.

Evaluation: toplevels are evaluated in file order. Files are in dependency
order: a file depends on the files its toplevels refer to; strongly connected
components of files come after those they depend on, ties broken by the order
the files are given, and the files of a component are in the order given.
Within a file, toplevels are evaluated top to bottom. A toplevel used before
its turn (eg. by an earlier one, or one in an earlier file of a cycle) is
evaluated on demand at that point, once. Toplevels used by toplevel splices
are evaluated at compile time, once.

## Expect tests

As OCaml's ppx_expect:

```ocaml
let%expect_test "power" =
  print_int (Int.add 3 2);
  [%expect {| 5 |}]
```

`let%expect_test "name" = e` is a toplevel (in any module) whose body has type
`unit`. `[%expect {|...|}]` (or `[%expect "..."]`, or `[%expect]` for empty)
has type `unit` and is allowed only in a test's body where it runs while the
test runs: at the body's level, so not in a quote's code nor in a toplevel
splice (which runs at compile time), but in a splice within a quote, which runs
when the quote is evaluated (`.<.~([%expect]; .<()>.)>.`).

Tests run only when testing (`stag test`), after the program's toplevels are
evaluated, in order. Each `[%expect]` compares the output printed since the
previous one with its string, both normalized: leading and trailing blank lines
and trailing whitespace removed, and the indentation common to all lines
removed (as a common prefix, so a tab and spaces differ). Output after the last
`[%expect]` must be empty. An `[%expect]` reached more than once is checked
once if its outputs are all the same, and is an error if they differ. A
runtime error fails the test, and stops it; the output since the last
`[%expect]` is shown with it. Mismatches are corrected in a copy of the file:
`{| 5 |}` for one line, otherwise a block indented from the `[%expect]`'s line
by its leading whitespace, and trailing output as a new `[%expect]` after the
test's body (parenthesized if it ends in a `match`). A test's local bindings
are not visible to other tests.

## Later

### Declaration splices

Typed code may be embedded as a leaf, keeping its hygiene: only the shape is
untyped. Its context entries are provided at the declaration splice, as for
implicit substitutions.

```ocaml
Stag.Code.erase : <'a>{..'r} -> Stag.Expr.t
```

Quotes of declarations are sugar for building these values, with antiquotes
in item, type and expression positions:

```ocaml
.< struct type t = .~(variants names) let f x = .~body end >.  (* list[Stag.Toplevel.t] *)
```

Typed declaration quotes (code types of signatures, `<sig ... end>{ctx}`) and
`let module` may come later, beside these.

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
