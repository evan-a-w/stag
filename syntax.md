# Syntax

The concrete syntax of stag. The parser is a hand written recursive descent
parser (no parser generators), producing `Syntax` with a `Loc.t` on every
node, for good errors.

## Lexical

- Comments `(* ... *)`, nesting, as OCaml.
- Literals: integers (OCaml's native `int`; a literal out of its range is a
  lexing error), `true`, `false`, chars `'c'`, strings `"s"` with OCaml
  escapes (`\n \t \\ \" \' \ddd`), quoted strings `{|raw|}` and
  `{id|raw|id}` (no escapes; `id` is lowercase letters and `_`), `()`.
- `lowercase_ident`: values, types, fields, type params. `Capitalized_ident`:
  constructors and modules. `` `Tag ``: polymorphic variant tags and
  polymorphic record fields (both are structural labels). `'a`: type
  variables.
- Paths: `M.N.x`, `M.t`, `M.C`. A path is a sequence of capitalized names
  followed by the final name; `r.x` with lowercase `r` is a field access.
- Keywords: `let in match with type of module struct end`, `true false`.
- `@` and `@@` are reserved (for modes, see below).
- Staging tokens: `.<` `>.` `.~`.
- Expect test tokens: `let%expect_test`, `[%expect`.

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
used for row parameters). A `{...}` argument always parses as a row; given to
a type parameter (rather than a row parameter) it is the record type of that
row. Polymorphic record labels are written with backticks, and code contexts
(variable names) without; a row argument accepts either. Weak variables
(`'_a`), presence variables (`` `x?'a ``), absent labels (`-x`), `as` cycles and
`where` constraints are printed but never written.

## Expressions

```ocaml
let f x (y : int) : t = e in body   (* local lets are always recursive, no `rec` *)
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
.~e{}                               (* explicit, nothing provided (Some []) *)
.~e{x = e1; y}                      (* explicit; `y` alone is a variable: (y, None) *)
```

Splice binds tighter than application (`.~f x` is `(.~f) x`), and looser than
field access (`.~r.x` is `.~(r.x)`). The substitutions `{...}` attach to the
splice only when written right after it, without space: `.~f{x}` provides `x`,
while `.~f {x = 1}` applies `.~f` to a record. A splice outside any quote is a
toplevel (compile-time) splice.

Precedence, loosest first:

| Construct                                  | Associativity      |
| ------------------------------------------ | ------------------ |
| `let ... in`, `match` (extend right)       |                    |
| `e1; e2`                                   | right              |
| `e1, e2`                                   | n-ary              |
| `C e`, `` `A e ``                          | right (`C D x`)    |
| application `f x`                          | left               |
| splice `.~e`, `.~e{...}`                   | prefix             |
| `e.x`, `e.M.x`, `` e.`x ``, `` e \ `x ``   | left, postfix      |

`let` and `match` may also start a tuple component (`a, let x = 1 in x, y` is
`(a, let x = 1 in (x, y))`). Restriction is postfix like field access, so
`` f r \ `x `` is `` f (r \ `x) ``. Record field values, extended records (after `|`)
and substitutions are at the tuple level (`{x = 1, 2}`); the record of an update
`{e with ...}` is at the splice level. Trailing `;` is allowed in `{...}`.
`C (a, b)` and `C ((a, b))` both have two arguments. A parenthesized
expression's location includes its parentheses.

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

## Primitives

Built-in functions, implemented by the interpreter, available in every program
at any level (in quotes and at compile time too, like persistent toplevels):

```ocaml
print_string : string -> unit      print_endline : string -> unit
print_int : int -> unit            print_char : char -> unit
Int.add sub mul div rem : int -> int -> int     Int.neg : int -> int
Int.equal lt le gt ge : int -> int -> bool      Int.compare : int -> int -> int
Int.to_string : int -> string      Int.of_string : string -> int
String.concat : string -> string -> string      String.length : string -> int
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

## Files

Files are modules, as in OCaml: `foo_bar.stag` is the module `Foo_bar`,
containing the file's declarations. A file name must be a letter followed by
letters, digits, `_` and `'`, then `.stag`. A program is a set of files, and
since toplevels in any module may refer to each other in any order, files may
refer to each other in any order too.

Evaluation: each toplevel is evaluated after those it refers to. Files are
evaluated in dependency order: a file depends on the files its toplevels refer
to; strongly connected components of files are evaluated after those they
depend on, ties broken by the order the files are given, and the files of a
component in the order given. Within that, toplevels are evaluated in order.
Toplevels used by toplevel splices are evaluated at compile time, once.

## Expect tests

As OCaml's ppx_expect:

```ocaml
let%expect_test "power" =
  print_int (Int.add 3 2);
  [%expect {| 5 |}]
```

`let%expect_test "name" = e` is a toplevel (in any module) whose body has type
`unit`. `[%expect {|...|}]` (or `[%expect "..."]`, or `[%expect]` for empty)
has type `unit` and is allowed only in a test's body, outside quotes and
toplevel splices.

Tests run only when testing (`stag test`), after the program's toplevels are
evaluated, in order. Each `[%expect]` compares the output printed since the
previous one with its string, both normalized: leading and trailing blank lines
and trailing whitespace removed, and the indentation common to all lines
removed. Output after the last `[%expect]` must be empty. A runtime error fails
the test, and stops it. Mismatches are corrected in a copy of the file:
`{| 5 |}` for one line, otherwise an indented block, and trailing output as a
new `[%expect]` at the end of the test.

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
