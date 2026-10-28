# Syntax

The concrete syntax of stag. The parser is a hand written recursive descent
parser (no parser generators), producing `Syntax` with a `Loc.t` on every
node, for good errors.

## Lexical

- Comments `(* ... *)`, nesting, as OCaml.
- Literals: integers (OCaml's native `int`; a literal out of its range is a
  lexing error; digits may be separated by `_`, as in `1_000`, but not lead
  with one), `true`, `false`, chars `'c'`, strings `"s"`, quoted strings
  `{|raw|}` and `{id|raw|id}` (no escapes; `id` is lowercase letters and `_`),
  `()`. There are no negative literals in source yet: write `Int.neg 1`
  (generated declarations may have them, see Declaration splices).
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
| `C e`, `` `A e ``                          | right (`C D x`)    |
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
module Stag = struct ... end      (* the syntax as data, see below *)
```

Its toplevels are checked only when used.

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
spliced, and every node is located at the splice. Values that are not syntax
(eg. a tuple of one component, an uppercase let name) are errors at the
splice; negative int literals are allowed. Errors in generated declarations
are reported at the splice, naming the declaration. Generated expect tests
run, but are not corrected.

A spliced module's members are known only once it has run, so name
resolution forces it: a path `M.x` through a spliced module runs its splice,
and a name not found among a module's members known so far runs the module's
pending includes, which may define it, before looking in enclosing modules.
Splices not forced so are run when checking reaches them, in order. A splice
that depends on what it defines is an error. While a splice runs, a name
already known in an enclosing module (or a prim) is found there without
running the includes of the modules within it, which may need what is
running; an include then defining it is an error. A name both spliced and
written in a module is a duplicate. Spliced types are named by the splice's
path, so splicing twice gives distinct types. Signatures list an include's
declarations in its place.

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

Reflection, for deriving: types are values, so a type expression `type t`
evaluates at compile time to its declaration, of type `Stag.Type.t['a]` for
the type `'a`:

```ocaml
include .~(Show.derive (type t))
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
