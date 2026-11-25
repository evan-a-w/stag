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
  Nominal records print with their unqualified field names, `{x = 1; y = None}`,
  which parse back where the record's type is known (see Type-directed fields).
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
- Root paths: `.M.N.x`, `.int`, `.Some`: a path from the root scope (see
  Root paths). A `.` directly followed by a letter or `_` is the root token,
  unless the character directly before it (nothing between, not even a
  comment) can end an atom: a letter, digit, `_`, `'`, `)`, `]`, `}`, `"` or
  `.`; then it is a `.` (a path's or a field access's). So `f .M.x`,
  `(.int)`, `.~.Gen.c`, `{.M.x = 1}`, `list[.option[int]]` and
  `(* c *).x` start root paths, and `r.x`, `M.x`, `(e).x`, `"s".x` do not.
  The longest tokens `.<` `.~` `..` `.{` are unaffected (`..x` is `..` then
  `x`).
- Keywords: `let rec in match with fun exception external type of module
  struct end`, `true false`, and `stack_ exclave_` (see
  [Modes](#modes)), so these are not identifiers anywhere, even qualified
  (`M.stack_`). Mode names (`local`, `const`, ...) are not keywords, but
  `external`: it is the `external` mode after `@`, unless it starts a
  declaration (`external name :`).
  `rec` is reserved, as lets are always recursive.
- `@` introduces a mode (see [Modes](#modes)); `@@` introduces a
  modality, of a record field, a constructor argument or `ptr`'s argument. `[@nontail]` is the one attribute.
- Staging tokens: `.<` `>.` `.~`, and `.{` (a splice's substitutions).
- Expect test tokens: `let%expect_test`, `[%expect`. Benchmarks: `let%bench`.
  Inline expansions: `include%inline`, `module%inline`, `end%inline`.

## Types

As printed by `Type.to_string_hum`, so printed types parse back:

```
int    list['a]    M.t    with_x[{..'a}]        (* args postfix in brackets *)
'a -> 'b * 'c                                    (* -> right assoc, * n-ary *)
{`x : int; `y : bool; ..'r}   {..'r}   {}       (* polymorphic record *)
[`A of int | `B; ..'r]   [..'r]   []             (* polymorphic variant *)
<'a>{x : int; ..'r}                              (* code type and context *)
<unit>{x : mut int; ..'r}                        (* code assigning x *)
<int>{x : immut int; ..'r}                       (* code not assigning x *)
```

A type argument in brackets is a type, or a row `{...}` (record-style row,
used for row parameters). A `{...}` argument always parses as a row; given to
a type parameter (rather than a row parameter) it is the record type of that
row (so `list[{}]` is a list of empty records). Rows are of polymorphic
records, so their labels are written with backticks (`` with_x[{`y : int}] ``),
everywhere except in the context of a code type, whose entries are variables,
written without (`<int>{x : int}`). A context entry of type `mut t` is a
variable the code assigns, one of type `immut t` a variable it doesn't, and
one of type `t` either (`immut` is a keyword only there, before a type);
`mut` and `immut` are only allowed there. Type arguments are written once, after the
name (`list[int]`, not `int list` or `list[a][b]`). A constructor's argument of
function type is parenthesized: `C of (int -> int)`. `C of int * int` has two
arguments, `C of (int * int)` one, a tuple. Weak variables
(`'_a`), presence variables (`` `x?'a ``), assignment variables (`mut?'a`), absent labels (`-x`), `as` cycles and
`where` constraints are printed but never written.

## Expressions

```ocaml
let f x (y : int) : t = e in body   (* lets are always recursive: no `rec` *)
let mut x = e in body   let mut x : t = e in body  (* mutable variable, see below *)
x <- e   M.x <- e                   (* assignment, of type unit *)
e1; e2                              (* sugar for let _ = e1 in e2 *)
fun x (y : int) _ -> e              (* anonymous function, of let's params *)
match e with | p -> e | ...         (* leading | optional; arms are e1; e2 level *)
f x y                               (* application, left assoc *)
(x : t)                             (* annotated variable: Expr.Var's annotation *)
(a, b, c)   ()   (e)
[a; b; c]   []                      (* the prelude's list, see List literals *)
C   C e   C (a, b)   C ((a, b))     (* C (a, b): 2 args, or 1 tuple if C has 1 *)
{x = 1; M.y = 2}   r.x   r.M.x      (* nominal records *)
{r with x = 1; M.y = 2}             (* nominal update, may change type params *)
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
outside any quote is a toplevel (compile-time) splice, of code or of
syntax (see Syntax splices).

Precedence, loosest first:

| Construct                                  | Associativity      |
| ------------------------------------------ | ------------------ |
| `let ... in`, `match`, `fun` (extend right) |                    |
| `e1; e2`                                   | right              |
| `x <- e`                                   | right              |
| `e1, e2`                                   | n-ary              |
| `C e`, `` `A e ``, `type t`               | right (`C D x`)    |
| application `f x`                          | left               |
| splice `.~e`, `.~e.{...}`                  | prefix             |
| `e.x`, `e.M.x`, `e.(.M.x)`, `` e.`x ``, `` e \ `x `` | left, postfix |

`let`, `match` and `fun` may also start a tuple component (`a, let x = 1 in x, y` is
`(a, let x = 1 in (x, y))`), but not an argument (`f (fun x -> x)`). Restriction is postfix like field access, so
`` f r \ `x `` is `` f (r \ `x) ``. Record field values, extended records (after `|`)
and substitutions are at the tuple level (`{x = 1, 2}`), and a `let`, `match`
or `fun` there ends at the next `;` rather than sequencing (`{x = let a = 1 in a; y = 2}`
has two fields; parenthesize to sequence); the record of an update
`{e with ...}` is at the splice level. Trailing `;` is allowed in `{...}`.
As in OCaml (and likewise in patterns), parentheses around a constructor's
argument hold its arguments, unless they directly hold a tuple's own
parentheses: `C (a, b)` is parsed as two arguments and `C ((a, b))` as one, a
tuple. Several arguments given to a constructor declared with one (`Some (a,
b)`, `C of (int * int)`, `C of 'a`) are that argument as a tuple, decided by
the typechecker from the declaration (and so also for generated code: `C` of
`[a; b]` in `Stag.Expr.Construct`). A constructor declared with several
(`C of int * int`) takes exactly that many: `C ((a, b))` is an arity error. A
polymorphic variant has at most one argument, so `` `A (a, b) `` is always
`` `A `` of a tuple. A parenthesized expression's location includes its
parentheses.

### List literals

`[a; b; c]` is sugar for `Cons (a, Cons (b, Cons (c, Nil)))` and `[]` for
`Nil`, in expressions and patterns, always naming the prelude's `Cons` and
`Nil` (as refs, like `Stag.Code.erase`'s, see Hygienic code): what
`Cons`, `Nil` or `list` name where the literal is written doesn't change it,
so `[1; 2]` is a `.list[int]` even beside a local `type list['a] = Nil | Cons`.
Items are at the tuple level, as record field values (`[a, b; c]` is
`[(a, b); c]`), and a `let`, `match` or `fun` item ends at the next `;`. A
trailing `;` is allowed. The literal's outer `Cons` spans the whole literal,
each inner one from the `;` before its item to the `]`, and the final `Nil`
the `]` (or the whole `[]`).

The printer prints a chain of the prelude's resolved `Cons` ending in its
`Nil` as a literal (so erased code shows lists as literals, eg. `[.A.x; y]`),
broken like records, one item per line with a trailing `;`, and docked after
`=` or a constructor (`Core.Sexp.List [`). A resolved `Cons` not ending in
`Nil` is printed as a root path, `.Cons (x, rest)`, and `Cons (a, Nil)` as
written stays as written.

### Root paths

A root path, `.M.x`, names a declaration from the root scope, which
declarations of the program's modules, local or not, never shadow. The root
scope holds the program's root modules (its files, or stag-build's libraries
and an executable's files), then the prelude's root names that none of them
shadows, then the prims' (`.String.length`, unless the program has a root
module `String`) and builtin types (`.int`). A root path may name a value,
constructor, field or type, through modules, wherever its path may be
written: `.raise (.Failure "x")`, `match o with .Some x -> x`,
`{.A.M.x = 1}`, `{r with .A.M.x = 1}`, `{.A.M.x = n}` in a pattern,
`x : .option[.int]`. A field access by a root path is parenthesized, `r.(.A.M.x)`
(only a root path may be). Binders are names, never root paths. In a file
`a.stag`, `.A.x` is its toplevel `x` from anywhere in it, even under a local
`x` or a module of its own `A`; `.x` alone is unbound unless a root module or
the prelude declares `x`.

`type` takes an atomic type: a name, possibly applied (`type M.t[int]`), or a
parenthesized type (`type (int -> t)`); as an argument it is parenthesized
(`f (type t)`).

`match` arms extend as far as possible, as in OCaml (wrap nested matches in
parentheses).

`fun` takes at least one parameter, each as a `let`'s: `x`, `_` or `(x : t)`
(no patterns; match instead), with distinct names. `fun x y -> e` is `let f x y = e in f` for a fresh `f`: its
type is that of such a let (so `let id = fun x -> x` is generalized, being a
value), and it has no name to recurse by.

### Pointers

`Ptr.make e` allocates a GC-managed `ptr[t]` holding the result of
evaluating `e : t`. Both `p.*` and `*p` read that value. In a pattern,
`*p` matches the pointee with pattern `p`. Dereferencing binds more
tightly than function application; for example, `f *p` calls `f` with the
pointee, not with the pointer.

`ptr[t @@ m]` says what stage and provenance (see [Modes](#modes)) the
values in the pointer's cell may have, as atoms (`portable external`) or a
mode variable, as in
`get : ptr['a @@ 'm0] -> 'a @ 'm1 with 'm0 <= 'm1`.
`ptr[t]` is `ptr[t @@ nonportable gc]`; `ptr[t @@ m]` is sugar for `ptr[t, m]`,
`ptr` being declared `ptr['a, 'p : mode]`. A cell's locality is always
`global`, so a locality atom is an error. A store through it takes values
of at most `m`, and a read through it is exactly `m` (on those axes),
whatever the pointer's own mode, but at least `portable`, as the cell may
change between evaluations. `Ptr.make e` infers `m` from `e` and the
cell's uses.

Recursive data types must cross a pointer or another indirection (such
as a function) before returning to the same type. For example:

```
type tree = Leaf of int | Branch of ptr[tree * tree]
let left t = match t with Branch *(l, _) -> l | Leaf _ -> t
```

List literals and patterns keep their syntax. The equivalent constructors
are `Nil` and `Cons` of a pointer to an element-and-tail pair. Constructing
a nonempty list therefore allocates even when its elements are values.

### Stores

`p.* <- v` (or `*p <- v`) stores `v` in the pointee of `p : ptr[t]`, with
`v : t`, and is a `unit`. It is the one way to mutate: records have no mutable
fields, and `r.x <- v` on a record value is an error. A field of a pointee is
stated by the destination: `p.*.x <- v` stores `v` in the `x` of the record `p`
points to, as does a longer path (`p.*.a.b <- v`), which copies the records
along the path. Records are values, so a store to one is not seen by holders
of a previous copy; it is seen through every pointer to the same pointee.

The destination is evaluated once, before the value. `<-` binds as in
[`let mut`](#mutable-variables): `p.*.x <- v; e` and `f (p.*.x <- v)`
parse as expected, and the value extends as far right as possible.

### Type-directed fields and constructors

As OCaml's disambiguation, an unqualified field (`x`, not `M.x`) or
constructor (`A`) is looked up in the nominal type it is known to belong to,
whether or not that type's names are in scope, and otherwise lexically (the
innermost declaration in scope, as for other names). The known type wins over
the lexical one: `v.x` with `v : N.r` is `N.r`'s `x` even where another type's
`x` shadows it, and if `N.r` has no field `x` it is an error (`The record type
N.r has no field x`, `The variant type M.v has no constructor C`), rather than
a fallback. A qualified name or a `Stag.Path.Ref` always names its
declaration. The known type is (after expanding aliases):

- `e.x`: the type of `e`, which is checked first.
- `{r with x = ...}`: the type of `r`, checked first, else the expected type.
- `{x = ...}` and record patterns: the expected type if any, else the type of
  the first qualified field if any, else the first of the types the fields
  are of in scope, in field order, that has all of them. All fields must be
  of that type: a qualified field of another is the error.
- `C ...` and constructor patterns: the expected type.

As in OCaml, `{r with x = ...}` is of `r`'s type but for the type parameters
only in the types of the fields it gives: `{a with v = "s"}` with
`a : p[int]` and `type p['a] = {v : 'a; w : int}` is a `p[string]`.

The expected type of an expression is what its context is known to require
at the point it is checked, as inference proceeds left to right: an annotated
`let` or return type, a function's parameter type in an application (the
function is checked first), a constructor's argument types or a field's type
once the constructor or field is resolved, a component of an expected tuple,
polymorphic record or variant, a `match` arm's or `let` body's or `e2` of
`e1; e2` the type of the whole, `e` in `.< e >.` the code type's inner type,
`e` in `.~e` the code type `<T>{..}` of the splice's type `T`.
A pattern's expected type is the scrutinee's, and inside it each
sub-pattern's that of its position. Where the type is only known later (eg. a
parameter whose type is fixed by a later use), the lookup is lexical. So the
order matters, and nothing warns of it (there is no `-principal`):
`let f v = let _ = (v : N.r) in v.x` finds `N.r`'s `x`, but
`let f v = let _ = v.x in (v : N.r)` the `x` in scope. Inside a quote, a
non-function toplevel is a free variable of the quote, whose type is not its
own (see modes.md), so in `.< N.v.x >.` the `x` is the one in scope whatever
`N.v`'s type.

## Patterns

```ocaml
_   x   1   'c'   "s"   true   ()   (p, q)
C   C p   C (p, q)   C ((p, q))     (* as in expressions *)
[p; q]   []                         (* list literals, as in expressions *)
{x = p; M.y = q}                    (* nominal *)
`A   `A p
{`x = p}   {`x = p; ..}   {`x = p; ..rest}   (* closed, open, bind rest *)
exception p                         (* only at the top of a match arm *)
```

## Exceptions

```ocaml
exception E                         (* a declaration, like a constructor of exn *)
exception F of int * string
raise (F (1, "x"))                  (* raise : exn -> 'a *)
match f x with
| y -> y                            (* value arms *)
| exception F (n, s) -> n           (* exception arms *)
| exception e -> raise e            (* any exception, re-raised *)
```

`exn` is a builtin type, extended by each `exception` declaration (in a
module, or generated by a declaration splice as `Stag.Toplevel.Exception`).
Exceptions are told apart by declaration, not name: `M.E` and `N.E` differ.
An exception's arguments may not mention type variables. `E args` is a value
of type `exn`, matched like any constructor (there is no exhaustiveness
checking: as for other types, an exception that no case matches is a match
failure at run time, so a match on `exn` usually wants a catch-all); values
print by path, as `M.E (1, "x")`.

There is no `try`: exceptions are handled by `exception p` arms of a `match`,
which catch what is raised while evaluating the scrutinee (not its arms), as
in OCaml. A match needs at least one value arm; exception arms return the
same type as value arms. `exception p` is a pattern only at the top of an arm
(`(exception E, x)` is an error). An exception no arm matches propagates.

Uncaught, an exception is an error at the `raise`: `Uncaught exception M.E (1,
"x")`, failing its expect test, or, raised at compile time (in a toplevel or
declaration splice), a compile error at the splice. Code quoted at one level
and run at another raises and handles exceptions as any code does.

Exceptions are for errors and the rare function that may fail in expected
cases; such a function's name ends in `_exn` (`Int.of_string_exn`). Prims
raise the prelude's `Division_by_zero`, `Invalid_argument of string` (bad
indices, `Char.of_code` out of range) and `Failure of string`. Exceeding an
evaluation limit (fuel, depth, time) and match failure are not exceptions,
and can't be handled.

### Mutable variables

`let mut x = e` binds a variable that `x <- e'` assigns. It binds only a
variable, optionally annotated (`let mut x : t = e`), with no parameters. It is
the one `let` that is not recursive: in `let mut x = x + 1 in ...` the `x` on
the right is the outer one. Its type is never generalized (`let mut l = []`
has a weak type, fixed by its uses). Only a variable bound by `let mut` can be
assigned; `<-` takes a path (`x`, `M.x`). Fields are assigned through a
pointer (see [Stores](#stores)): records have no mutable fields.

A closure captures a mutable local by reference: it shares the variable with
the code that binds it. Mutable locals are values of their level only: a
quote can't use one bound outside it, nor can compile-time code use a runtime
one. A closure capturing one is `nonportable` (see [modes.md](modes.md)), so
is not used directly in a quote: it becomes a free variable of the quote.

Toplevel `let mut x = e` declares a mutable global, assignable from anywhere
it is in scope (`M.x <- 1`). Its value can't need itself (see Evaluation),
but a function it holds may use it. It is
initialized in file order, or on demand when used earlier, like other
toplevels. It exists only at runtime: compile-time code (a toplevel splice)
may not use it, nor any toplevel that reaches one through the toplevels it
refers to. Mentioning one inside a quote is fine, even in compile-time code
(`.~(.<counter <- 1>.)`): code refers to a mutable global by ref, not as a
variable of the quote's context, so the generated code updates the program's
global when it runs. Expect tests run in order on one instance of the
program, so they see each other's assignments.

Code may assign a free variable of its quote: `.<x <- x + 1>.` has type
`<unit>{x : mut int; ..'r}`. Its context entry is then `mut`, and code
combining it with code that only reads `x` assigns `x` too. Where the code is
spliced, a `mut` entry must be provided by a mutable variable: a `let mut`
local in scope at the splice (implicitly, or by name in a substitution, `.{x}`
or `.{x = y}`), or a mutable toplevel (`.{x = M.counter}`); the code then
assigns that variable. Any other provider (`.{x = 1}`, an immutable variable)
is an error. Code that only reads `x` may be combined with code assigning it, or
spliced where `x` is immutable, so whether it assigns `x` is a variable,
like `x?'a` for presence: an entry `x : t`, in an annotation or printed, may
be assigned or not. An entry that may not be assigned, as code spliced under
a plain `let x` has, is `x : immut t`. A printed type names the variable,
`x : mut?'a t`, only where two entries share it or it is in a bound, so a
printed `x : t` reads back as the same type. Records' and
variants' entries are never `mut`, also through a row variable shared with a
context.

A substitution that is a variable is the variable itself: the code assigns it
if it is mutable, and reads its current value. Any other substitution is
evaluated once, in the order written, before the code runs, so
`.~c.{a = y; b = y <- 5}` gives `a` the value `y` has after `b`'s assignment.
This is the same at a toplevel splice and in a quote.

A substitution must be of a free variable of the code: substituting a name
the code's context is known not to have (it is closed without it, or the
entry is absent) is an error, as is one the code run at a toplevel splice
turns out not to use when its type doesn't list it. A toplevel constant used
in a quote is not a free variable (see modes.md), so `.~(.<n>.).{n = 1}` is
an error for `let n = 3`. Nor is a free variable of a quote inside the
generated code: in `.~(.< .<x>. >.).{x = 1}`, the inner `x` is provided where
that inner code is spliced, not by the substitution, so it is an error too.

### Modes

The syntax of modes (see [modes.md](modes.md) and
[semantics.md](semantics.md#modes)), all three axes checked. The interpreter
allocates everything on its heap, so `stack_`, `exclave_` and `[@nontail]` evaluate as their body.

A binding's annotation is the variable's mode: `let x @ m = e` needs `e`'s
mode to be at most `m`, and `x` is then `m` on the axes `m` names (and `e`'s
mode on the others), as are parameters `(x @ m)` and `let mut x @ m`. An
expression's annotation `(x @ m)` only checks: it bounds `x`'s mode, which it
keeps. So `let n @ portable = 1` is a `portable` `n`, while
`let n = (one @ portable)` is as `const` as `one`.

A mode, after `@`, is a mode variable (`'m`, standing for every axis), or one
or more of `const portable nonportable` (stage), `static gc external`
(provenance) and `global regional local` (locality), at most one per axis:
`@ local portable`.

A function type's side written without a mode, or the axes its mode doesn't
name, are the default, `nonportable gc global`, in an annotation as in a
declaration: `(f : int -> int)` takes and returns global values, and
`int @ local -> int` is `int @ nonportable gc local -> int`. A mode variable
keeps a side polymorphic: `(f : int @ 'm -> int @ 'n)`.

```ocaml
let x @ const = 1                       (* a binder's mode *)
let f (x @ local) (y : int @ 'm) : int @ global = ...
let mut name : string @ gc = ""
(e : t @ m)   (x @ local)               (* an annotated variable *)
int @ local -> int @ local              (* a function type's argument/result *)
'a @ 'm -> 'b @ 'n
stack_ e                                (* allocate e's allocations in the current region *)
exclave_ e                              (* end the region, then evaluate e *)
f x [@nontail]                          (* not a tail call *)
type r = {x : int @@ global; y : int}   (* always global, even in a local r *)
type t = A of int @@ global portable * int
type cell['a, 'p : mode] = {v : ptr['a @@ 'p]}
cell[string, external]   cell['a, gc portable]   cell['a, 'm]
```

`@` binds looser than `*` and tighter than `->`: `int * int @ local -> int`
is `(int * int) @ local -> int`. A mode is written only on a binder, an
annotation, or a function type's argument or result: `list[int @ local]` and
`(int @ local) -> int` are errors. `stack_` and `exclave_` extend as far right
as `fun` does. `[@nontail]` follows an application, but not as a
constructor's argument, where it would read as applying to the construction:
`C (f x [@nontail])`, not `C (f x) [@nontail]`.

A modality `t @@ m` (atoms, at most one per axis, or a mode variable) is
written only on a record field, a constructor argument or `ptr`'s type
argument. Its locality can only be `global` (`regional` and `local` are
relative to a region). It binds looser than `->`: `{f : int -> int @@ portable}` is a
portable function field. An exception's arguments are always global, so
take none. A declaration's mode parameters are declared `'p : mode`, and are
on stage and provenance only (a locality atom in a mode argument is an
error); a type argument of several atoms is a mode, and one of one atom or
a variable is read as a mode or a type by the declaration's parameter. A
name in an annotation is a mode variable or a type variable, not both.

An annotation's type may end with constraints between mode variables, or
between a variable and a mode, joined by `and`; each relates at least one
variable:

```ocaml
let compose : ('b @ 'n -> 'c) -> ('a -> 'b @ 'm) -> 'a -> 'c
    with 'm <= 'n and portable <= 'm = ...
let h (x : int @ 'm with 'm <= portable) = x
```

`with` is only in an annotation's type, never in a function type's argument or
in a declaration. `and` is not a keyword (there is no `let ... and`), so
following constraints it always continues them. A constraint between two
variables relates every axis; `stage('m) <= stage('n)` (or `locality(...)`,
`provenance(...)`) relates only that one, naming the same axis on both sides.
One between a variable and modes bounds the axes the modes name
(`portable external <= 'm`). A constraint on one axis may apply functions to
its sides: `meet(t, c)` and `release(t)` on the left of `<=`,
`static_if_const(t)` and `const_if_static(t)` on the right:

```
constraint := side <= side                     (every axis, or the atoms' axes)
            | left <= right                    (one axis)
side       := mode | 'm
left       := axis('m) | meet(left, atom) | release(left)
right      := axis('m) | static_if_const(right) | const_if_static(right)
axis       := stage | locality | provenance
```

`release` takes a locality term, `static_if_const` a stage term (giving a
provenance one) and `const_if_static` a provenance term (giving a stage one);
`meet`'s atom is of its term's axis, and both sides end on the same axis.
modes.md (Printed constraints) says what each means and where it arises.
Printed types use the same syntax, and read back as exactly themselves:

```ocaml
let stage_only : string @ 'm -> string @ 'n with stage('m) <= stage('n) = ...
let field : r2 @ 'm -> string @ 'n
    with meet(provenance('m), gc) <= provenance('n) = ...
```

In reflected syntax, a binder's or variable's annotation is a
``Stag.Annot.t = {`ty : option[Type_expr.t]; `mode : option[Mode_expr.t]}``
(`Stag.Annot.none`, `Stag.Annot.ty t`). Reflected syntax is checked to have
modes and modalities only where the parser allows them, so the annotation's
`` `ty `` has no mode of its own.

## Declarations

```ocaml
let f x y = e                          (* toplevels are order independent *)
let mut counter = 0                    (* mutable global *)
type nat = Z | S of nat
type list['a] = Nil | Cons of 'a * list['a]
type with_x[{..'r}] = {`x : int; ..'r} (* alias *)
type point = {x : int; y : int}        (* nominal record *)
type void = |                          (* empty variant *)
type t                                 (* abstract *)
module M = struct ... end
module M = .~e                         (* declaration splices, see below *)
include .~e
external labs : int -> int = "labs"   (* a C function, see below *)
```

### External functions

`external name : t = "symbol"` declares the C function `symbol`, found in the
process (by `dlsym`, in the interpreter, which calls it through libffi), as
the toplevel function `name : t`. `t` is an ordinary curried arrow, with no
type variables: `a -> b -> r` is C's `r symbol(a, b)`, and `unit -> r` is
`r symbol(void)`. Each argument and the result is `int` (C's `int64_t`),
`bool` or `char`, and the result may be `unit` (`void`). An `int` is 63 bits, so
a C result outside that range is a runtime error. Pointers (`string`,
`ptr[...]`) are not supported yet, and other types are rejected. The symbol
must be non-empty, with no NUL character. The C function must have exactly
that signature: nothing checks it. For instance C's `isdigit` returns an
`int`, so `external isdigit : char -> bool = "isdigit"` misreads its result
(only the low byte is read as a `bool`): declare it `char -> int`. An external is
a toplevel function like any other: it can be partially applied and passed
around, and C is called once it has all its arguments.

C is called at compile time only if the library declaring the external allows
it: the prelude's always may, and a stag-build library, executable or test
opts in with the stanza field `(compile_time_c_calls)` (see
[README.md](README.md#stag-build)). Calling another external while running
compile-time code (a toplevel splice, or a toplevel it uses) is an error,
when the call happens, naming the external and its library. The check is on
the library declaring the external, so a permitted library's functions that
call its externals may be called at compile time from anywhere. Calls at
runtime are always allowed. Files checked outside a project (`stag check`,
`stag run`, an editor buffer outside any project) may not call externals at
compile time. This keeps opening or checking a file from running arbitrary C
code.

Modes written in `t` are trusted, as C code is not checked, and unwritten
ones are a declaration's defaults, `nonportable gc global` (see
[modes.md](modes.md#arrow-modes)):
`external f : int @ portable -> int @ portable = "f"` returns portable
values, while a plain `int -> int` returns nonportable `gc` ones
(`@ external` declares a result outside the GC's heap). The values C passes
are pointer-free, so cross modes (but never to `const`): for now the modes
matter in the external's type, as where it is passed as a function value of
an annotated type. The external itself, a code pointer, has no mode of its
own to declare: as a toplevel function it is `static` and `const`, so may be
used anywhere.

```ocaml
external srand48 : int -> unit = "srand48"
external lrand48 : unit -> int = "lrand48"
let seeded = srand48 7; lrand48 ()
```

## Primitives

Built-in functions, implemented by the interpreter, available in every program
at any level (in quotes and at compile time too, like `const` toplevels):

```ocaml
print_string : string -> unit      print_endline : string -> unit
print_int : int -> unit            print_char : char -> unit
Int.add sub mul div rem : int -> int -> int     Int.neg : int -> int
Int.equal lt le gt ge : int -> int -> bool      Int.compare : int -> int -> int
Int.to_string : int -> string      Int.of_string_exn : string -> int
String.append : string -> string -> string      String.length : string -> int
String.get : string -> int -> char              String.sub : string -> int -> int -> string
String.equal : string -> string -> bool         String.compare : string -> string -> int
Char.equal : char -> char -> bool  Char.code : char -> int
Char.of_code : int -> char         Char.to_string : char -> string
Bool.not : bool -> bool            Bool.equal : bool -> bool -> bool
```

They are outside the program's root module, so the program's own definitions
shadow them: a toplevel `print_int` hides the prim, and a module `Int` hides
all of `Int.*`. A root path still names them (`.Int.add`, `.print_int`),
unless a root module of the program (eg. a file `int.stag`) shadows it. Division by zero, out of bounds indices, unparsable
`Int.of_string_exn` and invalid `Char.of_code` raise exceptions (see
Exceptions).

## Prelude

A prelude, written in stag, is in scope in every program, outside its root
module (inside the prims), so the program's definitions shadow it as they do
prims:

```ocaml
type list['a] = Nil | Cons of 'a * list['a]
type option['a] = None | Some of 'a
type result['a, 'e] = Ok of 'a | Error of 'e
exception Division_by_zero
exception Invalid_argument of string
exception Failure of string
val raise : exn -> 'a
module Stag = struct ... end      (* the syntax as data, see below *)
module Core = struct ... end      (* the core library, see below *)
```

Its toplevels are checked only when used. Its names are root paths too
(`.option`, `.Core.Int.to_string`), reachable under local declarations of
their names, until a root module of the program shadows them (eg. a file
`stag.stag` shadows `.Stag`): then the prelude's are not reachable, and an
unbound path through the shadowing module says so. Messages and signatures
write the prelude's names unqualified (`option[int]`), except where the
program shadows them, as their root path, `.option[int]`; where a root module
shadows them, no syntax names them and they are written `prelude.Core.Sexp.t`
(`<prelude.Core.Sexp.t>` for a ref in printed code, which does not parse).
The program's shadowed names are written likewise, as their root path
(`.A.t`). Its locations are in the file `<prelude>`. In a message, a prelude type or module
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
lengths), and most have an `_exn` variant raising `Failure` (`List.hd_exn`,
`Int.of_string_exn`, `Option.value_exn`, `Result.ok_exn`); `Sexp.of_string_exn`
raises `Sexp.Parse_error of Sexp.Parse_error.t`, and `Sexp.of_sexp_exn
int_of_sexp t` raises `Sexp.Of_sexp_error of Sexp.Of_sexp_error.t`. Out of
range indices (`String.get`, `String.sub`) and division by zero are
programming errors, raising the prelude's `Invalid_argument` and
`Division_by_zero` as the prims do. Functions on lists run in constant stack.
Each file's helpers are in its submodule `Internal`, and `Sexp_printer`,
`Sexp_parser` and `Sexp_derive` are `Sexp`'s, not meant for use outside
`core/`.

- `Core.List`, `Core.Option`, `Core.Result`: the usual combinators on the
  prelude's types (`map`, `fold_left`, `filter_map`, `find`, `zip`, `assoc`, a
  stable `sort`, `Option.value`, `Result.bind`, `Result.all`, ...).
- `Core.Int`, `Core.Char`, `Core.Bool`, `Core.String`: the prims of their
  name, re-exported, and helpers on them (`String.concat sep parts`,
  `split_on_char`, `strip`, `escaped`, `capitalize`, `Char.is_digit`, ...).
- `Core.Sexp`: `type t = Atom of string | List of list[t]`, printed and parsed
  exactly as by OCaml's Sexplib and Parsexp (checked against them in
  `driver/test/test_core_sexp.ml`). `to_string` is Sexplib's machine format
  (`(a"b c"(d)e()f)`), and `to_string_hum` its human one, packing lists onto
  lines of 78 columns as `Format` does. `of_string` (one sexp) and
  `of_string_many` accept what Parsexp's `Single` and `Many` do, comments
  included, and fail with its messages, as `result[_, Parse_error.t]`: a
  message with the offset, line (from 1) and column (from 0) of the error.
  The printers, the parser and the deriver below are `Sexp_printer`,
  `Sexp_parser` and `Sexp_derive` (`core/sexp_*.stag`), which `Sexp` calls.
- Conversions for derived code: `sexp_of_X`, `X_of_sexp` and
  `X_of_sexp_exn` for `unit`, `bool`, `int`, `char`, `string`, `list`,
  `option` and `result` (and `sexp_of_pair`, `pair_of_sexp`, and the same
  for triples), the container ones taking those of their elements (the
  `of_sexp` ones, for both). `X_of_sexp` returns `result[X, Of_sexp_error.t]`:
  a message and the sexp that failed, which for containers is the innermost
  failing element; `X_of_sexp_exn` raises `Of_sexp_error` with it.
  `bool_of_sexp` accepts `true`, `false`, `True` and `False`, as Sexplib does.
  `Sexp.sexp_of_t` and `Sexp.t_of_sexp` are the identity, and
  `Of_sexp_error.t` and `Parse_error.t` have conversions as records.
- `Core.Sexp.derive (type t)`: ppx_sexp_conv's `[@@deriving sexp]`, a
  declaration splice (`include .~(Core.Sexp.derive (type t))`) generating
  `sexp_of_t`, `t_of_sexp_exn` (raising `Sexp.Of_sexp_error`) and `t_of_sexp`
  (`Core.Sexp.Deriving.catch t_of_sexp_exn`, returning `result`) from the
  reflected declaration; for type `foo`, `sexp_of_foo` etc. The encoding and
  error messages are ppx_sexp_conv's: `A`, `(B x)`, `(C x y)` for constructors
  (also read with a lowercase first letter, `(c x y)`; a polymorphic variant
  tag's argument is one value, `(D (x y))`), `((x 1) (y 2))` for records
  (fields in any order, read fastest in the declared one; missing, extra and
  duplicate fields are errors), `(a b)` for tuples, and Core's conversions for
  the types above. Anonymous tuples and polymorphic variants are converted in
  place. A parameterised type's conversions take those of its parameters
  first, named `_of_a` for `'a` (`sexp_of_t _of_a v`, `t_of_sexp _of_a sexp`,
  `_of_a` returning a `result` for both `of_sexp`s). Other types are
  converted by name and root path, `u` of `a.stag` by `.A.sexp_of_u` and
  `.A.u_of_sexp_exn`, so a local module or value of the same name where the
  code is spliced doesn't capture them, and `M.t` can be derived outside
  `M`; `t` itself is named bare, as it is generated beside the splice, so
  recursive types work, and mutually recursive ones when derived in one
  include (`include .~(List.append (derive (type a)) (derive (type b)))`). A
  hand-written conversion of a type `u` must provide `sexp_of_u`,
  `u_of_sexp` and `u_of_sexp_exn`. As with ppx_sexp_conv, non-regular
  recursive types (`B of t[list['a]]`) fail to type the generated code.
  What the code refers to in `Core` and the prelude is by ref, so shadowing
  `Core`, `List`, `Cons` etc. where it is spliced doesn't change it. Abstract
  types, functions, code, row parameters and arguments, polymorphic records,
  empty variants, open polymorphic variants, the prelude's types and a type
  named `sexp` fail at compile time. Unlike ppx_sexp_conv, error messages
  name the conversion without the file (`t_of_sexp: ...`, not
  `a.ml.t_of_sexp: ...`), and there are no attributes (`[@sexp.option]`,
  `[@default]`, `[@@sexp.allow_extra_fields]`, ...). `tests/core_derive_sexp.stag`
  shows the generated code, by inline expansions.
- `Core.Sexp.sexp_of (type τ)`, `Core.Sexp.of_sexp (type τ)` and
  `Core.Sexp.of_sexp_raising (type τ)` (`Stag.Type.t['a] -> Stag.Expr.t`):
  ppx_sexp_conv's `[%sexp_of: τ]` and `[%of_sexp: τ]`, the syntax of the
  conversion of any type expression `τ` (`τ -> Sexp.t`,
  `Sexp.t -> result[τ, Of_sexp_error.t]` and `Sexp.t -> τ`, raising
  `Of_sexp_error`), as `derive` converts its fields: named types by their
  conversions' root paths (`type list[int]` gives
  `.Core.Sexp.sexp_of_list .Core.Sexp.sexp_of_int`), tuples and polymorphic
  variants in place, and `'a` by `_of_a` where it is spliced (returning a
  `result`, as for `derive`). They are used as syntax splices
  (`let s = .~(Core.Sexp.sexp_of (type list[int])) [1; 2]`,
  `let f _of_a v = .~(Core.Sexp.sexp_of (type list['a])) v`), or in
  generated declarations. A type with a polymorphic variant is annotated
  in the expression, so that it has exactly its converter's type (rather
  than an open row). The generator is named `of_sexp_raising`, not
  `of_sexp_exn`, as `Core.Sexp.of_sexp_exn` is already taken (it runs an
  `of_sexp` conversion, raising its error). What can't be converted fails
  at compile time naming the generator (`Sexp.of_sexp: functions have no
  sexp conversion`, `Sexp.derive: ...` for `derive`), while the generated
  code's parse errors at runtime name the conversion (`of_sexp: ...`).
  `tests/core_sexp_conv.stag` shows the generated code.

Since a file of `core/` shadows the prims of its name (`Core.String` hides
`String.length`), the prelude (only) can name any prim `Prim.<prim>`
(`Prim.String.length`), a path that nothing shadows. This is for values only;
prims have no types or modules.

## Declaration splices

Declarations are generated as data: the syntax is defined in stag, in the
prelude's module `Stag` (`Stag.Type_expr.t`, `Stag.Expr.t`, `Stag.Toplevel.t`,
etc., mirroring `Syntax`, with helpers such as `Stag.Expr.var`,
`Stag.Expr.fun_`, `Stag.Expr.list` (a list literal, of the prelude's `Cons`
and `Nil` wherever spliced), `Stag.Toplevel.let_`), and a declaration splice of a list of declarations is
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
spliced, except that types, constructors, fields and values may be named by
a `Stag.Path.Ref` (see Reflection and Hygienic code), which names the same declaration wherever
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

### Inline expansions

As ppx_deriving's `[@@deriving_inline]`, a declaration splice may show what it
generates in the source, between the splice and `end%inline`:

```ocaml
include%inline .~(value "x" (Stag.Expr.int 1))
  let x = 1
end%inline

module%inline M = .~(make_ops names)
  let add = ...
end%inline
```

The expansion is declarations, parsed but not checked: what is declared is
what the splice generates, as without `%inline`, so that a stale expansion
never changes the program's meaning. `stag check`, `stag run` and `stag-build
build` warn of each stale expansion; testing (`stag test`, `stag-build test`)
fails on them, and corrects them as expect tests are, in the same corrected
file.

An expansion is up to date when it prints as the generated declarations do
(so modulo layout and comments), each resolved reference and root path of the
generated code (types, constructors, fields, values and modules) printed as the
shortest suffix of its declaration's full path naming it where it is used in
the expansion, read as if written in place of the splice: the generated
declarations (of earlier expansions, and of this one, also within its modules)
are in scope there, and a single name is not one bound locally where it is
used. Else it is printed as its root path (`.M.t`) if that names it there. One
that neither names (a prelude declaration a root module shadows) is printed
`<prelude.M.t>`, which doesn't parse, so its expansion is always stale. So
`.A.sexp_of_u` written by a generator in `a.stag` prints as `sexp_of_u` where
nothing shadows it, `M.sexp_of_u` for `u` of `M` from outside `M`, and stays
`.A.sexp_of_u` where a local `sexp_of_u` would capture it. The
correction is the generated declarations printed so, indented by 2 from the
leading whitespace of the splice's line, with the file's line endings,
replacing the expansion and its comments but not comments on the splice's line
(an empty expansion is written `include%inline .~e end%inline`).

The declarations of an up to date expansion are located as written, so errors
in them, hover, definitions and the editor's outline point into the expansion
rather than at the splice. Splices within an expansion don't run. Generated
splices have no expansion. Formatting formats the expansion, keeping its
comments.

### Struct quotes

A struct quote is sugar for building declarations as data:

```ocaml
let make names body : list[Stag.Toplevel.t] =
  .< struct
    type t = .~(variants names)
    let f (x : .~ty) = .~body
    include .~(more names)
  end >.
```

Its declarations are untyped syntax, exactly the `list[Stag.Toplevel.t]` that
the `Stag` constructors would build: nothing in them is checked or resolved
where the quote is written, but where it is spliced (`include .~e`,
`module M = .~e`). So a name in it is a `Stag.Path.Name`, resolved at the
splice like those of any generated syntax, and a root path `.M.x` stays one
(naming the same declaration wherever it is spliced). To refer to a
declaration of the generator's own scope whatever is in scope at the splice,
embed erased code, `.~(Stag.Code.erase <..>)`, whose references are refs.

An antiquote `.~e` outside any quote within it is evaluated when the quote is
(the antiquotes in order), and spliced in according to its position:

- `include .~e` and `module M = .~e`: `e : list[Stag.Toplevel.t]`, its
  declarations in place (or as `M`'s). There is no bare `.~e` item: a list
  covers one declaration or none, and `include` reads as what it does;
- a type: `e : Stag.Type_expr.t`;
- a type declaration's definition, `type t[...] = .~e`:
  `e : Stag.Type_decl.Kind.t`;
- an expression: `e : Stag.Expr.t` (an antiquote with substitutions,
  `.~e.{x}`, is an error). Typed code goes in through
  `.~(Stag.Code.erase c)`, keeping its hygiene.

Binders are names as written; to bind a computed name, build that declaration
with the `Stag` constructors and append it.

A quote or struct quote within a struct quote is quoted with it, a splice
within that unquoting back into it, as for nested expression quotes: in
`.< struct let q = .< .~x >. end >.`, `.~x` is part of the generated syntax.
An `include%inline` in a struct quote is an error (generated declarations have
no expansions). The quote is expanded where it is checked into the
constructors' applications, so its errors (an antiquote of the wrong type) are
reported at the antiquote. A struct quote is a `Stag.Expr.Struct_quote` as data, and type
and definition antiquotes are `Stag.Type_expr.Splice` and
`Stag.Type_decl.Kind.Splice`.

### Type splices

Outside quotes, `.~e` may stand for a type, `e : Stag.Type_expr.t`, or for a
type declaration's definition, `e : Stag.Type_decl.Kind.t`, evaluated at
compile time:

```ocaml
type point = .~(Gen.record ["x"; "y"])
let origin : .~(Gen.ty "point") = {x = 0; y = 0}
let n = (x : .~(Gen.ty "int"))
type pair = (.~a * .~b)  (* not `type pair = .~a * .~b`: see below *)
```

After `type t =`, `.~e` is the definition; a type starting with a splice is
parenthesized there.

A declaration holding type splices is still the declaration as written: its
name is known up front (for `type t = .~e`, only its definition is computed),
so it may be used before it, shadows outer names throughout its module, and
keeps its source locations (errors, expect test corrections, the editor). When
the declaration is checked, on demand, each type splice is evaluated as a
compile-time expression (the usual rules on which toplevels it may use apply,
and its errors are at the `.~`), and the syntax it gives takes its place. An
error in that syntax is reported at the `.~` with a note printing it. Types
may refer to each other through splices, in any order. While `type t = .~e` is
being computed, `t` is nominal: `e` may name it, but needing its definition
(eg. reflecting on its declaration, or `e` giving an alias, which would then
have been used as nominal) is an error. Each type splice is evaluated once,
however often its declaration is used. What a splice gives may not itself hold
type splices (so evaluation ends); generated declarations may hold them
(`Stag.Type_expr.Splice`, `Stag.Type_decl.Kind.Splice`), evaluated the same
way when checked where they are included. A type splice within a quote or
compile-time code is an error, as nothing evaluates it there.

The constructors and fields `type t = .~e` defines are only known once `e` is
evaluated, so, like an include's declarations, they are visible only after the
declaration: a lookup of a constructor or field not otherwise found evaluates
the module's definition splices before it (in order, as it forces includes),
and none after it. Code `e` uses that is still being checked (eg. a function
before the declaration naming one of its constructors) can't see them: the
name is unbound, noting the splice. A splice that fails (its errors are
reported, and checking continues) makes its declaration failed: uses of it
are not reported again, and compile-time code using it is not run.

### Syntax splices

Outside quotes, `.~e` with `e : Stag.Expr.t` is a syntax splice: `e` is
evaluated at compile time, as a toplevel splice of code is, and the
expression it gives is checked in place of the splice, as if written there:

```ocaml
let one = .~(Stag.Expr.int 1)
let f x = .~(Stag.Expr.var "x")              (* the parameter x *)
let s = .~(Core.Sexp.sexp_of (type list[int])) [1; 2]
```

Its names resolve where it is spliced (locals included; refs and root paths
as such), and it is checked at the type expected there (an annotation's, a
constructor's argument's, ...), which also tells its constructors and fields
apart. Whether `.~e` splices syntax or code is decided by the type `e` has
(of neither is an error); a splice of syntax takes no substitutions `.{...}`
and can't give an `[%expect]` (all of it is at the splice, so it could not be
corrected). Errors in the expression, a mismatch with the expected type
included, are reported at the `.~`, with a note printing it. Within quotes,
a splice is of code (a `Stag.Expr.t` there is a type error).

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
Stag.Type_ref.path : Stag.Type_ref.t -> Stag.Type_ref.Path.t
  (* Program ["A"; "M"; "t"], Prelude ["int"], Prelude ["Core"; "Sexp"; "t"] *)

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
the name it is declared with. A type ref's `path` is the names of its path
from the root scope: `Program` for the program's, the file's module, then
the modules it is in (`.A.M.t`), and `Prelude` for the prelude's, which
the program's root modules may shadow (a file `core.stag` beside the
prelude's `Core`), as a path from within the prelude, a builtin's just its
name. `expr` gives the type with each type it names
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

### Hygienic code

Generated syntax refers to library values hygienically by erasing typed code:

```ocaml
Stag.Code.erase : <'a>{..'r} -> Stag.Expr.t
Stag.Value_ref.name : Stag.Value_ref.t -> string
```

`erase c` is the syntax of `c` with what it refers to outside it resolved
where the quote was checked, as `Stag.Path.Ref`s: toplevel values and prims
(`Stag.Expr.Var (Ref r, None)`, `r : Stag.Value_ref.t`), constructors and
fields. The code's free variables (its context entries) are
`Stag.Path.Name`s, resolved where the syntax is spliced: they are the holes
that generated binders, or the splice site, provide. Its own binders keep
their names, but are renamed (eg. `x_1`) where one would capture a variable
of the same name bound elsewhere, or a free one; a renamed binder providing
an implicit splice `.~c` (in a nested quote) provides it explicitly,
`.~c.{x = x_1}`. Type annotations (on parameters, a let's result, and
`(x : t)`) are kept as written, the types they name as refs (printed as
root paths, eg. `(y : .int)`), so generated code has the types its quote
did; types as values are given as by `Stag.Type.expr`.

```ocaml
(* gen.stag *)
let atom = .< Core.Sexp.Atom (Core.Int.to_string x) >.      (* free x *)
let derive_int name =
  [Stag.Toplevel.let_ name ["x"] (Stag.Code.erase atom)]

(* a.stag *)
module Core = struct end
include .~(Gen.derive_int "sexp_of_n")   (* still the prelude's Core.Sexp.Atom *)
```

A `Value_ref` names the toplevel wherever it is spliced, even where its name
is shadowed or not in scope, and its type is the toplevel's (generalized
there), as if named by a path there. Whatever made it was visible where the
quote was checked, so it exists by the time a splice can use it; checking it
on demand is subject to the usual rules (eg. a toplevel that needs what the
running splice generates is an error). Toplevels referred to from inside a
quote but not yet checked are the quote's free variables by their canonical
names, and are erased as refs too; a splice provides these from where it is
even with explicit substitutions, which can't name them (so
`.~(.<n>.).{n}` inside `module M` works while `M.n` is unchecked). A local the code embeds (a local function
of the code that made it, used in the quote) has no name where the code is
spliced: `erase` is a runtime error naming it. A splice inside a nested quote
of the code takes its entries from what is in scope where spliced, by name,
as written. `erase` works at run time too; only splicing needs compile time.
Printed (eg. in a message about generated code), a ref is the root path of
its declaration (`.A.helper`, `.Core.Sexp.Atom`, `.raise`, prims by their
public path, `.String.length`), which reads back as the same declaration
wherever it is spliced, even under binders or declarations of its name; a
prelude's declaration whose root module the program shadows has no syntax and
is printed `<prelude.Core.Sexp.Atom>`. Generated code may write root paths
itself too, as names: `Stag.Expr.var ".raise"`, `Stag.Path.Name ".A.M.x"`.

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
are evaluated at compile time, once. The exception: if the typechecker abandons
a compile-time evaluation part way (eg. a splice needs a type whose definition
is not yet computed, and is retried later), the toplevel it was evaluating is
evaluated again from the start when next used, so its side effects may repeat.
Prelude toplevels are evaluated on demand only.

Computing a toplevel's value may not need that value: this is checked, not
found when run. A value needs the toplevels it uses, except functions it only
refers to (not calls) and those in closures it only builds or in the code a
quote builds; calling, matching on or passing a toplevel to a function is taken
to need everything that toplevel refers to when run. So `let x = f 1` with `let
f y = x` is rejected (`The value of x may be needed to compute itself: x -> f
-> x`), even if `f` would not read `x`, while `let xs = `Cons (1, fun _ -> xs)`
and recursive functions are fine.

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

## Benchmarks

As OCaml's ppx_bench:

```ocaml
let%bench "fib" = fib 20                  (* e is evaluated once per iteration *)
let%bench "sort" n in [100; 1000] = sort (random_list n)  (* times random_list too *)
```

`let%bench "name" = e` is a toplevel (in any module) whose body `e` may have
any type; its value is discarded. `let%bench "name" x in [a; b; ...] = e` (at
least one argument, all of one type, written as a list literal) is a bench
per argument, named
`name/A` after the argument's value as printed (`sort/100`, `str/"a"`,
truncated with `...` past 40 characters), with `x` bound to it in `e` only.
Bodies and arguments are checked at level 0 as toplevel lets are, and like
expect tests are evaluated only when benchmarking (`stag bench`, `stag-build
bench`), after the program's toplevels, each argument just before its bench
runs, and only if that bench is selected. Unparameterised names are unique in
a program; a parameterised bench whose expanded name another has fails. A
runtime error, in the body or an argument, stops its bench and is reported;
the others still run.

## Later

### Declaration splices

Typed declaration quotes (code types of signatures, `<sig ... end>{ctx}`) and
`let module` may come later, beside struct quotes.

### Modalities and code modes

```ocaml
val n : int @@ const      (* modalities on signature items *)
<t @ m>{ctx}              (* code modes *)
```

### Other

- A stag-build library named `core` shadows the prelude's `Core` in the whole
  program, `.Core` included. Derivers living in the prelude's `Core` (eg. a
  `Sexp.derive` in `core/sexp_derive.stag`) are unaffected: inside their
  quotes they refer to sibling modules relatively (`Int.to_string`), and the
  code they generate holds refs, so a user's `core` doesn't capture them. Code
  outside `Core` can still not name it then.

`if`, infix operators (`let (+) x y = ...`, OCaml precedences),
interface files.
