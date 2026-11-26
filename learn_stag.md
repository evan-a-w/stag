# Learn stag in Y minutes

stag is a staged ML: OCaml-like syntax, by-value records and variants,
explicit pointers, typed quotes (`.< e >.`) and splices (`.~e`) that run code
at compile time, and OxCaml-style modes tracking *when* (stage), *where from*
(provenance) and *how long* (locality) a value lives. It is implemented in
OCaml, and today runs in an interpreter (a compiler to LLVM is planned, see
[semantics.md](semantics.md)).

```sh
stag check FILE.stag     # typecheck, print the signature
stag check -modes FILE.stag  # ... with every mode and mode constraint
stag run FILE.stag       # typecheck and evaluate
stag test FILE.stag      # run let%expect_test tests (-promote to accept)
stag fmt -inplace FILE.stag
```

Comments below show what the checker prints (`val x : t`), and the value a
toplevel ends up with (`val x : t = v`), what is printed (`prints: ...`) and
error messages (`Error: ...`). Each code block is a program of its own, the file
`doc.stag` (so the module `Doc`), and they are all checked and run by `driver/test/test_learn_stag.ml`, which fails
if a comment no longer holds. For the details, see [syntax.md](syntax.md),
[semantics.md](semantics.md) and [modes.md](modes.md).

## Basics

```stag
(* Comments are (* nestable *) as in OCaml. *)

let answer = 42                    (* val answer : int = 42 *)
let million = 1_000_000            (* val million : int = 1000000 *)
let negative = -7                  (* val negative : int = -7 *)
let greeting = "hello\tworld\n"    (* val greeting : string *)
let raw = {|no \escapes here|}     (* val raw : string = "no \\escapes here" *)
let letter = 'x'                   (* val letter : char = 'x' *)
let yes = true                     (* val yes : bool = true *)
let nothing = ()                   (* val nothing : unit = () *)

(* There are no infix operators (yet), and no binary minus: arithmetic and
   comparison are functions. `-` only makes a literal negative. *)
let sum = Int.add 1 2                       (* val sum : int = 3 *)
let diff = Int.sub 10 (-3)                  (* val diff : int = 13 *)
let quotient = Int.div 17 5                 (* val quotient : int = 3 *)
let smaller = Int.lt 1 2                    (* val smaller : bool = true *)
let joined = String.append "foo" "bar"      (* val joined : string = "foobar" *)
let shown = Int.to_string 42                (* val shown : string = "42" *)
let flipped = Bool.not true                 (* val flipped : bool = false *)

(* Functions. Every function is curried. *)
let add x y = Int.add x y          (* val add : int -> int -> int *)
let add5 = add 5                   (* val add5 : int -> int *)
let eight = add5 3                 (* val eight : int = 8 *)
let twice f x = f (f x)            (* val twice : ('a -> 'a) -> 'a -> 'a *)
let square = fun x -> Int.mul x x  (* val square : int -> int *)
let sixteen = twice square 2       (* val sixteen : int = 16 *)
let shout (s : string) : string = String.append s "!"

(* Every `let` is recursive: there is no `rec`. There is no `if` either:
   match on a bool. As a `let` is recursive, a value can't use the one it
   shadows, nor itself: `let s = String.length s` is an error (the `s` is
   the one being defined), so give it another name. *)
let fact n =
  match Int.equal n 0 with
  | true -> 1
  | false -> Int.mul n (fact (Int.sub n 1))
let fact10 = fact 10               (* val fact10 : int = 3628800 *)

(* Toplevels may be used before they are defined, and mutually recursive. *)
let is_even n = match n with 0 -> true | _ -> is_odd (Int.sub n 1)
let is_odd n = match n with 0 -> false | _ -> is_even (Int.sub n 1)
let even10 = is_even 10            (* val even10 : bool = true *)

(* `e1; e2` sequences, `let ... in` is local. *)
let hello =
  let name = "stag" in
  print_string "hello, ";
  print_endline name               (* prints: hello, stag *)
```

## Records, tuples and variants

Records, tuples and variants are *values*, copied as a whole, like C structs:
there are no mutable fields (mutation goes through pointers, below).

```stag
type point = {x : int; y : int}     (* type point = { x : int; y : int } *)
let origin = {x = 0; y = 0}         (* val origin : point = {Doc.x = 0; y = 0} *)
let moved = {origin with y = 5}     (* val moved : point = {Doc.x = 0; y = 5} *)
let y = moved.y                     (* val y : int = 5 *)

let pair = (1, "one")               (* val pair : int * string = (1, "one") *)

type shape = Circle of int | Rect of int * int | Empty
let shapes = (Circle 2, Rect (3, 4), Empty)

(* Type parameters are written in brackets, after the name. *)
type tree['a] = Leaf | Node of 'a * ptr[tree['a]] * ptr[tree['a]]

(* Polymorphic variants and polymorphic (structural) records use backticks. *)
let tag = `Num 7                    (* val tag : [`Num of int; ..'a] *)
let person = {`name = "ada"; `age = 36}
(* val person : {`age : int; `name : string} *)
let older = {person with `age = Int.add person.`age 1}
let with_id = {`id = 1 | person}   (* extend *)
let without_age = person \ `age    (* restrict *)
(* val without_age : {`name : string} *)

(* Functions on polymorphic records are row-polymorphic. *)
let get_name r = r.`name
(* val get_name : {`name : 'a; ..'b} -> 'a *)
```

## Pattern matching

```stag
type shape = Circle of int | Rect of int * int | Empty

let area s =
  match s with
  | Circle r -> Int.mul 3 (Int.mul r r)
  | Rect (w, h) -> Int.mul w h
  | Empty -> 0
let areas = (area (Circle 2), area (Rect (3, 4)))  (* val areas : int * int = (12, 12) *)

(* Literals, tuples, records, wildcards, nesting. Arms extend as far as
   possible, so parenthesize nested matches. *)
let describe p =
  match p with
  | (0, _) -> "zero"
  | (1, "one") -> "one, spelled out"
  | (n, _) -> Int.to_string n
let d = describe (1, "one")         (* val d : string = "one, spelled out" *)

let size v =
  match v with
  | `Num n -> n
  | `Str s -> String.length s
  | `Nothing -> 0
(* val size : [`Nothing | `Num of int | `Str of string] -> int *)

let name_of r = match r with {`name = n; ..} -> n

(* Exceptions: declared like constructors, handled by `exception` arms of a
   match (there is no `try`). *)
exception Not_found of string
let lookup k = match k with "a" -> 1 | _ -> raise (Not_found k)
let safe k =
  match lookup k with
  | v -> v
  | exception Not_found _ -> -1
let found = (safe "a", safe "b")    (* val found : int * int = (1, -1) *)

(* Or-patterns: either alternative, binding the same variables. *)
let corners s =
  match s with
  | Circle _ | Empty -> 0
  | Rect (n, _) | Rect (_, n) -> n  (* the first that matches binds n *)
let c = corners (Rect (3, 4))       (* val c : int = 3 *)
let small n = match n with 1 | 2 | 3 -> true | _ -> false
let s = (small 2, small 7)          (* val s : bool * bool = (true, false) *)
let either v = match v with `L n | `R n -> n
(* val either : [`L of 'a | `R of 'a] -> 'a *)
```

There is no exhaustiveness check: a value no arm matches fails at run time.

```stag-error
let f n = match n with 0 -> "zero"
let boom = f 1                      (* Error: Match failure *)
```

Both alternatives of an or-pattern must bind the same variables.

```stag-error
let g s = match s with Circle r | Empty -> 0 | Rect _ -> 1
(* Error: The variable r is bound by only one side of this or-pattern *)
```

## Pointers

`ptr[t]` is the mutable pointer type, `Ptr.make e` allocates, `p.*` (or `*p`)
reads, `p.* <- v` stores. `box[t]` is its immutable counterpart: `Box.make e`
allocates, `b.*` and `*b` read, and nothing can store into it. Recursive types
must go through a pointer or a box (or a function): the prelude's `list` is
`Nil | Cons of box['a * list['a]]`.

```stag
let p = Ptr.make 1                  (* val p : ptr[int] *)
let one = p.*                       (* val one : int = 1 *)
let stored = p.* <- 2; p.*          (* val stored : int = 2 *)

type point = {x : int; y : int}
let q = Ptr.make {x = 1; y = 2}
let copy = q.*                      (* records are values: a copy *)
let after =
  q.*.x <- 10;                      (* store into a field of the pointee *)
  (q.*, copy)
(* val after : point * point = ({Doc.x = 10; y = 2}, {Doc.x = 1; y = 2}) *)

(* `*p` in a pattern matches the pointee. *)
type tree = Leaf of int | Branch of ptr[tree * tree]
let rec_sum t =
  match t with
  | Leaf n -> n
  | Branch *(l, r) -> Int.add (rec_sum l) (rec_sum r)
let tree = Branch (Ptr.make (Leaf 1, Branch (Ptr.make (Leaf 2, Leaf 3))))
let total = rec_sum tree            (* val total : int = 6 *)
```

```stag-error
type bad = Bad of int * bad        (* Error: recursive *)
```

```stag
let b = Box.make (1, 2)             (* val b : box[int * int] *)
let pair = b.*                      (* val pair : int * int = (1, 2) *)
(* An unannotated dereference is of a ptr: annotate a box you are given. *)
let first (b : box[int * int]) = match b.* with (x, _) -> x
```

```stag-error
let b = Box.make 1
let bad (u : unit) = b.* <- 2
(* Error: A box is immutable *)
```

## Mutable variables

`let mut` binds a variable that `<-` assigns. Closures capture it by reference.

```stag
let count_to n =
  let mut total = 0 in
  let add k = total <- Int.add total k in
  add n;
  add n;
  total
let c = count_to 3                  (* val c : int = 6 *)

(* A toplevel `let mut` is a mutable global. *)
let mut counter = 0
let tick u = counter <- Int.add counter 1
let ticked = tick (); tick (); counter    (* val ticked : int = 2 *)
```

```stag-error
let x = 1
let f u = x <- 2                   (* Error: x is not mutable *)
```

## Lists, options and results

```stag
let xs = [1; 2; 3]                  (* val xs : list[int] *)
let rec_len l = match l with [] -> 0 | Cons *(_, rest) -> Int.add 1 (rec_len rest)
let first l = match l with [] -> None | [x] -> Some x | Cons *(x, _) -> Some x
let hd = first xs                   (* val hd : option[int] = Some 1 *)

(* The core library is the prelude's module Core. *)
let doubled = Core.List.map (Int.mul 2) xs   (* val doubled : list[int] *)
let total = Core.List.fold_left Int.add 0 xs (* val total : int = 6 *)
let evens = Core.List.filter (fun x -> Int.equal (Int.rem x 2) 0) [1; 2; 3; 4]
let found = Core.List.find (Int.equal 5) xs  (* val found : option[int] = None *)
let default = Core.Option.value found 0      (* val default : int = 0 *)
let parsed = Core.Int.of_string "12"         (* val parsed : option[int] = Some 12 *)
let res = Core.Result.map (Int.add 1) (Ok 1)
let words = Core.String.concat ", " ["a"; "b"]  (* val words : string = "a, b" *)
let sexp = Core.Sexp.to_string (Core.Sexp.List [Core.Sexp.Atom "a"; Core.Sexp.Atom "b c"])
(* val sexp : string *)
```

## Modules, files and tests

```stag
module Geometry = struct
  type point = {x : int; y : int}
  let origin = {x = 0; y = 0}
  module Inner = struct
    let unit_x = {x = 1; y = 0}
  end
end

let o = Geometry.origin
let ux = Geometry.Inner.unit_x.x    (* val ux : int = 1 *)
(* Fields and constructors are found through the type when it is known. *)
let get_x (p : Geometry.point) = p.x

(* A root path, starting with `.`, names a declaration from the root, whatever
   shadows it locally. This file is doc.stag, so its module is Doc. *)
let x = 10
let shadowed = let x = 1 in .Doc.x (* val shadowed : int = 10 *)
let prim = .Int.add 1 2            (* the prims and prelude are roots too *)

(* Expect tests, as ppx_expect: `stag test` corrects the [%expect]s. *)
let%expect_test "addition" =
  print_int (Int.add 1 2);
  [%expect {| 3 |}]

(* Benchmarks, run by `stag bench`. *)
let%bench "fact" = Int.mul 2 3
```

Each file is a module (`foo_bar.stag` is `Foo_bar`), files may refer to each
other in any order, and there are no interface files yet. Projects are built by
`stag-build`, from a `stag-project` file at the root and one `stag` stanza
file per directory, dune-style (see [README.md](README.md#stag-build)):

```stag-project
-- math/stag
(library (name math))
-- math/arith.stag
let square x = Int.mul x x
let%expect_test "square" =
  print_int (square 7);
  [%expect {| 49 |}]
-- ffi/stag
(library (name ffi) (compile_time_c_calls))
-- ffi/c.stag
external labs : int -> int = "labs"
-- bin/stag
(executable (name main) (libraries math ffi))
-- bin/main.stag
(* Library `math`'s file arith.stag is Math.Arith. *)
let main = print_endline (Int.to_string (Math.Arith.square 7))   (* prints: 49 *)
(* `(compile_time_c_calls)` lets ffi's externals run at compile time. *)
let at_compile_time = .~(match Ffi.C.labs (-3) with 3 -> .<"three">. | _ -> .<"?">.)
let print_it = print_endline at_compile_time    (* prints: three *)
```

`stag-build build` checks every stanza, `stag-build exec main` runs the
executable, `stag-build test` runs the expect tests of libraries and tests.

## Staging

```stag
(* A quote is a value: code, of type <t>{context}, the context being the free
   variables the code needs (open, ..'a, as more may be added). *)
let one = .< 1 >.                   (* val one : <int>{..'a} *)
let plus_x = .< Int.add x 1 >.      (* val plus_x : <int>{x : int; ..'a} *)

(* A splice inside a quote inserts code into code. *)
let two = .< Int.add .~one .~one >.

(* A splice outside any quote runs at compile time, and the code it gives
   takes its place. *)
let three = .~(.< Int.add 1 2 >.)   (* val three : int = 3 *)
let loud = .~(print_endline "compiling"; .< 0 >.)   (* prints: compiling *)

(* The splice's free variables are taken from the scope at the splice... *)
let f x = .~plus_x                  (* val f : int -> int *)
let f2 = f 1                        (* val f2 : int = 2 *)
(* ...or given explicitly, by substitutions. *)
let g y = .~plus_x.{x = Int.mul y 10}
let g2 = g 2                        (* val g2 : int = 21 *)

(* The classic: power, unrolled at compile time. *)
let power n x =
  match n with
  | 0 -> .< 1 >.
  | _ -> .< Int.mul .~x .~(power (Int.sub n 1) x) >.
let cube x = .~(power 3 .< x >.)    (* val cube : int -> int *)
let c = cube 3                      (* val c : int = 27 *)

(* Code can be turned into syntax, and declarations generated from it. An
   inline expansion shows what a declaration splice generates, and `stag test`
   corrects it if it is stale. *)
include%inline .~[Stag.Toplevel.let_ "square" ["y"] (Stag.Code.erase (power 2 .< y >.))]
  let square y = Int.mul y (Int.mul y 1)
end%inline
```

A name in scope inside a quote always means that variable. The quote embeds its
value if the stage axis of modes (below) allows it: `const` and `portable`
values. Anything else is an error. Names not in scope are free variables of the
quote, provided where the code is spliced, and `forget x in e` takes `x` out of
scope in `e`, so a quote in `e` can use a free `x`.

```stag
let n = 3                           (* a toplevel constant: const *)
let uses_n = .< Int.add n 1 >.      (* val uses_n : <int>{..'a} *)

(* Local constants, and plain data (ints, ...), are embedded. *)
let local_const = let k = "s" in .< k >.          (* val local_const : <string>{..'a} *)
let local_int = let k = Int.add 1 2 in .< k >.    (* val local_int : <int>{..'_a} *)

(* A computed string, a function's argument, or a closure capturing one, can't
   be embedded. `forget` makes the quote's `k` and `x` free variables instead:
   whatever `k` and `x` are where the code is spliced, not these. *)
let computed = let k = Int.to_string 3 in forget k in .< k >.
(* val computed : <'_a>{k : '_a; ..'_b} *)
let argument x = forget x in .< x >. (* val argument : 'a -> <'b>{x : 'b; ..'c} *)

(* Annotating an argument portable embeds it, but then callers must pass
   portable values. *)
let lift (x @ portable) = .< x >.   (* val lift : 'a -> <'a>{..'b} *)
let lifted = .~(lift 5)             (* val lifted : int = 5 *)

(* Code may assign a mutable free variable: its context says `mut`. *)
let incr = .< x <- Int.add x 1 >.   (* val incr : <unit>{x : mut int; ..'a} *)
let twice_incr u =
  let mut x = 0 in
  .~incr;
  .~incr;
  x
let two = twice_incr ()             (* val two : int = 2 *)
```

Without `forget`, using a variable the quote can't embed is an error:

```stag-error
let argument x = .< x >.
(* Error: The quote can't embed x, as it may not be portable. Bind it as (x @ portable) to embed it, or write [forget x in ...] to make x a free variable of the quote *)
```

A forgotten name is out of scope outside quotes too:

```stag-error
let shadowed x = let y = 1 in forget y in Int.add x y
(* Error: y is not in scope here: it was forgotten by [forget y in ...] *)
```

Compile-time code only sees what exists at compile time: no mutable globals,
and no C calls unless the library allows them.

```stag-error
let mut counter = 0
let at_compile_time = .~(match counter with 0 -> .< "zero" >. | _ -> .< "more" >.)
(* Error: counter *)
```

## Reflection and generated declarations

Types are values too, for writing derivers: `type t` is a `Stag.Type.t[t]`,
and declarations are generated as data (`Stag.Toplevel.t`, mirroring the
syntax), by declaration splices (`include .~e`, `module M = .~e`).

```stag
type color = Red | Green | Blue of int
let t = type color                  (* val t : Stag.Type.t[color] *)

(* The core library's sexp deriver, as ppx_sexp_conv. *)
include .~(Core.Sexp.derive (type color))
let s = Core.Sexp.to_string (sexp_of_color (Blue 3))   (* val s : string = "(Blue 3)" *)
let back = color_of_sexp (Core.Sexp.Atom "Red")          (* val back : result[color, Core.Sexp.Of_sexp_error.t] = Ok Doc.Red *)

(* Reflecting a declaration, at compile time. *)
let constructors ty =
  match Stag.Type.expr ty with
  | Stag.Type_expr.Con (Stag.Path.Ref r, _) ->
    (match Stag.Type.decl r with
     | Some {kind = Stag.Type.Kind.Variant cs; _} -> Core.List.length cs
     | _ -> 0)
  | _ -> 0
let n_colors = .~(Stag.Expr.int (constructors (type color)))   (* val n_colors : int = 3 *)

(* A struct quote builds declarations; antiquotes splice syntax into it. *)
let make_answer v = .< struct let answer = .~(Stag.Expr.int v) end >.
module Answer = .~(make_answer 42)
let answer = Answer.answer          (* val answer : int = 42 *)
```

## Modes

Every value has a mode, a point on three independent axes, each ordered from
most to least restrictive about the value (`a <= b`: an `a` may be used where
a `b` is expected):

- **stage** `const <= portable <= nonportable`: `const` is known at compile
  time (literals, toplevel constants, closures of them); `portable` may be
  embedded in code for a later stage; `nonportable` is anything else.
- **provenance** `static <= gc <= external`: what memory the value may reach:
  static data, the GC heap, or memory the GC doesn't manage (from C).
- **locality** `global <= regional <= local`: as OxCaml. `local` values live in
  the current function's region (a stack) and can't escape it; `regional` is
  local to an enclosing region.

Modes are inferred: nothing above needed one. They only show up when something
doesn't fit, or when asked for. A ```` ```stag modes ```` block of this
document prints signatures with modes; by default only modes other than the
defaults are printed.

```stag
(* stack_ allocates locally: the closure lives in sum3's region. *)
let sum3 a b c =
  let add = stack_ (fun x -> Int.add x a) in
  Int.add (add b) (add c)
let s = sum3 1 2 3                  (* val s : int = 7 *)

(* Pointer-free data (ints, bools, records of them, ...) crosses modes: it is
   the same value anywhere, so a local int is returned freely. *)
let read_local (p : ptr[int] @ local) = p.*
(* val read_local : ptr[int] @ local -> int *)
(* Crossing waits for the type: [c] is known to be an int only once it is
   compared, yet it crosses. *)
let sign (p : ptr[int] @ local) f =
  let c = f p in
  match Int.lt c 0 with
  | true -> c
  | false -> 0
(* val sign : ptr[int] @ local -> ((ptr[int] @ regional -> int) -> int) @ local *)
(* A type variable may be bounded to pointer-free types (with 'a :
   pointer_free), so its values cross: a local argument is returned global. *)
let keep : 'a @ 'm -> 'a @ global with 'a : pointer_free = fun x -> x
(* val keep : 'a -> 'a *)
let kept (x : int @ local) = keep x     (* val kept : int @ local -> int *)

(* exclave_ ends the region early, so a function can return a value allocated
   in its caller's region. *)
let make_local x = exclave_ stack_ (Ptr.make x)
(* val make_local : 'a -> ptr['a] @ local *)
let used = read_local (make_local 5)    (* val used : int = 5 *)

(* The components of a by-value tuple or record keep their own modes: the
   global pointer can be returned though its pair holds a local one. *)
let second x =
  let pair = (stack_ (Ptr.make x), Ptr.make x) in
  match pair with (_, g) -> g
(* val second : 'a -> ptr['a] *)
```

```stag-error
(* A local value can't escape its region. *)
let leak x = stack_ (Ptr.make x)
(* Error: This is local, because it is allocated by [stack_] *)
(* ... but must be at most regional, because the function returns it, ending
   its region (return it with [exclave_] instead) *)
```

While a weak variable's type isn't known, a constraint crossing would spare
waits on it, and `stag check -modes` (which prints modes in full) prints it
with what it waits on: after `let cell = Ptr.make (loop ())`, a function storing a local
argument into `cell` is `'_a @ 'm0 -> unit with local <= 'm0 and 'm0 <= global
unless pointer_free('_a)`. This form is only printed, never written.

```stag-error
(* An instance of a pointer-free variable at a type with pointers is an error
   where it is used. *)
let keep : 'a @ 'm -> 'a @ global with 'a : pointer_free = fun x -> x
let s = keep "s"
(* Error: This type has pointers, because its type turned out to be string *)
```

```stag-error
(* A tail call ends the caller's region first, so it can't be passed what is
   local to it... *)
let g (p : ptr[int] @ local) = p.*
let f x = let p = stack_ (Ptr.make x) in g p
(* Error: because it is passed in a tail call, after the current region has ended (mark the call [@nontail]) *)
```

```stag
(* ...unless it is marked non-tail. *)
let g (p : ptr[int] @ local) = p.*
let f x = let p = stack_ (Ptr.make x) in g p [@nontail]
let seven = f 7                     (* val seven : int = 7 *)
```

`[@nontail]` only helps when the call's result doesn't outlive the region. To
return a value built from a local result, the call must give it in the
caller's region, with `exclave_`:

```stag-error
let lb (x : int) = exclave_ stack_ (Box.make x)
let wrap (x : int) = Box.make (lb x)
(* Error: ... but must be at most regional, because the function returns it,
   ending its region (return it with [exclave_] instead) *)
```

```stag
let lb (x : int) = exclave_ stack_ (Box.make x)
let wrap (x : int) = exclave_ Box.make (lb x)
```

### Where allocations go, and regions

An allocation a function returns is made where its caller wants it: on the
heap if the result is used `global`ly, in the caller's region if `local`ly. The
same function serves both, so code can be "GC or arena" by how it is used.

```stag modes
let mk (x : int) = Box.make x
(* val mk : int @ _ -> box[int] @ 'm0 with gc <= 'm0 *)
let unbox (b : box[int]) = match b with *n -> n
let kept : box[int] = mk 1          (* on the heap *)
let temp (u : unit) = let b @ local = mk 2 in unbox b [@nontail]
(* in temp's region *)
```

`region_ e` evaluates `e` in a region of its own, freed when it ends, like a
function body: nothing local to it can be its value.

```stag
let mk (x : int) = Box.make x
let unbox (b : box[int]) = match b with *n -> n
let total = region_ (let a @ local = mk 1 in let b @ local = mk 2 in Int.add (unbox a) (unbox b))
(* val total : int = 3 *)
```

```stag-error
let mk (x : int) = Box.make x
let leaks (u : unit) = region_ (let b @ local = mk 1 in b)
(* Error: but must be at most regional, because the [region_] returns it, ending its region (return it with [exclave_] instead) *)
```


### Annotations

`(e @ m)` checks that `e` is at most `m`. On a binding, `let x @ m = e` and a
parameter `(x @ m)` are *exact*: `x` has mode `m` from then on. Modes on
arrows: `t @ m -> u @ n`. Mode variables `'m` are shared across one
toplevel's annotations, and `with` adds constraints between them.

```stag
(* A portable parameter can be embedded in code; callers must pass one. *)
let embed (x @ portable) = .< x >.

(* `@ local`: the parameter is borrowed, so callers may pass local values. *)
let first (p : (int * int) @ local) = match p with (a, _) -> a

(* Prims only read their arguments, so take local (and external) ones. *)
let len (s : string @ local) = String.length s

(* Any expression can be annotated, which only checks it. *)
let sum = (Int.add 1 2 : int @ portable)    (* val sum : int = 3 *)

(* A mode variable makes the result exactly as the argument (printed without
   modes, this is just string -> string). *)
let same (x : string @ 'm) : string @ 'm = x

(* A locality with a mode variable fixes the locality, the variable giving
   the stage and provenance: a cache's comparison is ['k @ local 'p -> ...]. *)
let first_len (f : string @ local 'm -> int) (s : string @ 'm) = f s

(* `with` bounds variables (written results are at least portable). *)
let capped : 'a @ 'm -> 'a @ 'n with 'n <= portable = fun x -> x
let ok = capped 1                   (* val ok : int = 1 *)
```

An arrow written without modes has the default ones, `global nonportable gc`,
as OCaml code would expect: annotating `(f : string -> string)` makes `f` take
and return global values. Write mode variables to keep it polymorphic.

```stag-error
let apply (f : string -> int) x = f x
let use (x @ local) = apply (fun y -> 0) x
(* Error: because its type is written without a mode *)
```

```stag
let apply (f : string @ 'm -> int) x = f x
let use (x @ local) = apply (fun y -> 0) x
```

```stag-error
let capped : 'a @ 'm -> 'a @ 'n with 'n <= portable = fun x -> x
let bad = capped (Int.to_string 3)
(* Error: This is nonportable *)
```

```stag-error
(* `(x @ m)` only checks; `let x @ m = e` makes `x` exactly `m`. *)
let s = "s"
let checked = (s @ external)       (* still static: fits a gc cell *)
let exact @ external = s           (* now external: doesn't *)
let cell = Ptr.make "c"
let ok = cell.* <- checked
let bad = cell.* <- exact
(* Error: This is external, because it is annotated external *)
```

```stag-error
(* Toplevels are global, and so must be what is stored through a pointer. *)
let store (p : ptr[ptr[int]]) x = p.* <- stack_ (Ptr.make x)
(* Error: must be at most global *)
```

### Modalities

`t @@ m` on a record field, constructor argument or pointee bounds what it may
hold. On a value it is a *meet*: reading a field `@@ global` from a local
record gives a global value. Through a pointer it is *exact*: every alias of
the cell agrees on `m`, so a store takes at most `m` and a read gives `m`.

```stag
type boxed = { inner : ptr[int] @@ global; other : ptr[int] }
let get_inner (b : boxed @ local) = b.inner   (* a global value: returnable *)
(* val get_inner : boxed @ local -> ptr[int] *)

(* ptr[t] is ptr[t @@ gc]; external data (from C) needs a pointee that says so. *)
let cell : ptr[string @@ external] = Ptr.make "s"
let put (s @ external) = cell.* <- s
```

```stag-error
let cell = Ptr.make "s"
let put (s @ external) = cell.* <- s
(* Error: must be at most gc *)
```

Types can take mode parameters (`'p : mode`), applied as `cell[t, m]`; `ptr`
itself is `ptr['a, 'p : mode]`, with `ptr[t @@ m]` sugar for `ptr[t, m]`.

```stag
type cell['a, 'p : mode] = { value : 'a @@ 'p }
let gc_cell : cell[string, gc] = { value = "x" }
let read_cell (c : cell['a, 'p]) = c.value

(* A declared arrow may take and give values of a mode parameter. The closure
   of [cmp a] holds [a], so is of ['p] too. *)
type sorter['k, 'p : mode] = { cmp : 'k @ 'p -> ('k @ 'p -> int) @ 'p }
let by_length : sorter[string, external] =
  { cmp = fun a b -> Int.sub (String.length a) (String.length b) }
```

### Variance

Where a value is used at a type (an argument, an annotation, a binding's or a
field's type), only the types' shapes must match: their modes need only fit,
each as its position says. A function taking more, or returning less, may be
used as one that doesn't: arguments are *contravariant*, results, immutable
fields and a `box`'s contents *covariant*. A pointer's contents are
*invariant*, as every alias of the cell must agree. A type's mode parameter
gets the variance of where it is used. Where nothing is used as the other, as
a match's arms, types are equal.

```stag
let apply (f : string -> int) = f "a"
let local_ok = apply (fun (s @ local) -> String.length s)  (* val local_ok : int = 1 *)
let fs : list[string -> int] = [fun (s @ local) -> String.length s]

type tagged['p : mode] = { payload : string @@ 'p }
let widen (t : tagged[gc]) : tagged[external] = t
```

```stag-error
let apply (f : string @ local -> int) = f "a"
let bad = apply (fun (s : string) -> let g @ global = s in String.length g)
(* Error: must be at most global *)
```

```stag-error
let p : ptr[string] = Ptr.make "a"
let widen (q : ptr[string @@ external]) = q
let bad = widen p
(* Error: This expression has type ptr[string] but an expression was expected of type ptr[string @@ external] *)
```

### Reading printed signatures

With modes printed, each arrow side gets a variable, `'m0`, `'m1`, ... and a
`with` clause lists the constraints between them. A variable written
once and constrained by nothing prints as `_`, which reads back as any mode. Read `a <= b` as "a flows
into b". A plain `'m0 <= 'm1` holds on every axis; `stage(..)`, `provenance(..)`,
`locality(..)` pick one axis; `meet(x, c)` caps by a modality `c`; `release(x)`
is a regional value seen once its region ends (it becomes `local`);
`static_if_const(..)` / `const_if_static(..)` are the links between stage and
provenance (a `const` value is `static`; a value allocated at run time is
`gc` unless `const`).

```stag modes
let id x = x
(* val id : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) *)

type r = { x : string @@ gc; y : string }
let get_x (r : r) = r.x
(* val get_x : r @ 'm0 -> string @ 'm1 with stage('m0) <= stage('m1) and meet(provenance('m0), gc) <= provenance('m1) and release(locality('m0)) <= locality('m1) *)

let embed (x @ portable) = .< x >.
(* val embed : 'a @ 'm0 -> <'a>{..'b} @ 'm1 with portable <= 'm0 and release(locality('m0)) <= locality('m1) and 'm0 <= portable gc and gc <= 'm1 *)
(* What is lifted into code is copied as static data, so must be in the GC heap: [gc]. *)

let mk (x : ptr[int] @ local) = exclave_ stack_ (Ptr.make 1)
(* val mk : ptr[int] @ 'm0 -> ptr[int @@ _] @ 'm1 with local <= 'm0 and local nonportable gc <= 'm1 *)

let same (x : string @ 'm) : string @ 'm = x
(* val same : string @ 'm0 -> string @ 'm0 with release(locality('m0)) <= locality('m0) *)
(* [release(..) <= ..] on one variable: 'm isn't regional, as a regional argument is local to the caller once returned. *)

let static_if : 'a @ 'm -> 'a @ 'n with provenance('n) <= static_if_const(stage('m)) = fun x -> x
(* val static_if : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and provenance('m1) <= static_if_const(stage('m0)) *)
```

### Example: a map and a cache

`Core.Map` (a persistent AVL tree) and `Core.Lru` (a cache of nodes linked by
pointers, indexed by a map) are written once for every mode. Their last
parameter is the provenance of the keys and values held: a map of GC
strings is also one of possibly external strings (it is covariant), but a
cache, being mutable, is invariant. What they build goes where the caller
wants it, as their builders return it with `exclave_`, and `int` values
cross every axis. A comparison takes keys of the map's or cache's
provenance: `Core.String.compare` takes any, while one only for GC keys
suits only a GC map or cache. A pair's components keep their own modes, so
`concat` can match `(l, r)` without either map's mode constraining the
other's.
A value used at compile time and lifted into code must be `portable`, and
at most `gc` (static data can't refer outside the GC heap); the code then
holds it as a constant (`const static`).

```stag
let m = Core.Map.of_list Core.Int.compare [(1, "a"); (2, "b")]
let wide : Core.Map.t[int, string, external] = m
let size (u : unit) =
  let local_m @ local = Core.Map.add Core.Int.compare m 3 "c" in
  Core.Map.cardinal local_m [@nontail]
let three = size ()                   (* val three : int = 3 *)

let keywords = .~(
  let table @ portable =
    Core.Map.of_list Core.String.compare [("let", 1); ("in", 2)] in
  .< table >.)
let is_let = Core.Map.mem Core.String.compare keywords "let"  (* val is_let : bool = true *)
let static_keywords @ const static = keywords

let cache : Core.Lru.t[int, string, gc] = Core.Lru.create 2 Core.Int.compare
let used =
  Core.Lru.add cache 1 "a";
  Core.Lru.add cache 2 "b";
  Core.Lru.add cache 3 "c";
  Core.Lru.find cache 1               (* val used : option[string] = None *)
```

```stag-error
let cache : Core.Lru.t[int, string, gc] = Core.Lru.create 2 Core.Int.compare
let as_external : Core.Lru.t[int, string, external] = cache
(* Error: but an expression was expected of type Core.Lru.t[int, string, external] *)
```

## C functions

`external` binds a C function, called through libffi. For now only immediates
(`int`, `bool`, `char`, `unit`) may cross.

```stag
external labs : int -> int = "labs"
let a = labs (Int.sub 0 42)         (* val a : int = 42 *)
```

```stag-error
external strlen : string -> int = "strlen"
(* Error: string *)
```

At compile time, C calls are only allowed in libraries whose stanza says
`(compile_time_c_calls)` (see the project above).
