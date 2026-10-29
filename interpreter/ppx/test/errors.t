Declarations [@@deriving rust] rejects, and names the ppx resolves wrongly
unless OCaml agrees.

  $ compile () {
  >   ocamlfind ocamlc -package base,sexplib0 -linkpkg \
  >     -I ../runtime/.ppx_rust_runtime.objs/byte ../runtime/ppx_rust_runtime.cma \
  >     -ppx "./driver.exe --as-ppx" "$1" -o "$1.exe" 2>&1 \
  >     | grep -v '^ *[0-9]* |\|^ *\^'
  >   true
  > }

  $ cat > gadt.ml <<'OCAML'
  > type t = A : int -> t [@@deriving rust]
  > OCAML
  $ compile gadt.ml
  File "gadt.ml", line 1, characters 9-21:
  Error: [@@deriving rust]: GADT constructors are unsupported

  $ cat > abstract.ml <<'OCAML'
  > type t [@@deriving rust]
  > OCAML
  $ compile abstract.ml
  File "abstract.ml", line 1, characters 0-24:
  Error: [@@deriving rust]: abstract types are unsupported

  $ cat > ocaml_unboxed.ml <<'OCAML'
  > type t = { x : int } [@@ocaml.unboxed] [@@deriving rust]
  > OCAML
  $ compile ocaml_unboxed.ml
  File "ocaml_unboxed.ml", line 1, characters 0-56:
  Error: [@@deriving rust]: [@@unboxed] is unsupported

  $ cat > unboxed.ml <<'OCAML'
  > type t = A of string [@@unboxed] [@@deriving rust]
  > OCAML
  $ compile unboxed.ml
  File "unboxed.ml", line 1, characters 0-50:
  Error: [@@deriving rust]: [@@unboxed] is unsupported

  $ cat > functor.ml <<'OCAML'
  > module F (X : sig end) = struct type t = int [@@deriving rust] end
  > module X = struct end
  > type t = F(X).t [@@deriving rust]
  > OCAML
  $ compile functor.ml
  File "functor.ml", line 3, characters 9-15:
  Error: [@@deriving rust]: functor applications are unsupported

  $ cat > shared_alias.ml <<'OCAML'
  > type t = int [@@deriving rust] [@@rust.shared]
  > OCAML
  $ compile shared_alias.ml
  File "shared_alias.ml", line 1, characters 0-46:
  Error: [@@deriving rust]: [@@rust.shared] on an alias is unsupported: put it
         on the declaration it aliases

  $ cat > shared_immediates.ml <<'OCAML'
  > type t = A | B [@@deriving rust] [@@rust.shared]
  > OCAML
  $ compile shared_immediates.ml
  File "shared_immediates.ml", line 1, characters 0-48:
  Error: [@@deriving rust]: [@@rust.shared] on a type without blocks is
         unsupported: its values are immediates

  $ cat > function.ml <<'OCAML'
  > type t = { f : int -> int } [@@deriving rust]
  > OCAML
  $ compile function.ml
  File "function.ml", line 1, characters 15-25:
  Error: [@@deriving rust]: unsupported type (only variables, constructors and
         tuples)

  $ cat > constraint.ml <<'OCAML'
  > type 'a t = 'a constraint 'a = int [@@deriving rust]
  > OCAML
  $ compile constraint.ml
  File "constraint.ml", line 1, characters 0-52:
  Error: [@@deriving rust]: constraints are unsupported

  $ cat > polymorphic_variant.ml <<'OCAML'
  > type t = [ `A ] [@@deriving rust]
  > OCAML
  $ compile polymorphic_variant.ml
  File "polymorphic_variant.ml", line 1, characters 9-15:
  Error: [@@deriving rust]: unsupported type (only variables, constructors and
         tuples)

A type referred to must derive rust: here [u] is shadowed by one that does
not.

  $ cat > shadowed.ml <<'OCAML'
  > type u = A [@@deriving rust]
  > module M = struct
  >   type u = string
  >   type t = { y : u } [@@deriving rust]
  > end
  > OCAML
  $ compile shadowed.ml
  File "shadowed.ml", line 4, characters 17-18:
  Error: The value rust_u has type u/2 Ppx_rust_runtime.Witness.t
         but an expression was expected of type u Ppx_rust_runtime.Witness.t
         Type u/2 is not compatible with type u = string
         File "shadowed.ml", line 3, characters 2-17:
           Definition of type u
         File "shadowed.ml", line 1, characters 0-28:
           Definition of type u/2

Names resolve as OCaml resolves them, to the declarations before them rather
than later ones of the same name: [N.t]'s [u] is the outer [u].

  $ cat > order.ml <<'OCAML'
  > type u = A [@@deriving rust]
  > module N = struct
  >   type t = { x : u } [@@deriving rust]
  >   type u = B of int [@@deriving rust]
  > end
  > let () =
  >   List.iter
  >     (fun d -> print_endline (Sexplib0.Sexp.to_string (Ppx_rust_runtime.Decl.sexp_of_t d)))
  >     (Ppx_rust_runtime.all ())
  > OCAML
  $ compile order.ml && ./order.ml.exe
  ((key(Order u))(params())(kind(Variant(((name A)(args(Tuple()))))))(shared false))
  ((key(Order N t))(params())(kind(Record(((name x)(ty(Decl(Order u)()))(mutable_ false)(skip false)))))(shared false))
  ((key(Order N u))(params())(kind(Variant(((name B)(args(Tuple(Int)))))))(shared false))

A builtin's name must be the builtin.

  $ cat > builtin.ml <<'OCAML'
  > module Option = struct type 'a t = A of 'a [@@deriving rust] end
  > type t = int Option.t [@@deriving rust]
  > OCAML
  $ compile builtin.ml
  File "builtin.ml", line 2, characters 9-21:
  Error: The value Stdlib.Fun.id has type int Option.t -> int Option.t
         but an expression was expected of type
           int Option.t -> 'a Stdlib.Option.t
         Type int Option.t is not compatible with type
           'a Stdlib.Option.t = 'a option

  $ cat > map.ml <<'OCAML'
  > module K = struct
  >   module Map = struct type 'a t = 'a list end
  > end
  > type t = int K.Map.t [@@deriving rust]
  > OCAML
  $ compile map.ml
  File "map.ml", line 4, characters 9-20:
  Error: Unbound type constructor K.t
