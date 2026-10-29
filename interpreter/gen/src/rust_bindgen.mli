open! Import

(** Generates the Rust types of registered OCaml declarations, their
    [FromOcaml] conversions and [ToOcaml] builders, as one file arranged into modules (included at
    the root of the [stag_ast] crate, whose support code it uses).

    - [t] of a module [M] is the Rust type [M] of its parent module, others
      are named in CamelCase ([Typed.Expr.t] is [typed::Expr], [Type.ty] is
      [type_::Ty]). Type parameters are generic parameters.
    - [int] is [i64], [char] is [u8], [string] is [String], ['a list] is
      [Vec<A>], ['a array] is [Box<[A]>], Core maps and sets are
      [crate::Map] and [crate::Set] (hash maps and sets), built back by the
      OCaml callbacks registered for their key types ([OcamlKey]).
    - Records with a mutable field, and [[@@rust.shared]] declarations, are
      stored: each OCaml value of theirs is converted once, into the
      [Arena] of the conversion, and referred to by a [Ref], so sharing and
      cycles are kept, both ways. The [Arena] has a [Store] per instantiation of a
      stored type.
    - A field is boxed where its type could otherwise contain itself.
    - An alias that re-exports a declaration under the same Rust name, eg.
      [Type.Var.t], is left out.
    - Declarations with [[@rust.skip]] fields must be in [rebuilt]
      ([Ppx_rust_runtime.rebuilt]): their values built in OCaml are rebuilt
      by the OCaml callback registered for them. *)
val generate
  :  rebuilt:Ppx_rust_runtime.Key.t list
  -> Ppx_rust_runtime.Decl.t list
  -> string Or_error.t
