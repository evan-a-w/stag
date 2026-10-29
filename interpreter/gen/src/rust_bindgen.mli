open! Import

(** Generates the Rust types of registered OCaml declarations, and their
    [FromOcaml] conversions, as one file arranged into modules (included at
    the root of the [stag_ast] crate, whose support code it uses).

    - [t] of a module [M] is the Rust type [M] of its parent module, others
      are named in CamelCase ([Typed.Expr.t] is [typed::Expr], [Type.ty] is
      [type_::Ty]). Type parameters are generic parameters.
    - [int] is [i64], [char] is [u8], [string] is [String], ['a list] is
      [Vec<A>], ['a array] is [Box<[A]>], Core maps and sets are
      [crate::Map] and [crate::Set], keeping the OCaml order.
    - Records with a mutable field, and [[@@rust.shared]] declarations, are
      stored: each OCaml value of theirs is converted once, into the
      [Arena] of the conversion, and referred to by an [Id], so sharing and
      cycles are kept. The [Arena] has a [Store] per instantiation of a
      stored type.
    - A field is boxed where its type could otherwise contain itself.
    - An alias that re-exports a declaration under the same Rust name, eg.
      [Type.Var.t], is left out. *)
val generate : Ppx_rust_runtime.Decl.t list -> string Or_error.t
