open! Import

(** Rust names of OCaml ones. *)

(** As OCaml's, but Rust keywords get a trailing [_]. *)
val field : string -> string

(** [type_expr] to [TypeExpr], ['bound] to [Bound]. *)
val camel : string -> string

(** The Rust module path and type name of the declaration [name] of the
    module [module_path]: [t] is named after its module, in the parent
    module ([Typed.Expr.t] is [typed::Expr]), and modules are lowercase
    ([Type] is [type_]). *)
val decl : module_path:string list -> name:string -> string list * string

(** [crate::a::b::Name], or relative to the module [from] when inside it. *)
val path : from:string list -> string list * string -> string

(** [RecordType] to [record_type]. *)
val snake : string -> string

(** The names the support code of the crate takes at its root, which
    generated types and modules may not. *)
val crate_types : String.Set.t

val crate_modules : String.Set.t
