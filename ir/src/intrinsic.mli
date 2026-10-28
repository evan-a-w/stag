open! Import

(** Built-in functions of the prelude's [Stag] module that query the
    typechecker, so are only available at compile time. They are named
    [Intrinsic.x], found only from within the prelude, which binds them to
    their public names, eg. [Stag.Type.expr]. *)

type t =
  | Type_expr (** [Stag.Type.t['a] -> Stag.Type_expr.t] *)
  | Type_decl
  (** [Stag.Ref.t -> option[{`params : list[Stag.Type_decl.Param.t]; `kind : Stag.Type.Kind.t}]] *)
  | Ref_name (** [Stag.Ref.t -> string] *)
[@@deriving sexp_of, equal]

(** Its name in the prelude, eg. [Stag.Type.expr]. *)
val public_name : t -> string

(** [var] makes the type of each type variable, so [ty] is an instance, and
    [record] the record type of a row. *)
val ty : t -> var:(unit -> Type.ty) -> record:(Type.row -> Type.ty) -> Type.ty

(** [Intrinsic.x] by its full path. *)
val find : Syntax.Path.t -> t option
