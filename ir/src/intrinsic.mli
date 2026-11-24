open! Import

(** Built-in polymorphic functions of the prelude, mostly of its [Stag] module,
    for reflection. They are named [Intrinsic.x], found only from within the
    prelude, which binds them to their public names, eg. [Stag.Type.expr]. Only
    [Type_decl] queries the typechecker, so is only available at compile time. *)

type t =
  | Type_expr (** [Stag.Type.t['a] -> Stag.Type_expr.t] *)
  | Type_ref_name (** [Stag.Type_ref.t -> string] *)
  | Type_ref_path (** [Stag.Type_ref.t -> Stag.Type_ref.Path.t] *)
  | Constructor_ref_name (** [Stag.Constructor_ref.t -> string] *)
  | Field_ref_name (** [Stag.Field_ref.t -> string] *)
  | Type_decl (** [Stag.Type_ref.t -> option[Stag.Type.Decl.t]] *)
  | Value_ref_name (** [Stag.Value_ref.t -> string] *)
  | Erase (** [<'a>{..'r} -> Stag.Expr.t] *)
  | Raise (** [exn -> 'a] *)
  | Ptr_make (** ['a -> ptr['a]] *)
  | Ptr_make_external (** ['a -> ptr['a @@ external]] *)
  | Assume_global (** ['a @ local -> 'a] *)
[@@deriving sexp_of, equal]

(** Its name in the prelude, eg. [Stag.Type.expr]. *)
val public_name : t -> string

(** [var] and [row] make the type of each type and row variable, so [ty] is an
    instance. *)
val ty : t -> var:(unit -> Type.ty) -> row:(unit -> Type.row) -> Type.ty

(** [Intrinsic.x] by its full path. *)
val find : Syntax.Path.t -> t option
