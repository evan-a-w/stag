open! Import

(** A declaration that is not abstract, as reflection gives it
    ([Stag.Type.Decl.t]): its constructors and fields by canonical name, and
    the types in it by canonical name ([Syntax.Decl_path.Resolved]). *)

module Kind : sig
  type t =
    | Variant of (string * Syntax.Type_expr.t list) list
    | Record of (string * Syntax.Type_expr.t) list
    | Alias of Syntax.Type_expr.t
end

type t =
  { params : Syntax.Type_decl.Param.t Located.t list
  ; kind : Kind.t
  }
