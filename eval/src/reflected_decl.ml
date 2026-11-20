open! Import

module Kind = struct
  type t =
    | Variant of (string * Syntax.Type_expr.t list) list
    | Record of (string * Syntax.Type_expr.t) list
    | Alias of Syntax.Type_expr.t
end

type t =
  { params : Syntax.Type_decl.Param.t Located.t list
  ; kind : Kind.t
  }

module Reflection = struct
  type nonrec t =
    { decl : t option
    ; decl_type : Typed.Record_type.t
    }
end
