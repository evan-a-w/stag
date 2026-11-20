open! Import

type _ t =
  | Expr : Syntax.Expr.t t
  | Type_expr : Syntax.Type_expr.t t
  | Type_decl_kind : Syntax.Type_decl.Kind.t t
  | Program : Syntax.Program.t t
