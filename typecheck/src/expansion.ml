open! Import

type t =
  { toplevel : Syntax.Toplevel.t
  ; generated : Syntax.Toplevel.t list
  }
[@@deriving sexp_of]
