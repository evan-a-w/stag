open! Import

(** The expression a toplevel splice of syntax gave, whose errors are reported
    at the splice. *)
type t =
  { splice : Loc.t
  ; generated : Syntax.Expr.t
  }

val wrap_error : t -> Typecheck_error.t -> Typecheck_error.t
