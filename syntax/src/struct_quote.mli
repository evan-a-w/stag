open! Import

(** The expression a struct quote [.< struct toplevels end >.] at [loc] is sugar
    for: building its declarations as a [list[Stag.Toplevel.t]] with the
    prelude's constructors, each antiquote's expression in its place. Errs, at
    the node, on an antiquote with substitutions, and on a resolved reference,
    which no expression makes. *)
val expand
  :  Syntax.Toplevel.t list
  -> loc:Loc.t
  -> (Syntax.Expr.t, Loc.t * string) Result.t
