open! Import

(** The expression a struct quote [.< struct toplevels end >.] at [loc] is sugar
    for: building its declarations as a [list[Stag.Toplevel.t]] with the
    prelude's constructors. Each antiquote is first bound, in order, by a let
    located at it and annotated with the type of its kind, so its errors are
    there. Errs, at the node, on an antiquote with substitutions, and on a
    resolved reference, which no expression makes. *)
val expand
  :  Syntax.Toplevel.t list
  -> loc:Loc.t
  -> (Syntax.Expr.t, Loc.t * string) Result.t
