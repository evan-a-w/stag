open! Import

(** Each prints the syntax with [Printer] at each width (by default 80, 40 and
    10), parses it back, and prints what went wrong if it does not parse, parses
    to different syntax (up to locations), or prints differently. Silent if all
    is well. *)

val type_expr : ?widths:int list -> Syntax.Type_expr.t -> unit
val pattern : ?widths:int list -> Syntax.Pattern.t -> unit
val expr : ?widths:int list -> Syntax.Expr.t -> unit
val program : ?widths:int list -> Syntax.Program.t -> unit

(** Without the [@ loc]s of [Located.t]s. *)
val strip_locs : Sexp.t -> Sexp.t
