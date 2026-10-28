open! Import

(** Each prints the syntax with [Printer] at each width (by default 80, 40 and
    10), parses it back, and prints what went wrong if it does not parse, parses
    to different syntax (up to locations), or prints differently. Silent if all
    is well. *)

val type_expr : ?widths:int list -> Syntax.Type_expr.t -> unit
val pattern : ?widths:int list -> Syntax.Pattern.t -> unit
val expr : ?widths:int list -> Syntax.Expr.t -> unit
val program : ?widths:int list -> Syntax.Program.t -> unit

(** As the others, for source text: formats it with [Printer.format], which
    must keep its comment texts in order as well as its syntax. *)
val source : ?widths:int list -> string -> unit

(** What [source] prints. *)
val source_errors : ?widths:int list -> string -> Sexp.t list

(** Without the [@ loc]s of [Located.t]s. *)
val strip_locs : Sexp.t -> Sexp.t
