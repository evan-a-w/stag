open! Import

(** Each prints the syntax with [Printer] at each width (by default 80, 40 and
    10), parses it back, and prints what went wrong if it does not parse, parses
    to different syntax (up to locations), or prints differently. Silent if all
    is well. A [Decl_path.Resolved] is taken to be the same as the path
    [Printer] prints for it. *)

val type_expr : ?widths:int list -> Syntax.Type_expr.t -> unit
val pattern : ?widths:int list -> Syntax.Pattern.t -> unit
val expr : ?widths:int list -> Syntax.Expr.t -> unit
val program : ?widths:int list -> Syntax.Program.t -> unit

(** As the others, for source text: formats it with [Formatter.format], which
    must keep its syntax and its comments in order, their texts the same but
    for the indentation of their later lines, all moved alike. *)
val source : ?widths:int list -> string -> unit

(** What [source] prints. *)
val source_errors : ?widths:int list -> string -> Sexp.t list

(** Without the [@ loc]s of [Located.t]s. *)
val strip_locs : Sexp.t -> Sexp.t

(** With each [Decl_path.Resolved] as the path [Printer] prints for it. *)
val without_resolved : Sexp.t -> Sexp.t
