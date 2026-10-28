open! Import

(** Syntax printed as stag source, laid out to [width] (80 by default), which
    parses back to the same syntax (up to locations).

    Syntax the parser never produces is printed as the closest that parses:
    one element [Tuple]s as their element, and empty ones and nominal records
    as [()] and [{}]. *)

val type_expr : ?width:int -> Syntax.Type_expr.t -> string
val pattern : ?width:int -> Syntax.Pattern.t -> string
val expr : ?width:int -> Syntax.Expr.t -> string
val toplevel : ?width:int -> Syntax.Toplevel.t -> string

(** The toplevels, each ending with a newline, separated by blank lines but
    between one line [let]s, and between one line [type]s. *)
val program : ?width:int -> Syntax.Program.t -> string
