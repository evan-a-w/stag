open! Import

(** Syntax printed as stag source, laid out to [width] (80 by default), which
    parses back to the same syntax (up to locations).

    Syntax the parser never produces is printed as the closest that parses:
    one element [Tuple]s as their element, and empty ones and nominal records
    as [()] and [{}], and a [Fun] of no parameters as its body.

    A [Decl_path.Resolved], which has no syntax of its own, is printed as its
    canonical name as a path, without the prelude's root for the prelude's
    names ([Prelude_path.to_string_hum]): it parses as a [Decl_path.Path],
    which names the same declaration only where that path is not shadowed
    (and is in scope). *)

val type_expr : ?width:int -> Syntax.Type_expr.t -> string
val pattern : ?width:int -> Syntax.Pattern.t -> string
val expr : ?width:int -> Syntax.Expr.t -> string
val toplevel : ?width:int -> Syntax.Toplevel.t -> string

(** The toplevels, each ending with a newline, separated by blank lines but
    between one line [let]s, and between one line [type]s. *)
val program : ?width:int -> Syntax.Program.t -> string

(** As [program] without the final newline, laid out as the members of a
    module at column [indent], each line indented by [indent] (blank lines
    empty). *)
val toplevels : ?width:int -> indent:int -> Syntax.Toplevel.t list -> string

(** As [program], without the final newline, and with a blank line between
    toplevels [blank_between] their locations even where [program] would have
    none. *)
val program_doc
  :  width:int
  -> blank_between:(Loc.t -> Loc.t -> bool)
  -> Syntax.Program.t
  -> Doc.t
