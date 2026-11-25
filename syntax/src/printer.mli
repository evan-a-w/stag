open! Import

(** Syntax printed as stag source, laid out to [width] (80 by default), which
    parses back to the same syntax (up to locations).

    Syntax the parser never produces is printed as the closest that parses: one
    element [Tuple]s as their element, and empty ones and nominal records as
    [()] and [{}], and a [Fun] of no parameters as its body.

    A [Decl_path.Resolved], which has no syntax of its own, is printed as the
    root path naming its declaration from anywhere ([Decl_path.resolved_source]
    given [root_modules], the program's root module names, none by default): it
    parses as a [Decl_path.Root] naming the same declaration. Where no syntax
    names it, it is printed as [<prelude.P>], which does not parse. A [Field] of
    a [Root] or [Resolved] path is printed as [e.(.M.x)]. *)

val type_expr
  :  ?width:int
  -> ?root_modules:String.Set.t
  -> Syntax.Type_expr.t
  -> string

val pattern
  :  ?width:int
  -> ?root_modules:String.Set.t
  -> Syntax.Pattern.t
  -> string

val expr : ?width:int -> ?root_modules:String.Set.t -> Syntax.Expr.t -> string

val toplevel
  :  ?width:int
  -> ?root_modules:String.Set.t
  -> Syntax.Toplevel.t
  -> string

(** The toplevels, each ending with a newline, separated by blank lines but
    between one line [let]s, and between one line [type]s. *)
val program : ?width:int -> Syntax.Program.t -> string

(** As [program] without the final newline, laid out as the members of a module
    at column [indent], each line indented by [indent] (blank lines empty). *)
val toplevels : ?width:int -> indent:int -> Syntax.Toplevel.t list -> string

(** As [program], without the final newline, and with a blank line between
    toplevels [blank_between] their locations even where [program] would have
    none. *)
val program_doc
  :  width:int
  -> blank_between:(Loc.t -> Loc.t -> bool)
  -> Syntax.Program.t
  -> Doc.t

(** The text of the [|] that starts a match arm or variant constructor, which
    starts a line when broken. *)
val case_bar : string

(** The texts of the first and last lines of a module with members. *)
val module_open : name:string -> string

val module_close : string
