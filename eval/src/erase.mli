open! Import

(** The syntax of code, for [Stag.Code.erase]: its own binders by their names
    (renamed, eg. [x_1], where a name would capture another variable), its free
    variables by name, and what it refers to outside it by canonical name
    ([Syntax.Decl_path.Resolved]): toplevels, prims, intrinsics, constructors
    and fields. Nodes keep the locations of the quotes they come from.

    Errs on a variable bound outside the code that is not a toplevel, a local
    function the code embeds, as nothing names it where the code is spliced. *)

(** Raised by [expr] on code with what did not check ([Typed.Expr.Desc.Error]). *)
exception Reached_error

val expr : Typed.Expr.t -> Syntax.Expr.t Or_error.t
