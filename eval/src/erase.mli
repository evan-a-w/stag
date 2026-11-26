open! Import

(** The syntax of code, for [Stag.Code.erase]: its own binders by their names
    (renamed, eg. [x_1], where a name would capture another variable), its free
    variables by name, and what it refers to outside it by canonical name
    ([Syntax.Decl_path.Resolved]): toplevels, prims, intrinsics, constructors
    and fields. Nodes keep the locations of the quotes they come from.

    Errs on a variable bound outside the code that is not a toplevel, a local
    function the code embeds, as nothing names it where the code is spliced. *)

module Error : sig
  type t =
    | Reached_error
    (** Of code with what did not check ([Typed.Expr.Desc.Error]). *)
    | Inexpressible of Base.Error.t
    (** Of what no syntax names where the code is spliced: a local it embeds, an
        external, or an intrinsic only the prelude may use. *)
  [@@deriving sexp_of]
end

val expr : Typed.Expr.t -> (Syntax.Expr.t, Error.t) Result.t
