open! Import

(** Conversions between [Syntax] and its representation as values of the types
    of the [Prelude] ([Stag.Expr.t] etc.). *)

module type S = sig
  type t

  val to_value : t -> Treewalk.Value.t

  (** Every node is located at [loc], as syntax generated where it is spliced.
      Errs on a value not of the prelude's type, eg. made by another module's
      constructors of the same name, or on a name that is not an identifier of
      the kind its position needs (eg. a lowercase constructor, or a keyword),
      saying which, and within which constructors. *)
  val of_value : Treewalk.Value.t -> loc:Loc.t -> t Or_error.t
end

module Type_expr : S with type t := Syntax.Type_expr.t
module Type_decl : S with type t := Syntax.Type_decl.t
module Pattern : S with type t := Syntax.Pattern.t
module Expr : S with type t := Syntax.Expr.t
module Toplevel : S with type t := Syntax.Toplevel.t
module Program : S with type t := Syntax.Program.t
