open! Import

module type S = sig
  type t

  val to_value : t -> Value.t

  (** Every node is located at [loc], as syntax generated where it is spliced.

      Errs, saying what was expected and where (within which constructors,
      list elements, pair components and record fields), on:
      - a value not of the prelude's type, eg. made by another module's
        constructors of the same name;
      - a name that is not an identifier of the kind its position needs, eg. a
        lowercase constructor, a keyword, or [_] where the parser reads a
        name (it is accepted only as a let's name or a parameter);
      - a shape that no source means: a tuple of one component, or of none
        except the type [()] ([()] is [Lit Unit] elsewhere), or a nominal
        record of no fields (expression, pattern or type declaration).

      Other shapes the parser can't produce but that are meaningful are
      accepted, left to the typechecker: a negative int literal, a match of no
      arms (of an empty
      type), a constructor applied to one tuple (of a
      constructor declared [C of (a * b)]), a polymorphic record extended or
      updated with no fields. *)
  val of_value : Value.t -> loc:Loc.t -> t Or_error.t
end

module type Syntax_value = sig
  (** Conversions between [Syntax] and its representation as values of the
      types of the [Prelude] ([Stag.Expr.t] etc.). *)

  module type S = S

  module Type_expr : S with type t := Syntax.Type_expr.t
  module Type_decl : S with type t := Syntax.Type_decl.t
  module Pattern : S with type t := Syntax.Pattern.t
  module Expr : S with type t := Syntax.Expr.t
  module Toplevel : S with type t := Syntax.Toplevel.t
  module Program : S with type t := Syntax.Program.t

  (** An [option[Stag.Type.Decl.t]]. *)
  val reflected_decl : Reflected_decl.t option -> Value.t
end
