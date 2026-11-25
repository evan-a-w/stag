open! Import

(** The splices of syntax that are outside any quote within it: the antiquotes
    of a struct quote's declarations, or the type splices of a declaration. A
    quote or struct quote within it quotes its splices, a splice within that
    unquoting again. *)

module Kind = Traverse.Splice.Kind

type t =
  { kind : Kind.t
  ; expr : Syntax.Expr.t
  ; loc : Loc.t (** Of the whole splice, [.~e]. *)
  ; has_subs : bool (** An expression splice with substitutions, [.~e.{x}]. *)
  }
[@@deriving sexp_of]

(** Replaces the expression of each splice, in order, or with [type_expr], each
    type splice by the type it gives. *)
val map
  :  ?type_expr:(t -> Syntax.Type_expr.t)
  -> Syntax.Toplevel.t list
  -> f:(t -> Syntax.Expr.t)
  -> Syntax.Toplevel.t list

val iter : Syntax.Toplevel.t list -> f:(t -> unit) -> unit

(** [placeholder i] stands for the [i]th splice's expression, which no program
    can write, and [index] gives [i] back. *)
val placeholder : int -> Syntax.Expr.t

val index : Syntax.Expr.t -> int option
