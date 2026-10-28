open! Import
open Syntax

(** Random syntax, of the forms the parser produces. *)

type t =
  { random : Random.State.t
  ; mutable size : int
  ; deep : bool (** Whether to take the recursive choices while size is left. *)
  }

val int : t -> int -> int
val bool : t -> bool
val pick : t -> 'a list -> 'a
val type_expr : t -> Type_expr.t
val pattern : t -> Pattern.t
val expr : t -> Expr.t
val toplevel : t -> Toplevel.t
