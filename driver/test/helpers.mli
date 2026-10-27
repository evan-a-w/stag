open! Import
open Syntax

(** Builders of unlocated syntax, for tests of what stag source can't express. *)

module Ty : sig
  val con : ?args:Type_expr.arg list -> string -> Type_expr.t
  val int : Type_expr.t
  val var : string -> Type_expr.t
end

val int : int -> Expr.t
val string : string -> Expr.t
val unit : Expr.t
val var : string -> Expr.t
val app : Expr.t -> Expr.t list -> Expr.t

(** [let name params = equal in in_] *)
val let_ : string -> string list -> Expr.t -> Expr.t -> Expr.t

(** [let name (arg : annot) ... : annot = equal in in_] *)
val expr_let
  :  string
     * (string * Type_expr.t option) list
     * Type_expr.t option
     * Expr.t
     * Expr.t
  -> Expr.t

val seq : Expr.t -> Expr.t -> Expr.t
val quote : Expr.t -> Expr.t
val splice : ?subs:(string * Expr.t option) list -> Expr.t -> Expr.t
val tuple : Expr.t list -> Expr.t
val construct : ?args:Expr.t list -> string -> Expr.t
val record : (string * Expr.t) list -> Expr.t
val match_ : Expr.t -> (Pattern.t * Expr.t) list -> Expr.t
val tag : ?arg:Expr.t -> string -> Expr.t
val typed : string -> Type_expr.t -> string * Type_expr.t option

(** As [expr_let], for a toplevel. *)
val top_let
  :  string * (string * Type_expr.t option) list * Type_expr.t option * Expr.t
  -> Toplevel.t

val top : ?annot:Type_expr.t -> string -> string list -> Expr.t -> Toplevel.t
val module_ : string -> Toplevel.t list -> Toplevel.t

module Pat : sig
  val var : string -> Pattern.t
  val construct : ?args:Pattern.t list -> string -> Pattern.t
  val record : (string * Pattern.t) list -> Pattern.t
  val tag : ?arg:Pattern.t -> string -> Pattern.t
end

module Decl : sig
  val ty : string -> Type_decl.Param.t Located.t

  val variant
    :  ?params:Type_decl.Param.t Located.t list
    -> string
    -> (string * Type_expr.t list) list
    -> Toplevel.t

  val record : string -> (string * Type_expr.t) list -> Toplevel.t
  val alias : string -> Type_expr.t -> Toplevel.t
  val abstract : string -> Toplevel.t
end
