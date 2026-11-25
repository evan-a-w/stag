open! Import
open Syntax

(** The one enumeration of the children of syntax, with how each child's scope
    differs from its parent's, which the walkers over syntax share. *)

module Splice : sig
  module Kind : sig
    type t =
      | Item
      (** [include .~e] or [module M = .~e], of a [list[Stag.Toplevel.t]]. *)
      | Type_expr (** [.~e] as a type, of a [Stag.Type_expr.t]. *)
      | Type_kind
      (** [type t = .~e], the definition, of a [Stag.Type_decl.Kind.t]. *)
      | Expr (** [.~e] as an expression, of a [Stag.Expr.t]. *)
    [@@deriving sexp_of, compare, equal]
  end

  type t =
    { kind : Kind.t
    ; loc : Loc.t (** Of the whole splice, [.~e]. *)
    ; has_subs : bool (** An expression splice with substitutions, [.~e.{x}]. *)
    }
  [@@deriving sexp_of]
end

module Namespace : sig
  type t =
    | Type
    | Constructor
    | Field
    | Value
  [@@deriving sexp_of, compare, equal]
end

(** What declarations a toplevel child is one of. *)
module Members : sig
  type t =
    | Module of string (** [module M = struct ... end]'s. *)
    | Struct_quote (** A struct quote's, one quote in. *)
    | Expansion
    (** An inline expansion's, [include%inline .~e ... end%inline] or
        [module%inline M = .~e ... end%inline]. *)
  [@@deriving sexp_of, compare, equal]
end

module Child : sig
  type t =
    { depth : int
    (** Quotes in, relative to its parent: [1] in a quote, [-1] in a splice. *)
    ; binds : string list
    (** The values bound in it that are not in its parent: by its parent's
        binders, or for one of [members], declared by the members before it. *)
    ; splice : Splice.t option (** It is the expression of this splice. *)
    ; members : Members.t option
    }
  [@@deriving sexp_of]

  (** In its parent's scope. *)
  val same : t
end

(** The values [pattern] binds. *)
val pattern_binders : Pattern.t -> string list

(** How to map each kind of child, given the context [enter] gives it from its
    parent's. Expansions are children only [into_expansions]. *)
module Visitor : sig
  type 'ctx t =
    { enter : 'ctx -> Child.t -> 'ctx
    ; into_expansions : bool
    ; expr : 'ctx t -> 'ctx -> Child.t -> Expr.t -> Expr.t
    ; pattern : 'ctx t -> 'ctx -> Pattern.t -> Pattern.t
    ; type_expr : 'ctx t -> 'ctx -> Type_expr.t -> Type_expr.t
    ; toplevel : 'ctx t -> 'ctx -> Toplevel.t -> Toplevel.t
    ; decl_path :
        'ctx -> Namespace.t -> Decl_path.t Located.t -> Decl_path.t Located.t
    }

  (** Maps each node to itself with its children mapped, and each path to
      itself. *)
  val default
    :  enter:('ctx -> Child.t -> 'ctx)
    -> into_expansions:bool
    -> 'ctx t
end

(** The node with each of its children (or path) mapped by the visitor, in
    source order, given the node's context. *)

val expr : 'ctx Visitor.t -> 'ctx -> Expr.t -> Expr.t
val pattern : 'ctx Visitor.t -> 'ctx -> Pattern.t -> Pattern.t
val type_expr : 'ctx Visitor.t -> 'ctx -> Type_expr.t -> Type_expr.t
val toplevel : 'ctx Visitor.t -> 'ctx -> Toplevel.t -> Toplevel.t
