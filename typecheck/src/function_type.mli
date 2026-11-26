open! Import

module Level : sig
  type t =
    { args : (Typed.Ident.t * Type.ty) list
    ; annots : (Typed.Ident.t * Syntax.Annot.t) list
    ; ty : Type.ty
    ; loc : Loc.t
    }
end

type t =
  { args : (Typed.Ident.t * Type.ty) list (** The outermost [fun]'s. *)
  ; annots : (Typed.Ident.t * Syntax.Annot.t) list
  ; inner : Level.t list
  (** The [fun]s directly nested in its body, outermost first: one function with
      theirs, as [fun a b -> e] is. *)
  ; body : Source.Expr.t (** The innermost [fun]'s. *)
  ; result_annot : Syntax.Type_expr.t option
  ; res_ty : Type.ty
  ; ty : Type.ty
  ; arg_modes : Mode.Var.t list (** Of every level's arguments. *)
  ; res_mode : Mode.Var.t (** The mode of the body's value. *)
  ; partial_modes : Mode.Var.t list
  (** Those of the closures of partial applications, of all but the last
      argument. *)
  }

val params : t -> (Typed.Ident.t * Type.ty) list

(** An annotation that fails to elaborate is reported, and ignored. Mode
    annotations on the arguments, and with [res_mode] on the result, bound their
    modes. Unless [merge] is false, [fun]s directly nested in an unannotated
    body are arguments of the same function. *)
val create
  :  ?merge:bool
  -> Fresh.t
  -> Scope.t
  -> args:(Source.Binder.t * Syntax.Annot.t) list
  -> annot:Syntax.Type_expr.t option
  -> res_mode:Syntax.Mode_expr.t option
  -> body:Source.Expr.t
  -> loc:Loc.t
  -> t
