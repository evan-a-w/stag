open! Import

type t =
  { ident : Typed.Ident.t
  ; ty : Type.ty
  ; loc : Loc.t
  ; path : Mode_shape.Path.t (** To the part of the scrutinee it is bound to. *)
  }

val under : Mode_shape.Path.Step.t -> t list -> t list
