open! Import

type t =
  { ident : Typed.Ident.t
  ; ty : Type.ty
  ; loc : Loc.t
  ; paths : Mode_shape.Path.t Nonempty_list.t
  (** To the parts of the scrutinee it may be bound to: several, if bound by the
      alternatives of an or-pattern. *)
  }

(** Bound to the whole of what is matched. *)
val bound : Typed.Ident.t -> Type.ty -> loc:Loc.t -> t

val under : Mode_shape.Path.Step.t -> t list -> t list

(** The variables of an or-pattern, of its [left] alternative's and [right]'s:
    one bound by both (to the same ident) is bound to the parts either binds it
    to. *)
val alternatives : t list -> t list -> t list
