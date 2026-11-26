open! Import

type t =
  { ident : Typed.Ident.t
  ; ty : Type.ty
  ; loc : Loc.t
  ; path : Mode_shape.Path.t
  }

let under step = List.map ~f:(fun t -> { t with path = step :: t.path })
