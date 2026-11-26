open! Import

type t =
  { ident : Typed.Ident.t
  ; ty : Type.ty
  ; loc : Loc.t
  ; paths : Mode_shape.Path.t Nonempty_list.t
  }

let bound ident ty ~loc = { ident; ty; loc; paths = [ [] ] }

let under step =
  List.map ~f:(fun t ->
    { t with paths = Nonempty_list.map t.paths ~f:(fun path -> step :: path) })
;;

let alternatives left right =
  List.map left ~f:(fun t ->
    match
      List.find right ~f:(fun other -> Typed.Ident.equal other.ident t.ident)
    with
    | Some other ->
      { t with
        paths = Nonempty_list.append t.paths (Nonempty_list.to_list other.paths)
      }
    | None -> t)
  @ List.filter right ~f:(fun other ->
    not (List.exists left ~f:(fun t -> Typed.Ident.equal other.ident t.ident)))
;;
