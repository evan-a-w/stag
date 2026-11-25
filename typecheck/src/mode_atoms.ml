open! Import

module Point = struct
  type t = T : 'a Mode.Axis.t * 'a -> t
end

let point : Syntax.Mode_expr.Atom.t -> Point.t = function
  | Const -> T (Stage, Const)
  | Portable -> T (Stage, Portable)
  | Nonportable -> T (Stage, Nonportable)
  | Static -> T (Provenance, Static)
  | Gc -> T (Provenance, Gc)
  | External -> T (Provenance, External)
  | Global -> T (Locality, Global)
  | Regional -> T (Locality, Regional)
  | Local -> T (Locality, Local)
;;

let invalid (atoms : Syntax.Mode_expr.Atom.t list) =
  let has atom = List.mem atoms atom ~equal:Syntax.Mode_expr.Atom.equal in
  if not (has Const)
  then None
  else
    List.find_map
      [ "gc", Syntax.Mode_expr.Atom.Gc; "external", External ]
      ~f:(fun (name, atom) ->
        Option.some_if
          (has atom)
          (sprintf
             "const and %s can't be combined: what is const is computed \
              without running code, so is static"
             name))
;;

let declared ~(default : Mode.Const.t) atoms ~loc =
  match invalid atoms with
  | Some why -> Error (Typecheck_error.create ~loc (Invalid_mode why))
  | None ->
    let const =
      List.fold atoms ~init:default ~f:(fun const atom ->
        let (T (axis, point)) = point atom in
        Mode.Const.set const axis point)
    in
    (match const.stage with
     | Const -> Ok { const with provenance = Static }
     | Portable | Nonportable -> Ok const)
;;
