open! Import

type parameters = string -> Int.Set.t option

let iter ~con ~var ~cycle roots =
  let visited = Int.Table.create () in
  let visit id ~f =
    match Hashtbl.find visited id with
    | Some `Visiting -> cycle ()
    | Some `Done -> Ok ()
    | None ->
      Hashtbl.set visited ~key:id ~data:`Visiting;
      let%map.Result () = f () in
      Hashtbl.set visited ~key:id ~data:`Done
  in
  let rec ty : Type.ty -> _ = function
    | TVar { id; state = Link target; _ } -> visit id ~f:(fun () -> ty target)
    | TVar v -> var v.id
    | TCon ("ptr", _) | TArrow _ -> Ok ()
    | TCon (name, args) ->
      let%bind.Result stored = con name in
      args_by_value stored 0 args
    | TRecord { id; row = fields } | TVariant { id; row = fields } ->
      visit id ~f:(fun () -> row fields)
    | TTuple tys -> List.fold_result tys ~init:() ~f:(fun () entry -> ty entry)
    | TCode { inner; context } ->
      let%bind.Result () = ty inner in
      row context
  and row : Type.row -> _ = function
    | REmpty -> Ok ()
    | RVar { id; state = Link target; _ } -> visit id ~f:(fun () -> row target)
    | RVar v -> var v.id
    | RExtend (_, presence, _, entry, rest) ->
      let%bind.Result () =
        match Type.canonical_presence presence with
        | Absent -> Ok ()
        | Present | PVar _ -> ty entry
      in
      row rest
  and arg : Type.arg -> _ = function
    | ATy entry -> ty entry
    | ARow fields -> row fields
  and args_by_value stored index = function
    | [] -> Ok ()
    | entry :: rest ->
      let%bind.Result () =
        match stored with
        | Some stored when not (Set.mem stored index) -> Ok ()
        | Some _ | None -> arg entry
      in
      args_by_value stored (index + 1) rest
  in
  List.fold_result roots ~init:() ~f:(fun () entry -> arg entry)
;;
