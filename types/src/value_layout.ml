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
    | TCon (name, _) when String.equal name Type.ptr_name -> Ok ()
    | TArrow _ -> Ok ()
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
    | AMode _ -> Ok ()
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

module Guard = struct
  type t =
    { pointer : bool
    ; structure : bool
    }

  let to_int { pointer; structure } =
    Bool.to_int pointer + (2 * Bool.to_int structure)
  ;;
end

let reaches_unguarded
  ?(through_structure = false)
  ~con
  ~needs_structure
  ~var
  roots
  =
  let visited = Hash_set.Poly.create () in
  let guarded (guard : Guard.t) =
    guard.pointer && (guard.structure || not needs_structure)
  in
  let reached (guard : Guard.t) = guard.structure || not through_structure in
  let visit id (guard : Guard.t) ~f =
    let key = id, Guard.to_int guard in
    if Hash_set.mem visited key
    then false
    else (
      Hash_set.add visited key;
      f ())
  in
  let rec ty (guard : Guard.t) (ty' : Type.ty) =
    (not (guarded guard))
    &&
    match ty' with
    | TVar { id; _ } when id = var -> reached guard
    | TVar { id; state = Link target; _ } ->
      visit id guard ~f:(fun () -> ty guard target)
    | TVar _ -> false
    | TCon (name, args) when String.equal name Type.ptr_name ->
      args_ { guard with pointer = true } args
    | TCon (name, args) ->
      let stored = con name in
      let guard = { guard with structure = true } in
      List.existsi args ~f:(fun index entry ->
        match stored with
        | Some stored when not (Set.mem stored index) ->
          arg { guard with pointer = true } entry
        | Some _ | None -> arg guard entry)
    | TArrow (from, to_, _) ->
      let guard = { guard with structure = true } in
      ty guard from || ty guard to_
    | TRecord { id; row = fields } | TVariant { id; row = fields } ->
      let guard = { guard with structure = true } in
      visit id guard ~f:(fun () -> row guard fields)
    | TTuple tys -> List.exists tys ~f:(ty { guard with structure = true })
    | TCode { inner; context } ->
      let guard = { guard with structure = true } in
      ty guard inner || row guard context
  and row guard (row' : Type.row) =
    (not (guarded guard))
    &&
    match row' with
    | REmpty -> false
    | RVar { id; _ } when id = var -> reached guard
    | RVar { id; state = Link target; _ } ->
      visit id guard ~f:(fun () -> row guard target)
    | RVar _ -> false
    | RExtend (_, presence, _, entry, rest) ->
      (match Type.canonical_presence presence with
       | Absent -> false
       | Present | PVar _ -> ty guard entry)
      || row guard rest
  and arg guard : Type.arg -> bool = function
    | ATy entry -> ty guard entry
    | ARow fields -> row guard fields
    | AMode _ -> false
  and args_ guard args = List.exists args ~f:(arg guard) in
  args_ { pointer = false; structure = false } roots
;;
