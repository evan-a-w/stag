open! Core

let instantiator fresh ~rank : Type.Mapper.t =
  let tys = Int.Table.create () in
  let rows = Int.Table.create () in
  let presences = Int.Table.create () in
  let inst table (var : _ Type.Var.t) ~wrap ~bound =
    match var.state with
    | Unbound _ | Link _ -> wrap var
    | Generic ->
      (match Hashtbl.find table var.id with
       | Some instance -> instance
       | None ->
         let var' = Fresh.var fresh ~rank in
         let instance = wrap var' in
         Hashtbl.set table ~key:var.id ~data:instance;
         var'.bounds <- List.map var.bounds ~f:bound;
         instance)
  in
  let rec mapper : Type.Mapper.t =
    { ty =
        (fun var ->
          inst
            tys
            var
            ~wrap:(fun var -> Type.TVar var)
            ~bound:Nothing.unreachable_code)
    ; row =
        (fun var ->
          inst
            rows
            var
            ~wrap:(fun var -> Type.RVar var)
            ~bound:(fun (bound : Type.bound) ->
              { bound with row = Type.map_row mapper bound.row }))
    ; presence =
        (fun var ->
          inst
            presences
            var
            ~wrap:(fun var -> Type.PVar var)
            ~bound:Nothing.unreachable_code)
    }
  in
  mapper
;;

let generalizer ~rank : Type.Mapper.t =
  let generalize (var : _ Type.Var.t) ~wrap ~bound =
    (match var.state with
     | Unbound rank' when rank' > rank ->
       var.state <- Generic;
       List.iter var.bounds ~f:bound
     | Unbound _ | Generic | Link _ -> ());
    wrap var
  in
  let rec mapper : Type.Mapper.t =
    { ty =
        generalize
          ~wrap:(fun var -> Type.TVar var)
          ~bound:Nothing.unreachable_code
    ; row =
        generalize
          ~wrap:(fun var -> Type.RVar var)
          ~bound:(fun (bound : Type.bound) ->
            ignore (Type.map_row mapper bound.row : Type.row))
    ; presence =
        generalize
          ~wrap:(fun var -> Type.PVar var)
          ~bound:Nothing.unreachable_code
    }
  in
  mapper
;;

let generalize_ty ~rank ty = Type.map_ty (generalizer ~rank) ty
let generalize_row ~rank row = Type.map_row (generalizer ~rank) row
let instantiate fresh ~rank = Type.map_ty (instantiator fresh ~rank)
