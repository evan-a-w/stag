open! Core

let instantiator fresh ~rank : Type.Mapper.t =
  let tys = Int.Table.create () in
  let contexts = Int.Table.create () in
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
    ; context =
        (fun var ->
          inst
            contexts
            var
            ~wrap:(fun var -> Type.CVar var)
            ~bound:(fun (bound : Type.bound) ->
              { bound with context = Type.map_context mapper bound.context }))
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
    ; context =
        generalize
          ~wrap:(fun var -> Type.CVar var)
          ~bound:(fun (bound : Type.bound) ->
            ignore (Type.map_context mapper bound.context : Type.context))
    ; presence =
        generalize
          ~wrap:(fun var -> Type.PVar var)
          ~bound:Nothing.unreachable_code
    }
  in
  mapper
;;

let generalize_ty ~rank ty = Type.map_ty (generalizer ~rank) ty

let generalize_context ~rank context =
  Type.map_context (generalizer ~rank) context
;;

let instantiate fresh ~rank = Type.map_ty (instantiator fresh ~rank)
