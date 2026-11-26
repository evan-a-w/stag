open! Import

type ('a, 'e) t =
  | T :
      { id : 'w Type_equal.Id.t
      ; state : 'w
      ; bind : 'w -> 'a -> (unit, 'e) Result.t
      ; instantiate : 'w -> at:Loc.t option -> 'w option
      ; generalize : 'w -> (unit, 'e) Result.t
      }
      -> ('a, 'e) t

let create id state ~bind ~instantiate ~generalize =
  T { id; state; bind; instantiate; generalize }
;;

let find (type w) ts (key : w Type_equal.Id.t) : w option =
  List.find_map ts ~f:(fun (T t) ->
    match Type_equal.Id.same_witness t.id key with
    | Some T -> Some (t.state : w)
    | None -> None)
;;

let bind ts target =
  Mode.Private.Undo.atomically (fun () ->
    List.fold_result ts ~init:() ~f:(fun () (T t) -> t.bind t.state target))
;;

let instantiate ?at ts =
  List.filter_map ts ~f:(fun (T t) ->
    Option.map (t.instantiate t.state ~at) ~f:(fun state -> T { t with state }))
;;

let generalize ts =
  List.filter_map ts ~f:(fun (T t) -> Result.error (t.generalize t.state))
;;
