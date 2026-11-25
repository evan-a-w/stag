open! Import

type 'a t =
  | T :
      { id : 'w Type_equal.Id.t
      ; state : 'w
      ; bind : 'w -> 'a -> (unit, Mode.Error.t) Result.t
      ; instantiate : 'w -> 'w option
      ; generalize : 'w -> (unit, Mode.Error.t) Result.t
      }
      -> 'a t

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
  List.fold_result ts ~init:() ~f:(fun () (T t) -> t.bind t.state target)
;;

let instantiate ts =
  List.filter_map ts ~f:(fun (T t) ->
    Option.map (t.instantiate t.state) ~f:(fun state -> T { t with state }))
;;

let generalize ts =
  List.filter_map ts ~f:(fun (T t) -> Result.error (t.generalize t.state))
;;
