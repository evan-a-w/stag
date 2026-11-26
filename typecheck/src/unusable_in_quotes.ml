open! Import

(* Kept on the entry's type variable, so that its instances and what it is bound
   to keep it too. By name, so that entries of other names unified with it don't
   find it. *)
let key : Typecheck_error.Unusable_in_quote.t String.Map.t Type_equal.Id.t =
  Type_equal.Id.create ~name:"unusable_in_quote" sexp_of_opaque
;;

let rec watcher by_name =
  Watcher.create
    key
    by_name
    ~bind:(fun by_name target ->
      Map.iteri by_name ~f:(fun ~key:name ~data:unusable ->
        record ~entry:target ~name unusable);
      Ok ())
    ~instantiate:(fun by_name ~at:_ -> Some by_name)
    ~generalize:(fun _ -> Ok ())

and record ~entry ~name unusable =
  match (entry : Type.ty) with
  | TVar { state = Link target; _ } -> record ~entry:target ~name unusable
  | TVar var ->
    let others, by_name =
      List.partition_map var.watchers ~f:(fun watcher ->
        match Watcher.find [ watcher ] key with
        | Some by_name -> Second by_name
        | None -> First watcher)
    in
    let by_name =
      match by_name with
      | [] -> String.Map.singleton name unusable
      | by_name :: _ ->
        Map.update by_name name ~f:(function
          | Some kept -> kept
          | None -> unusable)
    in
    Type.Var.set_watchers var (watcher by_name :: others)
  | TCon _ | TArrow _ | TTuple _ | TCode _ | TRecord _ | TVariant _ -> ()
;;

let rec find ~entry ~name =
  match (entry : Type.ty) with
  | TVar var ->
    (match
       ( Option.bind (Watcher.find var.watchers key) ~f:(fun by_name ->
           Map.find by_name name)
       , var.state )
     with
     | Some unusable, _ -> Some unusable
     | None, Link target -> find ~entry:target ~name
     | None, (Unbound _ | Generic) -> None)
  | TCon _ | TArrow _ | TTuple _ | TCode _ | TRecord _ | TVariant _ -> None
;;
