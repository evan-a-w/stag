open! Core

(* Lowers ranks to at most [rank]. [occurs] is the id of the variable being
   bound, if any, with the variable to report if it is reached. Bounds are not
   part of a type's structure, so are only rank lowered. *)
let rec adjuster ~rank ~(occurs : (Type.id * Type.arg) option)
  : Unification_error.t Type.Iter.t
  =
  let adjust (var : _ Type.Var.t) ~bound =
    match occurs with
    | Some (id, arg) when id = var.id ->
      Error (Unification_error.Occurs_check_failed arg)
    | Some _ | None ->
      if Type.Var.rank_exn var > rank
      then (
        var.state <- Unbound rank;
        List.fold_result var.bounds ~init:() ~f:(fun () b -> bound b))
      else Ok ()
  in
  { ty = adjust ~bound:Nothing.unreachable_code
  ; row =
      adjust ~bound:(fun (bound : Type.bound) ->
        Type.iter_row (adjuster ~rank ~occurs:None) bound.row)
  ; presence = adjust ~bound:Nothing.unreachable_code
  }
;;

let unify_presence ~presence1 ~presence2 ~error =
  match
    Type.canonical_presence presence1, Type.canonical_presence presence2
  with
  | Present, Present | Absent, Absent -> Ok ()
  | Present, Absent | Absent, Present -> Error error
  | PVar var1, PVar var2 when phys_equal var1 var2 -> Ok ()
  | PVar var, presence | presence, PVar var ->
    let rank = Type.Var.rank_exn var in
    (match presence with
     | PVar var' when Type.Var.rank_exn var' > rank ->
       var'.state <- Unbound rank
     | Present | Absent | PVar _ -> ());
    var.state <- Link presence;
    Ok ()
;;

(* Approximates [presence <= bound] by equality when both are unknown, so
   whether a program typechecks can depend on the order constraints arise in:
   see the "presence approximation" tests. *)
let presence_at_most ~presence ~bound ~error =
  match Type.canonical_presence presence, Type.canonical_presence bound with
  | Absent, _ | _, Present -> Ok ()
  | _ -> unify_presence ~presence1:presence ~presence2:bound ~error
;;

let presence_mismatch_reason name ~found
  : Unification_error.Row_mismatch_reason.t
  =
  match found with
  | None -> Missing_label name
  | Some (_ : Type.ty) -> Presence_mismatch name
;;

let rec unify_ty fresh ~ty1 ~ty2 : (unit, Unification_error.t) Result.t =
  let ty1 = Type.canonical_ty ty1 in
  let ty2 = Type.canonical_ty ty2 in
  let zip xs ys : (_, Unification_error.t) Result.t =
    match List.zip xs ys with
    | Ok pairs -> Ok pairs
    | Unequal_lengths ->
      Error
        (Type_mismatch
           (ty1, ty2, Args_length_mismatch (List.length xs, List.length ys)))
  in
  match ty1, ty2 with
  | TCon (name1, args1), TCon (name2, args2) ->
    if not (String.equal name1 name2)
    then Error (Type_mismatch (ty1, ty2, Base_types_not_equal (name1, name2)))
    else (
      let%bind.Result args = zip args1 args2 in
      List.fold_result args ~init:() ~f:(fun () (arg1, arg2) ->
        match arg1, arg2 with
        | ATy ty1, ATy ty2 -> unify_ty fresh ~ty1 ~ty2
        | ARow row1, ARow row2 -> unify_row fresh ~row1 ~row2
        | ATy _, ARow _ | ARow _, ATy _ ->
          Error (Type_mismatch (ty1, ty2, Mismatch_arg_kind (arg1, arg2)))))
  | TArrow (arg1, res1), TArrow (arg2, res2) ->
    let%bind.Result () = unify_ty fresh ~ty1:arg1 ~ty2:arg2 in
    unify_ty fresh ~ty1:res1 ~ty2:res2
  | TTuple tys1, TTuple tys2 ->
    let%bind.Result tys = zip tys1 tys2 in
    List.map tys ~f:(fun (ty1, ty2) -> unify_ty fresh ~ty1 ~ty2)
    |> Result.all_unit
  | ( TCode { inner = inner1; context = row1 }
    , TCode { inner = inner2; context = row2 } ) ->
    let%bind.Result () = unify_ty fresh ~ty1:inner1 ~ty2:inner2 in
    unify_row fresh ~row1 ~row2
  | TVar var1, TVar var2 when phys_equal var1 var2 -> Ok ()
  | TVar var, ty | ty, TVar var ->
    let%map.Result () =
      Type.iter_ty
        (adjuster
           ~rank:(Type.Var.rank_exn var)
           ~occurs:(Some (var.id, ATy (TVar var))))
        ty
    in
    var.state <- Link ty
  | _ -> Error (Type_mismatch (ty1, ty2, Structural_mismatch))

and bind_rvar fresh (var : _ Type.Var.t) row =
  let%bind.Result () =
    Type.iter_row
      (adjuster
         ~rank:(Type.Var.rank_exn var)
         ~occurs:(Some (var.id, ARow (RVar var))))
      row
  in
  var.state <- Link row;
  let bounds = var.bounds in
  var.bounds <- [];
  List.fold_result bounds ~init:() ~f:(fun () (bound : Type.bound) ->
    constrain_row fresh ~row ~bound:bound.row ~shadowed:bound.shadowed)

and unify_row fresh ~row1 ~row2 : (unit, Unification_error.t) Result.t =
  let row1 = Type.canonical_row row1 in
  let row2 = Type.canonical_row row2 in
  match row1, row2 with
  | REmpty, REmpty -> Ok ()
  | RExtend (x, p1, t1, rest1), c2 ->
    let tail1 = Type.tail_var rest1 in
    let%bind.Result p2, t2, rest2 = rewrite fresh x c2 in
    let%bind.Result () =
      match tail1 with
      | Some { state = Link _; _ } ->
        Error (Unification_error.Row_mismatch (row1, row2, Cyclic_row))
      | _ -> Ok ()
    in
    let%bind.Result () =
      unify_presence
        ~presence1:p1
        ~presence2:p2
        ~error:
          (Unification_error.Row_mismatch
             (row1, row2, presence_mismatch_reason x ~found:t2))
    in
    let%bind.Result () =
      match t2 with
      | None -> Ok ()
      | Some t2 -> unify_ty fresh ~ty1:t1 ~ty2:t2
    in
    unify_row fresh ~row1:rest1 ~row2:rest2
  | REmpty, RExtend (x, p2, _, rest2) ->
    let%bind.Result () =
      unify_presence
        ~presence1:Absent
        ~presence2:p2
        ~error:(Unification_error.Row_mismatch (row1, row2, Missing_label x))
    in
    unify_row fresh ~row1 ~row2:rest2
  | RVar var1, RVar var2 when phys_equal var1 var2 -> Ok ()
  | RVar var, row | row, RVar var -> bind_rvar fresh var row

(* Finds the first entry for [name], which has no type if [row] lacks
   [name] entirely. *)
and rewrite fresh name (row : Type.row)
  : (Type.presence * Type.ty option * Type.row, Unification_error.t) Result.t
  =
  match Type.canonical_row row with
  | REmpty -> Ok (Absent, None, REmpty)
  | RExtend (x, presence, ty, rest) ->
    (match String.equal x name with
     | true -> Ok (presence, Some ty, rest)
     | false ->
       let%map.Result presence', ty', rest' = rewrite fresh name rest in
       presence', ty', Type.RExtend (x, presence, ty, rest'))
  | RVar cvar ->
    let rank = Type.Var.rank_exn cvar in
    let px = Type.PVar (Fresh.var fresh ~rank) in
    let tx = Type.TVar (Fresh.var fresh ~rank) in
    let rest = Type.RVar (Fresh.var fresh ~rank) in
    let%map.Result () = bind_rvar fresh cvar (RExtend (name, px, tx, rest)) in
    px, Some tx, rest

and constrain_row fresh ~row ~bound ~shadowed =
  match Type.canonical_row row with
  | REmpty -> Ok ()
  | RExtend (name, presence, ty, rest) ->
    let%bind.Result () =
      match Type.canonical_presence presence with
      | _ when Set.mem shadowed name -> Ok ()
      | Absent -> Ok ()
      | Present | PVar _ ->
        let%bind.Result bound_presence, bound_ty, (_ : Type.row) =
          rewrite fresh name bound
        in
        let%bind.Result () =
          presence_at_most
            ~presence
            ~bound:bound_presence
            ~error:
              (Unification_error.Row_mismatch
                 (row, bound, presence_mismatch_reason name ~found:bound_ty))
        in
        (match bound_ty with
         | None -> Ok ()
         | Some bound_ty -> unify_ty fresh ~ty1:ty ~ty2:bound_ty)
    in
    constrain_row fresh ~row:rest ~bound ~shadowed:(Set.add shadowed name)
  | RVar var ->
    let rank = Type.Var.rank_exn var in
    (match Type.canonical_row bound with
     | RVar var' when phys_equal var var' -> Ok ()
     | _ ->
       let%map.Result () = Type.iter_row (adjuster ~rank ~occurs:None) bound in
       var.bounds <- { row = bound; shadowed } :: var.bounds)
;;

let present_entry fresh ~row ~name ~rank =
  let%bind.Result presence, found, (_ : Type.row) = rewrite fresh name row in
  let ty =
    Option.value_or_thunk found ~default:(fun () ->
      Type.TVar (Fresh.var fresh ~rank))
  in
  let%map.Result () =
    unify_presence
      ~presence1:Present
      ~presence2:presence
      ~error:
        (Unification_error.Row_mismatch
           ( RExtend (name, Present, ty, REmpty)
           , row
           , presence_mismatch_reason name ~found ))
  in
  ty
;;
