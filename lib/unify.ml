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
  ; context =
      adjust ~bound:(fun (bound : Type.bound) ->
        Type.iter_context (adjuster ~rank ~occurs:None) bound.context)
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
  : Unification_error.Context_mismatch_reason.t
  =
  match found with
  | None -> Empty_context_has_variable name
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
        | AContext context1, AContext context2 ->
          unify_context fresh ~context1 ~context2
        | ATy _, AContext _ | AContext _, ATy _ ->
          Error (Type_mismatch (ty1, ty2, Mismatch_arg_kind (arg1, arg2)))))
  | TArrow (arg1, res1), TArrow (arg2, res2) ->
    let%bind.Result () = unify_ty fresh ~ty1:arg1 ~ty2:arg2 in
    unify_ty fresh ~ty1:res1 ~ty2:res2
  | TTuple tys1, TTuple tys2 ->
    let%bind.Result tys = zip tys1 tys2 in
    List.map tys ~f:(fun (ty1, ty2) -> unify_ty fresh ~ty1 ~ty2)
    |> Result.all_unit
  | ( TCode { inner = inner1; context = context1 }
    , TCode { inner = inner2; context = context2 } ) ->
    let%bind.Result () = unify_ty fresh ~ty1:inner1 ~ty2:inner2 in
    unify_context fresh ~context1 ~context2
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

and bind_cvar fresh (var : _ Type.Var.t) context =
  let%bind.Result () =
    Type.iter_context
      (adjuster
         ~rank:(Type.Var.rank_exn var)
         ~occurs:(Some (var.id, AContext (CVar var))))
      context
  in
  var.state <- Link context;
  let bounds = var.bounds in
  var.bounds <- [];
  List.fold_result bounds ~init:() ~f:(fun () (bound : Type.bound) ->
    constrain_context
      fresh
      ~context
      ~bound:bound.context
      ~shadowed:bound.shadowed)

and unify_context fresh ~context1 ~context2
  : (unit, Unification_error.t) Result.t
  =
  let context1 = Type.canonical_context context1 in
  let context2 = Type.canonical_context context2 in
  match context1, context2 with
  | CEmpty, CEmpty -> Ok ()
  | CExtend (x, p1, t1, rest1), c2 ->
    let tail1 = Type.tail_var rest1 in
    let%bind.Result p2, t2, rest2 = rewrite fresh x c2 in
    let%bind.Result () =
      match tail1 with
      | Some { state = Link _; _ } ->
        Error
          (Unification_error.Context_mismatch
             (context1, context2, Cyclic_context))
      | _ -> Ok ()
    in
    let%bind.Result () =
      unify_presence
        ~presence1:p1
        ~presence2:p2
        ~error:
          (Unification_error.Context_mismatch
             (context1, context2, presence_mismatch_reason x ~found:t2))
    in
    let%bind.Result () =
      match t2 with
      | None -> Ok ()
      | Some t2 -> unify_ty fresh ~ty1:t1 ~ty2:t2
    in
    unify_context fresh ~context1:rest1 ~context2:rest2
  | CEmpty, CExtend (x, p2, _, rest2) ->
    let%bind.Result () =
      unify_presence
        ~presence1:Absent
        ~presence2:p2
        ~error:
          (Unification_error.Context_mismatch
             (context1, context2, Empty_context_has_variable x))
    in
    unify_context fresh ~context1 ~context2:rest2
  | CVar var1, CVar var2 when phys_equal var1 var2 -> Ok ()
  | CVar var, context | context, CVar var -> bind_cvar fresh var context

(* Finds the first entry for [name], which has no type if [context] lacks
   [name] entirely. *)
and rewrite fresh name (context : Type.context)
  : ( Type.presence * Type.ty option * Type.context
      , Unification_error.t )
      Result.t
  =
  match Type.canonical_context context with
  | CEmpty -> Ok (Absent, None, CEmpty)
  | CExtend (x, presence, ty, rest) ->
    (match String.equal x name with
     | true -> Ok (presence, Some ty, rest)
     | false ->
       let%map.Result presence', ty', rest' = rewrite fresh name rest in
       presence', ty', Type.CExtend (x, presence, ty, rest'))
  | CVar cvar ->
    let rank = Type.Var.rank_exn cvar in
    let px = Type.PVar (Fresh.var fresh ~rank) in
    let tx = Type.TVar (Fresh.var fresh ~rank) in
    let rest = Type.CVar (Fresh.var fresh ~rank) in
    let%map.Result () = bind_cvar fresh cvar (CExtend (name, px, tx, rest)) in
    px, Some tx, rest

and constrain_context fresh ~context ~bound ~shadowed =
  match Type.canonical_context context with
  | CEmpty -> Ok ()
  | CExtend (name, presence, ty, rest) ->
    let%bind.Result () =
      match Type.canonical_presence presence with
      | _ when Set.mem shadowed name -> Ok ()
      | Absent -> Ok ()
      | Present | PVar _ ->
        let%bind.Result bound_presence, bound_ty, (_ : Type.context) =
          rewrite fresh name bound
        in
        let%bind.Result () =
          presence_at_most
            ~presence
            ~bound:bound_presence
            ~error:
              (Unification_error.Context_mismatch
                 (context, bound, presence_mismatch_reason name ~found:bound_ty))
        in
        (match bound_ty with
         | None -> Ok ()
         | Some bound_ty -> unify_ty fresh ~ty1:ty ~ty2:bound_ty)
    in
    constrain_context
      fresh
      ~context:rest
      ~bound
      ~shadowed:(Set.add shadowed name)
  | CVar var ->
    let rank = Type.Var.rank_exn var in
    (match Type.canonical_context bound with
     | CVar var' when phys_equal var var' -> Ok ()
     | _ ->
       let%map.Result () =
         Type.iter_context (adjuster ~rank ~occurs:None) bound
       in
       var.bounds <- { context = bound; shadowed } :: var.bounds)
;;

let present_entry fresh ~context ~name ~rank =
  let%bind.Result presence, found, (_ : Type.context) =
    rewrite fresh name context
  in
  let ty =
    Option.value_or_thunk found ~default:(fun () ->
      Type.TVar (Fresh.var fresh ~rank))
  in
  let%map.Result () =
    unify_presence
      ~presence1:Present
      ~presence2:presence
      ~error:
        (Unification_error.Context_mismatch
           ( CExtend (name, Present, ty, CEmpty)
           , context
           , presence_mismatch_reason name ~found ))
  in
  ty
;;
