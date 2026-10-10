open! Core

type t =
  { mutable rank : int
  ; mutable next_id : int
  }

let fresh_tvar t ~rank =
  let new_id = t.next_id in
  t.next_id <- new_id + 1;
  ref (Type.Unbound (new_id, rank))
;;

let fresh_cvar t ~rank =
  let new_id = t.next_id in
  t.next_id <- new_id + 1;
  ref (Type.CUnbound (new_id, rank))
;;

let rec inst_ty t ~remap_ty ~remap_context ~(scheme : Type.ty) : Type.ty =
  let scheme = Type.canonical_ty scheme in
  match scheme with
  | TCon _ -> scheme
  | TArrow (ty1, ty2) ->
    TArrow
      ( inst_ty t ~remap_ty ~remap_context ~scheme:ty1
      , inst_ty t ~remap_ty ~remap_context ~scheme:ty2 )
  | TTuple tys ->
    TTuple
      (List.map tys ~f:(fun scheme ->
         inst_ty t ~remap_ty ~remap_context ~scheme))
  | TCode { inner; context } ->
    TCode
      { inner = inst_ty t ~remap_ty ~remap_context ~scheme:inner
      ; context = inst_context t ~remap_ty ~remap_context ~context
      }
  | TVar { contents = Link ty } -> inst_ty t ~remap_ty ~remap_context ~scheme:ty
  | TVar { contents = Type.Unbound (id, _) } -> (* weak *) scheme
  | TVar { contents = Type.Generic id } ->
    Hashtbl.find_or_add remap_ty id ~default:(fun () ->
      Type.TVar (fresh_tvar t ~rank:t.rank))

and inst_context t ~remap_ty ~remap_context ~(context : Type.context)
  : Type.context
  =
  let context = Type.canonical_context context in
  match context with
  | CEmpty -> CEmpty
  | CExtend (name, ty, context') ->
    CExtend
      ( name
      , inst_ty t ~remap_ty ~remap_context ~scheme:ty
      , inst_context t ~remap_ty ~remap_context ~context:context' )
  | CVar { contents = Type.CLink context' } ->
    inst_context t ~remap_ty ~remap_context ~context:context'
  | CVar { contents = Type.CUnbound (id, _) } -> (* weak *) context
  | CVar { contents = Type.CGeneric id } ->
    Hashtbl.find_or_add remap_context id ~default:(fun () ->
      Type.CVar (fresh_cvar t ~rank:t.rank))
;;

let rec generalize_ty t ~rank ~(ty : Type.ty) : Type.ty =
  let ty = Type.canonical_ty ty in
  match ty with
  | TCon _ -> ty
  | TArrow (ty1, ty2) ->
    TArrow (generalize_ty t ~rank ~ty:ty1, generalize_ty t ~rank ~ty:ty2)
  | TTuple tys -> TTuple (List.map tys ~f:(fun ty -> generalize_ty t ~rank ~ty))
  | TCode { inner; context } ->
    TCode
      { inner = generalize_ty t ~rank ~ty:inner
      ; context = generalize_context t ~rank ~context
      }
  | TVar { contents = Link ty } -> generalize_ty t ~rank ~ty
  | TVar ({ contents = Unbound (id, rank') } as tvar) ->
    if rank' > rank
    then (
      tvar := Generic id;
      TVar tvar)
    else ty
  | TVar { contents = Generic _ } -> ty

and generalize_context t ~rank ~(context : Type.context) : Type.context =
  let context = Type.canonical_context context in
  match context with
  | CEmpty -> CEmpty
  | CExtend (name, ty, context') ->
    CExtend
      ( name
      , generalize_ty t ~rank ~ty
      , generalize_context t ~rank ~context:context' )
  | CVar { contents = Type.CLink context' } ->
    generalize_context t ~rank ~context:context'
  | CVar ({ contents = Type.CUnbound (id, rank') } as cvar) ->
    if rank' > rank
    then (
      cvar := Type.CGeneric id;
      CVar cvar)
    else context
  | CVar { contents = Type.CGeneric _ } -> context
;;

module Unification_error = struct
  module Type_mismatch_reason = struct
    type t =
      | Base_types_not_equal of string * string
      | Args_length_mismatch of int * int
      | Mismatch_arg_kind of Type.arg * Type.arg
      | Structural_mismatch
    [@@deriving sexp, compare, equal]
  end

  module Context_mismatch_reason = struct
    type t =
      | Empty_context_has_variable of string
      | Cyclic_context
    [@@deriving sexp, compare, equal]
  end

  type t =
    | Type_mismatch of Type.ty * Type.ty * Type_mismatch_reason.t
    | Context_mismatch of
        Type.context * Type.context * Context_mismatch_reason.t
    | Occurs_check_failed of Type.Var.t
  [@@deriving sexp, compare, equal]
end

let rec unify_ty t ~ty1 ~ty2 : (Type.ty, Unification_error.t) Result.t =
  let ty1 = Type.canonical_ty ty1 in
  let ty2 = Type.canonical_ty ty2 in
  (* 1. Unify types
     2. Occurs check
     3. Rank minning *)
  match ty1, ty2 with
  | TCon (name1, args1), TCon (name2, args2) ->
    unify_con t ~ty1 ~ty2 ~name1 ~args1 ~name2 ~args2
  | TArrow (ty1', ty2'), TArrow (ty1'', ty2'') ->
    let%bind.Result ty1' = unify_ty t ~ty1:ty1' ~ty2:ty1'' in
    let%bind.Result ty2' = unify_ty t ~ty1:ty2' ~ty2:ty2'' in
    Ok (Type.TArrow (ty1', ty2'))
  | TTuple tys1, TTuple tys2 ->
    if List.length tys1 <> List.length tys2
    then
      Error
        (Type_mismatch
           (ty1, ty2, Args_length_mismatch (List.length tys1, List.length tys2)))
    else (
      let%bind.Result tys =
        List.map2_exn tys1 tys2 ~f:(fun ty1 ty2 -> unify_ty t ~ty1 ~ty2)
        |> Result.all
      in
      Ok (Type.TTuple tys))
  | ( TCode { inner = inner1; context = context1 }
    , TCode { inner = inner2; context = context2 } ) ->
    let%bind.Result inner = unify_ty t ~ty1:inner1 ~ty2:inner2 in
    let%bind.Result context = unify_context t ~context1 ~context2 in
    Ok (Type.TCode { inner; context })
  | TVar tvar1, TVar tvar2 when phys_equal tvar1 tvar2 -> Ok ty1
  | TVar r, ty | ty, TVar r -> bind_tvar t r ty
  | _ -> Error (Type_mismatch (ty1, ty2, Structural_mismatch))

and bind_tvar t (r : Type.tvar ref) ty =
  match !r with
  | Unbound (_, rank) ->
    let%map.Result () = adjust_ty t ~var:(TVar r) ~rank ~ty in
    r := Link ty;
    ty
  | Generic _ | Link _ ->
    (* other cases shouldn't happen *)
    failwith "bind_tvar: unexpected tvar state"

and bind_cvar t (r : Type.cvar ref) context =
  match !r with
  | CUnbound (_, rank) ->
    let%map.Result () = adjust_context t ~var:(CVar r) ~rank ~context in
    r := CLink context;
    context
  | CGeneric _ | CLink _ ->
    (* other cases shouldn't happen *)
    failwith "bind_cvar: unexpected cvar state"

(* adjust does occurs check + rank lowering *)
and adjust_ty t ~(var : Type.Var.t) ~rank ~ty =
  let ty = Type.canonical_ty ty in
  match ty with
  | TVar r' ->
    let%bind.Result () =
      match var with
      | TVar r when phys_equal r r' ->
        Error (Unification_error.Occurs_check_failed var)
      | _ -> Ok ()
    in
    (match !r' with
     | Link _ | Generic _ -> failwith "adjust: unexpected tvar state"
     | Unbound (id, rank') ->
       if rank' > rank then r' := Unbound (id, rank);
       Ok ())
  | TCon (_, args) ->
    List.map ~f:(fun arg -> adjust_arg t ~var ~rank ~arg) args
    |> Result.all_unit
  | TArrow (a, b) ->
    let%bind.Result () = adjust_ty t ~var ~rank ~ty:a in
    let%bind.Result () = adjust_ty t ~var ~rank ~ty:b in
    Ok ()
  | TTuple ts ->
    List.map ~f:(fun ty -> adjust_ty t ~var ~rank ~ty) ts |> Result.all_unit
  | TCode { inner; context } ->
    let%bind.Result () = adjust_context t ~var ~rank ~context in
    adjust_ty t ~var ~rank ~ty:inner

and adjust_context t ~(var : Type.Var.t) ~rank ~context =
  let context = Type.canonical_context context in
  match context with
  | CEmpty -> Ok ()
  | CExtend (_, ty, rest) ->
    let%bind.Result () = adjust_ty t ~var ~rank ~ty in
    adjust_context t ~var ~rank ~context:rest
  | CVar r' ->
    let%bind.Result () =
      match var with
      | CVar r when phys_equal r r' ->
        Error (Unification_error.Occurs_check_failed var)
      | CVar _ | TVar _ -> Ok ()
    in
    (match !r' with
     | CGeneric _ | CLink _ -> failwith "adjust_context: unexpected cvar state"
     | CUnbound (id, rank') ->
       if rank' > rank then r' := CUnbound (id, rank);
       Ok ())

and adjust_arg t ~(var : Type.Var.t) ~rank ~(arg : Type.arg) =
  match arg with
  | ATy ty -> adjust_ty t ~var ~rank ~ty
  | AContext context -> adjust_context t ~var ~rank ~context

and unify_context t ~context1 ~context2
  : (Type.context, Unification_error.t) Result.t
  =
  let context1 = Type.canonical_context context1 in
  let context2 = Type.canonical_context context2 in
  match context1, context2 with
  | CEmpty, CEmpty -> Ok CEmpty
  | CExtend (x, t1, rest1), c2 ->
    let tail1 = Type.tail_var rest1 in
    let%bind.Result t2, rest2 =
      rewrite t x c2
      |> Result.map_error ~f:(function
        | `Unification_error err -> err
        | `Mismatch_reason reason ->
          Unification_error.Context_mismatch (context1, context2, reason))
    in
    let%bind.Result () =
      match tail1 with
      | Some { contents = CLink _ } ->
        Error
          (Unification_error.Context_mismatch
             (context1, context2, Cyclic_context))
      | _ -> Ok ()
    in
    let%bind.Result (_ : Type.ty) = unify_ty t ~ty1:t1 ~ty2:t2 in
    let%map.Result (_ : Type.context) =
      unify_context t ~context1:rest1 ~context2:rest2
    in
    context1
  | CEmpty, CExtend (x, _, _) ->
    Error
      (Unification_error.Context_mismatch
         (context1, context2, Empty_context_has_variable x))
  | CVar r1, CVar r2 when phys_equal r1 r2 -> Ok context1
  | CVar r, c | c, CVar r -> bind_cvar t r c

and rewrite t var (context : Type.context)
  : ( Type.ty * Type.context
      , [ `Unification_error of Unification_error.t
        | `Mismatch_reason of Unification_error.Context_mismatch_reason.t
        ] )
      Result.t
  =
  match context with
  | CEmpty -> Error (`Mismatch_reason (Empty_context_has_variable var))
  | CExtend (x, ty, rest) ->
    (match String.equal x var with
     | true -> Ok (ty, rest)
     | false ->
       let%map.Result ty', rest' = rewrite t var rest in
       ty', Type.CExtend (x, ty, rest'))
  | CVar ({ contents = CUnbound (_, rank) } as r) ->
    let tx = Type.TVar (fresh_tvar t ~rank) in
    let r' = Type.CVar (fresh_cvar t ~rank) in
    r := CLink (CExtend (var, tx, r'));
    Ok (tx, r')
  | CVar _ -> failwith "rewrite: unexpected cvar state"

and unify_con t ~ty1 ~ty2 ~name1 ~args1 ~name2 ~args2
  : (Type.ty, Unification_error.t) Result.t
  =
  if
    not (String.equal name1 name2)
    (* can prob do something like follow public aliases here *)
  then Error (Type_mismatch (ty1, ty2, Base_types_not_equal (name1, name2)))
  else if not (List.length args1 = List.length args2)
  then
    Error
      (Type_mismatch
         (ty1, ty2, Args_length_mismatch (List.length args1, List.length args2)))
  else (
    let%map.Result l =
      List.fold2_exn args1 args2 ~init:(Ok []) ~f:(fun acc arg1 arg2 ->
        let%bind.Result acc = acc in
        let%map.Result this =
          unify_arg t ~arg1 ~arg2
          |> Result.map_error ~f:(function
            | `Unification_error err -> err
            | `Kind_mismatch ->
              Unification_error.Type_mismatch
                (ty1, ty2, Mismatch_arg_kind (arg1, arg2)))
        in
        this :: acc)
    in
    Type.TCon (name1, List.rev l))

and unify_arg t ~arg1 ~arg2
  : ( Type.arg
      , [ `Unification_error of Unification_error.t | `Kind_mismatch ] )
      Result.t
  =
  match arg1, arg2 with
  | ATy ty1, ATy ty2 ->
    let%map.Result ty =
      unify_ty t ~ty1 ~ty2
      |> Result.map_error ~f:(fun x -> `Unification_error x)
    in
    Type.ATy ty
  | AContext context1, AContext context2 ->
    let%map.Result context =
      unify_context t ~context1 ~context2
      |> Result.map_error ~f:(fun x -> `Unification_error x)
    in
    Type.AContext context
  | ATy _, AContext _ | AContext _, ATy _ -> Error `Kind_mismatch
;;
