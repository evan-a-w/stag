open! Core
module Unification_error = Unification_error
module Typecheck_error = Typecheck_error

module Toplevel = struct
  module State = struct
    type t =
      | Unchecked
      | In_progress of
          { index : int
          ; ty : Type.ty
          ; mutable recursive : bool
          }
      | Done of Env.Entry.t
    [@@deriving sexp_of]
  end

  type t =
    { name : string
    ; module_ : Module_scope.t
    ; args : Syntax.Binding.t list
    ; annot : Syntax.Type_expr.t option
    ; expr : Syntax.Expr.t
    ; mutable state : State.t
    }
  [@@deriving sexp_of]

  let is_function t = not (List.is_empty t.args)
  let is_value t = is_function t || Syntax.Expr.is_value t.expr
end

(* An upper bound of a context variable: every entry the variable gets must
   also be an entry of [context], unless shadowed where the bound arose. *)
module Bound = struct
  type t =
    { context : Type.context
    ; shadowed : String.Set.t
    }
end

type t =
  { mutable next_id : int
  ; context_bounds : Bound.t list Int.Table.t
    (* By context variable id. Kept out of [Type.context] as bounds may refer
       back to the variable. *)
  }

let create () = { next_id = 0; context_bounds = Int.Table.create () }
let context_bounds t id = Hashtbl.find_multi t.context_bounds id

let fresh_id t =
  let id = t.next_id in
  t.next_id <- id + 1;
  id
;;

let fresh_tvar t ~rank = ref (Type.Unbound (fresh_id t, rank))
let fresh_cvar t ~rank = ref (Type.CUnbound (fresh_id t, rank))
let fresh_pvar t ~rank = ref (Type.PUnbound (fresh_id t, rank))

module Remap = struct
  type t =
    { ty : Type.ty Int.Table.t
    ; context : Type.context Int.Table.t
    ; presence : Type.presence Int.Table.t
    }

  let create () =
    { ty = Int.Table.create ()
    ; context = Int.Table.create ()
    ; presence = Int.Table.create ()
    }
  ;;
end

let rec inst_ty' t ~(remap : Remap.t) ~(scheme : Type.ty) ~rank : Type.ty =
  let scheme = Type.canonical_ty scheme in
  match scheme with
  | TCon (name, args) ->
    TCon
      ( name
      , List.map args ~f:(function
          | ATy scheme -> Type.ATy (inst_ty' t ~remap ~scheme ~rank)
          | AContext context -> AContext (inst_context' t ~remap ~context ~rank))
      )
  | TArrow (ty1, ty2) ->
    TArrow
      (inst_ty' t ~remap ~scheme:ty1 ~rank, inst_ty' t ~remap ~scheme:ty2 ~rank)
  | TTuple tys ->
    TTuple (List.map tys ~f:(fun scheme -> inst_ty' t ~remap ~scheme ~rank))
  | TCode { inner; context } ->
    TCode
      { inner = inst_ty' t ~remap ~scheme:inner ~rank
      ; context = inst_context' t ~remap ~context ~rank
      }
  | TVar { contents = Link ty } -> inst_ty' t ~remap ~scheme:ty ~rank
  | TVar { contents = Type.Unbound _ } -> scheme
  | TVar { contents = Type.Generic id } ->
    Hashtbl.find_or_add remap.ty id ~default:(fun () ->
      Type.TVar (fresh_tvar t ~rank))

and inst_context' t ~(remap : Remap.t) ~(context : Type.context) ~rank
  : Type.context
  =
  let context = Type.canonical_context context in
  match context with
  | CEmpty -> CEmpty
  | CExtend (name, presence, ty, context') ->
    CExtend
      ( name
      , inst_presence t ~remap ~presence ~rank
      , inst_ty' t ~remap ~scheme:ty ~rank
      , inst_context' t ~remap ~context:context' ~rank )
  | CVar { contents = Type.CLink context' } ->
    inst_context' t ~remap ~context:context' ~rank
  | CVar { contents = Type.CUnbound _ } -> context
  | CVar { contents = Type.CGeneric id } ->
    (match Hashtbl.find remap.context id with
     | Some context -> context
     | None ->
       let id' = fresh_id t in
       let context = Type.CVar (ref (Type.CUnbound (id', rank))) in
       Hashtbl.set remap.context ~key:id ~data:context;
       List.iter (context_bounds t id) ~f:(fun (bound : Bound.t) ->
         Hashtbl.add_multi
           t.context_bounds
           ~key:id'
           ~data:
             { bound with
               context = inst_context' t ~remap ~context:bound.context ~rank
             });
       context)

and inst_presence t ~(remap : Remap.t) ~presence ~rank : Type.presence =
  match Type.canonical_presence presence with
  | (Present | Absent | PVar { contents = PUnbound _ }) as presence -> presence
  | PVar { contents = PGeneric id } ->
    Hashtbl.find_or_add remap.presence id ~default:(fun () ->
      Type.PVar (fresh_pvar t ~rank))
  | PVar { contents = PLink _ } -> assert false
;;

let inst_ty t ~scheme ~rank = inst_ty' t ~scheme ~remap:(Remap.create ()) ~rank

let rec generalize_ty t ~rank ~(ty : Type.ty) : Type.ty =
  let ty = Type.canonical_ty ty in
  match ty with
  | TCon (name, args) ->
    TCon
      ( name
      , List.map args ~f:(function
          | ATy ty -> Type.ATy (generalize_ty t ~rank ~ty)
          | AContext context -> AContext (generalize_context t ~rank ~context))
      )
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
  | CExtend (name, presence, ty, context') ->
    CExtend
      ( name
      , generalize_presence ~rank ~presence
      , generalize_ty t ~rank ~ty
      , generalize_context t ~rank ~context:context' )
  | CVar { contents = Type.CLink context' } ->
    generalize_context t ~rank ~context:context'
  | CVar ({ contents = Type.CUnbound (id, rank') } as cvar) ->
    if rank' > rank
    then (
      cvar := Type.CGeneric id;
      List.iter (context_bounds t id) ~f:(fun (bound : Bound.t) ->
        ignore
          (generalize_context t ~rank ~context:bound.context : Type.context));
      CVar cvar)
    else context
  | CVar { contents = Type.CGeneric _ } -> context

and generalize_presence ~rank ~presence : Type.presence =
  let presence = Type.canonical_presence presence in
  match presence with
  | PVar ({ contents = PUnbound (id, rank') } as pvar) when rank' > rank ->
    pvar := PGeneric id;
    presence
  | Present | Absent | PVar _ -> presence
;;

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
    let%map.Result () = adjust_ty t ~var:(Some (TVar r)) ~rank ~ty in
    r := Link ty;
    ty
  | Generic _ | Link _ ->
    (* other cases shouldn't happen *)
    failwith "bind_tvar: unexpected tvar state"

and bind_cvar t (r : Type.cvar ref) context =
  match !r with
  | CUnbound (id, rank) ->
    let%bind.Result () = adjust_context t ~var:(Some (CVar r)) ~rank ~context in
    r := CLink context;
    let bounds = context_bounds t id in
    Hashtbl.remove t.context_bounds id;
    let%map.Result () =
      List.fold_result bounds ~init:() ~f:(fun () (bound : Bound.t) ->
        constrain_context
          t
          ~context
          ~bound:bound.context
          ~shadowed:bound.shadowed)
    in
    context
  | CGeneric _ | CLink _ ->
    (* other cases shouldn't happen *)
    failwith "bind_cvar: unexpected cvar state"

(* adjust does occurs check (unless [var] is [None]) + rank lowering. Bounds
   are not part of a type's structure, so are only rank lowered. *)
and adjust_ty t ~(var : Type.Var.t option) ~rank ~ty =
  let ty = Type.canonical_ty ty in
  match ty with
  | TVar r' ->
    let%bind.Result () =
      match var with
      | Some (TVar r as var) when phys_equal r r' ->
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

and adjust_context t ~(var : Type.Var.t option) ~rank ~context =
  let context = Type.canonical_context context in
  match context with
  | CEmpty -> Ok ()
  | CExtend (_, presence, ty, rest) ->
    adjust_presence ~rank ~presence;
    let%bind.Result () = adjust_ty t ~var ~rank ~ty in
    adjust_context t ~var ~rank ~context:rest
  | CVar r' ->
    let%bind.Result () =
      match var with
      | Some (CVar r as var) when phys_equal r r' ->
        Error (Unification_error.Occurs_check_failed var)
      | Some (CVar _ | TVar _) | None -> Ok ()
    in
    (match !r' with
     | CGeneric _ | CLink _ -> failwith "adjust_context: unexpected cvar state"
     | CUnbound (id, rank') ->
       if rank' > rank
       then (
         r' := CUnbound (id, rank);
         List.fold_result
           (context_bounds t id)
           ~init:()
           ~f:(fun () (bound : Bound.t) ->
             adjust_context t ~var:None ~rank ~context:bound.context))
       else Ok ())

and adjust_presence ~rank ~presence =
  match Type.canonical_presence presence with
  | PVar ({ contents = PUnbound (id, rank') } as pvar) when rank' > rank ->
    pvar := PUnbound (id, rank)
  | Present | Absent | PVar _ -> ()

and adjust_arg t ~(var : Type.Var.t option) ~rank ~(arg : Type.arg) =
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
  | CExtend (x, p1, t1, rest1), c2 ->
    let tail1 = Type.tail_var rest1 in
    let%bind.Result p2, t2, rest2 = rewrite t x c2 in
    let%bind.Result () =
      match tail1 with
      | Some { contents = CLink _ } ->
        Error
          (Unification_error.Context_mismatch
             (context1, context2, Cyclic_context))
      | _ -> Ok ()
    in
    let%bind.Result () =
      unify_presence ~presence1:p1 ~presence2:p2
      |> Result.map_error ~f:(fun () ->
        Unification_error.Context_mismatch
          (context1, context2, presence_mismatch_reason x ~found:t2))
    in
    let%bind.Result () =
      match t2 with
      | None -> Ok ()
      | Some t2 -> unify_ty t ~ty1:t1 ~ty2:t2 |> Result.ignore_m
    in
    let%map.Result (_ : Type.context) =
      unify_context t ~context1:rest1 ~context2:rest2
    in
    context1
  | CEmpty, CExtend (x, p2, _, rest2) ->
    let%bind.Result () =
      unify_presence ~presence1:Absent ~presence2:p2
      |> Result.map_error ~f:(fun () ->
        Unification_error.Context_mismatch
          (context1, context2, Empty_context_has_variable x))
    in
    unify_context t ~context1 ~context2:rest2
  | CVar r1, CVar r2 when phys_equal r1 r2 -> Ok context1
  | CVar r, c | c, CVar r -> bind_cvar t r c

(* Finds the first entry for [var], which has no type if [context] lacks [var]
   entirely. *)
and rewrite t var (context : Type.context)
  : ( Type.presence * Type.ty option * Type.context
      , Unification_error.t )
      Result.t
  =
  match Type.canonical_context context with
  | CEmpty -> Ok (Absent, None, CEmpty)
  | CExtend (x, presence, ty, rest) ->
    (match String.equal x var with
     | true -> Ok (presence, Some ty, rest)
     | false ->
       let%map.Result presence', ty', rest' = rewrite t var rest in
       presence', ty', Type.CExtend (x, presence, ty, rest'))
  | CVar ({ contents = CUnbound (_, rank) } as r) ->
    let px = Type.PVar (fresh_pvar t ~rank) in
    let tx = Type.TVar (fresh_tvar t ~rank) in
    let r' = Type.CVar (fresh_cvar t ~rank) in
    let%map.Result (_ : Type.context) =
      bind_cvar t r (CExtend (var, px, tx, r'))
    in
    px, Some tx, r'
  | CVar _ -> failwith "rewrite: unexpected cvar state"

and presence_mismatch_reason name ~found
  : Unification_error.Context_mismatch_reason.t
  =
  match found with
  | None -> Empty_context_has_variable name
  | Some (_ : Type.ty) -> Presence_mismatch name

(* Names in [shadowed] are hidden by earlier entries of the context the
   constraint is about, so are not constrained. *)
and constrain_context t ~context ~bound ~shadowed =
  let rec go context ~shadowed =
    match Type.canonical_context context with
    | CEmpty -> Ok ()
    | CExtend (name, presence, ty, rest) ->
      let%bind.Result () =
        match Type.canonical_presence presence with
        | _ when Set.mem shadowed name -> Ok ()
        | Absent -> Ok ()
        | Present | PVar _ ->
          let%bind.Result bound_presence, bound_ty, (_ : Type.context) =
            rewrite t name bound
          in
          let%bind.Result () =
            presence_at_most ~presence ~bound:bound_presence
            |> Result.map_error ~f:(fun () ->
              Unification_error.Context_mismatch
                (context, bound, presence_mismatch_reason name ~found:bound_ty))
          in
          (match bound_ty with
           | None -> Ok ()
           | Some bound_ty ->
             unify_ty t ~ty1:ty ~ty2:bound_ty |> Result.ignore_m)
      in
      go rest ~shadowed:(Set.add shadowed name)
    | CVar ({ contents = CUnbound (id, rank) } as r) ->
      (match Type.canonical_context bound with
       | CVar r' when phys_equal r r' -> Ok ()
       | _ ->
         let%map.Result () = adjust_context t ~var:None ~rank ~context:bound in
         Hashtbl.add_multi
           t.context_bounds
           ~key:id
           ~data:{ Bound.context = bound; shadowed })
    | CVar { contents = CGeneric _ | CLink _ } ->
      failwith "constrain_context: unexpected cvar state"
  in
  go context ~shadowed

(* Approximates [presence <= bound] by equality when both are unknown, so
   whether a program typechecks can depend on the order constraints arise in:
   see the "presence approximation" tests. *)
and presence_at_most ~presence ~bound =
  match Type.canonical_presence presence, Type.canonical_presence bound with
  | Absent, _ | _, Present -> Ok ()
  | Present, _ -> unify_presence ~presence1:Present ~presence2:bound
  | _, Absent -> unify_presence ~presence1:presence ~presence2:Absent
  | PVar _, PVar _ -> unify_presence ~presence1:presence ~presence2:bound

and unify_presence ~presence1 ~presence2 : (unit, unit) Result.t =
  match
    Type.canonical_presence presence1, Type.canonical_presence presence2
  with
  | Present, Present | Absent, Absent -> Ok ()
  | Present, Absent | Absent, Present -> Error ()
  | PVar r1, PVar r2 when phys_equal r1 r2 -> Ok ()
  | PVar ({ contents = PUnbound (_, rank) } as r), presence
  | presence, PVar ({ contents = PUnbound (_, rank) } as r) ->
    adjust_presence ~rank ~presence;
    r := PLink presence;
    Ok ()
  | PVar _, _ | _, PVar _ -> failwith "unify_presence: unexpected pvar state"

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

let unify_ty' t ~ty1 ~ty2 =
  unify_ty t ~ty1 ~ty2
  |> Result.map_error ~f:Typecheck_error.unification_error
  |> Result.ignore_m
;;

let unify_context' t ~context1 ~context2 =
  unify_context t ~context1 ~context2
  |> Result.map_error ~f:Typecheck_error.unification_error
  |> Result.ignore_m
;;

(* Toplevels are checked on demand, so strongly connected components are found
   with Tarjan's algorithm as we go. [pending] are the visited toplevels whose
   component is not yet complete. *)
module Scc_frame = struct
  type t =
    { mutable lowlink : int
    ; mutable pending : Toplevel.t list
    }
end

(* What one call of [typecheck] checks. *)
module Program_env = struct
  type t =
    { type_env : Type_env.t
    ; toplevels : Toplevel.t String.Table.t
    }
end

module Scope = struct
  type t =
    { program : Program_env.t
    ; level : Env.Level.t
    ; rank : Type.rank
    ; locals : Env.t
    ; quote : quote option
    ; module_ : Module_scope.t
    ; scc : Scc_frame.t
    ; annotation_vars : Type_env.Annotation_vars.t
    }

  and quote =
    { id : int
    ; context : Type.context
    ; outside : t
    }

  let quote_id t = Option.map t.quote ~f:(fun quote -> quote.id)

  let entry t ~scheme ~persistent =
    { Env.Entry.scheme; level = t.level; quote_id = quote_id t; persistent }
  ;;
end

let elaborate (scope : Scope.t) annot =
  Type_env.elaborate
    scope.program.type_env
    scope.module_
    scope.annotation_vars
    annot
;;

let unpack_annot t scope ~rank annot =
  match annot with
  | None -> Ok (Type.TVar (fresh_tvar t ~rank))
  | Some annot -> elaborate scope annot
;;

let function_type t scope ~rank ~(args : Syntax.Binding.t list) ~annot =
  let%bind.Result args =
    List.map args ~f:(fun (name, annot) ->
      let%map.Result ty = unpack_annot t scope ~rank annot in
      name, ty)
    |> Result.all
  in
  let%map.Result res_ty = unpack_annot t scope ~rank annot in
  let ty =
    List.fold_right args ~init:res_ty ~f:(fun (_, arg_ty) ty ->
      Type.TArrow (arg_ty, ty))
  in
  args, res_ty, ty
;;

let check_arity ~(constructor : Type_env.Constructor.t) ~args =
  let expected = List.length constructor.args in
  let got = List.length args in
  if expected = got
  then Ok ()
  else
    Error
      (Typecheck_error.Constructor_arity_mismatch
         { constructor = constructor.name; expected; got })
;;

(* Returns the instantiated record type and each field's type. *)
let record_fields t (scope : Scope.t) fields ~all_required
  : (Type.ty * (Type.ty * 'a) list, Typecheck_error.t) Result.t
  =
  let%bind.Result fields =
    List.map fields ~f:(fun (path, x) ->
      let%map.Result field =
        Type_env.find_field scope.program.type_env scope.module_ path
      in
      field, x)
    |> Result.all
  in
  match fields with
  | [] -> Error Typecheck_error.Empty_record
  | (first, _) :: _ ->
    let%bind.Result () =
      List.fold_result fields ~init:() ~f:(fun () (field, _) ->
        if String.equal field.type_name first.type_name
        then Ok ()
        else
          Error
            (Typecheck_error.Fields_of_different_types
               { field = field.name
               ; expected_type = first.type_name
               ; actual_type = field.type_name
               }))
    in
    let names = List.map fields ~f:(fun (field, _) -> field.name) in
    let%bind.Result () =
      match List.find_a_dup names ~compare:String.compare with
      | None -> Ok ()
      | Some name -> Error (Typecheck_error.Duplicate_field name)
    in
    let%map.Result () =
      match
        List.find first.all_fields ~f:(fun name ->
          all_required && not (List.mem names name ~equal:String.equal))
      with
      | None -> Ok ()
      | Some name -> Error (Typecheck_error.Missing_field name)
    in
    let remap = Remap.create () in
    let inst scheme = inst_ty' t ~remap ~scheme ~rank:scope.rank in
    ( inst (TCon (first.type_name, first.params))
    , List.map fields ~f:(fun ((field : Type_env.Field.t), x) ->
        inst field.ty, x) )
;;

(* Returns the instantiated record type and the field's type. *)
let field_type t (scope : Scope.t) path =
  let%map.Result (field : Type_env.Field.t) =
    Type_env.find_field scope.program.type_env scope.module_ path
  in
  let remap = Remap.create () in
  let inst scheme = inst_ty' t ~remap ~scheme ~rank:scope.rank in
  inst (TCon (field.type_name, field.params)), inst field.ty
;;

let inst_constructor t (scope : Scope.t) (constructor : Type_env.Constructor.t) =
  let remap = Remap.create () in
  let inst scheme = inst_ty' t ~remap ~scheme ~rank:scope.rank in
  List.map constructor.args ~f:inst, inst constructor.result
;;

let rec pattern_type t (scope : Scope.t) ~(pattern : Syntax.Pattern.t)
  : (Type.ty * (string * Type.ty) list, Typecheck_error.t) Result.t
  =
  let fresh () = Type.TVar (fresh_tvar t ~rank:scope.rank) in
  let sub_patterns patterns ~types =
    List.zip_exn patterns types
    |> List.fold_result ~init:[] ~f:(fun bindings (pattern, ty) ->
      let%bind.Result pattern_ty, bindings' = pattern_type t scope ~pattern in
      let%map.Result () = unify_ty' t ~ty1:ty ~ty2:pattern_ty in
      bindings @ bindings')
  in
  match pattern with
  | Any -> Ok (fresh (), [])
  | Var name ->
    let ty = fresh () in
    Ok (ty, [ name, ty ])
  | Lit lit -> Ok (Syntax.Lit.ty lit, [])
  | Tuple patterns ->
    let types = List.map patterns ~f:(fun _ -> fresh ()) in
    let%map.Result bindings = sub_patterns patterns ~types in
    Type.TTuple types, bindings
  | Construct (path, patterns) ->
    let%bind.Result constructor =
      Type_env.find_constructor scope.program.type_env scope.module_ path
    in
    let%bind.Result () = check_arity ~constructor ~args:patterns in
    let types, result = inst_constructor t scope constructor in
    let%map.Result bindings = sub_patterns patterns ~types in
    result, bindings
  | Record fields ->
    let%bind.Result record, fields =
      record_fields t scope fields ~all_required:false
    in
    let%map.Result bindings =
      sub_patterns (List.map fields ~f:snd) ~types:(List.map fields ~f:fst)
    in
    record, bindings
;;

let constrain_context' t ~context ~bound ~shadowed =
  constrain_context t ~context ~bound ~shadowed
  |> Result.map_error ~f:Typecheck_error.unification_error
;;

let free_var t (quote : Scope.quote) ~name =
  let%bind.Result presence, found, (_ : Type.context) =
    rewrite t name quote.context
    |> Result.map_error ~f:Typecheck_error.unification_error
  in
  let ty =
    Option.value_or_thunk found ~default:(fun () ->
      Type.TVar (fresh_tvar t ~rank:quote.outside.rank))
  in
  unify_presence ~presence1:Present ~presence2:presence
  |> Result.map ~f:(fun () -> ty)
  |> Result.map_error ~f:(fun () ->
    Typecheck_error.Unification_error
      (Context_mismatch
         ( CExtend (name, Present, ty, CEmpty)
         , quote.context
         , presence_mismatch_reason name ~found )))
;;

module Resolution = struct
  type t =
    | Resolved of Type.ty
    | Free_in_quote of Scope.quote
    | Unresolved of Typecheck_error.t (** The error if the name is used. *)
end

module Capture = struct
  type t =
    | Local of Env.Entry.t (** Bound inside the quote, so captured. *)
    | Wrong_level of Env.Entry.t
    (** Bound at a later level, so neither captured nor reachable outside. *)
    | Outer

  (* Only binders of the enclosing quote capture: other names are free
     variables of the quote, even if they would be in scope when used
     directly. *)
  let of_entry (scope : Scope.t) (quote : Scope.quote) (entry : Env.Entry.t) =
    if [%equal: int option] entry.quote_id (Some quote.id)
    then Local entry
    else if entry.level > scope.level
    then Wrong_level entry
    else Outer
  ;;

  let find scope quote ~name =
    Env.find scope.Scope.locals name
    |> Option.value_map ~default:Outer ~f:(of_entry scope quote)
  ;;
end

let finish_scc t (members : Toplevel.t list) =
  let persistent = List.for_all members ~f:Toplevel.is_function in
  let recursive =
    List.exists members ~f:(fun member ->
      match member.state with
      | In_progress { recursive; _ } -> recursive
      | Unchecked | Done _ -> false)
  in
  (* Like OCaml's [let rec], a recursive non-function is not generalized. *)
  let generalize =
    List.for_all members ~f:Toplevel.is_value && (persistent || not recursive)
  in
  List.iter members ~f:(fun member ->
    match member.state with
    | In_progress { ty; _ } ->
      let scheme = if generalize then generalize_ty t ~rank:0 ~ty else ty in
      member.state <- Done (Env.Entry.toplevel ~scheme ~persistent)
    | Unchecked | Done _ ->
      raise_s
        [%message "finish_scc: member not in progress" (member : Toplevel.t)])
;;

(* [name] is the context entry the variable is provided as, if it becomes a
   free variable of the quote: a local's name or a toplevel's canonical name. *)
let entry_resolution t (scope : Scope.t) ~name (entry : Env.Entry.t option)
  : Resolution.t
  =
  match entry, scope.quote with
  | Some entry, _
    when Env.Entry.is_allowed_at
           entry
           ~used_at:scope.level
           ~quote_id:(Scope.quote_id scope) ->
    Resolved (inst_ty t ~scheme:entry.scheme ~rank:scope.rank)
  | Some entry, Some quote when entry.level <= scope.level ->
    Free_in_quote quote
  | None, Some quote -> Free_in_quote quote
  | Some entry, _ ->
    Unresolved
      (Variable_used_at_wrong_level
         { name; var_level = entry.level; used_at = scope.level })
  | None, None -> Unresolved (Not_in_scope (Syntax.Path.of_string name))
;;

let resolution_type t ~name : Resolution.t -> _ = function
  | Resolved ty -> Ok ty
  | Free_in_quote quote -> free_var t quote ~name
  | Unresolved error -> Error error
;;

let rec entry_type t scope ~name entry =
  resolution_type t ~name (entry_resolution t scope ~name entry)

and var_type t (scope : Scope.t) ~(path : Syntax.Path.t) =
  let name = Syntax.Path.to_string path in
  let local =
    match (path :> string list) with
    | [ name ] -> Env.find scope.locals name
    | _ -> None
  in
  match local with
  | Some entry -> entry_type t scope ~name (Some entry)
  | None ->
    (match%bind.Result Module_scope.find scope.module_ Value path with
     | Some name ->
       toplevel_type t scope (Hashtbl.find_exn scope.program.toplevels name)
     | None ->
       (match (path :> string list) with
        | [ _ ] -> entry_type t scope ~name None
        | _ -> Error (Typecheck_error.Not_in_scope path)))

(* Resolves context entry [name] at a splice site. *)
and entry_name_resolution t (scope : Scope.t) ~name =
  match
    Env.find scope.locals name, Hashtbl.find scope.program.toplevels name
  with
  | Some entry, _ -> Ok (entry_resolution t scope ~name (Some entry))
  | None, Some toplevel ->
    let%map.Result entry = toplevel_entry t scope toplevel in
    entry_resolution t scope ~name:toplevel.name (Some entry)
  | None, None -> Ok (entry_resolution t scope ~name None)

and toplevel_type t scope (toplevel : Toplevel.t) =
  let%bind.Result entry = toplevel_entry t scope toplevel in
  entry_type t scope ~name:toplevel.name (Some entry)

and toplevel_entry t (scope : Scope.t) (toplevel : Toplevel.t) =
  match toplevel.state with
  | Done entry -> Ok entry
  | In_progress in_progress ->
    (* [toplevel] is in the component being checked, so this is a recursive
       reference. *)
    in_progress.recursive <- true;
    scope.scc.lowlink <- Int.min scope.scc.lowlink in_progress.index;
    Ok (Env.Entry.toplevel ~scheme:in_progress.ty ~persistent:false)
  | Unchecked ->
    let%bind.Result scc = check_toplevel t scope.program toplevel in
    scope.scc.lowlink <- Int.min scope.scc.lowlink scc.lowlink;
    scope.scc.pending <- scc.pending @ scope.scc.pending;
    toplevel_entry t scope toplevel

and check_toplevel t program (toplevel : Toplevel.t)
  : (Scc_frame.t, Typecheck_error.t) Result.t
  =
  let index = fresh_id t in
  let scc = { Scc_frame.lowlink = index; pending = [ toplevel ] } in
  let rank = 1 in
  let scope =
    { Scope.program
    ; level = 0
    ; rank
    ; locals = Env.create ()
    ; quote = None
    ; module_ = toplevel.module_
    ; scc
    ; annotation_vars =
        Type_env.Annotation_vars.create ~fresh_id:(fun () -> fresh_id t) ~rank
    }
  in
  let%bind.Result args, res_ty, ty =
    function_type
      t
      scope
      ~rank:scope.rank
      ~args:toplevel.args
      ~annot:toplevel.annot
  in
  toplevel.state <- In_progress { index; ty; recursive = false };
  let%map.Result () = check_body t scope ~args ~res_ty ~body:toplevel.expr in
  if scc.lowlink = index
  then (
    finish_scc t scc.pending;
    scc.pending <- []);
  scc

and check_body t (scope : Scope.t) ~args ~res_ty ~body =
  with_monomorphic_bindings scope args ~f:(fun () ->
    let%bind.Result body_ty = expr_type t scope ~expr:body in
    unify_ty' t ~ty1:res_ty ~ty2:body_ty)

and with_monomorphic_bindings (scope : Scope.t) bindings ~f =
  Env.with_bindings
    scope.locals
    (List.map bindings ~f:(fun (name, ty) ->
       name, Scope.entry scope ~scheme:ty ~persistent:false))
    ~f

and expr_type t (scope : Scope.t) ~(expr : Syntax.Expr.t)
  : (Type.ty, Typecheck_error.t) Result.t
  =
  match expr with
  | Lit l -> Ok (Syntax.Lit.ty l)
  | Var (path, annot) ->
    let%bind.Result ty = var_type t scope ~path in
    (match annot with
     | None -> Ok ty
     | Some annot ->
       let%bind.Result annot = elaborate scope annot in
       let%map.Result () = unify_ty' t ~ty1:ty ~ty2:annot in
       ty)
  | App (f, x) ->
    let a : Type.ty = TVar (fresh_tvar t ~rank:scope.rank) in
    let b : Type.ty = TVar (fresh_tvar t ~rank:scope.rank) in
    let%bind.Result f_ty = expr_type t scope ~expr:f in
    let%bind.Result () = unify_ty' t ~ty1:(TArrow (a, b)) ~ty2:f_ty in
    let%bind.Result x_ty = expr_type t scope ~expr:x in
    let%map.Result () = unify_ty' t ~ty1:a ~ty2:x_ty in
    b
  | Let (name, args, annot, equal, in_) ->
    let rank = scope.rank + 1 in
    let%bind.Result arg_types, res_ty, ty =
      function_type t scope ~rank ~args ~annot
    in
    let%bind.Result () =
      Env.with_bindings
        scope.locals
        [ name, Scope.entry scope ~scheme:ty ~persistent:false ]
        ~f:(fun () ->
          check_body t { scope with rank } ~args:arg_types ~res_ty ~body:equal)
    in
    let persistent = not (List.is_empty args) in
    let scheme =
      if Syntax.Expr.binding_is_value name ~args ~equal
      then generalize_ty t ~rank:scope.rank ~ty
      else ty
    in
    Env.with_bindings
      scope.locals
      [ name, Scope.entry scope ~scheme ~persistent ]
      ~f:(fun () -> expr_type t scope ~expr:in_)
  | Quote expr ->
    let quote =
      { Scope.id = fresh_id t
      ; context = CVar (fresh_cvar t ~rank:scope.rank)
      ; outside = scope
      }
    in
    let%map.Result inner =
      expr_type
        t
        { scope with level = scope.level + 1; quote = Some quote }
        ~expr
    in
    Type.TCode { inner; context = quote.context }
  | Splice { expr; subs } ->
    let outside : Scope.t =
      match scope.quote with
      | Some quote -> { quote.outside with rank = scope.rank }
      | None -> { scope with level = scope.level - 1 }
    in
    let inner = Type.TVar (fresh_tvar t ~rank:scope.rank) in
    let context = Type.CVar (fresh_cvar t ~rank:scope.rank) in
    let%bind.Result code_ty = expr_type t outside ~expr in
    let%bind.Result () =
      unify_ty' t ~ty1:(TCode { inner; context }) ~ty2:code_ty
    in
    let%map.Result () =
      match subs with
      | None -> substitute_from_scope t scope ~context
      | Some subs -> substitute_explicitly t scope ~context ~subs
    in
    inner
  | Tuple exprs ->
    let%map.Result types =
      List.map exprs ~f:(fun expr -> expr_type t scope ~expr) |> Result.all
    in
    Type.TTuple types
  | Construct (path, args) ->
    let%bind.Result constructor =
      Type_env.find_constructor scope.program.type_env scope.module_ path
    in
    let%bind.Result () = check_arity ~constructor ~args in
    let types, result = inst_constructor t scope constructor in
    let%map.Result () = check_exprs t scope (List.zip_exn args types) in
    result
  | Record fields ->
    let%bind.Result record, fields =
      record_fields t scope fields ~all_required:true
    in
    let%map.Result () =
      check_exprs t scope (List.map fields ~f:(fun (ty, expr) -> expr, ty))
    in
    record
  | Field (expr, path) ->
    let%bind.Result record, field_ty = field_type t scope path in
    let%bind.Result expr_ty = expr_type t scope ~expr in
    let%map.Result () = unify_ty' t ~ty1:record ~ty2:expr_ty in
    field_ty
  | Match (expr, cases) ->
    let%bind.Result scrutinee = expr_type t scope ~expr in
    let result = Type.TVar (fresh_tvar t ~rank:scope.rank) in
    let%map.Result () =
      List.fold_result cases ~init:() ~f:(fun () (pattern, body) ->
        let%bind.Result pattern_ty, bindings = pattern_type t scope ~pattern in
        let%bind.Result () =
          match
            List.map bindings ~f:fst |> List.find_a_dup ~compare:String.compare
          with
          | None -> Ok ()
          | Some name -> Error (Typecheck_error.Duplicate_pattern_variable name)
        in
        let%bind.Result () = unify_ty' t ~ty1:scrutinee ~ty2:pattern_ty in
        with_monomorphic_bindings scope bindings ~f:(fun () ->
          let%bind.Result body_ty = expr_type t scope ~expr:body in
          unify_ty' t ~ty1:result ~ty2:body_ty))
    in
    result

and check_exprs t scope exprs =
  List.fold_result exprs ~init:() ~f:(fun () (expr, ty) ->
    let%bind.Result expr_ty = expr_type t scope ~expr in
    unify_ty' t ~ty1:ty ~ty2:expr_ty)

and substitute_from_scope t (scope : Scope.t) ~context =
  match scope.quote with
  | None -> substitute_outside_quote t scope ~context ~seen:String.Set.empty
  | Some quote ->
    substitute_in_quote t scope quote ~context ~seen:String.Set.empty

and substitute_outside_quote t scope ~context ~seen =
  match Type.canonical_context context with
  | CEmpty -> Ok ()
  | CExtend (name, presence, ty, rest) ->
    let%bind.Result () =
      match Type.canonical_presence presence with
      | _ when Set.mem seen name -> Ok ()
      | Absent -> Ok ()
      | Present | PVar _ ->
        (match%bind.Result entry_name_resolution t scope ~name with
         | Unresolved error ->
           (* Code that may use [name] can't, so doesn't. *)
           unify_presence ~presence1:presence ~presence2:Absent
           |> Result.map_error ~f:(fun () -> error)
         | resolution ->
           let%bind.Result var_ty = resolution_type t ~name resolution in
           unify_ty' t ~ty1:ty ~ty2:var_ty)
    in
    substitute_outside_quote t scope ~context:rest ~seen:(Set.add seen name)
  | CVar _ as context -> unify_context' t ~context1:context ~context2:CEmpty

and substitute_in_quote t scope quote ~context ~seen =
  match Type.canonical_context context with
  | CEmpty -> Ok ()
  | CExtend (name, presence, ty, rest) ->
    let%bind.Result () =
      match
        Type.canonical_presence presence, Capture.find scope quote ~name
      with
      | _ when Set.mem seen name -> Ok ()
      | Absent, _ -> Ok ()
      | _, Local entry ->
        unify_ty'
          t
          ~ty1:ty
          ~ty2:(inst_ty t ~scheme:entry.scheme ~rank:scope.rank)
      | _, Wrong_level entry ->
        unify_presence ~presence1:presence ~presence2:Absent
        |> Result.map_error ~f:(fun () ->
          Typecheck_error.Variable_used_at_wrong_level
            { name; var_level = entry.level; used_at = scope.level })
      | _, Outer ->
        constrain_context'
          t
          ~context:(CExtend (name, presence, ty, CEmpty))
          ~bound:quote.context
          ~shadowed:String.Set.empty
    in
    substitute_in_quote t scope quote ~context:rest ~seen:(Set.add seen name)
  | CVar _ as tail ->
    (* The code's remaining free variables are not known yet, so give the
       ones that would be captured here explicit entries. The rest must be
       free variables of the quote. *)
    let captured =
      Env.visible scope.locals
      |> List.filter_map ~f:(fun (name, entry) ->
        if Set.mem seen name
        then None
        else (
          match Capture.of_entry scope quote entry with
          | Local entry ->
            Some
              ( name
              , Type.PVar (fresh_pvar t ~rank:scope.rank)
              , inst_ty t ~scheme:entry.scheme ~rank:scope.rank )
          | Wrong_level _ ->
            Some (name, Absent, Type.TVar (fresh_tvar t ~rank:scope.rank))
          | Outer -> None))
    in
    let rest = Type.CVar (fresh_cvar t ~rank:scope.rank) in
    let%bind.Result () =
      unify_context'
        t
        ~context1:tail
        ~context2:
          (List.fold_right
             captured
             ~init:rest
             ~f:(fun (name, presence, ty) rest ->
               Type.CExtend (name, presence, ty, rest)))
    in
    constrain_context'
      t
      ~context:rest
      ~bound:quote.context
      ~shadowed:
        (List.fold captured ~init:seen ~f:(fun seen (name, _, _) ->
           Set.add seen name))

and substitute_explicitly t scope ~context ~subs =
  let%bind.Result () =
    match List.map subs ~f:fst |> List.find_a_dup ~compare:String.compare with
    | None -> Ok ()
    | Some name -> Error (Typecheck_error.Duplicate_substitution name)
  in
  let%bind.Result provided =
    List.fold_result subs ~init:[] ~f:(fun acc (name, expr) ->
      let%map.Result ty =
        match expr with
        | None -> var_type t scope ~path:(Syntax.Path.of_string name)
        | Some expr -> expr_type t scope ~expr
      in
      (name, ty) :: acc)
  in
  let provided =
    List.fold provided ~init:Type.CEmpty ~f:(fun rest (name, ty) ->
      Type.CExtend (name, PVar (fresh_pvar t ~rank:scope.Scope.rank), ty, rest))
  in
  unify_context' t ~context1:context ~context2:provided
;;

let typecheck t ~(program : Syntax.Program.t)
  : ((string * Type.ty) list, Typecheck_error.t) Result.t
  =
  let%bind.Result root =
    Module_scope.create
      (List.map Type_env.builtins ~f:Syntax.Toplevel.type_ @ program)
  in
  let%bind.Result type_env =
    Type_env.create root ~fresh_id:(fun () -> fresh_id t)
  in
  let toplevels =
    Module_scope.toplevels root
    |> List.filter_map ~f:(fun (module_, (toplevel : Syntax.Toplevel.t)) ->
      match toplevel with
      | Let (name, args, annot, expr) ->
        Some
          { Toplevel.name = Module_scope.canonical_name module_ name
          ; module_
          ; args
          ; annot
          ; expr
          ; state = Unchecked
          }
      | Type _ | Module _ -> None)
  in
  let program =
    { Program_env.type_env
    ; toplevels =
        List.map toplevels ~f:(fun toplevel -> toplevel.name, toplevel)
        |> String.Table.of_alist_exn
    }
  in
  List.fold_result toplevels ~init:[] ~f:(fun acc toplevel ->
    let%map.Result () =
      match toplevel.state with
      | Unchecked ->
        check_toplevel t program toplevel
        |> Result.map ~f:(ignore : Scc_frame.t -> unit)
      | In_progress _ | Done _ -> Ok ()
    in
    match toplevel.state with
    | Done { scheme; _ } -> (toplevel.name, scheme) :: acc
    | Unchecked | In_progress _ ->
      raise_s [%message "typecheck: toplevel not done" (toplevel : Toplevel.t)])
  |> Result.map ~f:List.rev
;;

let to_string_hum t ty =
  Type.to_string_hum
    ~context_bounds:(fun id ->
      List.map (context_bounds t id) ~f:(fun (bound : Bound.t) -> bound.context))
    ty
;;

let constrain_context t ~context ~bound =
  constrain_context t ~context ~bound ~shadowed:String.Set.empty
;;
