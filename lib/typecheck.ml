open! Core

module Toplevel = struct
  module State = struct
    type t =
      | Unchecked
      | In_progress of
          { index : int
          ; ty : Type.ty
          }
      | Done of Env.Entry.t
    [@@deriving sexp_of]
  end

  type t =
    { name : string
    ; args : Syntax.Binding.t list
    ; annot : Type.ty option
    ; expr : Syntax.Expr.t
    ; mutable state : State.t
    }
  [@@deriving sexp_of]
end

type t =
  { mutable next_id : int
  ; toplevels : Toplevel.t String.Table.t
  }

let create () = { next_id = 0; toplevels = String.Table.create () }

let fresh_id t =
  let id = t.next_id in
  t.next_id <- id + 1;
  id
;;

let fresh_tvar t ~rank = ref (Type.Unbound (fresh_id t, rank))
let fresh_cvar t ~rank = ref (Type.CUnbound (fresh_id t, rank))

let rec inst_ty' t ~remap_ty ~remap_context ~(scheme : Type.ty) ~rank : Type.ty =
  let scheme = Type.canonical_ty scheme in
  match scheme with
  | TCon _ -> scheme
  | TArrow (ty1, ty2) ->
    TArrow
      ( inst_ty' t ~remap_ty ~remap_context ~scheme:ty1 ~rank
      , inst_ty' t ~remap_ty ~remap_context ~scheme:ty2 ~rank )
  | TTuple tys ->
    TTuple
      (List.map tys ~f:(fun scheme ->
         inst_ty' t ~remap_ty ~remap_context ~scheme ~rank))
  | TCode { inner; context } ->
    TCode
      { inner = inst_ty' t ~remap_ty ~remap_context ~scheme:inner ~rank
      ; context = inst_context' t ~remap_ty ~remap_context ~context ~rank
      }
  | TVar { contents = Link ty } ->
    inst_ty' t ~remap_ty ~remap_context ~scheme:ty ~rank
  | TVar { contents = Type.Unbound _ } -> scheme
  | TVar { contents = Type.Generic id } ->
    Hashtbl.find_or_add remap_ty id ~default:(fun () ->
      Type.TVar (fresh_tvar t ~rank))

and inst_context' t ~remap_ty ~remap_context ~(context : Type.context) ~rank
  : Type.context
  =
  let context = Type.canonical_context context in
  match context with
  | CEmpty -> CEmpty
  | CExtend (name, ty, context') ->
    CExtend
      ( name
      , inst_ty' t ~remap_ty ~remap_context ~scheme:ty ~rank
      , inst_context' t ~remap_ty ~remap_context ~context:context' ~rank )
  | CVar { contents = Type.CLink context' } ->
    inst_context' t ~remap_ty ~remap_context ~context:context' ~rank
  | CVar { contents = Type.CUnbound _ } -> context
  | CVar { contents = Type.CGeneric id } ->
    Hashtbl.find_or_add remap_context id ~default:(fun () ->
      Type.CVar (fresh_cvar t ~rank))
;;

let inst_ty t ~scheme ~rank =
  inst_ty'
    t
    ~scheme
    ~remap_ty:(Int.Table.create ())
    ~remap_context:(Int.Table.create ())
    ~rank
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
  match Type.canonical_context context with
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

module Typecheck_error = struct
  type t =
    | Unification_error of Unification_error.t
    | Not_in_scope of string
    | Duplicate_toplevel of string
    | Duplicate_substitution of string
    | Variable_used_at_wrong_level of
        { name : string
        ; var_level : Env.Level.t
        ; used_at : Env.Level.t
        }
  [@@deriving sexp, compare, equal, variants]
end

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

module Scope = struct
  type t =
    { level : Env.Level.t
    ; rank : Type.rank
    ; locals : Env.t
    ; quote : quote option
    ; scc : Scc_frame.t
    }

  and quote =
    { id : int
    ; rank_at_quote : Type.rank
    ; free_vars : Type.ty String.Table.t
    ; other_free_vars : Type.context
      (* The context's tail: free variables of spliced code whose context is
         not fully known yet. *)
    ; outside : t
    }

  let quote_id t = Option.map t.quote ~f:(fun quote -> quote.id)

  let entry t ~scheme ~persistent =
    { Env.Entry.scheme; level = t.level; quote_id = quote_id t; persistent }
  ;;
end

let unpack_annot t ~rank annot =
  Option.value_or_thunk annot ~default:(fun () ->
    Type.TVar (fresh_tvar t ~rank))
;;

let function_type t ~rank ~(args : Syntax.Binding.t list) ~annot =
  let args =
    List.map args ~f:(fun (name, annot) -> name, unpack_annot t ~rank annot)
  in
  let res_ty = unpack_annot t ~rank annot in
  let ty =
    List.fold_right args ~init:res_ty ~f:(fun (_, arg_ty) ty ->
      Type.TArrow (arg_ty, ty))
  in
  args, res_ty, ty
;;

let free_var t (quote : Scope.quote) ~name =
  Hashtbl.find_or_add quote.free_vars name ~default:(fun () ->
    Type.TVar (fresh_tvar t ~rank:quote.rank_at_quote))
;;

let finish_scc t (members : Toplevel.t list) =
  let persistent =
    List.for_all members ~f:(fun member -> not (List.is_empty member.args))
  in
  List.iter members ~f:(fun member ->
    match member.state with
    | In_progress { ty; _ } ->
      let scheme = if persistent then generalize_ty t ~rank:0 ~ty else ty in
      member.state <- Done (Env.Entry.toplevel ~scheme ~persistent)
    | Unchecked | Done _ ->
      raise_s
        [%message "finish_scc: member not in progress" (member : Toplevel.t)])
;;

let rec var_type t (scope : Scope.t) ~name
  : (Type.ty, Typecheck_error.t) Result.t
  =
  let%bind.Result entry =
    match Env.find scope.locals name with
    | Some entry -> Ok (Some entry)
    | None ->
      (match Hashtbl.find t.toplevels name with
       | None -> Ok None
       | Some toplevel ->
         toplevel_entry t scope toplevel |> Result.map ~f:Option.some)
  in
  match entry, scope.quote with
  | Some entry, _
    when Env.Entry.is_allowed_at
           entry
           ~used_at:scope.level
           ~quote_id:(Scope.quote_id scope) ->
    Ok (inst_ty t ~scheme:entry.scheme ~rank:scope.rank)
  | Some entry, Some quote when entry.level <= scope.level ->
    Ok (free_var t quote ~name)
  | None, Some quote -> Ok (free_var t quote ~name)
  | Some entry, _ ->
    Error
      (Typecheck_error.Variable_used_at_wrong_level
         { name; var_level = entry.level; used_at = scope.level })
  | None, None -> Error (Typecheck_error.Not_in_scope name)

and toplevel_entry t (scope : Scope.t) (toplevel : Toplevel.t) =
  match toplevel.state with
  | Done entry -> Ok entry
  | In_progress { index; ty } ->
    scope.scc.lowlink <- Int.min scope.scc.lowlink index;
    Ok (Env.Entry.toplevel ~scheme:ty ~persistent:false)
  | Unchecked ->
    let%bind.Result scc = check_toplevel t toplevel in
    scope.scc.lowlink <- Int.min scope.scc.lowlink scc.lowlink;
    scope.scc.pending <- scc.pending @ scope.scc.pending;
    toplevel_entry t scope toplevel

and check_toplevel t (toplevel : Toplevel.t)
  : (Scc_frame.t, Typecheck_error.t) Result.t
  =
  let index = fresh_id t in
  let scc = { Scc_frame.lowlink = index; pending = [ toplevel ] } in
  let scope =
    { Scope.level = 0; rank = 1; locals = Env.create (); quote = None; scc }
  in
  let args, res_ty, ty =
    function_type t ~rank:scope.rank ~args:toplevel.args ~annot:toplevel.annot
  in
  toplevel.state <- In_progress { index; ty };
  let%map.Result () = check_body t scope ~args ~res_ty ~body:toplevel.expr in
  if scc.lowlink = index
  then (
    finish_scc t scc.pending;
    scc.pending <- []);
  scc

and check_body t (scope : Scope.t) ~args ~res_ty ~body =
  Env.with_bindings
    scope.locals
    (List.map args ~f:(fun (name, ty) ->
       name, Scope.entry scope ~scheme:ty ~persistent:false))
    ~f:(fun () ->
      let%bind.Result body_ty = expr_type t scope ~expr:body in
      unify_ty' t ~ty1:res_ty ~ty2:body_ty)

and expr_type t (scope : Scope.t) ~(expr : Syntax.Expr.t)
  : (Type.ty, Typecheck_error.t) Result.t
  =
  match expr with
  | Lit l -> Ok (Syntax.Lit.ty l)
  | Var (name, annot) ->
    let%bind.Result ty = var_type t scope ~name in
    (match annot with
     | None -> Ok ty
     | Some annot ->
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
    let args, res_ty, ty = function_type t ~rank ~args ~annot in
    let%bind.Result () =
      Env.with_bindings
        scope.locals
        [ name, Scope.entry scope ~scheme:ty ~persistent:false ]
        ~f:(fun () ->
          check_body t { scope with rank } ~args ~res_ty ~body:equal)
    in
    (* syntactic value restriction *)
    let persistent = not (List.is_empty args) in
    let scheme =
      if persistent then generalize_ty t ~rank:scope.rank ~ty else ty
    in
    Env.with_bindings
      scope.locals
      [ name, Scope.entry scope ~scheme ~persistent ]
      ~f:(fun () -> expr_type t scope ~expr:in_)
  | Quote expr ->
    let quote =
      { Scope.id = fresh_id t
      ; rank_at_quote = scope.rank
      ; free_vars = String.Table.create ()
      ; other_free_vars = CVar (fresh_cvar t ~rank:scope.rank)
      ; outside = scope
      }
    in
    let%map.Result inner =
      expr_type
        t
        { scope with level = scope.level + 1; quote = Some quote }
        ~expr
    in
    let context =
      Hashtbl.to_alist quote.free_vars
      |> List.sort ~compare:(Comparable.lift String.compare ~f:fst)
      |> List.fold_right ~init:quote.other_free_vars ~f:(fun (name, ty) rest ->
        Type.CExtend (name, ty, rest))
    in
    Type.TCode { inner; context }
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

and substitute_from_scope t scope ~context =
  match Type.canonical_context context with
  | CEmpty -> Ok ()
  | CExtend (name, ty, rest) ->
    let%bind.Result var_ty = var_type t scope ~name in
    let%bind.Result () = unify_ty' t ~ty1:ty ~ty2:var_ty in
    substitute_from_scope t scope ~context:rest
  | CVar _ as context ->
    (match (scope : Scope.t).quote with
     | None -> unify_context' t ~context1:context ~context2:CEmpty
     | Some quote ->
       unify_context' t ~context1:quote.other_free_vars ~context2:context)

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
        | None -> var_type t scope ~name
        | Some expr -> expr_type t scope ~expr
      in
      (name, ty) :: acc)
  in
  let provided =
    List.fold provided ~init:Type.CEmpty ~f:(fun rest (name, ty) ->
      Type.CExtend (name, ty, rest))
  in
  unify_context' t ~context1:context ~context2:provided
;;

let typecheck t ~(program : Syntax.Program.t)
  : ((string * Type.ty) list, Typecheck_error.t) Result.t
  =
  let toplevels =
    List.map program ~f:(fun (Let (name, args, annot, expr)) ->
      { Toplevel.name; args; annot; expr; state = Unchecked })
  in
  let%bind.Result () =
    List.fold_result toplevels ~init:() ~f:(fun () toplevel ->
      match Hashtbl.add t.toplevels ~key:toplevel.name ~data:toplevel with
      | `Ok -> Ok ()
      | `Duplicate -> Error (Typecheck_error.Duplicate_toplevel toplevel.name))
  in
  List.fold_result toplevels ~init:[] ~f:(fun acc toplevel ->
    let%map.Result () =
      match toplevel.state with
      | Unchecked ->
        check_toplevel t toplevel
        |> Result.map ~f:(ignore : Scc_frame.t -> unit)
      | In_progress _ | Done _ -> Ok ()
    in
    match toplevel.state with
    | Done { scheme; _ } -> (toplevel.name, scheme) :: acc
    | Unchecked | In_progress _ ->
      raise_s [%message "typecheck: toplevel not done" (toplevel : Toplevel.t)])
  |> Result.map ~f:List.rev
;;
