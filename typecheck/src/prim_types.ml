open! Import

let prim_type (scope : Scope.t) prim =
  List.fold_right2_exn
    (Prim.args prim)
    (Mode_rules.prim (Scope.modes scope) ~args:(Prim.args prim))
    ~init:(Prim.result prim)
    ~f:(fun arg modes res -> Type.TArrow (arg, res, modes))
;;

(* The intrinsic [var] is, through toplevels defined as one ([Box.make] is
   [Intrinsic.box_make]). *)
let intrinsic_of (scope : Scope.t) (var : Typed.Var_ref.t) =
  let rec go (var : Typed.Var_ref.t) ~seen =
    match var with
    | Intrinsic intrinsic -> Some intrinsic
    | (Bound (Toplevel ident) | Persistent (Toplevel ident))
      when not (List.mem seen ident.name ~equal:String.equal) ->
      (match Program_env.find_toplevel scope.program ident.name with
       | Some
           { state =
               Done
                 { typed = { kind = Value; body = { desc = Var var; _ }; _ }
                 ; _
                 }
           ; _
           } -> go var ~seen:(ident.name :: seen)
       | Some _ | None -> None)
    | Bound _ | Persistent _ | Free _ | Prim _ -> None
  in
  go var ~seen:[]
;;

let value_allocation scope (var : Typed.Var_ref.t)
  : Typed.Allocation.Kind.t option
  =
  match var with
  | Prim prim when Prim.arity prim > 1 -> Some Partial_application
  | _ ->
    (match intrinsic_of scope var with
     | Some Box_make -> Some Box
     | Some Ptr_make -> Some Pointer
     | Some _ | None -> None)
;;

let rec prim_head (expr : Typed.Expr.t) =
  match expr.desc with
  | Var (Prim prim) -> Some (prim, expr, 0)
  | App (f, _) ->
    Option.map (prim_head f) ~f:(fun (prim, head, args) -> prim, head, args + 1)
  | _ -> None
;;

let prim_partial ty ~args =
  let rec go ty args =
    match Type.canonical_ty ty with
    | Type.TArrow (_, res, modes) ->
      if args = 1
      then (
        match modes.res with
        | Var var -> Some var
        | Fixed _ | Var_at _ -> None)
      else go res (args - 1)
    | _ -> None
  in
  go ty args
;;

let value_sites (scope : Scope.t) (expr : Typed.Expr.t) =
  match expr.desc with
  | Var var ->
    (match value_allocation scope var with
     | None -> expr
     | Some kind ->
       { expr with
         made =
           { Typed.Expr.Made.none with
             made =
               Mode_rules.Alloc.on_heap (Scope.modes scope) ~loc:expr.loc ~kind
           }
       })
  | _ -> expr
;;

let intrinsic_type t (scope : Scope.t) (intrinsic : Intrinsic.t) =
  match intrinsic with
  | Assume_global ->
    (* Only its locality is assumed. *)
    let ty = Scope.fresh_ty t scope in
    let arg, res = Mode_rules.assume_global (Scope.modes scope) in
    Type.TArrow
      ( ty
      , ty
      , { arg = Var arg; res = Var res; curry = Arrow_modes.default_curry } )
  | Ptr_make ->
    let pointee = Scope.fresh_ty t scope in
    let arg, res, held = Mode_rules.ptr_make (Scope.modes scope) in
    Type.TArrow
      ( pointee
      , Type.ptr ~held pointee
      , { arg = Var arg; res = Var res; curry = Arrow_modes.default_curry } )
  | Box_make ->
    let contents = Scope.fresh_ty t scope in
    let arg, res = Mode_rules.box_make (Scope.modes scope) in
    Type.TArrow
      ( contents
      , Type.box contents
      , { arg = Var arg; res = Var res; curry = Arrow_modes.default_curry } )
  | _ ->
    Intrinsic.ty
      intrinsic
      ~var:(fun () -> Scope.fresh_ty t scope)
      ~row:(fun () -> Scope.fresh_row t scope)
;;
