open! Import

(* Canonicalizes the bounds of [var], dropping the ones that are met: bounds by
   known presences have been propagated, and ones by [var] itself or repeated
   arise from unifying bounded variables. *)
let tidy (var : (Type.presence, Type.presence_bound) Type.Var.t) =
  let presences, equalities =
    List.partition_map var.bounds ~f:(fun bound ->
      match Type.presence_bound_var bound, bound with
      | Some other, At_most _ when not (phys_equal other var) ->
        First (Some (`At_most, other))
      | Some other, At_least _ when not (phys_equal other var) ->
        First (Some (`At_least, other))
      | (Some _ | None), (At_most _ | At_least _) -> First None
      | _, If_present (ty1, ty2) ->
        if Type.same_ty ty1 ty2 then First None else Second bound)
  in
  var.bounds
  <- (List.filter_opt presences
      |> List.dedup_and_sort ~compare:(fun (side1, var1) (side2, var2) ->
        [%compare: [ `At_most | `At_least ] * Type.id]
          (side1, var1.Type.Var.id)
          (side2, var2.id))
      |> List.map ~f:(function
        | `At_most, other -> Type.At_most (PVar other)
        | `At_least, other -> At_least (PVar other)))
     @ equalities
;;

let lower_presence (var : (Type.presence, _) Type.Var.t) ~rank =
  match var.state with
  | Unbound rank' when rank' > rank -> var.state <- Unbound rank
  | Unbound _ | Generic | Link _ -> ()
;;

(* Entries for variables no longer unbound have no bounds to keep, so are
   dropped. *)
let add_in_bounds_of (var : _ Type.Var.t) bounded =
  let unbound_id (bounded : Type.bounded) =
    match bounded with
    | Row_var { state = Unbound _; id; _ }
    | Presence_var { state = Unbound _; id; _ } -> Some id
    | Row_var _ | Presence_var _ -> None
  in
  match var.state with
  | Unbound _ ->
    List.iter bounded ~f:(fun bounded ->
      match unbound_id bounded with
      | None -> ()
      | Some id ->
        if not
             (List.exists var.in_bounds_of ~f:(fun bounded ->
                [%equal: int option] (unbound_id bounded) (Some id)))
        then
          var.in_bounds_of
          <- bounded
             :: List.filter var.in_bounds_of ~f:(fun bounded ->
               Option.is_some (unbound_id bounded)))
  | Generic | Link _ -> ()
;;

(* Records [bounded] in the variables of a bound it gets. *)
let record_in_bounds_of
  (iter : Nothing.t Type.Iter.t -> (unit, Nothing.t) Result.t)
  bounded
  =
  let record ~guarded:_ var : (unit, Nothing.t) Result.t =
    add_in_bounds_of var [ bounded ];
    Ok ()
  in
  match
    iter
      { Type.Iter.ty = record
      ; row = record
      ; presence = record
      ; mode = (fun _ -> Ok ())
      }
  with
  | Ok () -> ()
  | Error _ -> .
;;

(* Bounds may relate variables of any ranks, so are not followed. *)
let adjuster ~rank ~seen ~in_bounds_of : Unification_error.t Type.Iter.t =
  let adjust ~guarded:_ (var : _ Type.Var.t) =
    seen var.id;
    add_in_bounds_of var in_bounds_of;
    (match var.state with
     | Unbound rank' when rank' > rank -> var.state <- Unbound rank
     | Unbound _ | Generic | Link _ -> ());
    Ok ()
  in
  { ty = adjust
  ; row = adjust
  ; presence = adjust
  ; mode =
      (fun var ->
        Mode.Var.lower_rank var ~rank;
        Ok ())
  }
;;

let adjust_binding fresh iter ~id ~rank ~in_bounds_of ~occurs target =
  let recursive = ref false in
  let%bind.Result () =
    iter
      (adjuster ~rank ~in_bounds_of ~seen:(fun other ->
         if id = other then recursive := true))
  in
  if not !recursive
  then Ok ()
  else if Value_layout.reaches_unguarded
            ~con:(Fresh.value_parameters fresh)
            ~needs_structure:false
            ~var:id
            [ target ]
  then Error (Unification_error.Occurs_check_failed occurs)
  else Ok ()
;;

(* Repeated pairs are assumed equal. Cuts through links also cover cycles
   without a structural record or variant, including different unrollings. *)
module State = struct
  type t =
    { fresh : Fresh.t
    ; nodes : Id_pair.Hash_set.t
    ; links : Id_pair.Hash_set.t
    ; unaligned : Type.ty Type.Link_pairs.t
    ; rows : Type.row Type.Link_pairs.t
    }

  let create fresh =
    { fresh
    ; nodes = Id_pair.Hash_set.create ()
    ; links = Id_pair.Hash_set.create ()
    ; unaligned = Type.Link_pairs.create ()
    ; rows = Type.Link_pairs.create ()
    }
  ;;

  let assume set id1 id2 =
    Id_pair.Hash_set.mem_or_add set (Id_pair.create id1 id2)
  ;;
end

let presence_mismatch_reason name ~found
  : Unification_error.Row_mismatch_reason.t
  =
  match found with
  | None -> Missing_label name
  | Some (_ : Type.ty) -> Presence_mismatch name
;;

(* Whether binding [var] to a row with [ty] would leave [ty] unchanged. *)
let fits ty ~(var : (Type.row, Type.bound) Type.Var.t) =
  let rank = Type.Var.rank_exn var in
  let check ~guarded:_ (var' : _ Type.Var.t) =
    match var'.state with
    | Unbound rank' when rank' <= rank && var'.id <> var.id -> Ok ()
    | Unbound _ | Generic | Link _ -> Error ()
  in
  Type.iter_ty
    { ty = check
    ; row = check
    ; presence = check
    ; mode =
        (fun var ->
          match Mode.Var.rank var with
          | Some rank' when rank' <= rank -> Ok ()
          | Some _ | None -> Error ())
    }
    ty
  |> Result.is_ok
;;

let fits_presence presence ~(var : (Type.row, Type.bound) Type.Var.t) =
  match Type.canonical_presence presence with
  | Present | Absent -> true
  | PVar var' ->
    (match var'.state with
     | Unbound rank' -> rank' <= Type.Var.rank_exn var
     | Generic | Link _ -> false)
;;

(* Whether entries of [presence] of types [ty1] and [ty2] are known to have
   equal types when present, so bounds with either are equivalent. *)
let bounded_id : Type.bounded -> Type.id = function
  | Row_var var -> var.id
  | Presence_var var -> var.id
;;

(* Whether a bound of [bounded] may have [ty]: a variable not in one has no
   [in_bounds_of] entry for it. Saves looking through many bounds. *)
let may_be_in_bounds_of ty ~(bounded : _ Type.Var.t) =
  let has (var : _ Type.Var.t) =
    List.exists var.in_bounds_of ~f:(fun other -> bounded_id other = bounded.id)
  in
  match ty with
  | `Ty ty ->
    (match Type.canonical_ty ty with
     | TVar ({ state = Unbound _; _ } as var) -> has var
     | _ -> true)
  | `Row row ->
    (match Type.tail_var row with
     | Some ({ state = Unbound _; _ } as var) -> has var
     | Some _ | None -> true)
;;

let equal_if_present presence ty1 ty2 =
  match Type.canonical_presence presence with
  | Absent -> true
  | Present -> false
  | PVar var
    when not
           (may_be_in_bounds_of (`Ty ty1) ~bounded:var
            && may_be_in_bounds_of (`Ty ty2) ~bounded:var) -> false
  | PVar var ->
    List.exists var.bounds ~f:(function
      | If_present (ty1', ty2') ->
        (Type.same_ty ty1 ty1' && Type.same_ty ty2 ty2')
        || (Type.same_ty ty1 ty2' && Type.same_ty ty2 ty1')
      | At_most _ | At_least _ -> false)
;;

(* The lists must have the same length. *)
let iter2_result xs ys ~(local_ f) =
  let rec loop xs ys =
    match xs, ys with
    | x :: xs, y :: ys ->
      (match f x y with
       | Ok () -> loop xs ys
       | Error _ as error -> error)
    | [], [] -> Ok ()
    | _ -> raise_s [%message "Unify.iter2_result: lengths differ"]
  in
  loop xs ys [@nontail]
;;

(* Like [iter2_result], but applying [f] to every pair, returning the first
   error. *)
let iter2_all xs ys ~(local_ f) =
  let rec loop xs ys ~first =
    match xs, ys with
    | x :: xs, y :: ys ->
      let result = f x y in
      loop
        xs
        ys
        ~first:
          (match first with
           | Ok () -> result
           | Error _ -> first)
    | [], [] -> first
    | _ -> raise_s [%message "Unify.iter2_all: lengths differ"]
  in
  loop xs ys ~first:(Ok ()) [@nontail]
;;

(* Whether two types can't be unified whatever their variables are. The depth
   bounds the search through cyclic types. *)
let clash ty1 ty2 =
  let rec clash ~depth ty1 ty2 =
    let all tys1 tys2 =
      List.length tys1 <> List.length tys2
      || List.exists2_exn tys1 tys2 ~f:(clash ~depth:(depth - 1))
    in
    depth > 0
    &&
    match Type.canonical_ty ty1, Type.canonical_ty ty2 with
    | TVar _, _ | _, TVar _ -> false
    | TCon (name1, args1), TCon (name2, args2) ->
      (not (String.equal name1 name2))
      || List.length args1 <> List.length args2
      || List.exists2_exn args1 args2 ~f:(fun arg1 arg2 ->
        match arg1, arg2 with
        | ATy ty1, ATy ty2 -> clash ~depth:(depth - 1) ty1 ty2
        | ARow _, ARow _ | AMode _, AMode _ -> false
        | (ATy _ | ARow _ | AMode _), _ -> true)
    | TArrow (arg1, res1, _), TArrow (arg2, res2, _) ->
      clash ~depth:(depth - 1) arg1 arg2 || clash ~depth:(depth - 1) res1 res2
    | TTuple tys1, TTuple tys2 -> all tys1 tys2
    | TCode { inner = inner1; _ }, TCode { inner = inner2; _ } ->
      clash ~depth:(depth - 1) inner1 inner2
    | TRecord _, TRecord _ | TVariant _, TVariant _ -> false
    | (TCon _ | TArrow _ | TTuple _ | TCode _ | TRecord _ | TVariant _), _ ->
      true
  in
  clash ~depth:8 ty1 ty2
;;

let poison ty =
  let bind ~guarded:_ (var : _ Type.Var.t) =
    (match var.state with
     | Unbound _ -> var.state <- Link Type.error
     | Generic | Link _ -> ());
    Ok ()
  in
  let ignore ~guarded:_ _ = Ok () in
  Type.iter_ty
    { ty = bind; row = ignore; presence = ignore; mode = (fun _ -> Ok ()) }
    ty
;;

let rec unify_ty' state ~ty1 ~ty2 : (unit, Unification_error.t) Result.t =
  match ty1, ty2 with
  | ( Type.TVar { id = id1; state = Link _; _ }
    , Type.TVar { id = id2; state = Link _; _ } )
    when State.assume state.State.links id1 id2 -> Ok ()
  | Type.TVar { state = Link _; _ }, Type.TVar { state = Link _; _ } ->
    unify_structure state ~ty1 ~ty2
  | _ when Type.Link_pairs.ty state.State.unaligned ty1 ty2 -> Ok ()
  | _ -> unify_structure state ~ty1 ~ty2

and unify_structure state ~ty1 ~ty2 =
  let ty1 = Type.canonical_ty ty1 in
  let ty2 = Type.canonical_ty ty2 in
  let same_length xs ys : (unit, Unification_error.t) Result.t =
    let length1 = List.length xs in
    let length2 = List.length ys in
    if length1 = length2
    then Ok ()
    else
      Error (Type_mismatch (ty1, ty2, Args_length_mismatch (length1, length2)))
  in
  match ty1, ty2 with
  | _ when phys_equal ty1 ty2 -> Ok ()
  | _, _ when Type.is_error ty1 -> poison ty2
  | _, _ when Type.is_error ty2 -> poison ty1
  | TCon (name1, args1), TCon (name2, args2) ->
    if not (String.equal name1 name2)
    then Error (Type_mismatch (ty1, ty2, Base_types_not_equal (name1, name2)))
    else (
      let%bind.Result () = same_length args1 args2 in
      iter2_result args1 args2 ~f:(fun arg1 arg2 ->
        match arg1, arg2 with
        | ATy ty1, ATy ty2 -> unify_ty' state ~ty1 ~ty2
        | ARow row1, ARow row2 -> unify_row' state ~row1 ~row2
        | AMode mode1, AMode mode2 ->
          Mode_arg.equate (Fresh.modes state.fresh) mode1 mode2
          |> Result.map_error ~f:(fun error ->
            Unification_error.Type_mismatch (ty1, ty2, Modes error))
        | (ATy _ | ARow _ | AMode _), _ ->
          Error (Type_mismatch (ty1, ty2, Mismatch_arg_kind (arg1, arg2)))))
  | TArrow (arg1, res1, modes1), TArrow (arg2, res2, modes2) ->
    let%bind.Result () = unify_ty' state ~ty1:arg1 ~ty2:arg2 in
    let%bind.Result () = unify_ty' state ~ty1:res1 ~ty2:res2 in
    Arrow_modes.equate (Fresh.modes state.fresh) modes1 modes2
    |> Result.map_error ~f:(fun error ->
      Unification_error.Type_mismatch (ty1, ty2, Modes error))
  | TTuple tys1, TTuple tys2 ->
    let%bind.Result () = same_length tys1 tys2 in
    iter2_all tys1 tys2 ~f:(fun ty1 ty2 -> unify_ty' state ~ty1 ~ty2)
  | ( TCode { inner = inner1; context = row1 }
    , TCode { inner = inner2; context = row2 } ) ->
    let%bind.Result () = unify_ty' state ~ty1:inner1 ~ty2:inner2 in
    unify_row' state ~row1 ~row2
  | TRecord { id = id1; row = row1 }, TRecord { id = id2; row = row2 }
  | TVariant { id = id1; row = row1 }, TVariant { id = id2; row = row2 } ->
    if State.assume state.State.nodes id1 id2
    then Ok ()
    else unify_row' state ~row1 ~row2
  | TVar var1, TVar var2 when phys_equal var1 var2 -> Ok ()
  | TVar var, ty | ty, TVar var ->
    let%map.Result () =
      adjust_binding
        state.State.fresh
        (fun f -> Type.iter_ty f ty)
        ~id:var.id
        ~rank:(Type.Var.rank_exn var)
        ~in_bounds_of:var.in_bounds_of
        ~occurs:(ATy (TVar var))
        (ATy ty)
    in
    var.state <- Link ty
  | _ -> Error (Type_mismatch (ty1, ty2, Structural_mismatch))

and bind_rvar state (var : _ Type.Var.t) row =
  let%bind.Result () =
    adjust_binding
      state.State.fresh
      (fun f -> Type.iter_row f row)
      ~id:var.id
      ~rank:(Type.Var.rank_exn var)
      ~in_bounds_of:var.in_bounds_of
      ~occurs:(ARow (RVar var))
      (ARow row)
  in
  var.state <- Link row;
  let%bind.Result () =
    if var.in_record then in_record' state row ~whole:row else Ok ()
  in
  let bounds = var.bounds in
  var.bounds <- [];
  List.fold_result bounds ~init:() ~f:(fun () (bound : Type.bound) ->
    constrain_row' state ~row ~bound:bound.row ~shadowed:bound.shadowed)

and in_record' state row ~whole =
  match Type.canonical_row row with
  | REmpty -> Ok ()
  | RExtend (name, _, assigned, _, rest) ->
    let%bind.Result () =
      unify_presence'
        state
        ~presence1:assigned
        ~presence2:Type.Absent
        ~error:(Unification_error.Assigned_in_record (name, whole))
    in
    in_record' state rest ~whole
  | RVar var ->
    var.in_record <- true;
    Ok ()

and unify_row' state ~row1 ~row2 : (unit, Unification_error.t) Result.t =
  if Type.Link_pairs.row state.State.rows row1 row2
  then Ok ()
  else unify_row_structure state ~row1 ~row2

and unify_row_structure state ~row1 ~row2 =
  let row1 = Type.canonical_row row1 in
  let row2 = Type.canonical_row row2 in
  match row1, row2 with
  | REmpty, REmpty -> Ok ()
  | RExtend (x, p1, a1, t1, rest1), c2 ->
    let tail1 = Type.tail_var rest1 in
    let%bind.Result p2, a2, t2, rest2 =
      rewrite state x c2 ~entry:t1 ~assigned:a1
    in
    let%bind.Result () =
      match tail1 with
      | Some { state = Link _; _ } ->
        Error (Unification_error.Row_mismatch (row1, row2, Cyclic_row))
      | _ -> Ok ()
    in
    let%bind.Result () =
      unify_presence'
        state
        ~presence1:p1
        ~presence2:p2
        ~error:
          (Unification_error.Row_mismatch
             (row1, row2, presence_mismatch_reason x ~found:t2))
    in
    let%bind.Result () =
      unify_presence'
        state
        ~presence1:a1
        ~presence2:a2
        ~error:(Unification_error.Row_mismatch (row1, row2, Assigned x))
    in
    let%bind.Result () =
      match t2 with
      | None -> Ok ()
      | Some t2 -> unify_if_present' state ~presence:p1 ~ty1:t1 ~ty2:t2
    in
    unify_row' state ~row1:rest1 ~row2:rest2
  | REmpty, RExtend (x, p2, _, _, rest2) ->
    let%bind.Result () =
      unify_presence'
        state
        ~presence1:Absent
        ~presence2:p2
        ~error:(Unification_error.Row_mismatch (row1, row2, Missing_label x))
    in
    unify_row' state ~row1 ~row2:rest2
  | RVar var1, RVar var2 when phys_equal var1 var2 -> Ok ()
  | RVar var, row | row, RVar var -> bind_rvar state var row

(* Finds the first entry for [name], which has no type if [row] lacks [name]
   entirely. A variable tail gets the entry, with type [entry] if given and it
   can be bound there as is: the entry has that type if present, so this is as
   general as a fresh type equal to it if present. Its [assigned] is the one
   given if so (the entry is to be unified with one that has it), else fresh. *)
and rewrite ?entry ?assigned state name (row : Type.row)
  : ( Type.presence * Type.presence * Type.ty option * Type.row
      , Unification_error.t )
      Result.t
  =
  match Type.canonical_row row with
  | REmpty -> Ok (Absent, Absent, None, REmpty)
  | RExtend (x, presence, assigned', ty, rest) ->
    (match String.equal x name with
     | true -> Ok (presence, assigned', Some ty, rest)
     | false ->
       let%map.Result presence', assigned'', ty', rest' =
         rewrite ?entry ?assigned state name rest
       in
       ( presence'
       , assigned''
       , ty'
       , Type.RExtend (x, presence, assigned', ty, rest') ))
  | RVar var ->
    let rank = Type.Var.rank_exn var in
    let px = Type.PVar (Fresh.var state.fresh ~rank) in
    let ax =
      match assigned with
      | Some assigned when fits_presence assigned ~var -> assigned
      | Some _ | None -> Type.PVar (Fresh.var state.fresh ~rank)
    in
    let tx =
      match entry with
      | Some ty when fits ty ~var -> ty
      | Some _ | None -> Type.TVar (Fresh.var state.fresh ~rank)
    in
    let rest = Type.RVar (Fresh.var state.fresh ~rank) in
    let%map.Result () =
      bind_rvar state var (RExtend (name, px, ax, tx, rest))
    in
    px, ax, Some tx, rest

and constrain_row' state ~row ~bound ~shadowed =
  match Type.canonical_row row with
  | REmpty -> Ok ()
  | RExtend (name, presence, assigned, ty, rest) ->
    let%bind.Result () =
      match Type.canonical_presence presence with
      | _ when Set.mem shadowed name -> Ok ()
      | Absent -> Ok ()
      | Present | PVar _ ->
        let%bind.Result bound_presence, bound_assigned, bound_ty, (_ : Type.row)
          =
          rewrite state name bound
        in
        let%bind.Result () =
          presence_at_most'
            state
            ~presence:assigned
            ~bound:bound_assigned
            ~error:(Unification_error.Row_mismatch (row, bound, Assigned name))
        in
        let%bind.Result () =
          presence_at_most'
            state
            ~presence
            ~bound:bound_presence
            ~error:
              (Unification_error.Row_mismatch
                 (row, bound, presence_mismatch_reason name ~found:bound_ty))
        in
        (match bound_ty with
         | None -> Ok ()
         | Some bound_ty ->
           unify_if_present' state ~presence ~ty1:ty ~ty2:bound_ty)
    in
    constrain_row' state ~row:rest ~bound ~shadowed:(Set.add shadowed name)
  | RVar var ->
    (match Type.canonical_row bound with
     | RVar var' when phys_equal var var' -> Ok ()
     | _
       when may_be_in_bounds_of (`Row bound) ~bounded:var
            && List.exists var.bounds ~f:(fun bound' ->
              Type.same_bound
                ~same_entry_ty:equal_if_present
                { row = bound; shadowed }
                bound') -> Ok ()
     | _ ->
       record_in_bounds_of (fun iter -> Type.iter_row iter bound) (Row_var var);
       var.bounds <- { row = bound; shadowed } :: var.bounds;
       Ok ())

(* Binding a variable passes its bounds on to what it is bound to. *)
and unify_presence' state ~presence1 ~presence2 ~error =
  match
    Type.canonical_presence presence1, Type.canonical_presence presence2
  with
  | Present, Present | Absent, Absent -> Ok ()
  | Present, Absent | Absent, Present -> Error error
  | PVar var1, PVar var2 when phys_equal var1 var2 -> Ok ()
  | PVar var, presence | presence, PVar var ->
    (match presence with
     | PVar var' ->
       lower_presence var' ~rank:(Type.Var.rank_exn var);
       add_in_bounds_of var' var.in_bounds_of
     | Present | Absent -> ());
    var.state <- Link presence;
    let bounds = var.bounds in
    var.bounds <- [];
    let%map.Result () =
      List.fold_result bounds ~init:() ~f:(fun () -> function
        | At_most bound -> presence_at_most' state ~presence ~bound ~error
        | At_least lower ->
          presence_at_most' state ~presence:lower ~bound:presence ~error
        | If_present (ty1, ty2) -> unify_if_present' state ~presence ~ty1 ~ty2)
    in
    (match Type.canonical_presence presence with
     | PVar var -> tidy var
     | Present | Absent -> ())

and presence_at_most' state ~presence ~bound ~error =
  match Type.canonical_presence presence, Type.canonical_presence bound with
  | Absent, _ | _, Present -> Ok ()
  | Present, _ | _, Absent ->
    unify_presence' state ~presence1:presence ~presence2:bound ~error
  | PVar lower, PVar upper when phys_equal lower upper -> Ok ()
  | PVar lower, PVar upper ->
    let has bounds ~side ~(other : (Type.presence, _) Type.Var.t) =
      List.exists bounds ~f:(fun (bound : Type.presence_bound) ->
        match side, bound with
        | `At_most, At_most presence | `At_least, At_least presence ->
          (match Type.canonical_presence presence with
           | PVar var -> phys_equal var other
           | Present | Absent -> false)
        | (`At_most | `At_least), (At_most _ | At_least _ | If_present _) ->
          false)
    in
    let known =
      if Stdlib.List.compare_lengths lower.bounds upper.bounds <= 0
      then has lower.bounds ~side:`At_most ~other:upper
      else has upper.bounds ~side:`At_least ~other:lower
    in
    if not known
    then (
      lower.bounds <- At_most (PVar upper) :: lower.bounds;
      upper.bounds <- At_least (PVar lower) :: upper.bounds);
    Ok ()

(* The types of entries with [presence] matter only if it is [Present], so their
   equality waits until it is known, or is [Absent] if they can't be equal. *)
and unify_if_present' state ~presence ~ty1 ~ty2 =
  if Type.same_ty ty1 ty2
  then Ok ()
  else (
    match Type.canonical_presence presence with
    | Present -> unify_ty' state ~ty1 ~ty2
    | Absent -> Ok ()
    | PVar _ when equal_if_present presence ty1 ty2 -> Ok ()
    | PVar _ when clash ty1 ty2 ->
      unify_presence'
        state
        ~presence1:presence
        ~presence2:Absent
        ~error:(Type_mismatch (ty1, ty2, Structural_mismatch))
      |> Result.map_error ~f:(fun error ->
        match unify_ty' state ~ty1 ~ty2 with
        | Ok () -> error
        | Error error -> error)
    | PVar var ->
      var.bounds <- If_present (ty1, ty2) :: var.bounds;
      List.iter [ ty1; ty2 ] ~f:(fun ty ->
        record_in_bounds_of
          (fun iter -> Type.iter_ty iter ty)
          (Presence_var var));
      Ok ())
;;

(* [lower <= upper]: their structures are unified, but the modes inside them are
   related by their positions' variances. Records, variants and code contexts
   are unified exactly. *)
module Sub_state = struct
  type t =
    { links : Int.Hash_set.t
    ; left : Type.ty Type.Link_pairs.t
    ; right : Type.ty Type.Link_pairs.t
    }

  let create () =
    { links = Int.Hash_set.create ()
    ; left = Type.Link_pairs.create ()
    ; right = Type.Link_pairs.create ()
    }
  ;;

  (* Ordered, as [a <= b] and [b <= a] differ. *)
  let assume t id1 id2 =
    let key = (id1 lsl 31) lor id2 in
    Hash_set.mem t.links key
    ||
    (Hash_set.add t.links key;
     false)
  ;;

  let seen t (lower : Type.ty) upper =
    match lower with
    | TVar { state = Link _; _ } -> Type.Link_pairs.ty t.left lower upper
    | _ -> Type.Link_pairs.ty t.right lower upper
  ;;
end

let modes_error ty1 ty2 error =
  Unification_error.Type_mismatch (ty1, ty2, Modes error)
;;

let rec sub_ty' state sub ~flip ~lower ~upper
  : (unit, Unification_error.t) Result.t
  =
  match lower, upper with
  | ( Type.TVar { id = id1; state = Link _; _ }
    , Type.TVar { id = id2; state = Link _; _ } )
    when Sub_state.assume sub id1 id2 -> Ok ()
  | Type.TVar { state = Link _; _ }, Type.TVar { state = Link _; _ } ->
    sub_structure state sub ~flip ~lower ~upper
  | _ when Sub_state.seen sub lower upper -> Ok ()
  | _ -> sub_structure state sub ~flip ~lower ~upper

(* Errors name the expected type first, which is [upper] unless [flip]: in a
   contravariant position, the expected type's part is [lower]. *)
and sub_structure state sub ~flip ~lower ~upper =
  let ty1 = Type.canonical_ty lower in
  let ty2 = Type.canonical_ty upper in
  let expected, actual = if flip then ty1, ty2 else ty2, ty1 in
  let modes_error error = modes_error expected actual error in
  let co ~lower ~upper = sub_ty' state sub ~flip ~lower ~upper in
  let contra ~lower ~upper = sub_ty' state sub ~flip:(not flip) ~lower ~upper in
  let ids = Fresh.modes state.State.fresh in
  (* Exact unification, naming the expected type first. *)
  let unify_ty ~lower ~upper =
    if flip
    then unify_ty' state ~ty1:lower ~ty2:upper
    else unify_ty' state ~ty1:upper ~ty2:lower
  in
  let mode_arg (variance : Variance.t) mode1 mode2 =
    (match variance with
     | Covariant -> Mode_arg.sub ids ~lower:mode1 ~upper:mode2
     | Contravariant -> Mode_arg.sub ids ~lower:mode2 ~upper:mode1
     | Invariant | Bivariant -> Mode_arg.equate ids mode1 mode2)
    |> Result.map_error ~f:modes_error
  in
  match ty1, ty2 with
  | _ when phys_equal ty1 ty2 -> Ok ()
  | TCon (name1, args1), TCon (name2, args2)
    when String.equal name1 name2 && List.length args1 = List.length args2 ->
    let variances =
      match Fresh.variances state.fresh name1 with
      | Some variances when List.length variances = List.length args1 ->
        variances
      | Some _ | None -> List.map args1 ~f:(fun _ -> Variance.Invariant)
    in
    iter2_result
      (List.zip_exn args1 args2)
      variances
      ~f:(fun (arg1, arg2) (variance : Variance.t) ->
        match arg1, arg2, variance with
        | ATy lower, ATy upper, Covariant -> co ~lower ~upper
        | ATy upper, ATy lower, Contravariant -> contra ~lower ~upper
        | ATy lower, ATy upper, (Invariant | Bivariant) ->
          unify_ty ~lower ~upper
        | AMode mode1, AMode mode2, variance -> mode_arg variance mode1 mode2
        | ARow row1, ARow row2, _ -> unify_row' state ~row1 ~row2
        | (ATy _ | ARow _ | AMode _), _, _ ->
          Error
            (Type_mismatch (expected, actual, Mismatch_arg_kind (arg1, arg2))))
  | TArrow (arg1, res1, modes1), TArrow (arg2, res2, modes2) ->
    let%bind.Result () = contra ~lower:arg2 ~upper:arg1 in
    let%bind.Result () = co ~lower:res1 ~upper:res2 in
    Arrow_modes.sub ids ~lower:modes1 ~upper:modes2
    |> Result.map_error ~f:modes_error
  | TTuple tys1, TTuple tys2 when List.length tys1 = List.length tys2 ->
    iter2_all tys1 tys2 ~f:(fun lower upper -> co ~lower ~upper)
  | ( TCode { inner = inner1; context = row1 }
    , TCode { inner = inner2; context = row2 } ) ->
    let%bind.Result () = co ~lower:inner1 ~upper:inner2 in
    if flip
    then unify_row' state ~row1 ~row2
    else unify_row' state ~row1:row2 ~row2:row1
  | _ -> unify_ty ~lower:ty1 ~upper:ty2
;;

let sub_ty fresh ~lower ~upper =
  sub_ty' (State.create fresh) (Sub_state.create ()) ~flip:false ~lower ~upper
;;

let lower_ty ~rank ty =
  match Type.iter_ty (adjuster ~rank ~seen:ignore ~in_bounds_of:[]) ty with
  | Ok () -> ()
  | Error error ->
    raise_s [%message "lower_ty: occurs check" (error : Unification_error.t)]
;;

let unify_ty fresh ~ty1 ~ty2 = unify_ty' (State.create fresh) ~ty1 ~ty2
let unify_row fresh ~row1 ~row2 = unify_row' (State.create fresh) ~row1 ~row2

(* Making a presence [Absent] drops its equalities and only makes others
   [Absent], so fails only if one is [Present]. *)
let make_absent fresh presence ~error =
  unify_presence'
    (State.create fresh)
    ~presence1:presence
    ~presence2:Absent
    ~error:(Row_mismatch (REmpty, REmpty, Presence_mismatch ""))
  |> Result.map_error ~f:(fun (_ : Unification_error.t) -> error)
;;

let presence_at_most fresh ~presence ~bound ~error =
  presence_at_most' (State.create fresh) ~presence ~bound ~error
;;

let unify_if_present fresh ~presence ~ty1 ~ty2 =
  unify_if_present' (State.create fresh) ~presence ~ty1 ~ty2
;;

let in_record fresh row = in_record' (State.create fresh) row ~whole:row

let constrain_row fresh ~row ~bound ~shadowed =
  constrain_row' (State.create fresh) ~row ~bound ~shadowed
;;

let present_entry ?(assigned = false) fresh ~row ~name ~rank =
  let state = State.create fresh in
  let%bind.Result presence, assigned', found, (_ : Type.row) =
    rewrite state name row
  in
  let ty =
    Option.value_or_thunk found ~default:(fun () ->
      Type.TVar (Fresh.var fresh ~rank))
  in
  let%bind.Result () =
    unify_presence'
      state
      ~presence1:Present
      ~presence2:presence
      ~error:
        (Unification_error.Row_mismatch
           ( RExtend (name, Present, assigned', ty, REmpty)
           , row
           , presence_mismatch_reason name ~found ))
  in
  if assigned
  then
    unify_presence'
      state
      ~presence1:Present
      ~presence2:assigned'
      ~error:
        (Unification_error.Row_mismatch
           (RExtend (name, Present, Present, ty, REmpty), row, Assigned name))
    |> Result.map ~f:(fun () -> ty)
  else Ok ty
;;
