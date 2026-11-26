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
  Type.Var.set_bounds
    var
    ((List.filter_opt presences
      |> List.dedup_and_sort ~compare:(fun (side1, var1) (side2, var2) ->
        [%compare: [ `At_most | `At_least ] * Type.Id.t]
          (side1, var1.Type.Var.id)
          (side2, var2.id))
      |> List.map ~f:(function
        | `At_most, other -> Type.At_most (PVar other)
        | `At_least, other -> At_least (PVar other)))
     @ equalities)
;;

module Presence_error = struct
  type t =
    | Conflict
    | Unification of Unification_error.t
  [@@deriving sexp_of]

  let unification error = Unification error
end

let on_conflict result ~error =
  Result.map_error result ~f:(function
    | Presence_error.Conflict -> error ()
    | Unification unification_error -> unification_error)
;;

(* Binds [var] to [target], running its watchers. If one fails, none of their
   changes are made, and [var] is bound to [failed] without them: what they
   would decide is never decided, nor what failed added later, so the error is
   reported once. Join edges settled by the binding are restored unsettled, and
   classes and edge lists merged apart. *)
let link (var : (_, _) Type.Var.t) target ~failed =
  Mode.Layout.attempt (fun () ->
    Type.Var.set_state var (Link target);
    Watcher.bind var.watchers target)
  |> Result.map_error ~f:(fun error ->
    Type.Var.set_state var (Link failed);
    error)
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
          Type.Var.set_in_bounds_of
            var
            (bounded
             :: List.filter var.in_bounds_of ~f:(fun bounded ->
               Option.is_some (unbound_id bounded))))
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
    Type.Var.lower_rank var ~rank;
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
            ~con:(Fresh.cons fresh).value_parameters
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
let bounded_id : Type.bounded -> Type.Id.t = function
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
     | Unbound _ -> Type.Var.set_state var (Link Type.error)
     | Generic | Link _ -> ());
    Ok ()
  in
  let ignore ~guarded:_ _ = Ok () in
  Type.iter_ty
    { ty = bind; row = ignore; presence = ignore; mode = (fun _ -> Ok ()) }
    ty
;;

(* [lower <= upper] between type variables not bound yet ([sub_ty]): the two are
   to have the same structure, with their modes related by variance, once either
   is bound. Kept on both, by a watcher which settles them once its variable is
   bound to a structure, relating each as [sub_ty] would have, or makes all its
   variable's class equal if none is bound when one is generalized. *)
module Sub_edge = struct
  type t =
    { lower : Type.ty
    ; upper : Type.ty
    ; mutable settled : bool
    }

  type edge = t
  type var = (Type.ty, Nothing.t) Type.Var.t

  (* The variables related by edges, transitively: binding one binds them all,
     to its structure, so a class never splits while its variables are unbound.
     [members] may also hold ones bound since. *)
  module Class = struct
    type t =
      { members : var list
      ; uppers : Int.Set.t Int.Map.t
      ; size : int
      }

    let create var = { members = [ var ]; uppers = Int.Map.empty; size = 1 }

    let mem_pair t ~lower ~upper =
      match Map.find t.uppers lower with
      | Some uppers -> Set.mem uppers upper
      | None -> false
    ;;

    let add_pair t ~lower ~upper =
      { t with
        uppers =
          Map.update t.uppers lower ~f:(function
            | Some uppers -> Set.add uppers upper
            | None -> Int.Set.singleton upper)
      }
    ;;

    let merge a b =
      let small, large = if a.size <= b.size then a, b else b, a in
      { members = List.rev_append small.members large.members
      ; uppers =
          Map.merge_skewed small.uppers large.uppers ~combine:(fun ~key:_ a b ->
            Set.union a b)
      ; size = a.size + b.size
      }
    ;;
  end

  (* The edges of the variables bound to one another, settled once one of them
     is bound to a structure. *)
  module Edges = struct
    type t =
      { edges : edge list
      ; count : int
      }

    let empty = { edges = []; count = 0 }

    let merge a b =
      let small, large = if a.count <= b.count then a, b else b, a in
      { edges = List.rev_append small.edges large.edges
      ; count = a.count + b.count
      }
    ;;
  end

  (* Merges are undone with an enclosing [atomically], as a link is: a link
     rolled back leaves its variables' classes and edges apart. *)
  module State = struct
    type t =
      { class_ : Class.t Undoable_union_find.t
      ; edges : Edges.t Undoable_union_find.t
      ; mutable join : bool
      }
  end

  let key : State.t Type_equal.Id.t =
    Type_equal.Id.create ~name:"sub_edges" sexp_of_opaque
  ;;

  let state (var : _ Type.Var.t) = Watcher.find var.watchers key

  let is_join var =
    match state var with
    | Some state -> state.join
    | None -> false
  ;;

  let same_class var other =
    match state var, state other with
    | Some state, Some other ->
      Undoable_union_find.same_class state.class_ other.class_
    | _ -> false
  ;;

  let union (state : State.t) (other : State.t) =
    Undoable_union_find.union state.class_ other.class_ ~f:Class.merge
  ;;

  (* [state]'s variable is bound to [other]'s: their edges are one's. *)
  let merge_edges (state : State.t) (other : State.t) =
    union state other;
    Undoable_union_find.union state.edges other.edges ~f:Edges.merge
  ;;

  let add (state : State.t) edge =
    let { Edges.edges; count } = Undoable_union_find.get state.edges in
    Undoable_union_find.set
      state.edges
      { edges = edge :: edges; count = count + 1 }
  ;;

  (* Drops the members of [var]'s class bound since they joined it. *)
  let prune (var : var) =
    Option.iter (state var) ~f:(fun state ->
      let class_ = Undoable_union_find.get state.class_ in
      let members =
        List.filter class_.members ~f:(fun member ->
          match member.state with
          | Unbound _ -> true
          | Generic | Link _ -> false)
      in
      Undoable_union_find.set
        state.class_
        { class_ with
          members
        ; size = class_.size - (List.length class_.members - List.length members)
        })
  ;;

  (* The unbound variables other than [var] whose structure is to be [var]'s. *)
  let related (var : var) =
    match state var with
    | None -> []
    | Some state ->
      List.filter
        (Undoable_union_find.get state.class_).members
        ~f:(fun member ->
          member.id <> var.id
          &&
          match member.state with
          | Unbound _ -> true
          | Generic | Link _ -> false)
  ;;

  (* Those of [ty]'s unbound variables that are [var] or to have its structure. *)
  let in_ (var : var) ty =
    let found = Int.Table.create () in
    let visit ~guarded:_ (other : var) =
      if (other.id = var.id || same_class var other)
         && not (Hashtbl.mem found other.id)
      then Hashtbl.set found ~key:other.id ~data:other;
      Ok ()
    in
    let ignore ~guarded:_ _ = Ok () in
    let iter : Nothing.t Type.Iter.t =
      { ty = visit; row = ignore; presence = ignore; mode = (fun _ -> Ok ()) }
    in
    match Type.iter_ty iter ty with
    | Ok () -> Hashtbl.data found
    | Error _ -> .
  ;;
end

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
          Mode_arg.equate mode1 mode2
          |> Result.map_error ~f:(fun error ->
            Unification_error.Type_mismatch (ty1, ty2, Modes error))
        | (ATy _ | ARow _ | AMode _), _ ->
          Error (Type_mismatch (ty1, ty2, Mismatch_arg_kind (arg1, arg2)))))
  | TArrow (arg1, res1, modes1), TArrow (arg2, res2, modes2) ->
    let%bind.Result () = unify_ty' state ~ty1:arg1 ~ty2:arg2 in
    let%bind.Result () = unify_ty' state ~ty1:res1 ~ty2:res2 in
    Arrow_modes.equate modes1 modes2
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
    let%bind.Result () = equate_related state var ty in
    let%bind.Result () =
      adjust_binding
        state.State.fresh
        (fun f -> Type.iter_ty f ty)
        ~id:var.id
        ~rank:(Type.Var.rank_exn var)
        ~in_bounds_of:var.in_bounds_of
        ~occurs:(ATy (TVar var))
        (ATy ty)
    in
    link var ty ~failed:Type.error
  | _ -> Error (Type_mismatch (ty1, ty2, Structural_mismatch))

(* What is to have [var]'s structure ([Sub_edge.related]) and is in [ty] is made
   equal to [var], as copying [ty]'s structure to it would not end: [var] is
   then in [ty], which the occurs check sees. *)
and equate_related state var ty =
  match Type.canonical_ty ty, Sub_edge.state var with
  | TVar _, _ | _, None -> Ok ()
  | _, Some _ ->
    Sub_edge.in_ var ty
    |> List.fold_result ~init:() ~f:(fun () (other : Sub_edge.var) ->
      if other.id = var.id
      then Ok ()
      else unify_ty' state ~ty1:(TVar other) ~ty2:(TVar var))

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
  let%bind.Result () = link var row ~failed:row in
  let%bind.Result () =
    if var.in_record then in_record' state row ~whole:row else Ok ()
  in
  let bounds = var.bounds in
  Type.Var.set_bounds var [];
  List.fold_result bounds ~init:() ~f:(fun () (bound : Type.bound) ->
    constrain_row' state ~row ~bound:bound.row ~shadowed:bound.shadowed)

and in_record' state row ~whole =
  match Type.canonical_row row with
  | REmpty -> Ok ()
  | RExtend (name, _, assigned, _, rest) ->
    let%bind.Result () =
      unify_presence' state ~presence1:assigned ~presence2:Type.Absent
      |> on_conflict ~error:(fun () ->
        Unification_error.Assigned_in_record (name, whole))
    in
    in_record' state rest ~whole
  | RVar var ->
    Type.Var.mark_in_record var;
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
      unify_presence' state ~presence1:p1 ~presence2:p2
      |> on_conflict ~error:(fun () ->
        Unification_error.Row_mismatch
          (row1, row2, presence_mismatch_reason x ~found:t2))
    in
    let%bind.Result () =
      unify_presence' state ~presence1:a1 ~presence2:a2
      |> on_conflict ~error:(fun () ->
        Unification_error.Row_mismatch (row1, row2, Assigned x))
    in
    let%bind.Result () =
      match t2 with
      | None -> Ok ()
      | Some t2 -> unify_if_present' state ~presence:p1 ~ty1:t1 ~ty2:t2
    in
    unify_row' state ~row1:rest1 ~row2:rest2
  | REmpty, RExtend (x, p2, _, _, rest2) ->
    let%bind.Result () =
      unify_presence' state ~presence1:Absent ~presence2:p2
      |> on_conflict ~error:(fun () ->
        Unification_error.Row_mismatch (row1, row2, Missing_label x))
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
          presence_at_most' state ~presence:assigned ~bound:bound_assigned
          |> on_conflict ~error:(fun () ->
            Unification_error.Row_mismatch (row, bound, Assigned name))
        in
        let%bind.Result () =
          presence_at_most' state ~presence ~bound:bound_presence
          |> on_conflict ~error:(fun () ->
            Unification_error.Row_mismatch
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
       Type.Var.add_bound var { row = bound; shadowed };
       Ok ())

(* Binding a variable passes its bounds on to what it is bound to. *)
and unify_presence' state ~presence1 ~presence2
  : (unit, Presence_error.t) Result.t
  =
  match
    Type.canonical_presence presence1, Type.canonical_presence presence2
  with
  | Present, Present | Absent, Absent -> Ok ()
  | Present, Absent | Absent, Present -> Error Conflict
  | PVar var1, PVar var2 when phys_equal var1 var2 -> Ok ()
  | PVar var, presence | presence, PVar var ->
    (match presence with
     | PVar var' ->
       Type.Var.lower_rank var' ~rank:(Type.Var.rank_exn var);
       add_in_bounds_of var' var.in_bounds_of
     | Present | Absent -> ());
    let%bind.Result () =
      link var presence ~failed:presence
      |> Result.map_error ~f:Presence_error.unification
    in
    let bounds = var.bounds in
    Type.Var.set_bounds var [];
    let%map.Result () =
      List.fold_result bounds ~init:() ~f:(fun () -> function
        | At_most bound -> presence_at_most' state ~presence ~bound
        | At_least lower ->
          presence_at_most' state ~presence:lower ~bound:presence
        | If_present (ty1, ty2) ->
          unify_if_present' state ~presence ~ty1 ~ty2
          |> Result.map_error ~f:Presence_error.unification)
    in
    (match Type.canonical_presence presence with
     | PVar var -> tidy var
     | Present | Absent -> ())

and presence_at_most' state ~presence ~bound =
  match Type.canonical_presence presence, Type.canonical_presence bound with
  | Absent, _ | _, Present -> Ok ()
  | Present, _ | _, Absent ->
    unify_presence' state ~presence1:presence ~presence2:bound
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
      Type.Var.add_bound lower (At_most (PVar upper));
      Type.Var.add_bound upper (At_least (PVar lower)));
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
      unify_presence' state ~presence1:presence ~presence2:Absent
      |> on_conflict ~error:(fun () ->
        Unification_error.Type_mismatch (ty1, ty2, Structural_mismatch))
      |> Result.map_error ~f:(fun error ->
        match unify_ty' state ~ty1 ~ty2 with
        | Ok () -> error
        | Error error -> error)
    | PVar var ->
      Type.Var.add_bound var (If_present (ty1, ty2));
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
    { links : Int.Hash_set.t Int.Table.t
    ; left : Type.ty Type.Link_pairs.t
    ; right : Type.ty Type.Link_pairs.t
    }

  let create () =
    { links = Int.Table.create ()
    ; left = Type.Link_pairs.create ()
    ; right = Type.Link_pairs.create ()
    }
  ;;

  (* Ordered, as [a <= b] and [b <= a] differ. *)
  let assume t id1 id2 =
    let uppers = Hashtbl.find_or_add t.links id1 ~default:Int.Hash_set.create in
    Hash_set.mem uppers id2
    ||
    (Hash_set.add uppers id2;
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

(* [ty]'s structure with a new variable of [rank] at each mode and type variable
   that [sub_ty] relates by variance, rather than equates: a type every value of
   [ty]'s structure may be used at, if its modes allow. [None] if [ty] has no
   such places. What is shared (through links, or a variable) is copied once, so
   a cycle's copy is a cycle. *)
let fresh_modes_copy fresh ~rank top =
  let ids = Fresh.modes fresh in
  let var () = Mode.Var.create ids ~rank in
  let in_progress = Int.Table.create () in
  let copies = Int.Table.create () in
  let variables = Int.Table.create () in
  (* [top] is shared like what a link points to, as a cycle may come back to it. *)
  let top_key = -1 in
  let rec copy (ty : Type.ty) : Type.ty option =
    match ty with
    | TVar { id; state = Link target; _ } ->
      shared ~key:(if phys_equal target top then top_key else id) target
    | TVar { id; state = Unbound _; _ } ->
      Some
        (Hashtbl.find_or_add variables id ~default:(fun () ->
           Type.TVar (Fresh.var fresh ~rank)))
    | TVar { state = Generic; _ } | TRecord _ | TVariant _ -> None
    | TCon (name, args) ->
      let variances =
        match (Fresh.cons fresh).variances name with
        | Some variances when List.length variances = List.length args ->
          variances
        | Some _ | None -> List.map args ~f:(fun _ -> Variance.Invariant)
      in
      let copied =
        List.map2_exn args variances ~f:(fun (arg : Type.arg) variance ->
          match arg, (variance : Variance.t) with
          | _, (Invariant | Bivariant) | ARow _, _ -> None
          | ATy ty, (Covariant | Contravariant) ->
            Option.map (copy ty) ~f:(fun ty -> Type.ATy ty)
          | AMode _, (Covariant | Contravariant) ->
            Some (Type.AMode (Var (var ()))))
      in
      if List.for_all copied ~f:Option.is_none
      then None
      else
        Some
          (TCon
             ( name
             , List.map2_exn args copied ~f:(fun arg copied ->
                 Option.value copied ~default:arg) ))
    | TArrow (arg, res, modes) ->
      Some
        (TArrow
           ( Option.value (copy arg) ~default:arg
           , Option.value (copy res) ~default:res
           , { modes with arg = Var (var ()); res = Var (var ()) } ))
    | TTuple tys ->
      let copied = List.map tys ~f:copy in
      if List.for_all copied ~f:Option.is_none
      then None
      else
        Some
          (TTuple
             (List.map2_exn tys copied ~f:(fun ty copied ->
                Option.value copied ~default:ty)))
    | TCode { inner; context } ->
      Option.map (copy inner) ~f:(fun inner -> Type.TCode { inner; context })
  (* Copied once, behind a link of its own, as what a link points to is: a cycle
     back to it while it is copied is to that link. *)
  and shared ~key target =
    match Hashtbl.find copies key, Hashtbl.find in_progress key with
    | Some copied, _ -> copied
    | None, Some (link, used) ->
      used := true;
      Some (Type.TVar link)
    | None, None ->
      let link = Fresh.var fresh ~rank in
      let used = ref false in
      Hashtbl.set in_progress ~key ~data:(link, used);
      let copied =
        match copy target with
        | Some copied ->
          Type.Var.set_state link (Link copied);
          Some (Type.TVar link)
        | None when !used ->
          raise_s
            [%message
              "Unify: a cycle's copy is used, but has nothing to relate"]
        | None -> None
      in
      Hashtbl.remove in_progress key;
      Hashtbl.set copies ~key ~data:copied;
      copied
  in
  match top with
  | _ when Type.is_error top -> None
  | TVar { state = Link _; _ } -> copy top
  | _ -> shared ~key:top_key top
;;

(* What is related has the same structure, so settling or fixing a join fails by
   modes, reported as what binding the variable needs. *)
let as_bound
  : (unit, Unification_error.t) Result.t -> (unit, Unification_error.t) Result.t
  = function
  | Error (Type_mismatch (_, _, Modes error)) -> Error (Bound_var error)
  | result -> result
;;

(* Which of [lower] and [upper] errors name as the expected type: [Upper], but
   in a contravariant position. *)
module Expected = struct
  type t =
    | Lower
    | Upper

  let flip = function
    | Lower -> Upper
    | Upper -> Lower
  ;;
end

(* Relates by variance even while a side is unknown if [join], or either side is
   a join variable ([join_var]): then within it too. *)
let rec sub_ty' state sub ~join ~expected ~lower ~upper
  : (unit, Unification_error.t) Result.t
  =
  let rec is_join : Type.ty -> bool = function
    | TVar ({ state = Link ty; _ } as var) -> Sub_edge.is_join var || is_join ty
    | TVar var -> Sub_edge.is_join var
    | _ -> false
  in
  let join = join || is_join lower || is_join upper in
  match lower, upper with
  | ( Type.TVar { id = id1; state = Link _; _ }
    , Type.TVar { id = id2; state = Link _; _ } )
    when Sub_state.assume sub id1 id2 -> Ok ()
  | Type.TVar { state = Link _; _ }, Type.TVar { state = Link _; _ } ->
    sub_structure state sub ~join ~expected ~lower ~upper
  | _ when Sub_state.seen sub lower upper -> Ok ()
  | _ -> sub_structure state sub ~join ~expected ~lower ~upper

and sub_structure state sub ~join ~expected:side ~lower ~upper =
  let ty1 = Type.canonical_ty lower in
  let ty2 = Type.canonical_ty upper in
  let expected, actual =
    match side with
    | Expected.Lower -> ty1, ty2
    | Upper -> ty2, ty1
  in
  let modes_error error = modes_error expected actual error in
  let co ~lower ~upper = sub_ty' state sub ~join ~expected:side ~lower ~upper in
  let contra ~lower ~upper =
    sub_ty' state sub ~join ~expected:(Expected.flip side) ~lower ~upper
  in
  (* Exact unification, naming the expected type first. *)
  let unify_ty ~lower ~upper =
    match side with
    | Lower -> unify_ty' state ~ty1:lower ~ty2:upper
    | Upper -> unify_ty' state ~ty1:upper ~ty2:lower
  in
  let mode_arg (variance : Variance.t) mode1 mode2 =
    (match variance with
     | Covariant -> Mode_arg.sub ~lower:mode1 ~upper:mode2
     | Contravariant -> Mode_arg.sub ~lower:mode2 ~upper:mode1
     | Invariant | Bivariant ->
       (match side with
        | Lower -> Mode_arg.equate mode1 mode2
        | Upper -> Mode_arg.equate mode2 mode1))
    |> Result.map_error ~f:modes_error
  in
  (* [var], not bound yet, takes [ty]'s structure with modes and variables of
     its own, which [ty]'s are related to by variance: [ty]'s structure equal,
     unless it has nothing to relate, or has [var]'s structure in it. *)
  let bind_copy (var : (Type.ty, Nothing.t) Type.Var.t) ty =
    let has_var = not (List.is_empty (Sub_edge.in_ var ty)) in
    match
      if has_var
      then None
      else fresh_modes_copy state.fresh ~rank:(Type.Var.rank_exn var) ty
    with
    | None -> unify_ty ~lower:ty1 ~upper:ty2
    | Some copy ->
      let%bind.Result () = unify_ty' state ~ty1:(TVar var) ~ty2:copy in
      co ~lower:ty1 ~upper:ty2
  in
  match ty1, ty2 with
  | _ when phys_equal ty1 ty2 -> Ok ()
  | TVar var1, TVar var2 when phys_equal var1 var2 -> Ok ()
  | ( TVar ({ state = Unbound _; _ } as var1)
    , TVar ({ state = Unbound _; _ } as var2) )
    when join -> Ok (add_edge state.fresh var1 var2)
  | TVar _, TVar _ -> unify_ty ~lower:ty1 ~upper:ty2
  | TVar ({ state = Unbound _; _ } as var), _ when join -> bind_copy var ty2
  | _, TVar ({ state = Unbound _; _ } as var) when join -> bind_copy var ty1
  | TCon (name1, args1), TCon (name2, args2)
    when String.equal name1 name2 && List.length args1 = List.length args2 ->
    let%bind.Result known, variances =
      match (Fresh.cons state.fresh).variances name1 with
      | Some variances when List.length variances = List.length args1 ->
        Ok (true, variances)
      | Some variances ->
        Error
          (Unification_error.Type_mismatch
             ( expected
             , actual
             , Args_length_mismatch (List.length variances, List.length args1)
             ))
      | None -> Ok (false, List.map args1 ~f:(fun _ -> Variance.Invariant))
    in
    let unknown_variances : Unification_error.t -> Unification_error.t
      = function
      | Type_mismatch (_, _, Modes error) when not known ->
        Type_mismatch (expected, actual, Unknown_variances (name1, error))
      | error -> error
    in
    Result.map_error ~f:unknown_variances
    @@ iter2_result
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
    Arrow_modes.sub ~lower:modes1 ~upper:modes2
    |> Result.map_error ~f:modes_error
  | TTuple tys1, TTuple tys2 when List.length tys1 = List.length tys2 ->
    iter2_all tys1 tys2 ~f:(fun lower upper -> co ~lower ~upper)
  | ( TCode { inner = inner1; context = row1 }
    , TCode { inner = inner2; context = row2 } ) ->
    let%bind.Result () = co ~lower:inner1 ~upper:inner2 in
    (match side with
     | Lower -> unify_row' state ~row1 ~row2
     | Upper -> unify_row' state ~row1:row2 ~row2:row1)
  | _ -> unify_ty ~lower:ty1 ~upper:ty2

(* [lower <= upper], kept on both until either is bound to a structure, then
   settled as [sub_ty]. *)
and add_edge fresh (lower : Sub_edge.var) (upper : Sub_edge.var) =
  let (lower_state : Sub_edge.State.t) = edges fresh ~join:false lower in
  let (upper_state : Sub_edge.State.t) = edges fresh ~join:false upper in
  Sub_edge.union lower_state upper_state;
  let class_ = Undoable_union_find.get lower_state.class_ in
  if not (Sub_edge.Class.mem_pair class_ ~lower:lower.id ~upper:upper.id)
  then (
    Undoable_union_find.set
      lower_state.class_
      (Sub_edge.Class.add_pair class_ ~lower:lower.id ~upper:upper.id);
    let edge =
      { Sub_edge.lower = TVar lower; upper = TVar upper; settled = false }
    in
    Sub_edge.add lower_state edge;
    Sub_edge.add upper_state edge)

(* [var]'s state, given the watcher keeping it if it has none yet: a join
   variable if [join]. Its edges are settled once [var] is bound to a structure,
   and [var] is [fix]ed if generalized while still unbound. Bound to another
   unbound variable, [var]'s edges become that one's. *)
and edges fresh ~join (var : Sub_edge.var) =
  match Sub_edge.state var with
  | Some state -> state
  | None ->
    let state =
      { Sub_edge.State.class_ =
          Undoable_union_find.create (Sub_edge.Class.create var)
      ; edges = Undoable_union_find.create Sub_edge.Edges.empty
      ; join
      }
    in
    Type.Var.set_watchers
      var
      (Watcher.create
         Sub_edge.key
         state
         ~bind:(fun state ty ->
           match Type.canonical_ty ty with
           | TVar ({ state = Unbound _; _ } as other) ->
             let other_state = edges fresh ~join:state.join other in
             if state.join && not other_state.join
             then
               Mode.Private.Undo.change
                 ~undo:(fun () -> other_state.join <- false)
                 (fun () -> other_state.join <- true);
             Sub_edge.merge_edges state other_state;
             Ok ()
           | _ -> Fresh.settle fresh (fun () -> settle fresh state))
         ~instantiate:(fun (_ : Sub_edge.State.t) ~at:_ -> None)
         ~generalize:(fun (_ : Sub_edge.State.t) -> fix fresh var)
       :: var.watchers);
    state

(* Each edge is settled once: it is in the lists of both its sides, which may be
   one list once merged, and [settled] skips it the second time. All are settled
   even after an error, so each adds what it can, and the first error is the
   bind's. *)
and settle fresh (state : Sub_edge.State.t) =
  let edges = List.rev (Undoable_union_find.get state.edges).edges in
  Undoable_union_find.set state.edges Sub_edge.Edges.empty;
  List.fold edges ~init:(Ok ()) ~f:(fun result (edge : Sub_edge.t) ->
    if edge.settled
    then result
    else (
      Mode.Private.Undo.change
        ~undo:(fun () -> edge.settled <- false)
        (fun () -> edge.settled <- true);
      let settled =
        sub_ty'
          (State.create fresh)
          (Sub_state.create ())
          ~join:true
          ~expected:Upper
          ~lower:edge.lower
          ~upper:edge.upper
        |> as_bound
      in
      Result.combine result settled ~ok:Fn.const ~err:Fn.const))

(* All that is to have [var]'s structure is made equal to it: as no use has told
   them apart, one type variable stands for them, as [unify_ty] would have made
   it. *)
and fix fresh var =
  Sub_edge.prune var;
  List.fold (Sub_edge.related var) ~init:(Ok ()) ~f:(fun result other ->
    unify_ty' (State.create fresh) ~ty1:(TVar other) ~ty2:(TVar var)
    |> as_bound
    |> Result.combine result ~ok:Fn.const ~err:Fn.const)
;;

let join_var fresh ~rank =
  let var = Fresh.var fresh ~rank in
  ignore (edges fresh ~join:true var : Sub_edge.State.t);
  var
;;

let fix_join fresh var =
  match var.Type.Var.state, Sub_edge.state var with
  | Unbound _, Some _ -> fix fresh var
  | (Unbound _ | Generic | Link _), _ -> Ok ()
;;

let sub_ty fresh ~lower ~upper =
  sub_ty'
    (State.create fresh)
    (Sub_state.create ())
    ~join:false
    ~expected:Upper
    ~lower
    ~upper
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
  unify_presence' (State.create fresh) ~presence1:presence ~presence2:Absent
  |> Result.map_error ~f:(fun (_ : Presence_error.t) -> error)
;;

let presence_at_most fresh ~presence ~bound =
  presence_at_most' (State.create fresh) ~presence ~bound
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
    unify_presence' state ~presence1:Present ~presence2:presence
    |> on_conflict ~error:(fun () ->
      Unification_error.Row_mismatch
        ( RExtend (name, Present, assigned', ty, REmpty)
        , row
        , presence_mismatch_reason name ~found ))
  in
  if assigned
  then
    unify_presence' state ~presence1:Present ~presence2:assigned'
    |> on_conflict ~error:(fun () ->
      Unification_error.Row_mismatch
        (RExtend (name, Present, Present, ty, REmpty), row, Assigned name))
    |> Result.map ~f:(fun () -> ty)
  else Ok ty
;;
