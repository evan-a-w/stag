open! Import

module Presence_var = struct
  type t = (Type.presence, Type.presence_bound) Type.Var.t
end

module Row_var = struct
  type t = (Type.row, Type.bound) Type.Var.t
end

(* What instances need once the whole type is copied. *)
module Pending = struct
  type t =
    | Presence_bounds of Presence_var.t * Presence_var.t
    | Row_bounds of Row_var.t * Row_var.t
    | Kept of Type.kept
end

let ok_exn result ~what =
  match result with
  | Ok () -> ()
  | Error error ->
    raise_s [%message "Scheme: bound failed" what (error : Unification_error.t)]
;;

let presence_at_most_exn fresh ~presence ~bound =
  Unify.presence_at_most
    fresh
    ~presence
    ~bound
    ~error:
      (Row_mismatch
         ( RExtend ("", presence, Absent, Type.unit, REmpty)
         , RExtend ("", bound, Absent, Type.unit, REmpty)
         , Presence_mismatch "" ))
  |> ok_exn ~what:"presence"
;;

let unify_if_present_exn fresh ~presence ~ty1 ~ty2 =
  Unify.unify_if_present fresh ~presence ~ty1 ~ty2 |> ok_exn ~what:"equality"
;;

let constrain_row_exn fresh ~row ~(bound : Type.bound) =
  Unify.constrain_row fresh ~row ~bound:bound.row ~shadowed:bound.shadowed
  |> ok_exn ~what:"row"
;;

let enqueue_kept ~pending ~seen_kept kept =
  if Hash_set.strict_add seen_kept (Type.kept_id kept) |> Result.is_ok
  then Queue.enqueue pending (Pending.Kept kept)
;;

let instantiator fresh ~rank ~(pending : Pending.t Queue.t) ~seen_kept
  : Type.Mapper.t
  =
  let tys = Int.Table.create () in
  let rows = Int.Table.create () in
  let presences = Int.Table.create () in
  let inst table (var : _ Type.Var.t) ~wrap ~bounds =
    match var.state with
    | Unbound _ | Link _ -> wrap var
    | Generic ->
      (match Hashtbl.find table var.id with
       | Some instance -> instance
       | None ->
         let var' = Fresh.var fresh ~rank in
         let instance = wrap var' in
         Hashtbl.set table ~key:var.id ~data:instance;
         bounds var var';
         List.iter var.kept ~f:(enqueue_kept ~pending ~seen_kept);
         instance)
  in
  { ty =
      (fun var ->
        inst tys var ~wrap:(fun var -> Type.TVar var) ~bounds:(fun _ _ -> ()))
  ; row =
      (fun var ->
        inst
          rows
          var
          ~wrap:(fun var -> Type.RVar var)
          ~bounds:(fun var var' ->
            Queue.enqueue pending (Row_bounds (var, var'))))
  ; presence =
      (fun var ->
        inst
          presences
          var
          ~wrap:(fun var -> Type.PVar var)
          ~bounds:(fun var var' ->
            Queue.enqueue pending (Presence_bounds (var, var'))))
  ; keeps =
      (fun var ->
        match var.state with
        | Generic -> false
        | Unbound _ | Link _ -> true)
  ; id = (fun () -> Fresh.id fresh)
  }
;;

(* Bounds are added as constraints, so that a variable that is not generalized
   gets the reverse presence bound, or the row bound's entries, and propagates
   what is known. Generalization keeps the bounds that held through the
   generalized variables, so this succeeds. *)
let rec add_instance_bounds fresh (mapper : Type.Mapper.t) pending =
  match Queue.dequeue pending with
  | None -> ()
  | Some (Pending.Presence_bounds (var, instance)) ->
    let instantiate presence =
      match Type.canonical_presence presence with
      | PVar var -> mapper.presence var
      | (Present | Absent) as presence -> presence
    in
    List.iter var.bounds ~f:(function
      | At_most bound ->
        presence_at_most_exn
          fresh
          ~presence:(PVar instance)
          ~bound:(instantiate bound)
      | At_least lower ->
        presence_at_most_exn
          fresh
          ~presence:(instantiate lower)
          ~bound:(PVar instance)
      | If_present (ty1, ty2) ->
        unify_if_present_exn
          fresh
          ~presence:(PVar instance)
          ~ty1:(Type.map_ty mapper ty1)
          ~ty2:(Type.map_ty mapper ty2));
    add_instance_bounds fresh mapper pending
  | Some (Row_bounds (var, instance)) ->
    List.iter (List.rev var.bounds) ~f:(fun bound ->
      constrain_row_exn
        fresh
        ~row:(RVar instance)
        ~bound:{ bound with row = Type.map_row mapper bound.row });
    add_instance_bounds fresh mapper pending
  | Some (Kept (Row_bound { lower; bound; _ })) ->
    constrain_row_exn
      fresh
      ~row:(RVar lower)
      ~bound:{ bound with row = Type.map_row mapper bound.row };
    add_instance_bounds fresh mapper pending
  | Some (Kept (If_present_kept { presence; ty1; ty2; _ })) ->
    unify_if_present_exn
      fresh
      ~presence:(PVar presence)
      ~ty1:(Type.map_ty mapper ty1)
      ~ty2:(Type.map_ty mapper ty2);
    add_instance_bounds fresh mapper pending
;;

let instantiate fresh ~rank =
  let pending = Queue.create () in
  let mapper =
    instantiator fresh ~rank ~pending ~seen_kept:(Int.Hash_set.create ())
  in
  fun ty ->
    let ty = Type.map_ty mapper ty in
    add_instance_bounds fresh mapper pending;
    ty
;;

let newly_generalized ~rank (var : _ Type.Var.t) =
  match var.state with
  | Unbound rank' when rank' > rank ->
    var.state <- Generic;
    true
  | Unbound _ | Generic | Link _ -> false
;;

let uppers (var : Presence_var.t) =
  List.filter_map var.bounds ~f:(function
    | At_most _ as bound -> Type.presence_bound_var bound
    | At_least _ | If_present _ -> None)
;;

let lowers (var : Presence_var.t) =
  List.filter_map var.bounds ~f:(function
    | At_least _ as bound -> Type.presence_bound_var bound
    | At_most _ | If_present _ -> None)
;;

let has_equalities (var : Presence_var.t) =
  List.exists var.bounds ~f:(function
    | If_present _ -> true
    | At_most _ | At_least _ -> false)
;;

let outside (var : Presence_var.t) =
  match var.state with
  | Generic -> false
  | Unbound _ | Link _ -> true
;;

(* Bounds are not transitively closed, so a bound [l <= u] between variables
   that are not generalized may only hold through generalized ones, which
   leave the bounds of the others, as instances get their own. Returns the
   generalized variables that may become [Present] through a variable that is
   not generalized, and then have equalities, which only an instance can
   impose on the others. *)
let close_presence_bounds fresh (generalized : Presence_var.t list) =
  let ids =
    Int.Hash_set.of_list (List.map generalized ~f:(fun var -> var.id))
  in
  let is_generalized (var : Presence_var.t) = Hash_set.mem ids var.id in
  let above (var : Presence_var.t) =
    let visited = Int.Hash_set.create () in
    let equalities = ref (has_equalities var) in
    let rec go acc var =
      List.fold (uppers var) ~init:acc ~f:(fun acc upper ->
        if outside upper
        then upper :: acc
        else if is_generalized upper && not (Hash_set.mem visited upper.id)
        then (
          Hash_set.add visited upper.id;
          if has_equalities upper then equalities := true;
          go acc upper)
        else acc)
    in
    Hash_set.add visited var.id;
    let above = go [] var in
    above, !equalities
  in
  let implied, with_equalities =
    List.filter_map generalized ~f:(fun var ->
      match List.filter (lowers var) ~f:outside with
      | [] -> None
      | lowers ->
        let above, equalities = above var in
        Some (List.cartesian_product lowers above, Option.some_if equalities var))
    |> List.unzip
  in
  List.iter generalized ~f:(fun var ->
    List.iter
      (uppers var @ lowers var)
      ~f:(fun (other : Presence_var.t) ->
        if outside other
        then
          other.bounds
          <- List.filter other.bounds ~f:(fun bound ->
               match Type.presence_bound_var bound with
               | Some var -> not (is_generalized var)
               | None -> true)));
  List.iter (List.concat implied) ~f:(fun (lower, upper) ->
    presence_at_most_exn fresh ~presence:(PVar lower) ~bound:(PVar upper));
  List.filter_opt with_equalities
;;

let lower ~rank (var : _ Type.Var.t) =
  match var.state with
  | Unbound rank' when rank' > rank -> var.state <- Unbound rank
  | Unbound _ | Generic | Link _ -> ()
;;

(* The bounds in [bounds] with generalized variables are kept, their other
   variables being lowered as they are not generalized with them. An earlier
   generalization kept the bounds of a variable that may be used by its
   generalized variables, and one that may not be used stays so, as only
   constraints on variables that are used reach others. So the generalized
   variables are those of this generalization. *)
let keep_with_generalized fresh ~rank ~args ~kept bounds =
  let iter bound iter =
    List.fold_result (args bound) ~init:() ~f:(fun () -> function
      | Type.ATy ty -> Type.iter_ty iter ty
      | ARow row -> Type.iter_row iter row)
  in
  let has_generalized ~guarded:_ (var : _ Type.Var.t) =
    match var.state with
    | Generic -> Error ()
    | Unbound _ | Link _ -> Ok ()
  in
  let lowerer ~guarded:_ var : (unit, Nothing.t) Result.t =
    lower ~rank var;
    Ok ()
  in
  List.partition_map bounds ~f:(fun bound ->
    match
      iter
        bound
        { Type.Iter.ty = has_generalized
        ; row = has_generalized
        ; presence = has_generalized
        }
    with
    | Ok () -> Second bound
    | Error () ->
      (match iter bound { ty = lowerer; row = lowerer; presence = lowerer } with
       | Ok () -> ()
       | Error _ -> .);
      First (kept bound ~id:(Fresh.id fresh)))
;;

let iter_kept (kept : Type.kept) iter =
  match kept with
  | Row_bound { bound; _ } -> Type.iter_row iter bound.row
  | If_present_kept { ty1; ty2; _ } ->
    let%bind.Result () = Type.iter_ty iter ty1 in
    Type.iter_ty iter ty2
;;

(* Each generalized variable of a kept bound keeps it, so that an instance of
   any of them adds it. This is done once the scheme is simplified, which may
   replace generalized variables. *)
let attach_kept ~is_generalized kept =
  List.iter kept ~f:(fun kept ->
    let attach ~guarded:_ (var : _ Type.Var.t) : (unit, Nothing.t) Result.t =
      (match var.state with
       | Generic ->
         if not (is_generalized var.id)
         then
           raise_s
             [%message "Scheme: bound with an earlier generalized variable"];
         (match var.kept with
          | last :: _ when phys_equal last kept -> ()
          | _ -> var.kept <- kept :: var.kept)
       | Unbound _ | Link _ -> ());
      Ok ()
    in
    match iter_kept kept { ty = attach; row = attach; presence = attach } with
    | Ok () -> ()
    | Error _ -> .)
;;

let keep_row_bounds fresh (var : Row_var.t) ~rank =
  let kept, bounds =
    keep_with_generalized
      fresh
      ~rank
      var.bounds
      ~args:(fun (bound : Type.bound) -> [ Type.ARow bound.row ])
      ~kept:(fun bound ~id -> Type.Row_bound { id; lower = var; bound })
  in
  var.bounds <- bounds;
  if not (List.is_empty kept) then lower ~rank var;
  kept
;;

let keep_equalities fresh (var : Presence_var.t) ~rank =
  let kept, bounds =
    keep_with_generalized
      fresh
      ~rank
      var.bounds
      ~args:(function
        | Type.If_present (ty1, ty2) -> [ Type.ATy ty1; ATy ty2 ]
        | At_most _ | At_least _ -> [])
      ~kept:(fun bound ~id ->
        match bound with
        | If_present (ty1, ty2) ->
          Type.If_present_kept { id; presence = var; ty1; ty2 }
        | At_most _ | At_least _ -> assert false)
  in
  var.bounds <- bounds;
  if not (List.is_empty kept) then lower ~rank var;
  kept
;;

(* Whether a variable may be reached once the variables of rank greater than
   [rank] that are not generalized are out of scope: reaching them from
   outside goes through bounds, as variables in a type have at most its rank.
   Answers are remembered: a variable a search reaches outside from may be
   used, and all those a failed search visits can't be. *)
let may_be_used ~rank =
  let known = Int.Table.create () in
  let visited = Int.Hash_set.create () in
  fun bounded ->
    let rec go (bounded : Type.bounded) =
      let visit (var : _ Type.Var.t) ~next =
        match var.state with
        | Unbound rank' when rank' <= rank -> true
        | Unbound _ ->
          (match Hashtbl.find known var.id with
           | Some used -> used
           | None ->
             Hash_set.strict_add visited var.id |> Result.is_ok
             && (List.exists var.in_bounds_of ~f:go || next ())
             &&
             (Hashtbl.set known ~key:var.id ~data:true;
              true))
        | Generic | Link _ -> false
      in
      match bounded with
      | Row_var var -> visit var ~next:(fun () -> false)
      | Presence_var var ->
        visit var ~next:(fun () ->
          List.exists
            (uppers var @ lowers var)
            ~f:(fun var -> go (Presence_var var)))
    in
    Hash_set.clear visited;
    let used = go bounded in
    if not used
    then
      Hash_set.iter visited ~f:(fun id -> Hashtbl.set known ~key:id ~data:false);
    used
;;

(* Whether the witness of a kept bound [l <= {..g}] can be left out: if [g]
   has no bounds and nothing else in the witness has it, an instance of [g]
   can be [l]. *)
let trivial_witnesses ~kept ~(with_equalities : Presence_var.t list) =
  let occurrences = Int.Table.create () in
  let rec counter : Nothing.t Type.Iter.t =
    let count (var : _ Type.Var.t) ~bounds =
      match
        Hashtbl.incr occurrences var.id;
        var.state
      with
      | Generic when Hashtbl.find_exn occurrences var.id = 1 -> bounds var
      | Generic | Unbound _ | Link _ -> Ok ()
    in
    { ty = (fun ~guarded:_ var -> count var ~bounds:(fun _ -> Ok ()))
    ; row =
        (fun ~guarded:_ var ->
          count var ~bounds:(fun (var : Row_var.t) ->
            List.fold_result var.bounds ~init:() ~f:(fun () bound ->
              Type.iter_row counter bound.row)))
    ; presence = (fun ~guarded:_ var -> count var ~bounds:presence_bounds)
    }
  and presence_bounds (var : Presence_var.t) =
    List.fold_result var.bounds ~init:() ~f:(fun () -> function
      | (At_most _ | At_least _) as bound ->
        (match Type.presence_bound_var bound with
         | Some other -> counter.presence ~guarded:false other
         | None -> Ok ())
      | If_present (ty1, ty2) ->
        let%bind.Result () = Type.iter_ty counter ty1 in
        Type.iter_ty counter ty2)
  in
  (match
     List.fold_result kept ~init:() ~f:(fun () -> function
       | Type.Row_bound { bound; _ } -> Type.iter_row counter bound.row
       | If_present_kept { ty1; ty2; _ } ->
         let%bind.Result () = Type.iter_ty counter ty1 in
         Type.iter_ty counter ty2)
     |> Result.bind ~f:(fun () ->
       List.fold_result with_equalities ~init:() ~f:(fun () var ->
         counter.presence ~guarded:false var))
   with
   | Ok () -> ()
   | Error _ -> .);
  fun (bound : Type.bound) ->
    match Type.canonical_row bound.row with
    | RVar ({ state = Generic; bounds = []; _ } as var) ->
      [%equal: int option] (Hashtbl.find occurrences var.id) (Some 1)
    | RVar _ | REmpty | RExtend _ -> false
;;

(* A row within a bound without entries has only entries the bound shadows,
   so is within every bound shadowing them too. *)
(* Whether the outer bounds already imply an instance of [kept]: an
   assignment of its generalized variables makes each kept bound one of the
   outer bounds, and the bounds of the variables assigned hold, so the
   variables of the instance, used nowhere else, can be chosen as such. Only
   bounds that are the same once assigned are recognized. *)
let outside_witnessed ~kept =
  let tys = Int.Table.create () in
  let rows = Int.Table.create () in
  let presences = Int.Table.create () in
  let assign table (var : _ Type.Var.t) witness ~same =
    match Hashtbl.find table var.id with
    | Some (_, assigned) -> same assigned witness
    | None ->
      Hashtbl.set table ~key:var.id ~data:(var, witness);
      true
  in
  let generic_free ty =
    match
      Type.iter_ty
        (let check ~guarded:_ (var : _ Type.Var.t) =
           match var.state with
           | Generic -> Error ()
           | Unbound _ | Link _ -> Ok ()
         in
         { ty = check; row = check; presence = check })
        ty
    with
    | Ok () -> true
    | Error () -> false
  in
  let match_ty ty ty' =
    match Type.canonical_ty ty with
    | TVar ({ state = Generic; _ } as var) ->
      assign tys var ty' ~same:Type.same_ty
    | ty -> generic_free ty && Type.same_ty ty ty'
  in
  let match_presence presence presence' =
    match Type.canonical_presence presence with
    | PVar ({ state = Generic; _ } as var) ->
      assign presences var presence' ~same:Type.same_presence
    | presence -> Type.same_presence presence presence'
  in
  let rec match_row row row' =
    match Type.canonical_row row, Type.canonical_row row' with
    | ( RExtend (label, presence, assigned, ty, rest)
      , RExtend (label', presence', assigned', ty', rest') ) ->
      String.equal label label'
      && match_presence presence presence'
      && match_presence assigned assigned'
      && match_ty ty ty'
      && match_row rest rest'
    | RVar ({ state = Generic; bounds = []; _ } as var), row' ->
      assign rows var row' ~same:Type.same_row
    | RExtend _, _ -> false
    | ((RVar _ | REmpty) as row), row' -> Type.same_row row row'
  in
  let instance_ty ty =
    match Type.canonical_ty ty with
    | TVar ({ state = Generic; _ } as var) ->
      Option.map (Hashtbl.find tys var.id) ~f:snd
    | ty -> Option.some_if (generic_free ty) ty
  in
  let instance_presence presence =
    match Type.canonical_presence presence with
    | PVar ({ state = Generic; _ } as var) ->
      Option.map (Hashtbl.find presences var.id) ~f:snd
    | presence -> Some presence
  in
  let has_bound presence ~f =
    match Type.canonical_presence presence with
    | PVar var -> List.exists var.bounds ~f
    | Present | Absent -> false
  in
  let holds lower upper =
    match Type.canonical_presence lower, Type.canonical_presence upper with
    | Absent, _ | _, Present -> true
    | lower, upper ->
      Type.same_presence lower upper
      || has_bound lower ~f:(function
        | At_most other -> Type.same_presence other upper
        | At_least _ | If_present _ -> false)
      || has_bound upper ~f:(function
        | At_least other -> Type.same_presence other lower
        | At_most _ | If_present _ -> false)
  in
  let has_equality presence ty1 ty2 =
    Type.same_ty ty1 ty2
    || has_bound presence ~f:(function
      | If_present (ty1', ty2') ->
        (Type.same_ty ty1 ty1' && Type.same_ty ty2 ty2')
        || (Type.same_ty ty1 ty2' && Type.same_ty ty2 ty1')
      | At_most _ | At_least _ -> false)
  in
  let row_bounds_match =
    List.for_all kept ~f:(function
      | Type.Row_bound { lower; bound; _ } ->
        List.exists lower.bounds ~f:(fun (bound' : Type.bound) ->
          Set.equal bound.shadowed bound'.shadowed
          &&
          let snapshot table = Hashtbl.copy table in
          let tys' = snapshot tys
          and rows' = snapshot rows
          and presences' = snapshot presences in
          match_row bound.row bound'.row
          ||
          let restore table saved =
            Hashtbl.clear table;
            Hashtbl.iteri saved ~f:(fun ~key ~data ->
              Hashtbl.set table ~key ~data)
          in
          restore tys tys';
          restore rows rows';
          restore presences presences';
          false)
      | If_present_kept _ -> true)
  in
  row_bounds_match
  && List.for_all kept ~f:(function
    | Type.Row_bound _ -> true
    | If_present_kept { presence; ty1; ty2; _ } ->
      (match instance_ty ty1, instance_ty ty2 with
       | Some ty1, Some ty2 -> has_equality (PVar presence) ty1 ty2
       | None, _ | _, None -> false))
  && Hashtbl.for_all presences ~f:(fun ((var : Presence_var.t), witness) ->
    List.for_all var.bounds ~f:(function
      | At_most upper ->
        Option.value_map
          (instance_presence upper)
          ~default:false
          ~f:(fun upper -> holds witness upper)
      | At_least lower ->
        Option.value_map
          (instance_presence lower)
          ~default:false
          ~f:(fun lower -> holds lower witness)
      | If_present (ty1, ty2) ->
        (match instance_ty ty1, instance_ty ty2 with
         | Some ty1, Some ty2 -> has_equality witness ty1 ty2
         | None, _ | _, None -> false)))
;;

let drop_subsumed_row_bounds (var : Row_var.t) =
  let rec empty row =
    match Type.canonical_row row with
    | REmpty -> true
    | RExtend (_, presence, _, _, rest) ->
      (match Type.canonical_presence presence with
       | Absent -> empty rest
       | Present | PVar _ -> false)
    | RVar _ -> false
  in
  match
    List.find var.bounds ~f:(fun (bound : Type.bound) -> empty bound.row)
  with
  | Some empty ->
    var.bounds
    <- List.filter var.bounds ~f:(fun bound ->
         phys_equal bound empty
         || not (Set.is_subset empty.shadowed ~of_:bound.shadowed))
  | None -> ()
;;

(* Calls [f] on the variables in [tys], [kept], and the bounds of [rows] and
   [presences] that [except_row] and [except_presence] don't accept, for each
   occurrence that is not reached again through a [Link], record or variant,
   and [link] on each occurrence of a [Link]. *)
let iter_used_vars
      ?(link = fun (_ : Type.id) -> ())
      ?(except_row = fun (_ : Type.bound) -> false)
      ?(except_presence = fun (_ : Type.presence_bound) -> false)
      ~tys
      ~(presences : Presence_var.t list)
      ~(rows : Row_var.t list)
      ~kept
      ~f
      ()
  =
  let nodes = Int.Hash_set.create () in
  let links = Int.Hash_set.create () in
  let rec iter_ty (ty : Type.ty) =
    match ty with
    | TVar { state = Link target; id; _ } ->
      link id;
      if Hash_set.strict_add links id |> Result.is_ok then iter_ty target
    | TVar var -> f var.id
    | TCon (_, args) ->
      List.iter args ~f:(function
        | ATy ty -> iter_ty ty
        | ARow row -> iter_row row)
    | TArrow (ty1, ty2) ->
      iter_ty ty1;
      iter_ty ty2
    | TTuple tys -> List.iter tys ~f:iter_ty
    | TCode { inner; context } ->
      iter_ty inner;
      iter_row context
    | TRecord { id; row } | TVariant { id; row } ->
      if Hash_set.strict_add nodes id |> Result.is_ok then iter_row row
  and iter_row (row : Type.row) =
    match row with
    | RVar { state = Link target; id; _ } ->
      link id;
      if Hash_set.strict_add links id |> Result.is_ok then iter_row target
    | RVar var -> f var.id
    | REmpty -> ()
    | RExtend (_, presence, assigned, ty, rest) ->
      (match Type.canonical_presence presence with
       | Absent -> ()
       | Present -> iter_ty ty
       | PVar var ->
         f var.id;
         iter_ty ty);
      (match Type.canonical_presence assigned with
       | Absent | Present -> ()
       | PVar var -> f var.id);
      iter_row rest
  in
  List.iter tys ~f:iter_ty;
  List.iter rows ~f:(fun var ->
    List.iter var.bounds ~f:(fun (bound : Type.bound) ->
      if not (except_row bound) then iter_row bound.row));
  List.iter presences ~f:(fun var ->
    List.iter var.bounds ~f:(fun bound ->
      match bound with
      | If_present (ty1, ty2) when not (except_presence bound) ->
        iter_ty ty1;
        iter_ty ty2
      | If_present _ | At_most _ | At_least _ -> ()));
  List.iter kept ~f:(function
    | Type.Row_bound { bound; _ } -> iter_row bound.row
    | If_present_kept { presence; ty1; ty2; _ } ->
      f presence.id;
      iter_ty ty1;
      iter_ty ty2)
;;

let used_vars ?except_row ?except_presence ~tys ~presences ~rows ~kept () =
  let used = Int.Hash_set.create () in
  iter_used_vars
    ?except_row
    ?except_presence
    ~tys
    ~presences
    ~rows
    ~kept
    ~f:(Hash_set.add used)
    ();
  used
;;

let only_generalized_bounds_have
      (var : _ Type.Var.t)
      ~is_generalized
      ~may_be_used
  =
  List.for_all var.in_bounds_of ~f:(fun bounded ->
    match bounded with
    | Row_var { id; _ } | Presence_var { id; _ } ->
      is_generalized id || not (may_be_used bounded))
;;

(* A bound [..'g] of a generalized row, where ['g] is generalized, has no
   bounds and is used nowhere else, holds whatever the row: ['g] can be it. *)
let drop_trivial_row_bounds
      ~tys
      ~is_generalized
      ~may_be_used
      ~presences
      ~(rows : Row_var.t list)
      ~kept
  =
  let bare_var (bound : Type.bound) =
    match Type.canonical_row bound.row with
    | RVar ({ state = Generic; bounds = []; _ } as var)
      when only_generalized_bounds_have var ~is_generalized ~may_be_used ->
      Some var
    | RVar _ | REmpty | RExtend _ -> None
  in
  let used =
    used_vars
      ~except_row:(fun bound -> Option.is_some (bare_var bound))
      ~tys
      ~presences
      ~rows
      ~kept
      ()
  in
  let bare = Int.Table.create () in
  List.iter rows ~f:(fun var ->
    List.iter var.bounds ~f:(fun bound ->
      Option.iter (bare_var bound) ~f:(fun bare_var ->
        Hashtbl.incr bare bare_var.id)));
  let trivial bound =
    match bare_var bound with
    | Some var ->
      (not (Hash_set.mem used var.id)) && Hashtbl.find_exn bare var.id = 1
    | None -> false
  in
  List.iter rows ~f:(fun var ->
    var.bounds <- List.filter var.bounds ~f:(fun bound -> not (trivial bound)))
;;

(* Likewise an equality [p => t = 'a] holds whatever [p] and [t] if ['a] is
   used nowhere else: ['a] can be [t]. *)
let drop_trivial_equalities
      ~tys
      ~is_generalized
      ~may_be_used
      ~(presences : Presence_var.t list)
      ~rows
      ~kept
  =
  let free (ty : Type.ty) =
    match Type.canonical_ty ty with
    | TVar ({ state = Generic; _ } as var)
      when only_generalized_bounds_have var ~is_generalized ~may_be_used ->
      Some var
    | TVar _ | TCon _ | TArrow _ | TTuple _ | TCode _ | TRecord _ | TVariant _
      -> None
  in
  let candidates : Type.presence_bound -> _ = function
    | If_present (ty1, ty2) -> List.filter_opt [ free ty1; free ty2 ]
    | At_most _ | At_least _ -> []
  in
  let used =
    used_vars
      ~except_presence:(fun bound -> not (List.is_empty (candidates bound)))
      ~tys
      ~presences
      ~rows
      ~kept
      ()
  in
  let vars_of ty = used_vars ~tys:[ ty ] ~presences:[] ~rows:[] ~kept:[] () in
  let references = Int.Table.create () in
  let with_sides =
    List.map presences ~f:(fun var ->
      ( var
      , List.map var.bounds ~f:(fun bound ->
          match bound with
          | If_present (ty1, ty2) when not (List.is_empty (candidates bound)) ->
            let vars1 = vars_of ty1 in
            let vars2 = vars_of ty2 in
            Hash_set.iter
              (Hash_set.union vars1 vars2)
              ~f:(Hashtbl.incr references);
            bound, Some (ty1, vars1, vars2)
          | If_present _ | At_most _ | At_least _ -> bound, None) ))
  in
  let trivial bound (ty1, vars1, vars2) =
    List.exists (candidates bound) ~f:(fun (var : _ Type.Var.t) ->
      let other =
        match free ty1 with
        | Some var1 when var1.id = var.id -> vars2
        | Some _ | None -> vars1
      in
      (not (Hash_set.mem used var.id))
      && (not (Hash_set.mem other var.id))
      && Hashtbl.find_exn references var.id = 1)
  in
  List.iter with_sides ~f:(fun ((var : Presence_var.t), bounds) ->
    var.bounds
    <- List.filter_map bounds ~f:(fun (bound, sides) ->
         match sides with
         | Some sides when trivial bound sides -> None
         | Some _ | None -> Some bound))
;;

(* A bound [B] of a generalized row whose entries' presences and types and
   tail may be variables [L] used nowhere else, but in bounds between the
   presences of [L] and others and in equalities [q => s = t] with [t] in
   [L], holds whenever another bound [B'] of the row does, if [L] can be the
   corresponding parts of [B'] with those bounds and equalities then ones
   the scheme has: an instance can choose [L] so. Instances of schemes with
   such bounds pile them up. *)
let drop_witnessed_row_bounds
      ~tys
      ~is_generalized
      ~may_be_used
      ~(presences : Presence_var.t list)
      ~(rows : Row_var.t list)
      ~kept
      ~(with_equalities : Presence_var.t list)
  =
  let counts = Int.Table.create () in
  let links = Int.Table.create () in
  iter_used_vars
    ~link:(Hashtbl.incr links)
    ~tys
    ~presences
    ~rows
    ~kept
    ~f:(Hashtbl.incr counts)
    ();
  let count id = Hashtbl.find counts id |> Option.value ~default:0 in
  let rec unshared_ty (ty : Type.ty) =
    match ty with
    | TVar { state = Link target; id; _ } ->
      if [%equal: int option] (Hashtbl.find links id) (Some 1)
      then unshared_ty target
      else ty
    | ty -> ty
  in
  let rec unshared_row (row : Type.row) =
    match row with
    | RVar { state = Link target; id; _ } ->
      if [%equal: int option] (Hashtbl.find links id) (Some 1)
      then unshared_row target
      else row
    | row -> row
  in
  let with_equalities =
    Int.Hash_set.of_list
      (List.map with_equalities ~f:(fun (var : Presence_var.t) -> var.id))
  in
  let presences = List.filter presences ~f:(fun var -> not (outside var)) in
  let relations = Int.Table.create () in
  let equalities = Int.Table.create () in
  List.iter presences ~f:(fun owner ->
    List.iter owner.bounds ~f:(fun bound ->
      match bound with
      | At_most _ | At_least _ ->
        Option.iter (Type.presence_bound_var bound) ~f:(fun var ->
          Hashtbl.add_multi relations ~key:var.id ~data:(owner, bound))
      | If_present (ty1, ty2) ->
        let side (ty : Type.ty) other =
          match ty with
          | TVar ({ state = Generic; _ } as var) ->
            Hashtbl.add_multi equalities ~key:var.id ~data:(owner, bound, other)
          | TVar _
          | TCon _
          | TArrow _
          | TTuple _
          | TCode _
          | TRecord _
          | TVariant _ -> ()
        in
        side ty1 ty2;
        side ty2 ty1));
  let has (owner : Presence_var.t) bound =
    List.exists owner.bounds ~f:(phys_equal bound)
  in
  let usable (var : _ Type.Var.t) =
    is_generalized var.id
    && only_generalized_bounds_have var ~is_generalized ~may_be_used
  in
  let local_presence presence =
    match Type.canonical_presence presence with
    | PVar ({ state = Generic; _ } as var)
      when count var.id = 1
           && (not (Hash_set.mem with_equalities var.id))
           && (not (has_equalities var))
           && usable var -> Some var
    | PVar _ | Present | Absent -> None
  in
  let local_ty presence ty =
    match Type.canonical_presence presence, unshared_ty ty with
    | (Present | PVar _), TVar ({ state = Generic; _ } as var)
      when count var.id = 1 + List.length (Hashtbl.find_multi equalities var.id)
           && usable var -> Some var
    | (Present | PVar _ | Absent), _ -> None
  in
  let local_row row =
    match unshared_row row with
    | RVar ({ state = Generic; bounds = []; _ } as var)
      when count var.id = 1 && usable var -> true
    | RVar _ | REmpty | RExtend _ -> false
  in
  (* The variables of [L] with what they are in [row'], if [row] is [row']
     but for them. The positions of [L] are reached through [Link]s used
     nowhere else, as [iter_used_vars] counts what they reach once. *)
  let rec witness row (row' : Type.row) ~presences ~tys =
    match unshared_row row, Type.canonical_row row' with
    | ( RExtend (label, presence, assigned, ty, rest)
      , RExtend (label', presence', assigned', ty', rest') )
      when String.equal label label' ->
      let witness_presence presence presence' presences =
        match local_presence presence with
        | Some var -> Some ((var, presence') :: presences)
        | None ->
          Option.some_if (Type.same_presence presence presence') presences
      in
      let%bind.Option presences =
        witness_presence presence presence' presences
      in
      let%bind.Option presences =
        witness_presence assigned assigned' presences
      in
      let%bind.Option tys =
        match local_ty presence ty with
        | Some var -> Some ((var, ty') :: tys)
        | None -> Option.some_if (Type.same_ty ty ty') tys
      in
      witness rest rest' ~presences ~tys
    | RExtend _, _ | RVar { state = Link _; _ }, _ -> None
    | (RVar _ | REmpty), row' ->
      Option.some_if (local_row row || Type.same_row row row') (presences, tys)
  in
  let holds (lower : Type.presence) (upper : Type.presence) =
    match Type.canonical_presence lower, Type.canonical_presence upper with
    | Absent, _ | _, Present -> true
    | lower, upper when Type.same_presence lower upper -> true
    | lower, upper ->
      let bounded (presence : Type.presence) ~f =
        match presence with
        | PVar var -> List.exists var.bounds ~f
        | Present | Absent -> false
      in
      bounded lower ~f:(function
        | At_most other -> Type.same_presence other upper
        | At_least _ | If_present _ -> false)
      || bounded upper ~f:(function
        | At_least other -> Type.same_presence other lower
        | At_most _ | If_present _ -> false)
  in
  let witnessed ~presences:local_presences ~tys:local_tys =
    let locals = Int.Hash_set.create () in
    List.iter local_presences ~f:(fun ((var : Presence_var.t), _) ->
      Hash_set.add locals var.id);
    List.iter local_tys ~f:(fun ((var : (Type.ty, Nothing.t) Type.Var.t), _) ->
      Hash_set.add locals var.id);
    let not_local (presence : Type.presence) =
      match Type.canonical_presence presence with
      | PVar var -> not (Hash_set.mem locals var.id)
      | Present | Absent -> true
    in
    let no_locals ty =
      let found = ref false in
      iter_used_vars
        ~tys:[ ty ]
        ~presences:[]
        ~rows:[]
        ~kept:[]
        ~f:(fun id -> if Hash_set.mem locals id then found := true)
        ();
      not !found
    in
    List.for_all local_presences ~f:(fun ((var : Presence_var.t), witness) ->
      List.for_all var.bounds ~f:(function
        | At_most upper -> not_local upper && holds witness upper
        | At_least lower -> not_local lower && holds lower witness
        | If_present _ -> false)
      && List.for_all
           (Hashtbl.find_multi relations var.id)
           ~f:(fun ((owner : Presence_var.t), bound) ->
             (not (has owner bound))
             || (not_local (PVar owner)
                 &&
                 match bound with
                 | At_most _ -> holds (PVar owner) witness
                 | At_least _ -> holds witness (PVar owner)
                 | If_present _ -> false)))
    && List.for_all
         local_tys
         ~f:(fun ((var : (Type.ty, Nothing.t) Type.Var.t), witness) ->
           List.for_all
             (Hashtbl.find_multi equalities var.id)
             ~f:(fun ((owner : Presence_var.t), bound, other) ->
               (not (has owner bound))
               || (no_locals other
                   && (Type.same_ty other witness
                       || List.exists owner.bounds ~f:(function
                         | If_present (ty1, ty2) ->
                           (Type.same_ty ty1 other && Type.same_ty ty2 witness)
                           || (Type.same_ty ty1 witness
                               && Type.same_ty ty2 other)
                         | At_most _ | At_least _ -> false)))))
  in
  let drop ~presences:local_presences ~tys:local_tys =
    let remove (owner : Presence_var.t) bound =
      owner.bounds
      <- List.filter owner.bounds ~f:(fun other -> not (phys_equal other bound))
    in
    List.iter local_presences ~f:(fun ((var : Presence_var.t), _) ->
      List.iter (Hashtbl.find_multi relations var.id) ~f:(fun (owner, bound) ->
        remove owner bound);
      var.bounds <- []);
    List.iter local_tys ~f:(fun ((var : (Type.ty, Nothing.t) Type.Var.t), _) ->
      List.iter
        (Hashtbl.find_multi equalities var.id)
        ~f:(fun (owner, bound, _) -> remove owner bound))
  in
  List.iter rows ~f:(fun row ->
    List.iter row.bounds ~f:(fun (bound : Type.bound) ->
      let witness =
        List.find_map row.bounds ~f:(fun (other : Type.bound) ->
          if
            phys_equal other bound
            || not (Set.equal other.shadowed bound.shadowed)
          then None
          else (
            match witness bound.row other.row ~presences:[] ~tys:[] with
            | Some (presences, tys) when witnessed ~presences ~tys ->
              Some (presences, tys)
            | Some _ | None -> None))
      in
      Option.iter witness ~f:(fun (presences, tys) ->
        drop ~presences ~tys;
        row.bounds
        <- List.filter row.bounds ~f:(fun other -> not (phys_equal other bound)))))
;;

(* A generalized presence that nothing uses, which is not the upper bound of
   another, can be [Absent]: no instance can make it [Present], and its other
   bounds then hold. Such presences come from instances whose types were
   dropped, or are of absent entries, whose types don't matter. *)
let rec drop_unused_presences
          ~tys
          ~is_generalized
          ~may_be_used
          ~(presences : Presence_var.t list)
          ~(rows : Row_var.t list)
          ~kept
  =
  let used = used_vars ~tys ~presences ~rows ~kept () in
  let unused (var : Presence_var.t) =
    (not (Hash_set.mem used var.id))
    && List.for_all var.bounds ~f:(function
      | At_least _ -> false
      | At_most _ | If_present _ -> true)
    && only_generalized_bounds_have var ~is_generalized ~may_be_used
  in
  match List.partition_tf presences ~f:unused with
  | [], _ -> ()
  | unused, presences ->
    List.iter unused ~f:(fun var ->
      List.iter (uppers var) ~f:(fun upper ->
        upper.bounds
        <- List.filter upper.bounds ~f:(function
             | At_least lower -> not (Type.same_presence lower (PVar var))
             | At_most _ | If_present _ -> true));
      var.state <- Link Absent;
      var.bounds <- []);
    drop_unused_presences
      ~tys
      ~is_generalized
      ~may_be_used
      ~presences
      ~rows
      ~kept
;;

(* A generalized presence with an equality between types that [clash] can
   only be [Absent]. One that is the upper bound of others is left as is, as
   they would have to be [Absent] too. *)
let decide_clashing_presences (presences : Presence_var.t list) =
  List.iter presences ~f:(fun var ->
    match var.state with
    | Generic
      when (not
              (List.exists var.bounds ~f:(function
                 | At_least _ -> true
                 | At_most _ | If_present _ -> false)))
           && List.exists var.bounds ~f:(function
             | If_present (ty1, ty2) -> Unify.clash ty1 ty2
             | At_most _ | At_least _ -> false) ->
      List.iter (uppers var) ~f:(fun (upper : Presence_var.t) ->
        upper.bounds
        <- List.filter upper.bounds ~f:(function
             | At_least lower -> not (Type.same_presence lower (PVar var))
             | At_most _ | If_present _ -> true));
      var.state <- Link Absent;
      var.bounds <- []
    | Generic | Unbound _ | Link _ -> ())
;;

let generalize fresh ~rank tys =
  let presences = Queue.create () in
  let rows = Queue.create () in
  let in_bounds_of = Queue.create () in
  let generalized = Int.Hash_set.create () in
  let generalize (var : _ Type.Var.t) =
    newly_generalized ~rank var
    &&
    (Hash_set.add generalized var.id;
     Queue.enqueue in_bounds_of var.in_bounds_of;
     true)
  in
  let rec generalizer : Nothing.t Type.Iter.t =
    { ty =
        (fun ~guarded:_ var ->
          ignore (generalize var : bool);
          Ok ())
    ; row =
        (fun ~guarded:_ var ->
          if generalize var
          then (
            Queue.enqueue rows var;
            List.fold_result
              var.bounds
              ~init:()
              ~f:(fun () (bound : Type.bound) ->
                Type.iter_row generalizer bound.row))
          else Ok ())
    ; presence =
        (fun ~guarded var ->
          if generalize var
          then (
            Queue.enqueue presences var;
            List.fold_result var.bounds ~init:() ~f:(fun () bound ->
              match bound with
              | At_most _ | At_least _ ->
                (match Type.presence_bound_var bound with
                 | Some other -> generalizer.presence ~guarded other
                 | None -> Ok ())
              | If_present (ty1, ty2) ->
                let%bind.Result () = Type.iter_ty generalizer ty1 in
                Type.iter_ty generalizer ty2))
          else Ok ())
    }
  in
  (match
     List.fold_result tys ~init:() ~f:(fun () -> Type.iter_ty generalizer)
   with
   | Ok () -> ()
   | Error _ -> .);
  let is_generalized = Hash_set.mem generalized in
  let may_be_used = may_be_used ~rank in
  let bounded = Int.Hash_set.create () in
  let kept =
    Queue.to_list in_bounds_of
    |> List.concat
    |> List.concat_map ~f:(fun (var : Type.bounded) ->
      let unbound (var : _ Type.Var.t) =
        match var.state with
        | Unbound _ -> Hash_set.strict_add bounded var.id |> Result.is_ok
        | Generic | Link _ -> false
      in
      match var with
      | Row_var row_var when unbound row_var && may_be_used var ->
        keep_row_bounds fresh row_var ~rank
      | Presence_var presence_var when unbound presence_var && may_be_used var
        -> keep_equalities fresh presence_var ~rank
      | Row_var _ | Presence_var _ -> [])
  in
  decide_clashing_presences (Queue.to_list presences);
  Entry_types.simplify
    ~is_generalized
    ~tys
    ~presences:(Queue.to_list presences)
    ~rows:(Queue.to_list rows)
    ~kept;
  Entry_types.dedup
    ~presences:(Queue.to_list presences)
    ~rows:(Queue.to_list rows);
  let kept = Entry_types.dedup_kept kept in
  let with_equalities = close_presence_bounds fresh (Queue.to_list presences) in
  List.iter (Queue.to_list rows) ~f:drop_subsumed_row_bounds;
  drop_trivial_row_bounds
    ~tys
    ~is_generalized
    ~may_be_used
    ~presences:(Queue.to_list presences)
    ~rows:(Queue.to_list rows)
    ~kept;
  drop_trivial_equalities
    ~tys
    ~is_generalized
    ~may_be_used
    ~presences:(Queue.to_list presences)
    ~rows:(Queue.to_list rows)
    ~kept;
  drop_witnessed_row_bounds
    ~tys
    ~is_generalized
    ~may_be_used
    ~presences:(Queue.to_list presences)
    ~rows:(Queue.to_list rows)
    ~kept
    ~with_equalities;
  drop_unused_presences
    ~tys
    ~is_generalized
    ~may_be_used
    ~presences:
      (Queue.to_list presences
       |> List.filter ~f:(fun (var : Presence_var.t) ->
         match var.state with
         | Generic -> true
         | Unbound _ | Link _ -> false))
    ~rows:(Queue.to_list rows)
    ~kept;
  attach_kept ~is_generalized kept;
  (* The outside constraints the kept bounds and equalities imply are those of
     an instance, whose variables are not used elsewhere. *)
  if
    not
      ((List.is_empty kept && List.is_empty with_equalities)
       || (List.is_empty with_equalities && outside_witnessed ~kept))
  then (
    let pending = Queue.create () in
    let seen_kept = Int.Hash_set.create () in
    let mapper = instantiator fresh ~rank ~pending ~seen_kept in
    let trivial = trivial_witnesses ~kept ~with_equalities in
    List.iter kept ~f:(fun kept ->
      match kept with
      | Row_bound { bound; _ } when trivial bound -> ()
      | Row_bound { bound; _ } ->
        ignore (Type.map_row mapper bound.row : Type.row);
        enqueue_kept ~pending ~seen_kept kept
      | If_present_kept { ty1; ty2; _ } ->
        ignore (Type.map_ty mapper ty1 : Type.ty);
        ignore (Type.map_ty mapper ty2 : Type.ty);
        enqueue_kept ~pending ~seen_kept kept);
    List.iter with_equalities ~f:(fun var ->
      ignore (mapper.presence var : Type.presence));
    add_instance_bounds fresh mapper pending)
;;
