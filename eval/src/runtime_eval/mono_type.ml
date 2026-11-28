open! Import

module Mono_ty = struct
  type t = Type.ty

  let sexp_of_t ty = Sexp.Atom (Type.to_string_hum ty)
end

module Owner_key = struct
  module T = struct
    type t =
      { toplevel : string
      ; lets : Typed.Allocation.Id.t list
      }
    [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)

  let of_owner ({ toplevel; lets } : Typed.Allocation.Owner.t) =
    { toplevel = toplevel.name
    ; lets = List.map lets ~f:(fun (l : Typed.Allocation.Owner.Let.t) -> l.id)
    }
  ;;
end

let hash_fold_step state (step : Typed.Allocation.Side.Step.t) =
  match step with
  | Arg -> Int.hash_fold_t state 0
  | Res -> Int.hash_fold_t state 1
  | Component i -> Int.hash_fold_t (Int.hash_fold_t state 2) i
  | Type_arg i -> Int.hash_fold_t (Int.hash_fold_t state 3) i
  | Field name -> String.hash_fold_t (Int.hash_fold_t state 4) name
  | Code -> Int.hash_fold_t state 5
;;

let hash_fold_side state ({ steps; side } : Typed.Allocation.Side.t) =
  let state = List.fold steps ~init:state ~f:hash_fold_step in
  Int.hash_fold_t
    state
    (match side with
     | `Arg -> 0
     | `Res -> 1)
;;

module Owner_side = struct
  module T = struct
    type t =
      { owner : Owner_key.t
      ; side : Typed.Allocation.Side.t
      }
    [@@deriving compare, equal, sexp_of]

    let hash_fold_t state { owner; side } =
      hash_fold_side (Owner_key.hash_fold_t state owner) side
    ;;

    let hash = Ppx_hash_lib.Std.Hash.of_fold hash_fold_t
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)
end

module Local_sides = struct
  module T = struct
    type t = Owner_side.Set.t [@@deriving compare, equal, sexp_of]

    let hash_fold_t state t =
      Set.fold t ~init:state ~f:(fun state elt ->
        Owner_side.hash_fold_t state elt)
    ;;

    let hash = Ppx_hash_lib.Std.Hash.of_fold hash_fold_t
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)

  let empty = Owner_side.Set.empty

  let of_list pairs =
    List.map pairs ~f:(fun (owner, side) -> { Owner_side.owner; side })
    |> Owner_side.Set.of_list
  ;;

  let is_local t ~owner ~side = Core.Set.mem t { Owner_side.owner; side }

  let replace_owner t ~owner ~sides =
    let without =
      Core.Set.filter t ~f:(fun (elt : Owner_side.t) ->
        not (Owner_key.equal elt.owner owner))
    in
    List.fold sides ~init:without ~f:(fun acc side ->
      Core.Set.add acc { Owner_side.owner; side })
  ;;
end

module Mode_pins = struct
  module T = struct
    type t = (int * Mode.Locality.t) list [@@deriving compare, equal, sexp_of]

    let hash_fold_t state t =
      List.fold t ~init:state ~f:(fun state (var_id, locality) ->
        Int.hash_fold_t
          (Int.hash_fold_t state var_id)
          (Mode.Locality.index locality))
    ;;

    let hash = Ppx_hash_lib.Std.Hash.of_fold hash_fold_t
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)

  let empty = []
end

module Key = struct
  module T = struct
    type t =
      | Con of string * arg list
      | Arrow of t * t
      | Tuple of t list
      | Code of
          { inner : t
          ; context : (string * t) list
          }
      | Record of (string * t) list
      | Variant of (string * t) list
      | Mu of t
      | Var of int
    [@@deriving compare, equal, hash, sexp_of]

    and arg =
      | Ty of t
      | Row of (string * t) list
    [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)
end

(* Intermediate key tree before resolving recursive structural node ids to exact
   de Bruijn indices. *)
module Raw_key = struct
  type t =
    | Con of string * arg list
    | Arrow of t * t
    | Tuple of t list
    | Code of
        { inner : t
        ; context : (string * t) list
        }
    | Record of int * (string * t) list
    | Variant of int * (string * t) list
    | Ref of int

  and arg =
    | Ty of t
    | Row of (string * t) list
end

let local_index = Mode.Locality.index Local

let bounds pins node =
  if Map.is_empty pins
  then Mode.Var.Node.lower node, Mode.Var.Node.upper node
  else (
    let reason = Mode.Reason.create "the instance has it" in
    match
      Mode.Var.assuming
        (fun () ->
          Map.data pins
          |> List.map ~f:(fun (node, value) ->
            Result.bind (Mode.Var.Node.at_least node value reason) ~f:(fun () ->
              Mode.Var.Node.at_most node value reason))
          |> Result.all_unit)
        ~read:(fun () -> Mode.Var.Node.lower node, Mode.Var.Node.upper node)
    with
    | Some value -> value
    | None ->
      raise_s
        [%message
          "Mono_type: pinned mode instances cannot hold simultaneously"
            ~pins:
              (Map.data pins
               |> List.map ~f:(fun (node, value) ->
                 ( Mode.Var.Node.var_id node
                 , value
                 , Mode.Var.Node.lower node
                 , Mode.Var.Node.upper node ))
               : (int * int * int * int) list)])
;;

let side_of (ty : Type.ty) (side : Typed.Allocation.Side.t)
  : Arrow_modes.Side.t option
  =
  let rec field (row : Type.row) name =
    match Type.canonical_row row with
    | RExtend (name', _, _, ty, rest) ->
      if String.equal name name' then Some ty else field rest name
    | REmpty | RVar _ -> None
  in
  let rec walk (at : [ `Ty of Type.ty | `Row of Type.row ]) steps =
    match steps, at with
    | [], `Ty ty -> Some ty
    | (step : Typed.Allocation.Side.Step.t) :: steps, `Ty ty ->
      (match step, Type.canonical_ty ty with
       | Arg, TArrow (arg, _, _) -> walk (`Ty arg) steps
       | Res, TArrow (_, res, _) -> walk (`Ty res) steps
       | Component i, TTuple tys ->
         Option.bind (List.nth tys i) ~f:(fun ty -> walk (`Ty ty) steps)
       | Type_arg i, TCon (_, args) ->
         Option.bind (List.nth args i) ~f:(function
           | ATy ty -> walk (`Ty ty) steps
           | ARow row -> walk (`Row row) steps
           | AMode _ -> None)
       | Field name, (TRecord { row; _ } | TVariant { row; _ }) ->
         Option.bind (field row name) ~f:(fun ty -> walk (`Ty ty) steps)
       | Code, TCode { inner; _ } -> walk (`Ty inner) steps
       | Field name, TCode { context; _ } ->
         Option.bind (field context name) ~f:(fun ty -> walk (`Ty ty) steps)
       | _ -> None)
    | Field name :: steps, `Row row ->
      Option.bind (field row name) ~f:(fun ty -> walk (`Ty ty) steps)
    | _, `Row _ -> None
  in
  match walk (`Ty ty) side.steps with
  | None -> None
  | Some ty ->
    (match Type.canonical_ty ty with
     | TArrow (_, _, modes) ->
       Some
         (match side.side with
          | `Arg -> modes.arg
          | `Res -> modes.res)
     | _ -> None)
;;

let expand_alias_once (resolver : Runtime_types.Resolver.t) ~fresh_id name args =
  match Runtime_types.Resolver.find resolver name with
  | Some ({ params; kind = Alias body; _ } : Signature.Type_decl.t)
    when List.length params = List.length args ->
    let ty_bindings = ref Int.Map.empty in
    let row_bindings = ref Int.Map.empty in
    List.iter2_exn params args ~f:(fun (param : Type.arg) (arg : Type.arg) ->
      match param, arg with
      | ATy param_ty, ATy arg_ty ->
        (match Type.canonical_ty param_ty with
         | TVar { id; _ } ->
           ty_bindings := Map.set !ty_bindings ~key:id ~data:arg_ty
         | _ -> ())
      | ATy param_ty, ARow arg_row ->
        (match Type.canonical_ty param_ty with
         | TVar { id; _ } ->
           ty_bindings
           := Map.set
                !ty_bindings
                ~key:id
                ~data:(Type.TRecord { id = fresh_id (); row = arg_row })
         | _ -> ())
      | ARow param_row, ARow arg_row ->
        (match Type.canonical_row param_row with
         | RVar { id; _ } ->
           row_bindings := Map.set !row_bindings ~key:id ~data:arg_row
         | _ -> ())
      | _, AMode _ | AMode _, _ | ARow _, ATy _ -> ());
    let ty_map = !ty_bindings in
    let row_map = !row_bindings in
    if Map.is_empty ty_map && Map.is_empty row_map
    then Some body
    else
      Some
        (Type.map_ty
           { keeps =
               (fun var ->
                 (not (Map.mem ty_map var.id)) && not (Map.mem row_map var.id))
           ; ty =
               (fun var ->
                 match Map.find ty_map var.id with
                 | Some ty -> ty
                 | None -> TVar var)
           ; row =
               (fun var ->
                 match Map.find row_map var.id with
                 | Some row -> row
                 | None -> RVar var)
           ; presence = (fun var -> PVar var)
           ; mode = Fn.id
           ; id = fresh_id
           }
           body)
  | Some _ | None -> None
;;

let expand_aliases (resolver : Runtime_types.Resolver.t) ~fresh_id ty =
  let rec go ~fuel ty =
    let canonical = Type.canonical_ty ty in
    if fuel <= 0
    then canonical
    else (
      match canonical with
      | TCon (name, args) ->
        (match expand_alias_once resolver ~fresh_id name args with
         | Some expanded -> go ~fuel:(fuel - 1) expanded
         | None -> canonical)
      | _ -> canonical)
  in
  go ~fuel:128 ty
;;

(* Collects mode variable pairings between [scheme] and its instantiation [ty]. *)
let instance_mode_vars ~resolver ~fresh_id ~scheme ~ty =
  let pairs = Queue.create () in
  let seen = Int.Hash_set.create () in
  let side (s : Arrow_modes.Side.t) (s' : Arrow_modes.Side.t) =
    match s, s' with
    | Var var, Var var' -> Queue.enqueue pairs (var, `Var var')
    | Var var, (Fixed { locality; _ } | Var_at { locality; _ }) ->
      Queue.enqueue pairs (var, `Fixed (Mode.Locality.index locality))
    | (Fixed _ | Var_at _), _ -> ()
  in
  let rec walk (scheme : Type.ty) (ty : Type.ty) =
    match scheme, Type.canonical_ty ty with
    | TVar { id; state = Link scheme; _ }, _ ->
      if not (Hash_set.mem seen id)
      then (
        Hash_set.add seen id;
        walk scheme ty)
    | TVar _, _ -> ()
    | TArrow (a, r, modes), TArrow (a', r', modes') ->
      side modes.arg modes'.arg;
      side modes.res modes'.res;
      side modes.curry modes'.curry;
      walk a a';
      walk r r'
    | TTuple tys, TTuple tys' when List.length tys = List.length tys' ->
      List.iter2_exn tys tys' ~f:walk
    | TCon (n1, args), TCon (n2, args')
      when String.equal n1 n2 && List.length args = List.length args' ->
      List.iter2_exn args args' ~f:(fun (arg : Type.arg) (arg' : Type.arg) ->
        match arg, arg' with
        | ATy ty, ATy ty' -> walk ty ty'
        | ARow row, ARow row' -> rows row row'
        | AMode _, AMode _ -> ()
        | _ -> ())
    | TCon (n1, args1), ty' ->
      (match expand_alias_once resolver ~fresh_id n1 args1 with
       | Some expanded -> walk expanded ty'
       | None ->
         (match ty' with
          | TCon (n2, args2) ->
            (match expand_alias_once resolver ~fresh_id n2 args2 with
             | Some expanded' -> walk scheme expanded'
             | None -> ())
          | _ -> ()))
    | scheme, TCon (n2, args2) ->
      (match expand_alias_once resolver ~fresh_id n2 args2 with
       | Some expanded' -> walk scheme expanded'
       | None -> ())
    | TCode { inner; context }, TCode { inner = inner'; context = context' } ->
      walk inner inner';
      rows context context'
    | ( (TRecord { id; row } | TVariant { id; row })
      , (TRecord { row = row'; _ } | TVariant { row = row'; _ }) ) ->
      if not (Hash_set.mem seen (-id - 1))
      then (
        Hash_set.add seen (-id - 1);
        rows row row')
    | _ -> ()
  and rows (row : Type.row) (row' : Type.row) =
    match row with
    | RExtend (name, _, _, ty, rest) ->
      let rec find (row' : Type.row) =
        match Type.canonical_row row' with
        | RExtend (name', pres', _, ty', rest') ->
          if String.equal name name'
          then (
            match Type.canonical_presence pres' with
            | Absent -> None
            | Present | PVar _ -> Some ty')
          else find rest'
        | REmpty | RVar _ -> None
      in
      Option.iter (find row') ~f:(walk ty);
      rows rest row'
    | RVar { state = Link row; _ } -> rows row row'
    | REmpty | RVar _ -> ()
  in
  walk scheme ty;
  Queue.to_list pairs
;;

module Int_pair = struct
  module T = struct
    type t = int * int [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)
end

module Subst = struct
  type t =
    { ty_map : Type.ty Int.Map.t
    ; row_map : Type.row Int.Map.t
    ; pres_map : Type.presence Int.Map.t
    ; pins : (Mode.Var.Node.t * int) Int.Map.t
    ; local_sides : Local_sides.t
    }

  let empty =
    { ty_map = Int.Map.empty
    ; row_map = Int.Map.empty
    ; pres_map = Int.Map.empty
    ; pins = Int.Map.empty
    ; local_sides = Local_sides.empty
    }
  ;;

  let local_sides t = t.local_sides

  let mode_pins t : Mode_pins.t =
    Map.to_alist t.pins
    |> List.map ~f:(fun (var_id, (_node, value)) ->
      var_id, Mode.Locality.of_index_exn value)
  ;;

  let sexp_of_t t =
    [%message
      ""
        ~local_sides:(t.local_sides : Local_sides.t)
        ~mode_pins:(mode_pins t : Mode_pins.t)]
  ;;

  let is_already_concrete ty =
    Result.is_ok
      (Type.iter_ty
         { ty = (fun ~guarded:_ _ -> Error ())
         ; row = (fun ~guarded:_ _ -> Error ())
         ; presence = (fun ~guarded:_ _ -> Error ())
         ; mode = (fun _ -> Ok ())
         }
         ty)
  ;;

  let is_already_concrete_row row =
    Result.is_ok
      (Type.iter_row
         { ty = (fun ~guarded:_ _ -> Error ())
         ; row = (fun ~guarded:_ _ -> Error ())
         ; presence = (fun ~guarded:_ _ -> Error ())
         ; mode = (fun _ -> Ok ())
         }
         row)
  ;;

  let rec concretize_with ~fresh_id ~ty_map ~row_map ~pres_map ~resolving ty =
    if is_already_concrete ty
    then Type.canonical_ty ty
    else Type.map_ty (mapper ~fresh_id ~ty_map ~row_map ~pres_map ~resolving) ty

  and concretize_row_with ~fresh_id ~ty_map ~row_map ~pres_map ~resolving row =
    if is_already_concrete_row row
    then Type.canonical_row row
    else
      Type.map_row (mapper ~fresh_id ~ty_map ~row_map ~pres_map ~resolving) row

  and mapper ~fresh_id ~ty_map ~row_map ~pres_map ~resolving : Type.Mapper.t =
    { keeps = (fun _ -> false)
    ; ty =
        (fun var ->
          match Map.find ty_map var.id with
          | Some bound_ty when not (Set.mem resolving var.id) ->
            concretize_with
              ~fresh_id
              ~ty_map:Int.Map.empty
              ~row_map:Int.Map.empty
              ~pres_map:Int.Map.empty
              ~resolving:(Set.add resolving var.id)
              bound_ty
          | Some _ | None -> Type.unit)
    ; row =
        (fun var ->
          match Map.find row_map var.id with
          | Some bound_row when not (Set.mem resolving var.id) ->
            concretize_row_with
              ~fresh_id
              ~ty_map:Int.Map.empty
              ~row_map:Int.Map.empty
              ~pres_map:Int.Map.empty
              ~resolving:(Set.add resolving var.id)
              bound_row
          | Some _ | None -> Type.REmpty)
    ; presence =
        (fun var ->
          match Map.find pres_map var.id with
          | Some bound_pres ->
            (match Type.canonical_presence bound_pres with
             | Present -> Type.Present
             | Absent | PVar _ -> Type.Absent)
          | None -> Type.Absent)
    ; mode = Fn.id
    ; id = fresh_id
    }
  ;;

  let concretize t ~fresh_id ty =
    concretize_with
      ~fresh_id
      ~ty_map:t.ty_map
      ~row_map:t.row_map
      ~pres_map:t.pres_map
      ~resolving:Int.Set.empty
      ty
  ;;

  let sorted_concrete_row_entries (row : Type.row) =
    let rec go seen acc (row : Type.row) =
      match Type.canonical_row row with
      | REmpty | RVar _ -> acc
      | RExtend (label, pres, _assigned, ty, rest) ->
        if Set.mem seen label
        then go seen acc rest
        else (
          let seen = Set.add seen label in
          match Type.canonical_presence pres with
          | Present -> go seen ((label, ty) :: acc) rest
          | Absent | PVar _ -> go seen acc rest)
    in
    go String.Set.empty [] row
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  ;;

  let key_of_concrete_ty ty =
    let recursive = Int.Hash_set.create () in
    let rec to_raw ancestors (ty : Type.ty) : Raw_key.t =
      match Type.canonical_ty ty with
      | TVar _ -> Raw_key.Con ("unit", [])
      | TCon (name, args) ->
        let args =
          List.filter_map args ~f:(function
            | ATy ty -> Some (Raw_key.Ty (to_raw ancestors ty))
            | ARow row -> Some (Raw_key.Row (row_to_raw ancestors row))
            | AMode _ -> None)
        in
        Raw_key.Con (name, args)
      | TArrow (a, r, _) ->
        Raw_key.Arrow (to_raw ancestors a, to_raw ancestors r)
      | TTuple tys -> Raw_key.Tuple (List.map tys ~f:(to_raw ancestors))
      | TCode { inner; context } ->
        Raw_key.Code
          { inner = to_raw ancestors inner
          ; context = row_to_raw ancestors context
          }
      | TRecord { id; row } ->
        if Set.mem ancestors id
        then (
          Hash_set.add recursive id;
          Raw_key.Ref id)
        else Raw_key.Record (id, row_to_raw (Set.add ancestors id) row)
      | TVariant { id; row } ->
        if Set.mem ancestors id
        then (
          Hash_set.add recursive id;
          Raw_key.Ref id)
        else Raw_key.Variant (id, row_to_raw (Set.add ancestors id) row)
    and row_to_raw ancestors row =
      sorted_concrete_row_entries row
      |> List.map ~f:(fun (label, ty) -> label, to_raw ancestors ty)
    in
    let raw = to_raw Int.Set.empty ty in
    let rec resolve mu_stack (raw : Raw_key.t) : Key.t =
      match raw with
      | Con (name, args) ->
        Key.Con
          ( name
          , List.map args ~f:(function
              | Raw_key.Ty t -> Key.Ty (resolve mu_stack t)
              | Raw_key.Row entries ->
                Key.Row
                  (List.map entries ~f:(fun (l, t) -> l, resolve mu_stack t)))
          )
      | Arrow (a, r) -> Key.Arrow (resolve mu_stack a, resolve mu_stack r)
      | Tuple ts -> Key.Tuple (List.map ts ~f:(resolve mu_stack))
      | Code { inner; context } ->
        Key.Code
          { inner = resolve mu_stack inner
          ; context = List.map context ~f:(fun (l, t) -> l, resolve mu_stack t)
          }
      | Record (id, entries) ->
        if Hash_set.mem recursive id
        then (
          let mu_stack = id :: mu_stack in
          Key.Mu
            (Key.Record
               (List.map entries ~f:(fun (l, t) -> l, resolve mu_stack t))))
        else
          Key.Record (List.map entries ~f:(fun (l, t) -> l, resolve mu_stack t))
      | Variant (id, entries) ->
        if Hash_set.mem recursive id
        then (
          let mu_stack = id :: mu_stack in
          Key.Mu
            (Key.Variant
               (List.map entries ~f:(fun (l, t) -> l, resolve mu_stack t))))
        else
          Key.Variant
            (List.map entries ~f:(fun (l, t) -> l, resolve mu_stack t))
      | Ref id ->
        let index =
          List.findi mu_stack ~f:(fun _ binder -> Int.equal binder id)
          |> Option.value_exn
          |> fst
        in
        Key.Var index
    in
    resolve [] raw
  ;;

  let key_of_ty t ~fresh_id ty = key_of_concrete_ty (concretize t ~fresh_id ty)
  let expand_aliases = expand_aliases

  let is_monomorphic t ~scheme ~owner ~owner_sides =
    let has_owner_sides =
      match owner with
      | None -> false
      | Some owner ->
        not (List.is_empty (owner_sides (Owner_key.of_owner owner)))
    in
    if has_owner_sides
    then false
    else (
      let result =
        Type.iter_ty
          { ty =
              (fun ~guarded:_ var ->
                match var.state with
                | Generic when not (Map.mem t.ty_map var.id) -> Error ()
                | Generic | Unbound _ | Link _ -> Ok ())
          ; row =
              (fun ~guarded:_ var ->
                match var.state with
                | Generic when not (Map.mem t.row_map var.id) -> Error ()
                | Generic | Unbound _ | Link _ -> Ok ())
          ; presence =
              (fun ~guarded:_ var ->
                match var.state with
                | Generic when not (Map.mem t.pres_map var.id) -> Error ()
                | Generic | Unbound _ | Link _ -> Ok ())
          ; mode =
              (fun var ->
                let node = Mode.Var.node var Locality in
                let lower, upper = bounds t.pins node in
                if lower < upper then Error () else Ok ())
          }
          scheme
      in
      Result.is_ok result)
  ;;

  let compute_local_sides
    ~owner
    ~owner_sides
    ~scheme
    ~pins
    ~is_toplevel
    ~parent_local_sides
    =
    match owner with
    | None -> if is_toplevel then Local_sides.empty else parent_local_sides
    | Some owner ->
      let owner_key = Owner_key.of_owner owner in
      let sides = owner_sides owner_key in
      let new_local_sides =
        List.filter_map sides ~f:(fun side ->
          match side_of scheme side with
          | None -> None
          | Some arrow_side ->
            let value =
              match arrow_side with
              | Fixed { locality; _ } | Var_at { locality; _ } ->
                Mode.Locality.index locality
              | Var var -> snd (bounds pins (Mode.Var.node var Locality))
            in
            if value >= local_index then Some side else None)
      in
      if is_toplevel
      then
        Local_sides.of_list
          (List.map new_local_sides ~f:(fun side -> owner_key, side))
      else
        Local_sides.replace_owner
          parent_local_sides
          ~owner:owner_key
          ~sides:new_local_sides
  ;;

  let least_instance ~owner ~owner_sides ~scheme =
    let pins = ref Int.Map.empty in
    let (_ : (unit, Nothing.t) Result.t) =
      Type.iter_ty
        { ty = (fun ~guarded:_ _ -> Ok ())
        ; row = (fun ~guarded:_ _ -> Ok ())
        ; presence = (fun ~guarded:_ _ -> Ok ())
        ; mode =
            (fun var ->
              let node = Mode.Var.node var Locality in
              let key = Mode.Var.Node.var_id node in
              if not (Map.mem !pins key)
              then (
                let lower, _upper = bounds !pins node in
                pins := Map.set !pins ~key ~data:(node, lower));
              Ok ())
        }
        scheme
    in
    let pins = !pins in
    let local_sides =
      compute_local_sides
        ~owner:(Some owner)
        ~owner_sides
        ~scheme
        ~pins
        ~is_toplevel:true
        ~parent_local_sides:Local_sides.empty
    in
    { empty with pins; local_sides }
  ;;

  let match_scheme
    ~resolver
    ~fresh_id
    ~is_toplevel
    ~inherit_pins
    ~owner
    ~owner_sides
    ~parent
    ~scheme
    ~instance
    =
    (* 1. Match mode variables on the raw [scheme] and [instance] using
          [parent.pins] so mode polymorphism resolves identically to
          [region_check]. *)
    let mode_pairs =
      instance_mode_vars ~resolver ~fresh_id ~scheme ~ty:instance
    in
    let init_pins =
      if inherit_pins
      then
        List.fold mode_pairs ~init:parent.pins ~f:(fun pins (var, var') ->
          match var' with
          | `Var var' when Int.equal (Mode.Var.id var) (Mode.Var.id var') ->
            pins
          | `Var _ | `Fixed _ ->
            Map.remove pins (Mode.Var.Node.var_id (Mode.Var.node var Locality)))
      else Int.Map.empty
    in
    let pins =
      List.fold mode_pairs ~init:init_pins ~f:(fun pins (var, var') ->
        let node = Mode.Var.node var Locality in
        let lower, upper = bounds pins node in
        let used =
          match var' with
          | `Var var' -> snd (bounds parent.pins (Mode.Var.node var' Locality))
          | `Fixed used -> used
        in
        Map.set
          pins
          ~key:(Mode.Var.Node.var_id node)
          ~data:(node, Int.max lower (Int.min used upper)))
    in
    let local_sides =
      compute_local_sides
        ~owner
        ~owner_sides
        ~scheme
        ~pins
        ~is_toplevel
        ~parent_local_sides:parent.local_sides
    in
    (* 2. Match type, row, and presence variables against the concretized
       [instance]. *)
    let concrete_instance = concretize parent ~fresh_id instance in
    let ty_map = ref (if is_toplevel then Int.Map.empty else parent.ty_map) in
    let row_map = ref (if is_toplevel then Int.Map.empty else parent.row_map) in
    let pres_map =
      ref (if is_toplevel then Int.Map.empty else parent.pres_map)
    in
    let seen_pairs = Int_pair.Hash_set.create () in
    let rec match_ty (s : Type.ty) (i : Type.ty) =
      match Type.canonical_ty s, Type.canonical_ty i with
      | TVar { id; state = Generic; _ }, i ->
        ty_map := Map.set !ty_map ~key:id ~data:i
      | TVar _, _ -> ()
      | TArrow (sa, sr, _), TArrow (ia, ir, _) ->
        match_ty sa ia;
        match_ty sr ir
      | TTuple ss, TTuple is when List.length ss = List.length is ->
        List.iter2_exn ss is ~f:match_ty
      | TCon (sn, sargs), TCon (in_, iargs)
        when String.equal sn in_ && List.length sargs = List.length iargs ->
        List.iter2_exn sargs iargs ~f:(fun sa ia ->
          match sa, ia with
          | ATy st, ATy it -> match_ty st it
          | ARow sr, ARow ir -> match_row sr ir
          | AMode _, AMode _ -> ()
          | _ -> ())
      | TCon (sn, sargs), i ->
        (match expand_alias_once resolver ~fresh_id sn sargs with
         | Some expanded -> match_ty expanded i
         | None ->
           (match i with
            | TCon (in_, iargs) ->
              (match expand_alias_once resolver ~fresh_id in_ iargs with
               | Some expanded' -> match_ty s expanded'
               | None -> ())
            | _ -> ()))
      | s, TCon (in_, iargs) ->
        (match expand_alias_once resolver ~fresh_id in_ iargs with
         | Some expanded' -> match_ty s expanded'
         | None -> ())
      | TCode { inner = si; context = sc }, TCode { inner = ii; context = ic }
        ->
        match_ty si ii;
        match_row sc ic
      | TRecord { id = id1; row = sr }, TRecord { id = id2; row = ir }
      | TVariant { id = id1; row = sr }, TVariant { id = id2; row = ir } ->
        if not (Hash_set.mem seen_pairs (id1, id2))
        then (
          Hash_set.add seen_pairs (id1, id2);
          match_row sr ir)
      | _ -> ()
    and match_row (s_row : Type.row) (i_row : Type.row) =
      (* Collect the effective (first-label-wins) entries of the concrete
         [i_row]. *)
      let rec collect_inst seen acc (row : Type.row) =
        match Type.canonical_row row with
        | REmpty | RVar _ -> acc
        | RExtend (label, pres, _assigned, ty, rest) ->
          if Set.mem seen label
          then collect_inst seen acc rest
          else (
            let seen = Set.add seen label in
            let pres =
              match Type.canonical_presence pres with
              | Present -> Type.Present
              | Absent | PVar _ -> Type.Absent
            in
            collect_inst seen (Map.set acc ~key:label ~data:(pres, ty)) rest)
      in
      let inst_entries = collect_inst String.Set.empty String.Map.empty i_row in
      let rec walk_scheme consumed (row : Type.row) =
        match Type.canonical_row row with
        | REmpty -> ()
        | RVar { id; state = Generic; _ } ->
          let residual =
            Map.fold_right
              inst_entries
              ~init:Type.REmpty
              ~f:(fun ~key:label ~data:(pres, ty) acc ->
                if Set.mem consumed label
                then acc
                else Type.RExtend (label, pres, Type.Absent, ty, acc))
          in
          row_map := Map.set !row_map ~key:id ~data:residual
        | RVar _ -> ()
        | RExtend (label, s_pres, _assigned, s_ty, s_rest) ->
          if Set.mem consumed label
          then walk_scheme consumed s_rest
          else (
            let consumed = Set.add consumed label in
            (match Map.find inst_entries label with
             | Some (i_pres, i_ty) ->
               (match Type.canonical_presence s_pres with
                | PVar { id; state = Generic; _ } ->
                  pres_map := Map.set !pres_map ~key:id ~data:i_pres
                | Present | Absent | PVar _ -> ());
               (match i_pres with
                | Present -> match_ty s_ty i_ty
                | Absent | PVar _ -> ())
             | None ->
               (match Type.canonical_presence s_pres with
                | PVar { id; state = Generic; _ } ->
                  pres_map := Map.set !pres_map ~key:id ~data:Type.Absent
                | Present | Absent | PVar _ -> ()));
            walk_scheme consumed s_rest)
      in
      walk_scheme String.Set.empty s_row
    in
    match_ty scheme concrete_instance;
    { ty_map = !ty_map
    ; row_map = !row_map
    ; pres_map = !pres_map
    ; pins
    ; local_sides
    }
  ;;
end
