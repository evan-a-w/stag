open! Import
open Source_helpers
module Arrow_modes = Stag_types.Arrow_modes
module Allocation = Typed.Allocation
module Prim = Stag_ir.Prim
module Mode = Stag_mode

module Outcome = struct
  type t =
    | Returned of
        { value : string
        ; on_heap : int
        ; in_regions : int
        }
    | Use_after_free of string
    | Unsupported of string
    | Rejected of string list
  [@@deriving sexp_of]
end

exception Use_after_free of string
exception Unsupported of string

let unsupported fmt = ksprintf (fun s -> raise (Unsupported s)) fmt

module Region = struct
  type t =
    { name : string
    ; mutable live : bool
    }

  let heap = { name = "the heap"; live = true }
  let create name = { name; live = true }
  let free t = t.live <- false
end

(* The instance of each owner running: whether each of its sides that a site
   follows is [local], and its type's side nodes pinned to what they are, for
   reading the instances it uses. *)
module Instance = struct
  type t =
    { local : (string * Allocation.Side.t) list
    ; pins : (Mode.Var.Node.t * int) Int.Map.t
    }

  let empty = { local = []; pins = Int.Map.empty }

  let is_local t ~owner ~side =
    List.exists t.local ~f:(fun (owner', side') ->
      String.equal owner owner' && Allocation.Side.equal side side')
  ;;
end

let owner_key (owner : Allocation.Owner.t) =
  Sexp.to_string [%sexp (owner : Allocation.Owner.t)]
;;

module Value = struct
  type t =
    | Prim_value of Prim.Value.t
    | Tuple of t list
    | Construct of Typed.Constructor_ref.t * t list
    | Record of t array
    | Poly_record of t String.Map.t
    | Poly_variant of string * t
    | Obj of obj
    | Prim_fn of Prim.t * t list (** Latest first. *)

  and obj =
    { id : int
    ; region : Region.t
    ; site : string
    ; payload : payload
    }

  and payload =
    | Box of t
    | Ptr of t ref
    | Cell of t ref
    | Closure of closure

  and closure =
    { params : Typed.Ident.t list
    ; body : Typed.Expr.t
    ; env : env
    ; instance : Instance.t
    ; partials : Allocation.t list (** Of the applications still to come. *)
    ; owner : Allocation.Owner.t
    }

  and binding =
    | Value of t
    | Generic_value of
        { body : Typed.Expr.t
        ; env : env
        ; ctx : ctx
        ; owner : Allocation.Owner.t
        ; scheme : Type.ty
        }
    | Generic_function of
        { obj : obj
        ; owner : Allocation.Owner.t
        ; scheme : Type.ty
        }

  and env = binding ref Typed.Ident.Map.t

  (* [region]: the innermost (a function's or [region_]'s); [caller]: the one
     around that; [current]: the innermost not yet ended. *)
  and ctx =
    { region : Region.t
    ; caller : Region.t
    ; current : Region.t
    ; instance : Instance.t
    ; owner : Allocation.Owner.t
    }

  let unit = Prim_value Unit

  let to_string = function
    | Prim_value (Int n) -> Int.to_string n
    | Prim_value Unit -> "()"
    | Prim_value (Bool b) -> Bool.to_string b
    | _ -> "<value>"
  ;;
end

open Value

type t =
  { sites : (Allocation.Kind.t * Loc.t, Allocation.t list) Hashtbl.Poly.t
  ; sides : (string, Allocation.Side.t list) Hashtbl.t
  ; toplevels : (string, Typed.Toplevel.t) Hashtbl.t
  ; values : (string, Value.t) Hashtbl.t
  ; ident_info : Typed.Ident_info.t Typed.Ident.Map.t
  ; mutable next_id : int
  ; mutable fuel : int
  ; mutable on_heap : int
  ; mutable in_regions : int
  ; tail_calls_end_regions : bool
  }

let site t kind loc =
  match Hashtbl.find t.sites (kind, loc) with
  | Some (allocation :: _) -> allocation
  | Some [] | None ->
    unsupported
      !"no %{Sexp} site at %s"
      [%sexp (kind : Allocation.Kind.t)]
      (Loc.to_string loc)
;;

let describe (allocation : Allocation.t) =
  sprintf
    !"%{Sexp} at %s"
    [%sexp (allocation.kind : Allocation.Kind.t)]
    (Loc.to_string allocation.loc)
;;

let placement (ctx : ctx) (allocation : Allocation.t) =
  match allocation.follows with
  | Some { owner; side; if_local }
    when Instance.is_local ctx.instance ~owner:(owner_key owner) ~side ->
    if_local
  | Some _ | None -> allocation.placement
;;

let alloc
  t
  (allocation : Allocation.t)
  ~(at : Region.t)
  ~caller
  payload
  (placement : Allocation.Placement.t)
  =
  t.next_id <- t.next_id + 1;
  (match placement with
   | Heap -> t.on_heap <- t.on_heap + 1
   | Region | Caller_region -> t.in_regions <- t.in_regions + 1);
  { id = t.next_id
  ; region =
      (match placement with
       | Heap -> Region.heap
       | Region -> at
       | Caller_region -> caller)
  ; site = describe allocation
  ; payload
  }
;;

let make t ctx kind loc payload =
  let allocation = site t kind loc in
  alloc
    t
    allocation
    ~at:ctx.region
    ~caller:ctx.caller
    payload
    (placement ctx allocation)
;;

let live (obj : obj) ~what =
  if not obj.region.live
  then
    raise
      (Use_after_free
         (sprintf
            "%s of the %s, made in %s, after it ended"
            what
            obj.site
            obj.region.name))
;;

(* The node of a side of an arrow in [ty]. *)
let side_of (ty : Type.ty) (side : Allocation.Side.t) =
  let rec field (row : Type.row) name =
    match Type.canonical_row row with
    | RExtend (name', _, _, ty, rest) ->
      if String.equal name name' then ty else field rest name
    | REmpty | RVar _ -> unsupported "no field %s" name
  in
  let rec walk (at : [ `Ty of Type.ty | `Row of Type.row ]) steps =
    match steps, at with
    | [], `Ty ty -> ty
    | (step : Allocation.Side.Step.t) :: steps, `Ty ty ->
      (match step, Type.canonical_ty ty with
       | Arg, TArrow (arg, _, _) -> walk (`Ty arg) steps
       | Res, TArrow (_, res, _) -> walk (`Ty res) steps
       | Component i, TTuple tys -> walk (`Ty (List.nth_exn tys i)) steps
       | Type_arg i, TCon (_, args) ->
         (match List.nth_exn args i with
          | ATy ty -> walk (`Ty ty) steps
          | ARow row -> walk (`Row row) steps
          | AMode _ -> unsupported "a side in a mode argument")
       | Field name, (TRecord { row; _ } | TVariant { row; _ }) ->
         walk (`Ty (field row name)) steps
       | Code, TCode { inner; _ } -> walk (`Ty inner) steps
       | _ -> unsupported "a side not in its type")
    | Field name :: steps, `Row row -> walk (`Ty (field row name)) steps
    | _, `Row _ -> unsupported "a side in a row"
  in
  match Type.canonical_ty (walk (`Ty ty) side.steps) with
  | TArrow (_, _, modes) ->
    (match side.side with
     | `Arg -> modes.arg
     | `Res -> modes.res)
  | _ -> unsupported "a side not of an arrow"
;;

let prim_of_lit : Syntax.Lit.t -> Prim.Value.t = function
  | Int n -> Int n
  | Bool b -> Bool b
  | String s -> String s
  | Char c -> Char c
  | Unit -> Unit
;;

let local = Mode.Locality.index Local

(* The bounds of [node] with [pins] holding. *)
let bounds pins node =
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
        "the instances used from can't all hold"
          ~pins:
            (Map.data pins
             |> List.map ~f:(fun (node, value) ->
               ( Mode.Var.Node.var_id node
               , value
               , Mode.Var.Node.lower node
               , Mode.Var.Node.upper node ))
             : (int * int * int * int) list)]
;;

(* The mode variables of [scheme], each with what it is at in [ty], an instance
   of it: another variable, or a fixed locality. *)
let instance_vars ~scheme ~ty =
  let pairs = Queue.create () in
  let seen = Int.Hash_set.create () in
  let side (s : Arrow_modes.Side.t) (s' : Arrow_modes.Side.t) =
    match s, s' with
    | Var var, Var var' -> Queue.enqueue pairs (var, `Var var')
    | Var var, Fixed const ->
      Queue.enqueue pairs (var, `Fixed (Mode.Locality.index const.locality))
    | Fixed _, _ -> ()
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
    | TTuple tys, TTuple tys' -> List.iter2_exn tys tys' ~f:walk
    | TCon (_, args), TCon (_, args') ->
      List.iter2_exn args args' ~f:(fun (arg : Type.arg) (arg' : Type.arg) ->
        match arg, arg' with
        | ATy ty, ATy ty' -> walk ty ty'
        | ARow row, ARow row' -> rows row row'
        (* A mode argument has no locality. *)
        | AMode _, AMode _ -> ()
        | _ -> unsupported "an instance of another shape")
    | TCode { inner; _ }, TCode { inner = inner'; _ } -> walk inner inner'
    | ( (TRecord { id; row } | TVariant { id; row })
      , (TRecord { row = row'; _ } | TVariant { row = row'; _ }) ) ->
      if not (Hash_set.mem seen (-id - 1))
      then (
        Hash_set.add seen (-id - 1);
        rows row row')
    | _ -> unsupported "an instance of another shape"
  and rows (row : Type.row) (row' : Type.row) =
    match row with
    | RExtend (name, _, _, ty, rest) ->
      let rec find (row' : Type.row) =
        match Type.canonical_row row' with
        | RExtend (name', _, _, ty', rest') ->
          if String.equal name name' then Some ty' else find rest'
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

(* The instance of [owner] used at type [ty], from [instance]: each of its mode
   variables as local as it may be there. *)
let instantiate t (instance : Instance.t) ~owner ~scheme ~ty =
  let key = owner_key owner in
  (* Each variable of the scheme as local as both its own bounds, given those
     chosen so far, and how it is used allow. *)
  let pins =
    List.fold
      (instance_vars ~scheme ~ty)
      ~init:instance.pins
      ~f:(fun pins (var, var') ->
        let node = Mode.Var.node var Locality in
        let lower, upper = bounds pins node in
        let used =
          match var' with
          | `Var var' ->
            snd (bounds instance.pins (Mode.Var.node var' Locality))
          | `Fixed used -> used
        in
        Map.set
          pins
          ~key:(Mode.Var.Node.var_id node)
          ~data:(node, Int.max lower (Int.min used upper)))
  in
  let sides = Hashtbl.find t.sides key |> Option.value ~default:[] in
  { Instance.pins
  ; local =
      List.filter_map sides ~f:(fun side ->
        let value =
          match side_of scheme side with
          | Fixed const -> Mode.Locality.index const.locality
          | Var var -> snd (bounds pins (Mode.Var.node var Locality))
        in
        if value >= local then Some (key, side) else None)
      @ instance.local
  }
;;

let partials t (loc : Loc.t) =
  Hashtbl.find t.sites (Partial_application, loc) |> Option.value ~default:[]
;;

let closure t ctx ~kind ~loc ~params ~body ~env ~partials_at ~owner =
  Obj
    (make
       t
       ctx
       kind
       loc
       (Closure
          { params
          ; body
          ; env
          ; instance = ctx.instance
          ; partials = partials t partials_at
          ; owner
          }))
;;

let tick t =
  t.fuel <- t.fuel - 1;
  if t.fuel < 0 then unsupported "out of fuel"
;;

let rec toplevel t (ident : Typed.Ident.t) =
  match Hashtbl.find t.values ident.name with
  | Some value -> value
  | None ->
    let (top : Typed.Toplevel.t) =
      match Hashtbl.find t.toplevels ident.name with
      | Some top -> top
      | None -> unsupported "toplevel %s" ident.name
    in
    let owner = { Allocation.Owner.toplevel = top.ident; lets = [] } in
    let ctx =
      { region = Region.heap
      ; caller = Region.heap
      ; current = Region.heap
      ; instance = Instance.empty
      ; owner
      }
    in
    let value =
      match top.args with
      | _ :: _ ->
        t.next_id <- t.next_id + 1;
        Obj
          { id = t.next_id
          ; region = Region.heap
          ; site = "toplevel " ^ ident.name
          ; payload =
              Closure
                { params = top.args
                ; body = top.body
                ; env = Typed.Ident.Map.empty
                ; instance = Instance.empty
                ; partials = partials t top.body.loc
                ; owner
                }
          }
      | [] ->
        let value = eval t ctx Typed.Ident.Map.empty top.body ~tail:false in
        if top.mut then unsupported "a mutable toplevel" else value
    in
    Hashtbl.set t.values ~key:ident.name ~data:value;
    value

and reference t ctx env (expr : Typed.Expr.t) (var : Typed.Var_ref.t) =
  match var with
  | Bound (Local ident) | Persistent (Local ident) ->
    (match Map.find env ident with
     | None -> unsupported "unbound %s" ident.name
     | Some binding ->
       (match !binding with
        | Value (Obj ({ payload = Cell cell; _ } as obj)) ->
          live obj ~what:"a read";
          !cell
        | Value value -> value
        | Generic_value { body; env; ctx = at; owner; scheme } ->
          let instance =
            instantiate t ctx.instance ~owner ~scheme ~ty:expr.ty
          in
          eval t { at with instance } env body ~tail:false
        | Generic_function { obj; owner; scheme } ->
          (match obj.payload with
           | Closure closure ->
             let instance =
               instantiate t ctx.instance ~owner ~scheme ~ty:expr.ty
             in
             Obj { obj with payload = Closure { closure with instance } }
           | Box _ | Ptr _ | Cell _ -> assert false)))
  | Bound (Toplevel ident) | Persistent (Toplevel ident) ->
    (match toplevel t ident with
     | Obj ({ payload = Closure closure; _ } as obj) ->
       let top = Hashtbl.find_exn t.toplevels ident.name in
       let instance =
         instantiate
           t
           ctx.instance
           ~owner:{ toplevel = top.ident; lets = [] }
           ~scheme:top.scheme
           ~ty:expr.ty
       in
       Obj { obj with payload = Closure { closure with instance } }
     | value -> value)
  | Prim prim -> Prim_fn (prim, [])
  | Free name -> unsupported "free %s" name
  | Intrinsic _ -> unsupported "an intrinsic not applied"

and apply t ctx value arg ~tail =
  match value with
  | Prim_fn (prim, applied) ->
    let applied = arg :: applied in
    if List.length applied < Prim.arity prim
    then Prim_fn (prim, applied)
    else (
      let args =
        List.rev_map applied ~f:(function
          | Prim_value value -> value
          | _ -> unsupported "a prim of a non-prim value")
      in
      match Prim.apply prim ~output:ignore args with
      | Ok value -> Prim_value value
      | Error _ -> unsupported "a prim raising")
  | Obj ({ payload = Closure closure; _ } as obj) ->
    live obj ~what:"a call";
    (* A tail call ends the region of the function making it first. *)
    let caller =
      if tail && t.tail_calls_end_regions
      then (
        Region.free ctx.region;
        ctx.caller)
      else ctx.current
    in
    (match closure.params with
     | [] -> assert false
     | [ param ] ->
       let region =
         Region.create (sprintf "the region of a call of %s" obj.site)
       in
       let env = Map.set closure.env ~key:param ~data:(ref (Value arg)) in
       let result =
         eval
           t
           { region
           ; caller
           ; current = region
           ; instance = closure.instance
           ; owner = closure.owner
           }
           env
           closure.body
           ~tail:true
       in
       Region.free region;
       result
     | param :: params ->
       let partial, partials =
         match closure.partials with
         | partial :: partials -> partial, partials
         | [] -> unsupported "no partial application site"
       in
       let ctx' = { ctx with instance = closure.instance } in
       (match placement ctx' partial with
        | Region -> unsupported "a partial application in its own region"
        | Heap -> t.on_heap <- t.on_heap + 1
        | Caller_region -> t.in_regions <- t.in_regions + 1);
       t.next_id <- t.next_id + 1;
       Obj
         { id = t.next_id
         ; region =
             (match placement ctx' partial with
              | Heap -> Region.heap
              | Region | Caller_region -> caller)
         ; site = describe partial
         ; payload =
             Closure
               { closure with
                 params
               ; env = Map.set closure.env ~key:param ~data:(ref (Value arg))
               ; partials
               }
         })
  | _ -> unsupported "applying a non-function"

and eval t ctx env (expr : Typed.Expr.t) ~tail : Value.t =
  tick t;
  let eval' ?(ctx = ctx) ?(env = env) ?(tail = false) expr =
    eval t ctx env expr ~tail
  in
  match expr.desc with
  | Lit lit -> Prim_value (prim_of_lit lit)
  | Var var -> reference t ctx env expr var
  | App ({ desc = Var var; _ }, arg) when Option.is_some (allocates var) ->
    let arg = eval' arg in
    (match allocates var with
     | Some Box -> Obj (make t ctx Box expr.loc (Box arg))
     | Some _ -> Obj (make t ctx Pointer expr.loc (Ptr (ref arg)))
     | None -> assert false)
  | App (f, arg) ->
    let f = eval' f in
    let arg = eval' arg in
    apply t ctx f arg ~tail
  | Let { ident; args; body; in_; generalized; _ } ->
    let binding = ref (Value unit) in
    let env' = Map.set env ~key:ident ~data:binding in
    let owner () =
      { ctx.owner with lets = ctx.owner.lets @ [ ident.name, expr.loc ] }
    in
    let scheme () = (Map.find_exn t.ident_info ident).ty in
    (match args with
     | _ :: _ ->
       let obj =
         match
           closure
             t
             ctx
             ~kind:Closure
             ~loc:expr.loc
             ~params:args
             ~body
             ~env:env'
             ~partials_at:expr.loc
             ~owner:(if generalized then owner () else ctx.owner)
         with
         | Obj obj -> obj
         | _ -> assert false
       in
       binding
       := if generalized
          then Generic_function { obj; owner = owner (); scheme = scheme () }
          else Value (Obj obj)
     | [] ->
       binding
       := if generalized
          then
            Generic_value
              { body
              ; env
              ; ctx = { ctx with owner = owner () }
              ; owner = owner ()
              ; scheme = scheme ()
              }
          else Value (eval' body));
    eval' ~env:env' ~tail in_
  | Let_mut { ident; body; in_; _ } ->
    let value = eval' body in
    let cell = make t ctx Cell expr.loc (Cell (ref value)) in
    eval' ~env:(Map.set env ~key:ident ~data:(ref (Value (Obj cell)))) ~tail in_
  | Assign (Bound (Local ident), value) ->
    let value = eval' value in
    (match Option.map (Map.find env ident) ~f:( ! ) with
     | Some (Value (Obj ({ payload = Cell cell; _ } as obj))) ->
       live obj ~what:"an assignment";
       cell := value;
       unit
     | _ -> unsupported "assigning a non-cell")
  | Assign _ -> unsupported "assigning a toplevel"
  | Fun { args; body; _ } ->
    closure
      t
      ctx
      ~kind:Closure
      ~loc:expr.loc
      ~params:args
      ~body
      ~env
      ~partials_at:expr.loc
      ~owner:ctx.owner
  | Annotated (expr, _) -> eval' ~tail expr
  | Marked (Nontail, expr) -> eval' expr
  | Marked (Stack, expr) -> eval' ~tail expr
  | Marked (Region, expr) ->
    let region =
      Region.create (sprintf "the region_ at %s" (Loc.to_string expr.loc))
    in
    let value =
      eval'
        ~ctx:{ ctx with region; caller = ctx.current; current = region }
        expr
    in
    Region.free region;
    value
  | Marked (Exclave, expr) ->
    Region.free ctx.region;
    eval' ~ctx:{ ctx with current = ctx.caller } ~tail expr
  | Tuple exprs -> Tuple (List.map exprs ~f:(fun expr -> eval' expr))
  | Construct (constructor, exprs) ->
    Construct (constructor, List.map exprs ~f:(fun expr -> eval' expr))
  | Record fields ->
    let values =
      match fields with
      | [] -> [||]
      | (field, _) :: _ ->
        Array.create ~len:(Array.length field.record.fields) unit
    in
    List.iter fields ~f:(fun (field, expr) ->
      values.(field.index) <- eval' expr);
    Record values
  | Field (record, field) ->
    (match eval' record with
     | Record values -> values.(field.index)
     | _ -> unsupported "a field of a non-record")
  | Poly_record fields ->
    Poly_record
      (String.Map.of_alist_reduce
         (List.map fields ~f:(fun (name, expr) -> name, eval' expr))
         ~f:(fun first _ -> first))
  | Poly_field (record, name) ->
    (match eval' record with
     | Poly_record fields -> Map.find_exn fields name
     | _ -> unsupported "a field of a non-record")
  | Poly_variant (tag, arg) ->
    Poly_variant (tag, Option.value_map arg ~default:unit ~f:(fun e -> eval' e))
  | Deref expr ->
    (match eval' expr with
     | Obj ({ payload = Box value | Ptr { contents = value }; _ } as obj) ->
       live obj ~what:"a read";
       value
     | _ -> unsupported "dereferencing a non-pointer")
  | Store (place, value) ->
    (* The cell stored into, and the path of fields in what it holds. *)
    let rec path (place : Typed.Expr.t) fields =
      match place.desc with
      | Deref pointer -> eval' pointer, fields
      | Field (record, field) -> path record (`Field field.index :: fields)
      | Poly_field (record, name) -> path record (`Poly name :: fields)
      | _ -> unsupported "storing into a non-path"
    in
    let pointer, fields = path place [] in
    let value = eval' value in
    let rec update old = function
      | [] -> value
      | `Field index :: fields ->
        (match old with
         | Record values ->
           let values = Array.copy values in
           values.(index) <- update values.(index) fields;
           Record values
         | _ -> unsupported "a field of a non-record")
      | `Poly name :: fields ->
        (match old with
         | Poly_record values ->
           Poly_record
             (Map.set
                values
                ~key:name
                ~data:(update (Map.find_exn values name) fields))
         | _ -> unsupported "a field of a non-record")
    in
    (match pointer with
     | Obj ({ payload = Ptr cell; _ } as obj) ->
       live obj ~what:"a store";
       cell := update !cell fields;
       unit
     | _ -> unsupported "storing into a non-pointer")
  | Match { scrutinee; cases; exception_cases = [] } ->
    let value = eval' scrutinee in
    (match
       List.find_map cases ~f:(fun (pattern, body) ->
         Option.map (matches env pattern value) ~f:(fun env -> env, body))
     with
     | Some (env, body) -> eval' ~env ~tail body
     | None -> unsupported "no case matches")
  | Match _
  | Record_update _
  | Poly_extend _
  | Poly_restrict _
  | Poly_update _
  | Splice _
  | Toplevel_splice _
  | Quote _
  | Expect _
  | External _
  | Type _
  | Error _ ->
    unsupported
      !"%{Sexp}"
      (Typed.Expr.Desc.sexp_of_t (fun _ -> Atom "_") expr.desc)

and allocates (var : Typed.Var_ref.t) : Allocation.Kind.t option =
  match var with
  | Intrinsic Box_make -> Some Box
  | Intrinsic Ptr_make -> Some Pointer
  | Bound (Toplevel ident) | Persistent (Toplevel ident) ->
    if String.equal ident.name (Prelude_path.canonical "Box.make")
    then Some Box
    else if String.equal ident.name (Prelude_path.canonical "Ptr.make")
    then Some Pointer
    else None
  | _ -> None

and matches env (pattern : Typed.Pattern.t) (value : Value.t) =
  match pattern.desc, value with
  | Any, _ -> Some env
  | Var ident, _ -> Some (Map.set env ~key:ident ~data:(ref (Value value)))
  | Lit lit, Prim_value value ->
    if [%equal: Sexp.t]
         [%sexp (prim_of_lit lit : Prim.Value.t)]
         [%sexp (value : Prim.Value.t)]
    then Some env
    else None
  | Tuple patterns, Tuple values -> all env patterns values
  | ( Deref pattern
    , Obj ({ payload = Box value | Ptr { contents = value }; _ } as obj) ) ->
    live obj ~what:"a match";
    matches env pattern value
  | Construct (constructor, patterns), Construct (constructor', values) ->
    if String.equal constructor.name constructor'.name
    then all env patterns values
    else None
  | Record fields, Record values ->
    all
      env
      (List.map fields ~f:snd)
      (List.map fields ~f:(fun (field, _) -> values.(field.index)))
  | Poly_variant (tag, pattern), Poly_variant (tag', value) ->
    if String.equal tag tag'
    then (
      match pattern with
      | None -> Some env
      | Some pattern -> matches env pattern value)
    else None
  | Poly_record { fields; rest }, Poly_record values ->
    let env =
      all
        env
        (List.map fields ~f:snd)
        (List.map fields ~f:(fun (name, _) -> Map.find_exn values name))
    in
    (match rest with
     | Closed | Open -> env
     | Bind ident ->
       Option.map env ~f:(fun env ->
         let rest =
           List.fold fields ~init:values ~f:(fun values (name, _) ->
             Map.remove values name)
         in
         Map.set env ~key:ident ~data:(ref (Value (Poly_record rest)))))
  | _ -> unsupported "a pattern of another shape"

and all env patterns values =
  List.fold2_exn patterns values ~init:(Some env) ~f:(fun env pattern value ->
    Option.bind env ~f:(fun env -> matches env pattern value))
;;

(* What [values] reach, failing at an allocation of a region that has ended. *)
let reachable values =
  let seen = Int.Hash_set.create () in
  let rec value = function
    | Prim_value _ | Prim_fn _ -> ()
    | Tuple values | Construct (_, values) -> List.iter values ~f:value
    | Record values -> Array.iter values ~f:value
    | Poly_record values -> Map.iter values ~f:value
    | Poly_variant (_, v) -> value v
    | Obj obj ->
      if not (Hash_set.mem seen obj.id)
      then (
        Hash_set.add seen obj.id;
        live obj ~what:"a reference, from a toplevel,";
        match obj.payload with
        | Box v -> value v
        | Ptr cell | Cell cell -> value !cell
        | Closure closure -> env closure.env ~body:closure.body)
  and env env ~body =
    Map.iteri env ~f:(fun ~key:ident ~data:binding ->
      if Typed.Expr.refers_to body ~f:(fun binder ->
           Typed.Binder.equal binder (Local ident))
      then bound !binding)
  and bound = function
    | Value v -> value v
    | Generic_value { env = e; body; _ } -> env e ~body
    | Generic_function { obj; _ } -> value (Obj obj)
  in
  List.iter values ~f:value
;;

let run ?(override = Fn.id) ?(tail_calls_end_regions = true) source ~entry =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let typecheck = Typecheck.create ~stamps ~interpreter () in
  match Typecheck.typecheck typecheck ~program:(program source) with
  | exception exn -> Outcome.Rejected [ Exn.to_string exn ]
  | Error { errors; partial = _ } ->
    Outcome.Rejected (List.map errors ~f:Typecheck_error.message)
  | Ok { program; _ } ->
    let t =
      { sites = Hashtbl.Poly.create ()
      ; sides = String.Table.create ()
      ; toplevels = String.Table.create ()
      ; values = String.Table.create ()
      ; ident_info = program.ident_info
      ; next_id = 0
      ; fuel = 1_000_000
      ; on_heap = 0
      ; in_regions = 0
      ; tail_calls_end_regions
      }
    in
    List.iter program.allocations ~f:(fun allocation ->
      let allocation = override allocation in
      Hashtbl.add_multi
        t.sites
        ~key:(allocation.kind, allocation.loc)
        ~data:allocation;
      Option.iter allocation.follows ~f:(fun { owner; side; _ } ->
        let key = owner_key owner in
        let sides = Hashtbl.find t.sides key |> Option.value ~default:[] in
        if not (List.mem sides side ~equal:Allocation.Side.equal)
        then Hashtbl.set t.sides ~key ~data:(sides @ [ side ])));
    Hashtbl.map_inplace t.sites ~f:List.rev;
    List.iter program.sccs ~f:(fun scc ->
      List.iter scc.members ~f:(fun (top : Typed.Toplevel.t) ->
        Hashtbl.set t.toplevels ~key:top.ident.name ~data:top));
    (match Hashtbl.find t.toplevels entry with
     | None -> Outcome.Unsupported ("no toplevel " ^ entry)
     | Some top ->
       (try
          let f = toplevel t top.ident in
          let result =
            apply
              t
              { region = Region.heap
              ; caller = Region.heap
              ; current = Region.heap
              ; instance = Instance.empty
              ; owner = { toplevel = top.ident; lets = [] }
              }
              f
              unit
              ~tail:false
          in
          reachable (Hashtbl.data t.values);
          Outcome.Returned
            { value = Value.to_string result
            ; on_heap = t.on_heap
            ; in_regions = t.in_regions
            }
        with
        | Use_after_free message -> Outcome.Use_after_free message
        | Unsupported message -> Outcome.Unsupported message))
;;
