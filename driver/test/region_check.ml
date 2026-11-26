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
    | Runtime_error of string
    | Unsupported of string
    | Rejected of string list
  [@@deriving sexp_of]
end

exception Use_after_free of string
exception Runtime_error of string
exception Unsupported of string

let unsupported fmt = ksprintf (fun s -> raise (Unsupported s)) fmt

module Region = struct
  type t =
    { name : string
    ; mutable live : bool
    }

  let heap = { name = "the heap"; live = true }
  let create name = { name; live = true }
  let is_heap t = phys_equal t heap
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

let owner_key ({ toplevel; lets } : Allocation.Owner.t) =
  Sexp.to_string
    [%sexp
      (toplevel.name : string)
      , (List.map lets ~f:(fun let_ -> let_.id) : Allocation.Id.t list)]
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
    | Prim_fn of Prim.t * Allocation.t option
    (** A prim named as a value, with what applying it to too few arguments
        makes. *)
    | Allocator of Allocation.t
    (** [Box.make] or [Ptr.make] named as a value, with what applying it makes. *)

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
    | Prim_partial of Prim.t * t list * Allocation.t
    (** The arguments given so far, latest first, and what it is made by. *)

  and closure =
    { params : Typed.Ident.t list
    ; body : Typed.Expr.t
    ; env : env
    ; instance : Instance.t
    ; partials : Allocation.t list
    (** Of the applications still to come, given 1, 2, ... arguments. *)
    ; owner : Allocation.Owner.t
    }

  and binding =
    | Undefined (** A let's own binder, while its value is evaluated. *)
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
    ; running : (obj * env) option
    (** The closure running, and what it captures. *)
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
  { sites : Allocation.t Allocation.Id.Table.t
  ; sides : (string, Allocation.Side.t list) Hashtbl.t
  ; toplevels : (string, Typed.Toplevel.t) Hashtbl.t
  ; values : (string, Value.t) Hashtbl.t
  (** Of each toplevel: a function's closure, or a value of each instance. *)
  ; ident_info : Typed.Ident_info.t Typed.Ident.Map.t
  ; mutable next_id : int
  ; mutable fuel : int
  ; mutable on_heap : int
  ; mutable in_regions : int
  ; tail_calls_end_regions : bool
  }

let site t id =
  match Hashtbl.find t.sites id with
  | Some allocation -> allocation
  | None -> unsupported !"no site %{Sexp}" [%sexp (id : Allocation.Id.t)]
;;

(* What [expr] makes: [unsupported] if it makes nothing. *)
let made t (expr : Typed.Expr.t) =
  match expr.made.made with
  | Some id -> site t id
  | None -> unsupported "no site at %s" (Loc.to_string expr.loc)
;;

(* A [Box.make] or [Ptr.make] application. *)
let is_block t (expr : Typed.Expr.t) =
  match expr.desc, expr.made.made with
  | App _, Some id ->
    (match (site t id).kind with
     | Box | Pointer -> true
     | Closure | Partial_application | Code | Cell -> false)
  | _ -> false
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

(* The allocations [value] points to, not through another allocation. *)
let rec pointees value ~f =
  match value with
  | Prim_value _ | Prim_fn _ | Allocator _ -> ()
  | Tuple values | Construct (_, values) -> List.iter values ~f:(pointees ~f)
  | Record values -> Array.iter values ~f:(pointees ~f)
  | Poly_record values -> Map.iter values ~f:(pointees ~f)
  | Poly_variant (_, value) -> pointees value ~f
  | Obj obj -> f obj
;;

let escapes value ~what = pointees value ~f:(live ~what)

(* What a closure with [env] running [body] holds. *)
let rec captured env ~body =
  Map.fold env ~init:[] ~f:(fun ~key:ident ~data:binding values ->
    if Typed.Expr.refers_to body ~f:(fun binder ->
         Typed.Binder.equal binder (Local ident))
    then (
      match !binding with
      | Undefined -> values
      | Value value -> value :: values
      | Generic_value { env; body; _ } ->
        captured (Map.remove env ident) ~body @ values
      | Generic_function { obj; _ } -> Obj obj :: values)
    else values)
;;

let held = function
  | Box value -> [ value ]
  | Ptr cell | Cell cell -> [ !cell ]
  | Closure closure -> captured closure.env ~body:closure.body
  | Prim_partial (_, args, _) -> args
;;

(* Each allocation [values] reach, once. *)
let iter_reached values ~f =
  let seen = Int.Hash_set.create () in
  let rec value v =
    pointees v ~f:(fun obj ->
      if not (Hash_set.mem seen obj.id)
      then (
        Hash_set.add seen obj.id;
        f obj;
        List.iter (held obj.payload) ~f:value))
  in
  List.iter values ~f:value
;;

let alloc
  t
  (allocation : Allocation.t)
  ~(at : Region.t)
  ~caller
  ?(also = [])
  payload
  (placement : Allocation.Placement.t)
  =
  t.next_id <- t.next_id + 1;
  (match placement with
   | Heap -> t.on_heap <- t.on_heap + 1
   | Region | Caller_region -> t.in_regions <- t.in_regions + 1);
  let region =
    match placement with
    | Heap -> Region.heap
    | Region -> at
    | Caller_region -> caller
  in
  let site = describe allocation in
  List.iter
    (also @ held payload)
    ~f:(escapes ~what:(sprintf "a pointer, from the %s," site));
  { id = t.next_id; region; site; payload }
;;

(* What toplevels reach outlives every region: it is all on the heap. *)
let check_globals t =
  iter_reached (Hashtbl.data t.values) ~f:(fun obj ->
    live obj ~what:"a reference, from a toplevel,";
    if not (Region.is_heap obj.region)
    then
      raise
        (Use_after_free
           (sprintf
              "a reference, from a toplevel, to the %s, made in %s, which ends \
               first"
              obj.site
              obj.region.name)))
;;

let free t (region : Region.t) =
  region.live <- false;
  check_globals t
;;

let make ?also t ctx (allocation : Allocation.t) payload =
  alloc
    t
    allocation
    ~at:ctx.region
    ~caller:ctx.caller
    ?also
    payload
    (placement ctx allocation)
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
          | Fixed { locality; _ } | Var_at { locality; _ } ->
            Mode.Locality.index locality
          | Var var -> snd (bounds pins (Mode.Var.node var Locality))
        in
        if value >= local then Some (key, side) else None)
      @ instance.local
  }
;;

(* A function whose body is a function is one function of the parameters of
   both, as the typechecker has it. *)
let rec merged params (body : Typed.Expr.t) =
  match body.desc with
  | Fun { args; body; _ } -> merged (params @ Nonempty_list.to_list args) body
  | _ -> params, body
;;

(* The closure [expr] makes. *)
let closure t ctx (expr : Typed.Expr.t) ~params ~body ~env ~owner =
  let params, body = merged params body in
  Obj
    (make
       t
       ctx
       (made t expr)
       (Closure
          { params
          ; body
          ; env
          ; instance = ctx.instance
          ; partials = List.map expr.made.partials ~f:(site t)
          ; owner
          }))
;;

let tick t =
  t.fuel <- t.fuel - 1;
  if t.fuel < 0 then unsupported "out of fuel"
;;

let toplevel_owner (top : Typed.Toplevel.t) =
  { Allocation.Owner.toplevel = top.ident; lets = [] }
;;

(* The toplevel [ident] used at type [ty] from [instance]: a function's closure,
   of that instance, or the value of that instance, each instance of a value
   evaluated once (a compiler makes a copy of the value for each way its sites
   are made). Without [ty], of its least instance, as the entry is. *)
let rec toplevel ?ty t (instance : Instance.t) (ident : Typed.Ident.t) =
  let (top : Typed.Toplevel.t) =
    match Hashtbl.find t.toplevels ident.name with
    | Some top -> top
    | None -> unsupported "toplevel %s" ident.name
  in
  let owner = toplevel_owner top in
  let instance =
    match ty with
    | Some ty -> instantiate t instance ~owner ~scheme:top.scheme ~ty
    | None -> instance
  in
  let memo key ~f =
    match Hashtbl.find t.values key with
    | Some value -> value
    | None ->
      let value = f () in
      Hashtbl.set t.values ~key ~data:value;
      check_globals t;
      value
  in
  match top.kind with
  | Function args ->
    (match
       memo ident.name ~f:(fun () ->
         t.next_id <- t.next_id + 1;
         let params, body = merged (Nonempty_list.to_list args) top.body in
         Obj
           { id = t.next_id
           ; region = Region.heap
           ; site = "toplevel " ^ ident.name
           ; payload =
               Closure
                 { params
                 ; body
                 ; env = Typed.Ident.Map.empty
                 ; instance = Instance.empty
                 ; partials = List.map top.partials ~f:(site t)
                 ; owner
                 }
           })
     with
     | Obj ({ payload = Closure closure; _ } as obj) ->
       Obj { obj with payload = Closure { closure with instance } }
     | value -> value)
  | (Value | Mut) as kind ->
    let key = owner_key owner in
    let own =
      List.filter instance.local ~f:(fun (owner, _) -> String.equal owner key)
    in
    memo
      (sprintf
         !"%s %{Sexp}"
         ident.name
         [%sexp (List.map own ~f:snd : Allocation.Side.t list)])
      ~f:(fun () ->
        (* An initializer's region ends when it does. *)
        let region =
          Region.create (sprintf "the initializer of %s" ident.name)
        in
        let ctx =
          { region
          ; caller = Region.heap
          ; current = region
          ; instance = { instance with local = own }
          ; owner
          ; running = None
          }
        in
        let value = eval t ctx Typed.Ident.Map.empty top.body ~tail:false in
        free t region;
        (match kind with
         | Mut -> unsupported "a mutable toplevel"
         | Value | Function _ -> ());
        value)

and reference t ctx env (expr : Typed.Expr.t) (var : Typed.Var_ref.t) =
  match var, expr.made.made with
  | Prim prim, made -> Prim_fn (prim, Option.map made ~f:(site t))
  | _, Some id -> Allocator (site t id)
  | (Bound (Local ident) | Persistent (Local ident)), None ->
    (match Map.find env ident with
     | None -> unsupported "unbound %s" ident.name
     | Some binding ->
       Option.iter ctx.running ~f:(fun (obj, captures) ->
         match Map.find captures ident with
         | Some captured when phys_equal captured binding ->
           live obj ~what:"a read of what it captures, by the closure"
         | Some _ | None -> ());
       (match !binding with
        | Undefined ->
          raise
            (Runtime_error
               (sprintf
                  "%s is used before its value is defined (a value may not \
                   depend on itself), at %s"
                  ident.name
                  (Loc.to_string expr.loc)))
        | Value (Obj ({ payload = Cell cell; _ } as obj)) ->
          live obj ~what:"a read";
          !cell
        | Value value -> value
        | Generic_value { body; env; ctx = at; owner; scheme } as generic ->
          let instance =
            instantiate t ctx.instance ~owner ~scheme ~ty:expr.ty
          in
          (* Each use evaluates it again, standing for its one evaluation: using
             it in that is using it before it is defined. *)
          binding := Undefined;
          let value = eval t { at with instance } env body ~tail:false in
          binding := generic;
          value
        | Generic_function { obj; owner; scheme } ->
          (match obj.payload with
           | Closure closure ->
             let instance =
               instantiate t ctx.instance ~owner ~scheme ~ty:expr.ty
             in
             Obj { obj with payload = Closure { closure with instance } }
           | Box _ | Ptr _ | Cell _ | Prim_partial _ -> assert false)))
  | (Bound (Toplevel ident) | Persistent (Toplevel ident)), None ->
    toplevel t ctx.instance ident ~ty:expr.ty
  | Free name, None -> unsupported "free %s" name
  | Intrinsic _, None -> unsupported "an intrinsic not applied"

(* [prim] given [applied] (latest first) then [args]: a closure made by [site]
   while too few. *)
and apply_prim t ctx prim applied args ~site =
  let rec take applied = function
    | [] -> applied, []
    | arg :: args when List.length applied < Prim.arity prim ->
      take (arg :: applied) args
    | args -> applied, args
  in
  let applied, rest = take applied args in
  if List.length applied < Prim.arity prim
  then (
    match site with
    | Some site -> Obj (make t ctx site (Prim_partial (prim, applied, site)))
    | None -> unsupported "a partial application of a prim without a site")
  else (
    let values =
      List.rev_map applied ~f:(function
        | Prim_value value -> value
        | _ -> unsupported "a prim of a non-prim value")
    in
    let result =
      match Prim.apply prim ~output:ignore values with
      | Ok value -> Prim_value value
      | Error _ -> unsupported "a prim raising"
    in
    match rest with
    | [] -> result
    | _ :: _ -> unsupported "applying a prim's result")

and apply t ctx value args ~tail =
  match args with
  | [] -> value
  | arg :: rest ->
    (match value with
     | Prim_fn (prim, site) -> apply_prim t ctx prim [] args ~site
     | Obj ({ payload = Prim_partial (prim, applied, site); _ } as obj) ->
       live obj ~what:"a call";
       apply_prim t ctx prim applied args ~site:(Some site)
     | Allocator allocation ->
       apply t ctx (block t ctx allocation arg) rest ~tail
     | Obj ({ payload = Closure closure; _ } as obj) ->
       let given = List.length args in
       if given < List.length closure.params
       then partial t ctx obj closure args ~tail
       else (
         let now = List.take args (List.length closure.params) in
         let rest = List.drop args (List.length closure.params) in
         let result =
           call t ctx obj closure now ~tail:(tail && List.is_empty rest)
         in
         apply t ctx result rest ~tail)
     | _ -> unsupported "applying a non-function")

(* [closure], of [obj], given all its parameters. *)
and call t ctx obj closure args ~tail =
  (* A tail call ends the region of the function making it first. *)
  let caller =
    if tail && t.tail_calls_end_regions
    then (
      free t ctx.region;
      ctx.caller)
    else ctx.current
  in
  live obj ~what:"a call";
  if tail
  then
    List.iter args ~f:(fun arg ->
      escapes arg ~what:"a reference, from an argument of a tail call,");
  let region = Region.create (sprintf "the region of a call of %s" obj.site) in
  let env =
    List.fold2_exn
      closure.params
      args
      ~init:closure.env
      ~f:(fun env param arg -> Map.set env ~key:param ~data:(ref (Value arg)))
  in
  let result =
    eval
      t
      { region
      ; caller
      ; current = region
      ; instance = closure.instance
      ; owner = closure.owner
      ; running = Some (obj, closure.env)
      }
      env
      closure.body
      ~tail:true
  in
  free t region;
  escapes
    result
    ~what:(sprintf "a reference, from the result of a call of %s," obj.site);
  result

(* [closure], of [obj], given too few of its parameters: a closure. *)
and partial t ctx obj closure args ~tail =
  let given = List.length args in
  let partial = List.nth closure.partials (given - 1) in
  let partial =
    match partial with
    | Some partial -> partial
    | None -> unsupported "no partial application site"
  in
  let caller =
    if tail && t.tail_calls_end_regions
    then (
      free t ctx.region;
      ctx.caller)
    else ctx.current
  in
  live obj ~what:"a partial application";
  let ctx' = { ctx with instance = closure.instance } in
  let placement = placement ctx' partial in
  (match placement with
   | Region -> unsupported "a partial application in its own region"
   | Heap | Caller_region -> ());
  let params = List.take closure.params given in
  let unapplied = List.drop closure.params given in
  Obj
    (alloc
       t
       partial
       ~at:caller
       ~caller
       ~also:[ Obj obj ]
       (Closure
          { closure with
            params = unapplied
          ; env =
              List.fold2_exn params args ~init:closure.env ~f:(fun env p a ->
                Map.set env ~key:p ~data:(ref (Value a)))
          ; partials = List.drop closure.partials given
          })
       placement)

and eval t ctx env (expr : Typed.Expr.t) ~tail : Value.t =
  tick t;
  let eval' ?(ctx = ctx) ?(env = env) ?(tail = false) expr =
    eval t ctx env expr ~tail
  in
  match expr.desc with
  | Lit lit -> Prim_value (prim_of_lit lit)
  | Var var -> reference t ctx env expr var
  | App (_, arg) when is_block t expr -> block t ctx (made t expr) (eval' arg)
  | App _ ->
    (* A function given several arguments at once is called with them all, as
       compiled code would, making no closure in between. *)
    let rec spine (expr : Typed.Expr.t) args =
      match expr.desc with
      | App (f, arg) when not (is_block t expr) -> spine f (arg :: args)
      | _ -> expr, args
    in
    let head, args = spine expr [] in
    (match head.desc with
     | Var (Prim prim) ->
       let args = List.map args ~f:(fun arg -> eval' arg) in
       apply_prim
         t
         ctx
         prim
         []
         args
         ~site:(Option.map expr.made.made ~f:(site t))
     | _ ->
       let f = eval' head in
       let args = List.map args ~f:(fun arg -> eval' arg) in
       apply t ctx f args ~tail)
  | Toplevel_splice { expansion; _ } -> eval' expansion ~tail
  | Seq (first, second) ->
    ignore (eval' first : Value.t);
    eval' second ~tail
  | Let { ident; args; body; in_; generalized; _ } ->
    let binding = ref Undefined in
    let env' = Map.set env ~key:ident ~data:binding in
    let owner () =
      match expr.made.owns with
      | Some id ->
        { ctx.owner with
          lets = ctx.owner.lets @ [ { id; name = ident.name; loc = expr.loc } ]
        }
      | None -> unsupported "a generalized let owning no sites"
    in
    let scheme () = (Map.find_exn t.ident_info ident).ty in
    (match args with
     | _ :: _ ->
       let obj =
         match
           closure
             t
             ctx
             expr
             ~params:args
             ~body
             ~env:env'
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
              ; env = env'
              ; ctx = { ctx with owner = owner () }
              ; owner = owner ()
              ; scheme = scheme ()
              }
          else Value (eval' ~env:env' body));
    eval' ~env:env' ~tail in_
  | Let_mut { ident; body; in_; _ } ->
    let value = eval' body in
    let cell = make t ctx (made t expr) (Cell (ref value)) in
    eval' ~env:(Map.set env ~key:ident ~data:(ref (Value (Obj cell)))) ~tail in_
  | Assign (Bound (Local ident), value) ->
    let value = eval' value in
    (match Option.map (Map.find env ident) ~f:( ! ) with
     | Some (Value (Obj ({ payload = Cell cell; _ } as obj))) ->
       live obj ~what:"an assignment";
       escapes value ~what:"a reference, from what is assigned,";
       cell := value;
       check_globals t;
       unit
     | _ -> unsupported "assigning a non-cell")
  | Assign _ -> unsupported "assigning a toplevel"
  | Fun { args; body; _ } ->
    closure
      t
      ctx
      expr
      ~params:(Nonempty_list.to_list args)
      ~body
      ~env
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
    free t region;
    escapes value ~what:"a reference, from the value of a [region_],";
    value
  | Marked (Exclave, expr) ->
    free t ctx.region;
    eval' ~ctx:{ ctx with current = ctx.caller } ~tail expr
  | Tuple exprs -> Tuple (List.map exprs ~f:(fun expr -> eval' expr))
  | Construct (constructor, exprs) ->
    Construct (constructor, List.map exprs ~f:(fun expr -> eval' expr))
  | Record { record; fields } ->
    let values = Array.create ~len:(Array.length record.fields) unit in
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
       escapes value ~what:"a reference, from what is stored,";
       cell := update !cell fields;
       check_globals t;
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
  | Quote _
  | Expect _
  | External _
  | Type _
  | Error _ ->
    unsupported
      !"%{Sexp}"
      (Typed.Expr.Desc.sexp_of_t (fun _ -> Atom "_") expr.desc)

and block t ctx (allocation : Allocation.t) arg =
  Obj
    (make
       t
       ctx
       allocation
       (match allocation.kind with
        | Box -> Box arg
        | Pointer -> Ptr (ref arg)
        | Closure | Partial_application | Code | Cell ->
          unsupported "applying what isn't [Box.make] or [Ptr.make]"))

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
      { sites = Allocation.Id.Table.create ()
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
      Hashtbl.add_exn t.sites ~key:allocation.id ~data:allocation;
      Option.iter allocation.follows ~f:(fun { owner; side; _ } ->
        let key = owner_key owner in
        let sides = Hashtbl.find t.sides key |> Option.value ~default:[] in
        if not (List.mem sides side ~equal:Allocation.Side.equal)
        then Hashtbl.set t.sides ~key ~data:(sides @ [ side ])));
    List.iter program.sccs ~f:(fun scc ->
      List.iter scc.members ~f:(fun (top : Typed.Toplevel.t) ->
        Hashtbl.set t.toplevels ~key:top.ident.name ~data:top));
    (match Hashtbl.find t.toplevels entry with
     | None -> Outcome.Unsupported ("no toplevel " ^ entry)
     | Some top ->
       (try
          let f = toplevel t Instance.empty top.ident in
          let result =
            apply
              t
              { region = Region.heap
              ; caller = Region.heap
              ; current = Region.heap
              ; instance = Instance.empty
              ; owner = { toplevel = top.ident; lets = [] }
              ; running = None
              }
              f
              [ unit ]
              ~tail:false
          in
          check_globals t;
          Outcome.Returned
            { value = Value.to_string result
            ; on_heap = t.on_heap
            ; in_regions = t.in_regions
            }
        with
        | Use_after_free message -> Outcome.Use_after_free message
        | Runtime_error message -> Outcome.Runtime_error message
        | Unsupported message -> Outcome.Unsupported message))
;;
