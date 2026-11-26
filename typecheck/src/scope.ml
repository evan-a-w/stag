open! Import

module Param = struct
  type t =
    { ty : Type.ty
    ; mode : Mode.Var.t
    }
end

type t =
  { program : Program_env.t
  ; level : Env.Level.t
  ; rank : Type.Rank.t
  ; locals : Env.t
  ; quote : quote option
  ; module_ : Module_scope.t
  ; scc : Scc_frame.t
  ; annotation_vars : Type_env.Annotation_vars.t
  ; in_expect_test : bool
  ; generated : Generated.t option
  ; syntax_splice : Syntax_splice.t option
  ; locks : Env.Lock.t list
  ; params : Param.t list
  ; allocations : Allocation_sites.t option
  ; owner : Allocation_sites.Frame.t option
  ; uses : Local_uses.t
  ; expansion : bool
  ; warnings : Warning.Settings.t
  }

and quote =
  { id : int
  ; context : Type.row
  ; outside : t
  ; mode : Mode.Var.t
  }

let quote_id t = Option.map t.quote ~f:(fun quote -> quote.id)

let entry
  ?(defining = false)
  ?(held : Binding.t option)
  ?shape
  t
  ~ident
  ~scheme
  ~(mode : Binding.t)
  =
  { Env.Entry.scheme
  ; level = t.level
  ; quote_id = quote_id t
  ; mode
  ; held
  ; shape
  ; locks = t.locks
  ; binder = Local ident
  ; defining
  }
;;

module Bounds = struct
  type t =
    | Snapshot
    | Live
end

let known_stage t ~(bounds : Bounds.t) =
  let raised = Int.Table.create () in
  let iter : Nothing.t Type.Iter.t =
    { ty = (fun ~guarded:_ _ -> Ok ())
    ; row = (fun ~guarded:_ _ -> Ok ())
    ; presence = (fun ~guarded:_ _ -> Ok ())
    ; mode =
        (fun var ->
          Hashtbl.set raised ~key:(Mode.Var.id var) ~data:var;
          Ok ())
    }
  in
  List.iter
    (List.map t.params ~f:(fun param -> param.ty)
     @ Stack.to_list t.program.unfinished)
    ~f:(fun ty ->
      match Type.iter_ty iter ty with
      | Ok () -> ()
      | Error _ -> .);
  List.iter t.params ~f:(fun { ty = _; mode } ->
    Hashtbl.set raised ~key:(Mode.Var.id mode) ~data:mode);
  let upper var = (Mode.Var.upper var).stage in
  let find id =
    match Hashtbl.find raised id with
    | Some _ as var -> var
    | None -> Hashtbl.find t.program.parameter_modes id
  in
  let raised =
    match bounds with
    | Snapshot ->
      let uppers = Hashtbl.map t.program.parameter_modes ~f:upper in
      Hashtbl.iteri raised ~f:(fun ~key ~data ->
        Hashtbl.set uppers ~key ~data:(upper data));
      Hashtbl.find uppers
    | Live -> fun id -> Option.map (find id) ~f:upper
  in
  fun mode -> Mode.Var.lower_assuming mode Stage ~raised
;;

let enter_closure ids t mode =
  { t with
    locks = { Env.Lock.id = Fresh.id ids; kind = Closure mode } :: t.locks
  }
;;

(* Plain data may be used at any locality and provenance, and at a stage up to
   [portable]: a copy of it is as good. Of a type not yet known, until it is
   found to have pointers. *)
let mode_crossing t ty =
  match Type_env.layout_of t.program.type_env ty with
  | Pointer_free -> Mode.Crossing.immediate
  | Has_pointers -> Mode.Crossing.none
  | Depends [ layout ] -> Mode.Crossing.while_pointer_free layout
  | Depends layouts ->
    Mode.Crossing.while_pointer_free
      (Var_layout.above (Type_env.layouts t.program.type_env) layouts)
;;

let embeddable t ty mode ~known_stage ~(finality : Var_layout.Finality.t) =
  let layout : [ `Pointer_free | `Has_pointers | `Unknown ] =
    match Type_env.layout_of t.program.type_env ty with
    | Pointer_free -> `Pointer_free
    | Has_pointers -> `Has_pointers
    | Depends layouts ->
      if List.exists layouts ~f:Mode.Layout.has_pointers
      then `Has_pointers
      else if List.exists layouts ~f:Mode.Layout.is_unknown
      then (
        match finality with
        | Final -> `Has_pointers
        | Provisional -> `Unknown)
      else `Pointer_free
  in
  match layout with
  | `Pointer_free -> `Yes
  | `Unknown -> `Wait
  | `Has_pointers ->
    if not (Mode.Stage.le (known_stage mode) Portable)
    then `No
    else if List.is_empty (Mode.Var.waits_on mode Stage)
    then `Yes
    else (
      match finality with
      | Final -> `No
      | Provisional -> `Wait)
;;

let fresh_ty ids t = Type.TVar (Fresh.var ids ~rank:t.rank)
let join_ty ids t = Type.TVar (Unify.join_var ids ~rank:t.rank)
let fresh_row ids t = Type.RVar (Fresh.var ids ~rank:t.rank)
let fresh_presence ids t = Type.PVar (Fresh.var ids ~rank:t.rank)
let instantiate ?at ids t = Scheme.instantiate ?at ids ~rank:t.rank
let splices t = Option.is_none t.quote && t.level >= 0

let reporter t =
  { Reporter.component = t.scc.component
  ; module_ = t.module_
  ; syntax_splice = t.syntax_splice
  ; generated = t.generated
  ; expansion = t.expansion
  }
;;

let report ?once t (error : Typecheck_error.t) =
  Program_env.report_by t.program (reporter t) ?once error
;;

let warning t name (warning : Typecheck_error.t) =
  let once = Sexp.to_string [%sexp (warning : Typecheck_error.t)] in
  match Warning.Settings.level t.warnings name with
  | Off -> ()
  | Warn -> Program_env.warn t.program (reporter t) ~once warning
  | Error -> report t ~once warning
;;

let mode_holds t ~loc = function
  | Ok () -> true
  | Error error ->
    let once =
      match error with
      | Mode.Error.Mismatch { upper_because; _ } ->
        Sexp.to_string [%sexp (upper_because : Mode.Reason.t list)]
      | Has_pointers { bound; _ } ->
        Sexp.to_string [%sexp (bound.reason : Mode.Reason.t)]
      | Unequal _ | Exceeds _ -> Sexp.to_string [%sexp (error : Mode.Error.t)]
    in
    report t ~once (Typecheck_error.create ~loc (Mode_error error));
    false
;;

let modes t =
  Mode_rules.Ctx.create
    ~modes:t.program.modes
    ~rank:t.rank
    ~holds:(mode_holds t)
    ~crossing:(mode_crossing t)
    ~report:(report t)
    ~allocated:
      (match t.allocations, t.quote with
       | Some sites, None when t.level = 0 ->
         fun site -> Some (Allocation_sites.add sites site ~frame:t.owner)
       | _ -> fun _ -> None)
    ~locks:t.locks
;;

let arrow_mode t = Mode_rules.arrow_mode (modes t)

let mode_var t name ~loc =
  match Type_env.Annotation_vars.mode t.annotation_vars name ~loc with
  | Ok var -> var
  | Error error ->
    report t error;
    Mode_rules.arrow_mode (modes t)
;;

let unifies ids t ~ty1 ~ty2 ~loc =
  match Unify.unify_ty ids ~ty1 ~ty2 with
  | Ok () -> true
  | Error error ->
    report t (Typecheck_error.unification_error ~loc error);
    false
;;

let subtypes ids t ~lower ~upper ~loc =
  match Unify.sub_ty ids ~lower ~upper with
  | Ok () -> true
  | Error error ->
    report t (Typecheck_error.unification_error ~loc error);
    false
;;

let elaborate_resolved t annot =
  Type_env.elaborate
    t.program.type_env
    t.module_
    t.annotation_vars
    annot
    ~splices:(splices t)
;;
