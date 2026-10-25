open! Core

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

type t = Fresh.t

let create = Fresh.create
let lift result = Result.map_error result ~f:Typecheck_error.unification_error

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
    ; context : Type.row
    ; outside : t
    }

  let quote_id t = Option.map t.quote ~f:(fun quote -> quote.id)

  let entry t ~scheme ~persistent =
    { Env.Entry.scheme; level = t.level; quote_id = quote_id t; persistent }
  ;;

  let fresh_ty ids t = Type.TVar (Fresh.var ids ~rank:t.rank)
  let fresh_row ids t = Type.RVar (Fresh.var ids ~rank:t.rank)
  let fresh_presence ids t = Type.PVar (Fresh.var ids ~rank:t.rank)
  let instantiate ids t = Scheme.instantiate ids ~rank:t.rank

  let elaborate t annot =
    Type_env.elaborate t.program.type_env t.module_ t.annotation_vars annot
  ;;
end

(* Where a name is used, given its entry if it has one. *)
module Resolution = struct
  type t =
    | Local of Env.Entry.t
    (** Usable, and bound in the enclosing quote if there is one. *)
    | Outside_quote of
        { quote : Scope.quote
        ; usable : Env.Entry.t option
        }
    (** Not bound in the enclosing [quote], so a free variable of it, unless
        used directly while [usable]. Splices never capture the name. *)
    | Unusable of Typecheck_error.t (** The error if the name is used. *)

  let of_entry (scope : Scope.t) ~name (entry : Env.Entry.t option) =
    let usable entry =
      Env.Entry.is_allowed_at
        entry
        ~used_at:scope.level
        ~quote_id:(Scope.quote_id scope)
    in
    match scope.quote, entry with
    | None, Some entry when usable entry -> Local entry
    | Some quote, Some entry
      when [%equal: int option] entry.quote_id (Some quote.id) -> Local entry
    | Some quote, Some entry when entry.level <= scope.level ->
      Outside_quote { quote; usable = Option.some_if (usable entry) entry }
    | Some quote, None -> Outside_quote { quote; usable = None }
    | _, Some entry ->
      Unusable
        (Variable_used_at_wrong_level
           { name; var_level = entry.level; used_at = scope.level })
    | None, None -> Unusable (Not_in_scope (Syntax.Path.of_string name))
  ;;
end

let check_no_duplicates names ~error =
  match List.find_a_dup names ~compare:String.compare with
  | None -> Ok ()
  | Some name -> Error (error name)
;;

let function_type t scope ~(args : Syntax.Binding.t list) ~annot =
  let annot_type = function
    | None -> Ok (Scope.fresh_ty t scope)
    | Some annot -> Scope.elaborate scope annot
  in
  let%bind.Result args =
    List.map args ~f:(fun (name, annot) ->
      let%map.Result ty = annot_type annot in
      name, ty)
    |> Result.all
  in
  let%map.Result res_ty = annot_type annot in
  let ty =
    List.fold_right args ~init:res_ty ~f:(fun (_, arg_ty) ty ->
      Type.TArrow (arg_ty, ty))
  in
  args, res_ty, ty
;;

(* Pairs each of [args] with its instantiated type, along with the result
   type. *)
let constructor_types t (scope : Scope.t) path ~args =
  let%bind.Result (constructor : Type_env.Constructor.t) =
    Type_env.find_constructor scope.program.type_env scope.module_ path
  in
  match List.zip args constructor.args with
  | Unequal_lengths ->
    Error
      (Typecheck_error.Constructor_arity_mismatch
         { constructor = constructor.name
         ; expected = List.length constructor.args
         ; got = List.length args
         })
  | Ok args ->
    let inst = Scope.instantiate t scope in
    let result = inst constructor.result in
    Ok (List.map args ~f:(fun (arg, ty) -> arg, inst ty), result)
;;

(* Returns the instantiated record type, and each field paired with its
   type. *)
let record_fields t (scope : Scope.t) fields ~all_required =
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
  | ((first : Type_env.Field.t), _) :: _ ->
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
      check_no_duplicates names ~error:(fun name ->
        Typecheck_error.Duplicate_field name)
    in
    let%map.Result () =
      match
        List.find first.all_fields ~f:(fun name ->
          not (List.mem names name ~equal:String.equal))
      with
      | Some name when all_required ->
        Error (Typecheck_error.Missing_field name)
      | Some _ | None -> Ok ()
    in
    let inst = Scope.instantiate t scope in
    let fields = List.map fields ~f:(fun (field, x) -> x, inst field.ty) in
    inst first.record, fields
;;

(* Returns the instantiated record type and the field's type. *)
let field_type t (scope : Scope.t) path =
  let%map.Result (field : Type_env.Field.t) =
    Type_env.find_field scope.program.type_env scope.module_ path
  in
  let inst = Scope.instantiate t scope in
  let ty = inst field.ty in
  inst field.record, ty
;;

let rec pattern_type t (scope : Scope.t) ~(pattern : Syntax.Pattern.t)
  : (Type.ty * (string * Type.ty) list, Typecheck_error.t) Result.t
  =
  let sub_patterns patterns =
    List.fold_result patterns ~init:[] ~f:(fun bindings (pattern, ty) ->
      let%bind.Result pattern_ty, bindings' = pattern_type t scope ~pattern in
      let%map.Result () = Unify.unify_ty t ~ty1:ty ~ty2:pattern_ty |> lift in
      bindings @ bindings')
  in
  match pattern with
  | Any -> Ok (Scope.fresh_ty t scope, [])
  | Var name ->
    let ty = Scope.fresh_ty t scope in
    Ok (ty, [ name, ty ])
  | Lit lit -> Ok (Syntax.Lit.ty lit, [])
  | Tuple patterns ->
    let types = List.map patterns ~f:(fun _ -> Scope.fresh_ty t scope) in
    let%map.Result bindings = sub_patterns (List.zip_exn patterns types) in
    Type.TTuple types, bindings
  | Construct (path, args) ->
    let%bind.Result args, result = constructor_types t scope path ~args in
    let%map.Result bindings = sub_patterns args in
    result, bindings
  | Record fields ->
    let%bind.Result record, fields =
      record_fields t scope fields ~all_required:false
    in
    let%map.Result bindings = sub_patterns fields in
    record, bindings
;;

(* [name] is the context entry the variable is provided as, if it becomes a
   free variable of the quote: a local's name or a toplevel's canonical name. *)
let resolution_type t (scope : Scope.t) ~name : Resolution.t -> _ = function
  | Local entry | Outside_quote { usable = Some entry; _ } ->
    Ok (Scope.instantiate t scope entry.scheme)
  | Outside_quote { quote; usable = None } ->
    Unify.present_entry t ~row:quote.context ~name ~rank:quote.outside.rank
    |> lift
  | Unusable error -> Error error
;;

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
      let scheme = if generalize then Scheme.generalize_ty ~rank:0 ty else ty in
      member.state <- Done (Env.Entry.toplevel ~scheme ~persistent)
    | Unchecked | Done _ ->
      raise_s
        [%message "finish_scc: member not in progress" (member : Toplevel.t)])
;;

let rec var_type t (scope : Scope.t) ~(path : Syntax.Path.t) =
  let local =
    match (path :> string list) with
    | [ name ] ->
      Env.find scope.locals name |> Option.map ~f:(fun entry -> name, entry)
    | _ -> None
  in
  let%bind.Result name, entry =
    match local with
    | Some (name, entry) -> Ok (name, Some entry)
    | None ->
      (match%bind.Result Module_scope.find scope.module_ Value path with
       | Some name ->
         let%map.Result entry =
           toplevel_entry
             t
             scope
             (Hashtbl.find_exn scope.program.toplevels name)
         in
         name, Some entry
       | None ->
         (match (path :> string list) with
          | [ name ] -> Ok (name, None)
          | _ -> Error (Typecheck_error.Not_in_scope path)))
  in
  resolution_type t scope ~name (Resolution.of_entry scope ~name entry)

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
  let index = Fresh.id t in
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
    ; annotation_vars = Type_env.Annotation_vars.create t ~rank
    }
  in
  let%bind.Result args, res_ty, ty =
    function_type t scope ~args:toplevel.args ~annot:toplevel.annot
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
    Unify.unify_ty t ~ty1:res_ty ~ty2:body_ty |> lift)

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
       let%bind.Result annot = Scope.elaborate scope annot in
       let%map.Result () = Unify.unify_ty t ~ty1:ty ~ty2:annot |> lift in
       ty)
  | App (f, x) ->
    let a = Scope.fresh_ty t scope in
    let b = Scope.fresh_ty t scope in
    let%bind.Result f_ty = expr_type t scope ~expr:f in
    let%bind.Result () =
      Unify.unify_ty t ~ty1:(TArrow (a, b)) ~ty2:f_ty |> lift
    in
    let%bind.Result x_ty = expr_type t scope ~expr:x in
    let%map.Result () = Unify.unify_ty t ~ty1:a ~ty2:x_ty |> lift in
    b
  | Let (name, args, annot, equal, in_) ->
    let inner = { scope with rank = scope.rank + 1 } in
    let%bind.Result arg_types, res_ty, ty =
      function_type t inner ~args ~annot
    in
    let%bind.Result () =
      Env.with_bindings
        scope.locals
        [ name, Scope.entry scope ~scheme:ty ~persistent:false ]
        ~f:(fun () -> check_body t inner ~args:arg_types ~res_ty ~body:equal)
    in
    let persistent = not (List.is_empty args) in
    let scheme =
      if Syntax.Expr.binding_is_value name ~args ~equal
      then Scheme.generalize_ty ~rank:scope.rank ty
      else ty
    in
    Env.with_bindings
      scope.locals
      [ name, Scope.entry scope ~scheme ~persistent ]
      ~f:(fun () -> expr_type t scope ~expr:in_)
  | Quote expr ->
    let quote =
      { Scope.id = Fresh.id t
      ; context = Scope.fresh_row t scope
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
    let inner = Scope.fresh_ty t scope in
    let context = Scope.fresh_row t scope in
    let%bind.Result code_ty = expr_type t outside ~expr in
    let%bind.Result () =
      Unify.unify_ty t ~ty1:(TCode { inner; context }) ~ty2:code_ty |> lift
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
    let%bind.Result args, result = constructor_types t scope path ~args in
    let%map.Result () = check_exprs t scope args in
    result
  | Record fields ->
    let%bind.Result record, fields =
      record_fields t scope fields ~all_required:true
    in
    let%map.Result () = check_exprs t scope fields in
    record
  | Field (expr, path) ->
    let%bind.Result record, field_ty = field_type t scope path in
    let%bind.Result expr_ty = expr_type t scope ~expr in
    let%map.Result () = Unify.unify_ty t ~ty1:record ~ty2:expr_ty |> lift in
    field_ty
  | Match (expr, cases) ->
    let%bind.Result scrutinee = expr_type t scope ~expr in
    let result = Scope.fresh_ty t scope in
    let%map.Result () =
      List.fold_result cases ~init:() ~f:(fun () (pattern, body) ->
        let%bind.Result pattern_ty, bindings = pattern_type t scope ~pattern in
        let%bind.Result () =
          check_no_duplicates (List.map bindings ~f:fst) ~error:(fun name ->
            Typecheck_error.Duplicate_pattern_variable name)
        in
        let%bind.Result () =
          Unify.unify_ty t ~ty1:scrutinee ~ty2:pattern_ty |> lift
        in
        with_monomorphic_bindings scope bindings ~f:(fun () ->
          let%bind.Result body_ty = expr_type t scope ~expr:body in
          Unify.unify_ty t ~ty1:result ~ty2:body_ty |> lift))
    in
    result

and check_exprs t scope exprs =
  List.fold_result exprs ~init:() ~f:(fun () (expr, ty) ->
    let%bind.Result expr_ty = expr_type t scope ~expr in
    Unify.unify_ty t ~ty1:ty ~ty2:expr_ty |> lift)

and substitute_from_scope t (scope : Scope.t) ~context =
  let rec go context ~seen =
    match Type.canonical_row context with
    | REmpty -> Ok ()
    | RExtend (name, presence, ty, rest) ->
      let%bind.Result () =
        match Type.canonical_presence presence with
        | _ when Set.mem seen name -> Ok ()
        | Absent -> Ok ()
        | Present | PVar _ -> substitute_entry t scope ~name ~presence ~ty
      in
      go rest ~seen:(Set.add seen name)
    | RVar _ as tail ->
      (match scope.quote with
       | None -> Unify.unify_row t ~row1:tail ~row2:REmpty |> lift
       | Some quote -> substitute_tail t scope quote ~tail ~seen)
  in
  go context ~seen:String.Set.empty

and substitute_entry t (scope : Scope.t) ~name ~presence ~ty =
  let%bind.Result entry =
    match Env.find scope.locals name, scope.quote with
    | Some entry, _ -> Ok (Some entry)
    (* Toplevels are free variables of the quote, like any name not bound in
       it. *)
    | None, Some _ -> Ok None
    | None, None ->
      (match Hashtbl.find scope.program.toplevels name with
       | None -> Ok None
       | Some toplevel ->
         toplevel_entry t scope toplevel |> Result.map ~f:Option.some)
  in
  match Resolution.of_entry scope ~name entry with
  | Unusable error ->
    (* Code that may use [name] can't, so doesn't. *)
    Unify.unify_presence ~presence1:presence ~presence2:Absent ~error
  | Local entry ->
    Unify.unify_ty t ~ty1:ty ~ty2:(Scope.instantiate t scope entry.scheme)
    |> lift
  | Outside_quote { quote; _ } ->
    Unify.constrain_row
      t
      ~row:(RExtend (name, presence, ty, REmpty))
      ~bound:quote.context
      ~shadowed:String.Set.empty
    |> lift

(* The code's remaining free variables are not known yet, so give the ones that
   would be captured here explicit entries. The rest must be free variables of
   the quote. *)
and substitute_tail t scope (quote : Scope.quote) ~tail ~seen =
  let captured =
    Env.visible scope.locals
    |> List.filter_map ~f:(fun (name, entry) ->
      if Set.mem seen name
      then None
      else (
        match Resolution.of_entry scope ~name (Some entry) with
        | Local entry ->
          Some
            ( name
            , Scope.fresh_presence t scope
            , Scope.instantiate t scope entry.scheme )
        | Unusable _ -> Some (name, Absent, Scope.fresh_ty t scope)
        | Outside_quote _ -> None))
  in
  let rest = Scope.fresh_row t scope in
  let%bind.Result () =
    Unify.unify_row
      t
      ~row1:tail
      ~row2:
        (List.fold_right
           captured
           ~init:rest
           ~f:(fun (name, presence, ty) rest ->
             Type.RExtend (name, presence, ty, rest)))
    |> lift
  in
  Unify.constrain_row
    t
    ~row:rest
    ~bound:quote.context
    ~shadowed:
      (List.fold captured ~init:seen ~f:(fun seen (name, _, _) ->
         Set.add seen name))
  |> lift

and substitute_explicitly t scope ~context ~subs =
  let%bind.Result () =
    check_no_duplicates (List.map subs ~f:fst) ~error:(fun name ->
      Typecheck_error.Duplicate_substitution name)
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
    List.fold provided ~init:Type.REmpty ~f:(fun rest (name, ty) ->
      Type.RExtend (name, Scope.fresh_presence t scope, ty, rest))
  in
  Unify.unify_row t ~row1:context ~row2:provided |> lift
;;

let typecheck t ~(program : Syntax.Program.t)
  : (Signature.t, Typecheck_error.t) Result.t
  =
  let%bind.Result root =
    Module_scope.create
      (List.map Type_env.builtins ~f:Syntax.Toplevel.type_ @ program)
  in
  let%bind.Result type_env = Type_env.create root t in
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
  let program_env =
    { Program_env.type_env
    ; toplevels =
        List.map toplevels ~f:(fun toplevel -> toplevel.name, toplevel)
        |> String.Table.of_alist_exn
    }
  in
  let%map.Result () =
    List.fold_result toplevels ~init:() ~f:(fun () toplevel ->
      match toplevel.state with
      | Unchecked -> check_toplevel t program_env toplevel |> Result.ignore_m
      | In_progress _ | Done _ -> Ok ())
  in
  let rec signature module_ toplevels =
    let canonical_name = Module_scope.canonical_name module_ in
    List.map
      toplevels
      ~f:(fun (toplevel : Syntax.Toplevel.t) : Signature.Item.t ->
        match toplevel with
        | Let (name, _, _, _) ->
          let toplevel =
            Hashtbl.find_exn program_env.toplevels (canonical_name name)
          in
          (match toplevel.state with
           | Done { scheme; persistent; _ } ->
             Value { name; scheme; persistent }
           | Unchecked | In_progress _ ->
             raise_s
               [%message "typecheck: toplevel not done" (toplevel : Toplevel.t)])
        | Type decl ->
          Type (Type_env.find_decl_exn type_env (canonical_name decl.name))
        | Module (name, toplevels) ->
          Module
            { name
            ; items = signature (Module_scope.enter module_ name) toplevels
            })
  in
  signature root program
;;
