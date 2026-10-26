open! Core

module Toplevel = struct
  module State = struct
    type t =
      | Unchecked
      | In_progress of
          { index : int
          ; ty : Type.ty
          ; args : Typed.Ident.t list
          ; mutable body : Typed.Expr.t option
          ; mutable recursive : bool
          }
      | Done of
          { entry : Env.Entry.t
          ; typed : Typed.Toplevel.t
          }
    [@@deriving sexp_of]
  end

  type t =
    { ident : Typed.Ident.t
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
    ; mutable next_stamp : int
    ; mutable idents : Type.ty Typed.Ident.Map.t
    ; sccs : Typed.Scc.t Queue.t
    }

  let fresh_ident t name =
    let ident = { Typed.Ident.name; stamp = t.next_stamp } in
    t.next_stamp <- t.next_stamp + 1;
    ident
  ;;

  let bind t name ty =
    let ident = fresh_ident t name in
    t.idents <- Map.add_exn t.idents ~key:ident ~data:ty;
    ident
  ;;
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

  let entry t ~ident ~scheme ~persistent =
    { Env.Entry.scheme
    ; level = t.level
    ; quote_id = quote_id t
    ; persistent
    ; binder = Local ident
    }
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

let function_type t (scope : Scope.t) ~(args : Syntax.Binding.t list) ~annot =
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
  let args =
    List.map args ~f:(fun (name, ty) ->
      Program_env.bind scope.program name ty, ty)
  in
  args, res_ty, ty
;;

let constructor_ref (constructor : Type_env.Constructor.t) =
  { Typed.Constructor_ref.type_name = constructor.type_name
  ; name = constructor.name
  ; index = constructor.index
  }
;;

let field_ref (field : Type_env.Field.t) =
  { Typed.Field_ref.type_name = field.type_name
  ; name = field.name
  ; index = field.index
  }
;;

(* Pairs each of [args] with its instantiated type, along with the
   constructor and the result type. *)
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
    Ok
      ( constructor_ref constructor
      , List.map args ~f:(fun (arg, ty) -> arg, inst ty)
      , result )
;;

(* Returns the instantiated record type, and each field with its [x] paired
   with its type. *)
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
    let fields =
      List.map fields ~f:(fun (field, x) -> field_ref field, (x, inst field.ty))
    in
    inst first.record, fields
;;

(* Returns the field, the instantiated record type and the field's type. *)
let field_type t (scope : Scope.t) path =
  let%map.Result (field : Type_env.Field.t) =
    Type_env.find_field scope.program.type_env scope.module_ path
  in
  let inst = Scope.instantiate t scope in
  let ty = inst field.ty in
  field_ref field, inst field.record, ty
;;

(* Rows of polymorphic records and variants, whose entries are [Present]. *)
module Poly_row = struct
  let present entries ~tail =
    List.fold_right entries ~init:tail ~f:(fun (label, ty) rest ->
      Type.RExtend (label, Present, ty, rest))
  ;;

  (* [entries] before a fresh tail, which is also returned. *)
  let open_ t scope entries =
    let tail = Scope.fresh_row t scope in
    present entries ~tail, tail
  ;;

  let variant t scope ~tag ~arg =
    Fresh.variant t (fst (open_ t scope [ tag, arg ]))
  ;;

  let check_distinct_labels entries =
    check_no_duplicates (List.map entries ~f:fst) ~error:(fun label ->
      Typecheck_error.Duplicate_row_entry label)
  ;;
end

let rec pattern_type t (scope : Scope.t) ~(pattern : Syntax.Pattern.t)
  : ( Typed.Pattern.t * (Typed.Ident.t * Type.ty) list
      , Typecheck_error.t )
      Result.t
  =
  let node desc ty = { Typed.Pattern.desc; ty } in
  let bind name ty = Program_env.bind scope.program name ty in
  let sub_patterns patterns =
    let%map.Result typed, bindings =
      List.fold_result
        patterns
        ~init:([], [])
        ~f:(fun (typed, bindings) (pattern, ty) ->
          let%bind.Result pattern, bindings' = pattern_type t scope ~pattern in
          let%map.Result () =
            Unify.unify_ty t ~ty1:ty ~ty2:pattern.Typed.Pattern.ty |> lift
          in
          pattern :: typed, bindings @ bindings')
    in
    List.rev typed, bindings
  in
  match pattern with
  | Any -> Ok (node Any (Scope.fresh_ty t scope), [])
  | Var name ->
    let ty = Scope.fresh_ty t scope in
    let ident = bind name ty in
    Ok (node (Var ident) ty, [ ident, ty ])
  | Lit lit -> Ok (node (Lit lit) (Syntax.Lit.ty lit), [])
  | Tuple patterns ->
    let types = List.map patterns ~f:(fun _ -> Scope.fresh_ty t scope) in
    let%map.Result patterns, bindings =
      sub_patterns (List.zip_exn patterns types)
    in
    node (Tuple patterns) (Type.TTuple types), bindings
  | Construct (path, args) ->
    let%bind.Result constructor, args, result =
      constructor_types t scope path ~args
    in
    let%map.Result args, bindings = sub_patterns args in
    node (Construct (constructor, args)) result, bindings
  | Record fields ->
    let%bind.Result record, fields =
      record_fields t scope fields ~all_required:false
    in
    let%map.Result patterns, bindings = sub_patterns (List.map fields ~f:snd) in
    ( node (Record (List.zip_exn (List.map fields ~f:fst) patterns)) record
    , bindings )
  | Poly_variant (tag, arg) ->
    let%map.Result arg, bindings =
      match arg with
      | None -> Ok (None, [])
      | Some pattern ->
        let%map.Result pattern, bindings = pattern_type t scope ~pattern in
        Some pattern, bindings
    in
    let arg_ty =
      Option.value_map arg ~default:Type.unit ~f:(fun pattern -> pattern.ty)
    in
    ( node (Poly_variant (tag, arg)) (Poly_row.variant t scope ~tag ~arg:arg_ty)
    , bindings )
  | Poly_record { fields; rest } ->
    let%bind.Result () = Poly_row.check_distinct_labels fields in
    let%map.Result fields, bindings =
      List.fold_result
        fields
        ~init:([], [])
        ~f:(fun (fields, bindings) (label, pattern) ->
          let%map.Result pattern, bindings' = pattern_type t scope ~pattern in
          (label, pattern) :: fields, bindings @ bindings')
    in
    let fields = List.rev fields in
    let tail, rest, rest_bindings =
      match rest with
      | Closed -> Type.REmpty, Typed.Pattern.Rest.Closed, []
      | Open -> Scope.fresh_row t scope, Open, []
      | Bind name ->
        let tail = Scope.fresh_row t scope in
        let ty = Fresh.record t tail in
        let ident = bind name ty in
        tail, Bind ident, [ ident, ty ]
    in
    let row =
      Poly_row.present
        (List.map fields ~f:(fun (label, (pattern : Typed.Pattern.t)) ->
           label, pattern.ty))
        ~tail
    in
    ( node (Poly_record { fields; rest }) (Fresh.record t row)
    , bindings @ rest_bindings )
;;

(* Closing of the scrutinee and residuals of catch-alls in matches on
   polymorphic variants. *)
module Variant_match = struct
  (* The tags handled irrefutably by the cases so far, in order, each once. *)
  type t = string list

  let empty = []

  (* A top level variable pattern matches what the handled tags' cases have
     not: the scrutinee without their first entries. *)
  let pattern_type t (scope : Scope.t) handled ~(pattern : Syntax.Pattern.t) =
    match pattern, handled with
    | Var name, _ :: _ ->
      let row, rest =
        Poly_row.open_
          t
          scope
          (List.map handled ~f:(fun tag -> tag, Scope.fresh_ty t scope))
      in
      let rest = Fresh.variant t rest in
      let ident = Program_env.bind scope.program name rest in
      Ok
        ( { Typed.Pattern.desc = Var ident; ty = Fresh.variant t row }
        , [ ident, rest ] )
    | _ -> pattern_type t scope ~pattern
  ;;

  let add_case handled : Syntax.Pattern.t -> t = function
    | Poly_variant (tag, arg)
      when Option.for_all arg ~f:Syntax.Pattern.is_irrefutable
           && not (List.mem handled tag ~equal:String.equal) ->
      handled @ [ tag ]
    | _ -> handled
  ;;

  let is_empty_nominal (scope : Scope.t) scrutinee =
    match Type.canonical_ty scrutinee with
    | TCon (name, _) -> Type_env.is_empty_variant scope.program.type_env name
    | _ -> false
  ;;

  (* Without a top level catch-all, a variant scrutinee may only have the tags
     of the cases, and an empty match's none, unless the scrutinee is already
     known to be an empty nominal type. *)
  let close t (scope : Scope.t) ~scrutinee (patterns : Syntax.Pattern.t list) =
    let tags =
      List.filter_map patterns ~f:(function
        | Poly_variant (tag, _) -> Some tag
        | _ -> None)
      |> List.stable_dedup ~compare:String.compare
    in
    let has_catch_all =
      List.exists patterns ~f:(function
        | Var _ | Any -> true
        | _ -> false)
    in
    if
      has_catch_all
      || (List.is_empty tags && not (List.is_empty patterns))
      || (List.is_empty patterns && is_empty_nominal scope scrutinee)
    then Ok ()
    else (
      let closed =
        Poly_row.present
          (List.map tags ~f:(fun tag -> tag, Scope.fresh_ty t scope))
          ~tail:REmpty
      in
      Unify.unify_ty t ~ty1:(Fresh.variant t closed) ~ty2:scrutinee |> lift)
  ;;
end

(* [name] is the context entry the variable is provided as, if it becomes a
   free variable of the quote: a local's name or a toplevel's canonical name. *)
let resolution_type t (scope : Scope.t) ~name : Resolution.t -> _ = function
  | Local entry ->
    Ok (Scope.instantiate t scope entry.scheme, Typed.Var_ref.Bound entry.binder)
  | Outside_quote { usable = Some entry; _ } ->
    Ok (Scope.instantiate t scope entry.scheme, Persistent entry.binder)
  | Outside_quote { quote; usable = None } ->
    let%map.Result ty =
      Unify.present_entry t ~row:quote.context ~name ~rank:quote.outside.rank
      |> lift
    in
    ty, Typed.Var_ref.Free name
  | Unusable error -> Error error
;;

let refers_to_toplevel (expr : Typed.Expr.t) ~f =
  let binder : Typed.Binder.t -> bool = function
    | Toplevel ident -> f ident
    | Local _ -> false
  in
  let snds pairs = List.map pairs ~f:snd in
  let rec go (expr : Typed.Expr.t) =
    match expr.desc with
    | Lit _ | Var (Free _) -> false
    | Var (Bound binder' | Persistent binder') -> binder binder'
    | App (e1, e2) | Let { body = e1; in_ = e2; _ } -> go e1 || go e2
    | Splice { code; subs = Explicit subs } -> go code || go_all (snds subs)
    | Splice { code; subs = Implicit captures } ->
      go code || Map.exists captures ~f:binder
    | Quote e | Field (e, _) | Poly_field (e, _) | Poly_restrict (e, _) -> go e
    | Tuple es | Construct (_, es) -> go_all es
    | Record fields -> go_all (snds fields)
    | Poly_record fields -> go_all (snds fields)
    | Poly_extend (fields, e) | Poly_update (e, fields) ->
      go e || go_all (snds fields)
    | Match (e, cases) -> go e || go_all (snds cases)
    | Poly_variant (_, e) -> Option.exists e ~f:go
  and go_all es = List.exists es ~f:go in
  go expr
;;

let finish_scc (program : Program_env.t) (members : Toplevel.t list) =
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
  let members =
    List.map members ~f:(fun member ->
      match member.state with
      | In_progress { index; ty; args; body = Some body; _ } ->
        if generalize then Scheme.generalize_ty ~rank:0 ty;
        program.idents <- Map.add_exn program.idents ~key:member.ident ~data:ty;
        let typed =
          { Typed.Toplevel.ident = member.ident; args; scheme = ty; body }
        in
        member.state
        <- Done
             { entry =
                 Env.Entry.toplevel ~ident:member.ident ~scheme:ty ~persistent
             ; typed
             };
        index, typed
      | In_progress { body = None; _ } | Unchecked | Done _ ->
        raise_s
          [%message "finish_scc: member not checked" (member : Toplevel.t)])
    |> List.sort ~compare:(Comparable.lift Int.compare ~f:fst)
    |> List.map ~f:snd
  in
  let idents =
    Typed.Ident.Set.of_list
      (List.map members ~f:(fun (member : Typed.Toplevel.t) -> member.ident))
  in
  Queue.enqueue
    program.sccs
    { Typed.Scc.members
    ; recursive =
        List.exists members ~f:(fun member ->
          refers_to_toplevel member.body ~f:(Set.mem idents))
    }
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
  | Done { entry; _ } -> Ok entry
  | In_progress in_progress ->
    (* [toplevel] is in the component being checked, so this is a recursive
       reference. *)
    in_progress.recursive <- true;
    scope.scc.lowlink <- Int.min scope.scc.lowlink in_progress.index;
    Ok
      (Env.Entry.toplevel
         ~ident:toplevel.ident
         ~scheme:in_progress.ty
         ~persistent:false)
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
  toplevel.state
  <- In_progress
       { index
       ; ty
       ; args = List.map args ~f:fst
       ; body = None
       ; recursive = false
       };
  let%map.Result body = check_body t scope ~args ~res_ty ~body:toplevel.expr in
  (match toplevel.state with
   | In_progress in_progress -> in_progress.body <- Some body
   | Unchecked | Done _ ->
     raise_s
       [%message "check_toplevel: not in progress" (toplevel : Toplevel.t)]);
  if scc.lowlink = index
  then (
    finish_scc program scc.pending;
    scc.pending <- []);
  scc

and check_body t (scope : Scope.t) ~args ~res_ty ~body =
  with_monomorphic_bindings scope args ~f:(fun () ->
    check_expr t scope ~expr:body ~ty:res_ty)

and with_monomorphic_bindings (scope : Scope.t) bindings ~f =
  Env.with_bindings
    scope.locals
    (List.map bindings ~f:(fun ((ident : Typed.Ident.t), ty) ->
       ident.name, Scope.entry scope ~ident ~scheme:ty ~persistent:false))
    ~f

and expr_type t (scope : Scope.t) ~(expr : Syntax.Expr.t)
  : (Typed.Expr.t, Typecheck_error.t) Result.t
  =
  let node desc ty = { Typed.Expr.desc; ty } in
  let tys = List.map ~f:(fun (expr : Typed.Expr.t) -> expr.ty) in
  let labelled_tys =
    List.map ~f:(fun (label, (expr : Typed.Expr.t)) -> label, expr.ty)
  in
  match expr with
  | Lit l -> Ok (node (Lit l) (Syntax.Lit.ty l))
  | Var (path, annot) ->
    let%bind.Result ty, var_ref = var_type t scope ~path in
    let%map.Result () =
      match annot with
      | None -> Ok ()
      | Some annot ->
        let%bind.Result annot = Scope.elaborate scope annot in
        Unify.unify_ty t ~ty1:ty ~ty2:annot |> lift
    in
    node (Var var_ref) ty
  | App (f, x) ->
    let a = Scope.fresh_ty t scope in
    let b = Scope.fresh_ty t scope in
    let%bind.Result f = check_expr t scope ~expr:f ~ty:(TArrow (a, b)) in
    let%map.Result x = check_expr t scope ~expr:x ~ty:a in
    node (App (f, x)) b
  | Let (name, args, annot, equal, in_) ->
    let inner = { scope with rank = scope.rank + 1 } in
    let%bind.Result arg_types, res_ty, ty =
      function_type t inner ~args ~annot
    in
    let ident = Program_env.bind scope.program name ty in
    let%bind.Result body =
      Env.with_bindings
        scope.locals
        [ name, Scope.entry scope ~ident ~scheme:ty ~persistent:false ]
        ~f:(fun () -> check_body t inner ~args:arg_types ~res_ty ~body:equal)
    in
    let persistent = not (List.is_empty args) in
    let generalized = Syntax.Expr.binding_is_value name ~args ~equal in
    if generalized then Scheme.generalize_ty ~rank:scope.rank ty;
    let%map.Result in_ =
      Env.with_bindings
        scope.locals
        [ name, Scope.entry scope ~ident ~scheme:ty ~persistent ]
        ~f:(fun () -> expr_type t scope ~expr:in_)
    in
    node
      (Let { ident; args = List.map arg_types ~f:fst; body; in_; generalized })
      in_.ty
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
    node (Quote inner) (TCode { inner = inner.ty; context = quote.context })
  | Splice { expr; subs } ->
    let outside : Scope.t =
      match scope.quote with
      | Some quote -> { quote.outside with rank = scope.rank }
      | None -> { scope with level = scope.level - 1 }
    in
    let inner = Scope.fresh_ty t scope in
    let context = Scope.fresh_row t scope in
    let%bind.Result code =
      check_expr t outside ~expr ~ty:(TCode { inner; context })
    in
    let%map.Result subs =
      match subs with
      | None ->
        let%map.Result captures = substitute_from_scope t scope ~context in
        Typed.Expr.Subs.Implicit captures
      | Some subs ->
        let%map.Result subs = substitute_explicitly t scope ~context ~subs in
        Typed.Expr.Subs.Explicit subs
    in
    node (Splice { code; subs }) inner
  | Tuple exprs ->
    let%map.Result exprs =
      List.map exprs ~f:(fun expr -> expr_type t scope ~expr) |> Result.all
    in
    node (Tuple exprs) (Type.TTuple (tys exprs))
  | Construct (path, args) ->
    let%bind.Result constructor, args, result =
      constructor_types t scope path ~args
    in
    let%map.Result args = check_exprs t scope args in
    node (Construct (constructor, args)) result
  | Record fields ->
    let%bind.Result record, fields =
      record_fields t scope fields ~all_required:true
    in
    let%map.Result exprs = check_exprs t scope (List.map fields ~f:snd) in
    node (Record (List.zip_exn (List.map fields ~f:fst) exprs)) record
  | Field (expr, path) ->
    let%bind.Result field, record, field_ty = field_type t scope path in
    let%map.Result expr = check_expr t scope ~expr ~ty:record in
    node (Field (expr, field)) field_ty
  | Match (expr, cases) ->
    let%bind.Result scrutinee = expr_type t scope ~expr in
    let result = Scope.fresh_ty t scope in
    let%bind.Result (_ : Variant_match.t), typed_cases =
      List.fold_result
        cases
        ~init:(Variant_match.empty, [])
        ~f:(fun (handled, typed_cases) (pattern, body) ->
          let%bind.Result typed_pattern, bindings =
            Variant_match.pattern_type t scope handled ~pattern
          in
          let%bind.Result () =
            check_no_duplicates
              (List.map bindings ~f:(fun ((ident : Typed.Ident.t), _) ->
                 ident.name))
              ~error:(fun name ->
                Typecheck_error.Duplicate_pattern_variable name)
          in
          let%bind.Result () =
            Unify.unify_ty t ~ty1:scrutinee.ty ~ty2:typed_pattern.ty |> lift
          in
          let%map.Result body =
            with_monomorphic_bindings scope bindings ~f:(fun () ->
              check_expr t scope ~expr:body ~ty:result)
          in
          ( Variant_match.add_case handled pattern
          , (typed_pattern, body) :: typed_cases ))
    in
    let%map.Result () =
      Variant_match.close
        t
        scope
        ~scrutinee:scrutinee.ty
        (List.map cases ~f:fst)
    in
    node (Match (scrutinee, List.rev typed_cases)) result
  | Poly_record fields ->
    let%map.Result fields = labelled_types t scope fields in
    node
      (Poly_record fields)
      (Fresh.record t (Poly_row.present (labelled_tys fields) ~tail:REmpty))
  | Poly_field (expr, label) ->
    let ty = Scope.fresh_ty t scope in
    let%map.Result expr, (_ : Type.row) =
      check_open_record t scope expr [ label, ty ]
    in
    node (Poly_field (expr, label)) ty
  | Poly_extend (fields, expr) ->
    let%bind.Result fields = labelled_types t scope fields in
    let rest = Scope.fresh_row t scope in
    let%map.Result expr = check_expr t scope ~expr ~ty:(Fresh.record t rest) in
    node
      (Poly_extend (fields, expr))
      (Fresh.record t (Poly_row.present (labelled_tys fields) ~tail:rest))
  | Poly_restrict (expr, label) ->
    let%map.Result expr, rest =
      check_open_record t scope expr [ label, Scope.fresh_ty t scope ]
    in
    node (Poly_restrict (expr, label)) (Fresh.record t rest)
  | Poly_update (expr, fields) ->
    let%bind.Result fields = labelled_types t scope fields in
    let%map.Result expr, rest =
      check_open_record
        t
        scope
        expr
        (List.map fields ~f:(fun (label, _) -> label, Scope.fresh_ty t scope))
    in
    node
      (Poly_update (expr, fields))
      (Fresh.record t (Poly_row.present (labelled_tys fields) ~tail:rest))
  | Poly_variant (tag, arg) ->
    let%map.Result arg =
      match arg with
      | None -> Ok None
      | Some expr ->
        let%map.Result expr = expr_type t scope ~expr in
        Some expr
    in
    let arg_ty =
      Option.value_map arg ~default:Type.unit ~f:(fun (expr : Typed.Expr.t) ->
        expr.ty)
    in
    node (Poly_variant (tag, arg)) (Poly_row.variant t scope ~tag ~arg:arg_ty)

and labelled_types t scope fields =
  let%bind.Result () = Poly_row.check_distinct_labels fields in
  List.map fields ~f:(fun (label, expr) ->
    let%map.Result expr = expr_type t scope ~expr in
    label, expr)
  |> Result.all

and check_expr t scope ~expr ~ty =
  let%bind.Result expr = expr_type t scope ~expr in
  let%map.Result () = Unify.unify_ty t ~ty1:ty ~ty2:expr.ty |> lift in
  expr

and check_exprs t scope exprs =
  List.fold_result exprs ~init:[] ~f:(fun checked (expr, ty) ->
    let%map.Result expr = check_expr t scope ~expr ~ty in
    expr :: checked)
  |> Result.map ~f:List.rev

(* Checks [expr] has a record type with [entries] first, returning the rest of
   its row. *)
and check_open_record t scope expr entries =
  let row, rest = Poly_row.open_ t scope entries in
  let%map.Result expr = check_expr t scope ~expr ~ty:(Fresh.record t row) in
  expr, rest

(* Returns the binder each entry that may be captured is captured by. *)
and substitute_from_scope t (scope : Scope.t) ~context =
  let rec go context ~seen ~captures =
    match Type.canonical_row context with
    | REmpty -> Ok captures
    | RExtend (name, presence, ty, rest) ->
      let%bind.Result captured =
        match Type.canonical_presence presence with
        | _ when Set.mem seen name -> Ok None
        | Absent -> Ok None
        | Present | PVar _ -> substitute_entry t scope ~name ~presence ~ty
      in
      let captures =
        Option.fold captured ~init:captures ~f:(fun captures binder ->
          (name, binder) :: captures)
      in
      go rest ~seen:(Set.add seen name) ~captures
    | RVar _ as tail ->
      (match scope.quote with
       | None ->
         let%map.Result () =
           Unify.unify_row t ~row1:tail ~row2:REmpty |> lift
         in
         captures
       | Some quote ->
         let%map.Result captured = substitute_tail t scope quote ~tail ~seen in
         captured @ captures)
  in
  let%map.Result captures = go context ~seen:String.Set.empty ~captures:[] in
  String.Map.of_alist_exn captures

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
    let%map.Result () =
      Unify.unify_presence ~presence1:presence ~presence2:Absent ~error
    in
    None
  | Local entry ->
    let%map.Result () =
      Unify.unify_ty t ~ty1:ty ~ty2:(Scope.instantiate t scope entry.scheme)
      |> lift
    in
    Some entry.binder
  | Outside_quote { quote; _ } ->
    let%map.Result () =
      Unify.constrain_row
        t
        ~row:(RExtend (name, presence, ty, REmpty))
        ~bound:quote.context
        ~shadowed:String.Set.empty
      |> lift
    in
    None

(* The code's remaining free variables are not known yet, so give the ones that
   would be captured here explicit entries. The rest must be free variables of
   the quote. Returns the binders of the ones that may be captured. *)
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
            , Scope.instantiate t scope entry.scheme
            , Some entry.binder )
        | Unusable _ -> Some (name, Absent, Scope.fresh_ty t scope, None)
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
           ~f:(fun (name, presence, ty, _) rest ->
             Type.RExtend (name, presence, ty, rest)))
    |> lift
  in
  let%map.Result () =
    Unify.constrain_row
      t
      ~row:rest
      ~bound:quote.context
      ~shadowed:
        (List.fold captured ~init:seen ~f:(fun seen (name, _, _, _) ->
           Set.add seen name))
    |> lift
  in
  List.filter_map captured ~f:(fun (name, _, _, binder) ->
    Option.map binder ~f:(fun binder -> name, binder))

and substitute_explicitly t scope ~context ~subs =
  let%bind.Result () =
    check_no_duplicates (List.map subs ~f:fst) ~error:(fun name ->
      Typecheck_error.Duplicate_substitution name)
  in
  let%bind.Result provided =
    List.fold_result subs ~init:[] ~f:(fun acc (name, expr) ->
      let%map.Result expr =
        match expr with
        | None ->
          let%map.Result ty, var_ref =
            var_type t scope ~path:(Syntax.Path.of_string name)
          in
          { Typed.Expr.desc = Var var_ref; ty }
        | Some expr -> expr_type t scope ~expr
      in
      (name, expr) :: acc)
  in
  let row =
    List.fold provided ~init:Type.REmpty ~f:(fun rest (name, expr) ->
      Type.RExtend (name, Scope.fresh_presence t scope, expr.ty, rest))
  in
  let%map.Result () = Unify.unify_row t ~row1:context ~row2:row |> lift in
  List.rev provided
;;

module Checked = struct
  type t =
    { signature : Signature.t
    ; program : Typed.Program.t
    }
end

let typecheck t ~(program : Syntax.Program.t)
  : (Checked.t, Typecheck_error.t) Result.t
  =
  let%bind.Result root =
    Module_scope.create
      (List.map Type_env.builtins ~f:Syntax.Toplevel.type_ @ program)
  in
  let%bind.Result type_env = Type_env.create root t in
  let program_env =
    { Program_env.type_env
    ; toplevels = String.Table.create ()
    ; next_stamp = 0
    ; idents = Typed.Ident.Map.empty
    ; sccs = Queue.create ()
    }
  in
  let toplevels =
    Module_scope.toplevels root
    |> List.filter_map ~f:(fun (module_, (toplevel : Syntax.Toplevel.t)) ->
      match toplevel with
      | Let (name, args, annot, expr) ->
        let name = Module_scope.canonical_name module_ name in
        let toplevel =
          { Toplevel.ident = Program_env.fresh_ident program_env name
          ; module_
          ; args
          ; annot
          ; expr
          ; state = Unchecked
          }
        in
        Hashtbl.add_exn program_env.toplevels ~key:name ~data:toplevel;
        Some toplevel
      | Type _ | Module _ -> None)
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
           | Done { entry = { scheme; persistent; _ }; _ } ->
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
  { Checked.signature = signature root program
  ; program =
      { sccs = Queue.to_list program_env.sccs; idents = program_env.idents }
  }
;;
