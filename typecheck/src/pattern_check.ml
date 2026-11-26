open! Import

let tuple_components (expected : Type.ty option) n =
  match Option.map expected ~f:Type.canonical_ty with
  | Some (TTuple tys) when List.length tys = n -> List.map tys ~f:Option.some
  | Some _ | None -> List.init n ~f:(fun _ -> None)
;;

let row_entry (expected : Type.ty option) kind ~label =
  let rec find (row : Type.row) =
    match Type.canonical_row row with
    | RExtend (label', _, _, ty, rest) ->
      if String.equal label label' then Some ty else find rest
    | REmpty | RVar _ -> None
  in
  match Option.map expected ~f:Type.canonical_ty, kind with
  | Some (TRecord { row; _ }), `Record | Some (TVariant { row; _ }), `Variant ->
    find row
  | _ -> None
;;

let either_known ty fallback =
  match Type.canonical_ty ty with
  | TVar _ -> fallback
  | _ -> Some ty
;;

(* The right alternative of an or-pattern binds the variables its [left] does,
   to the same idents: those it has bound are [matched]. *)
module Alternative = struct
  type t =
    { left : Pattern_var.t String.Map.t
    ; matched : String.Hash_set.t
    }

  let create left =
    { left =
        String.Map.of_alist_reduce
          (List.map left ~f:(fun (var : Pattern_var.t) -> var.ident.name, var))
          ~f:(fun first _ -> first)
    ; matched = String.Hash_set.create ()
    }
  ;;

  let bind t (scope : Scope.t) fresh_ty name ~ty ~loc ~fresh =
    match Map.find t.left name with
    | None ->
      Scope.report
        scope
        (Typecheck_error.create ~loc (Or_pattern_variable_missing name));
      fresh ()
    | Some (left : Pattern_var.t) ->
      Hash_set.add t.matched name;
      Unify.unify_ty fresh_ty ~ty1:left.ty ~ty2:ty
      |> Result.iter_error ~f:(fun error ->
        Scope.report
          scope
          (Typecheck_error.create
             ~loc
             (Or_pattern_variable_type { name; error })));
      left.ident
  ;;

  (* Of the variables [left] binds that the right alternative didn't. *)
  let report_unmatched t (scope : Scope.t) =
    Map.iter t.left ~f:(fun (var : Pattern_var.t) ->
      if not (Hash_set.mem t.matched var.ident.name)
      then
        Scope.report
          scope
          (Typecheck_error.create
             ~loc:var.loc
             (Or_pattern_variable_missing var.ident.name)))
  ;;
end

let rec pattern_type'
  ?expected
  ~(alternative : Alternative.t option)
  t
  (scope : Scope.t)
  ~(pattern : Source.Pattern.t)
  : Typed.Pattern.t * Pattern_var.t list
  =
  let pattern_type = pattern_type' ~alternative in
  let loc = Source.Pattern.loc pattern in
  let node desc ty = { Typed.Pattern.desc; ty; loc } in
  let bind binder ty =
    let loc = Source.Binder.loc binder ~enclosing:loc in
    let fresh () =
      Program_env.bind scope.program binder ty ~level:scope.level
    in
    let ident =
      match alternative with
      | None -> fresh ()
      | Some alternative ->
        Alternative.bind
          alternative
          scope
          t
          (Source.Binder.name binder)
          ~ty
          ~loc
          ~fresh
    in
    ident, Pattern_var.bound ident ty ~loc
  in
  (* Each pattern of type [ty], known to be of [expected] (by default [ty]), of
     the part of the block at [steps]. *)
  let sub_patterns ?expected patterns ~steps =
    let expected =
      Option.value
        expected
        ~default:(List.map patterns ~f:(fun (_, ty) -> Some ty))
    in
    let typed =
      List.map3_exn
        patterns
        expected
        steps
        ~f:(fun (pattern, ty) expected step ->
          let pattern, bindings = pattern_type t scope ~pattern ?expected in
          ( (if Scope.unifies t scope ~ty1:ty ~ty2:pattern.ty ~loc:pattern.loc
             then pattern
             else Typed.Pattern.error [ pattern ] ~loc:pattern.loc)
          , Pattern_var.under step bindings ))
    in
    List.map typed ~f:fst, List.concat_map typed ~f:snd
  in
  (* What did not check binds its variables still, so that their uses are not
     errors too. *)
  let fail error patterns =
    Scope.report scope error;
    let patterns, bindings =
      sub_patterns
        (List.map patterns ~f:(fun pattern -> pattern, Scope.fresh_ty t scope))
        ~steps:
          (List.map patterns ~f:(fun _ ->
             Mode_shape.Path.Step.Through Modality.id))
    in
    Typed.Pattern.error patterns ~loc, bindings
  in
  match Source.Pattern.view pattern with
  | Any -> node Any (Scope.fresh_ty t scope), []
  | Var binder ->
    let ty = Scope.fresh_ty t scope in
    let ident, var = bind binder ty in
    node (Var ident) ty, [ var ]
  | Lit lit -> node (Lit lit) (Syntax.Lit.ty lit), []
  | Deref inner ->
    (match Option.bind expected ~f:Type.of_box with
     | Some contents ->
       let inner, bindings =
         pattern_type t scope ~pattern:inner ~expected:contents
       in
       ( node (Deref inner) (Type.box inner.ty)
       , Pattern_var.under (Through Modality.id) bindings )
     | None ->
       let held, expected =
         match Option.bind expected ~f:Type.of_ptr with
         | Some (pointee, held) -> held, Some pointee
         | None -> Mode_rules.fresh_held (Scope.modes scope), None
       in
       let inner, bindings = pattern_type t scope ~pattern:inner ?expected in
       ( node (Deref inner) (Type.ptr ~held inner.ty)
       , Pattern_var.under (Through (Modality.of_cell held)) bindings ))
  | Tuple patterns ->
    let types = List.map patterns ~f:(fun _ -> Scope.fresh_ty t scope) in
    let patterns, bindings =
      sub_patterns
        (List.zip_exn patterns types)
        ~expected:(tuple_components expected (List.length patterns))
        ~steps:
          (List.mapi patterns ~f:(fun index _ ->
             Mode_shape.Path.Step.Part (Index index, Modality.id)))
    in
    node (Tuple patterns) (Type.TTuple types), bindings
  | Construct (constructor, args) ->
    (match
       Members.constructor_types
         t
         scope
         constructor
         ~args
         ~tuple:Source.Pattern.tuple
         ~expected
         ~loc
     with
     | Error error -> fail error args
     | Ok (constructor, args, result) ->
       let args, bindings =
         sub_patterns
           (List.map args ~f:(fun (arg, ty, _) -> arg, ty))
           ~steps:
             (List.mapi args ~f:(fun index (_, _, modality) ->
                Mode_shape.Path.Step.Part
                  (Arg (constructor.name, index), modality)))
       in
       node (Construct (constructor, args)) result, bindings)
  | Record fields ->
    (match
       Members.record_fields t scope fields ~all_required:false ~expected ~loc
     with
     | Error error -> fail error (List.map fields ~f:snd)
     | Ok (record, (_ : Typed.Record_type.t), fields, (_ : bool)) ->
       let steps =
         List.map fields ~f:(fun ((field : Typed.Field_ref.t), _) ->
           let name = Typed.Field_ref.name field in
           Mode_shape.Path.Step.Part
             ( Field name
             , Type_env.field_modality scope.program.type_env name ~record ))
       in
       let patterns, bindings = sub_patterns (List.map fields ~f:snd) ~steps in
       ( node (Record (List.zip_exn (List.map fields ~f:fst) patterns)) record
       , bindings ))
  | Poly_variant (tag, arg) ->
    let arg, bindings =
      match arg with
      | None -> None, []
      | Some pattern ->
        let pattern, bindings =
          pattern_type
            t
            scope
            ~pattern
            ?expected:(row_entry expected `Variant ~label:tag)
        in
        Some pattern, Pattern_var.under (Part (Tag tag, Modality.id)) bindings
    in
    let arg_ty =
      Option.value_map arg ~default:Type.unit ~f:(fun pattern -> pattern.ty)
    in
    ( node (Poly_variant (tag, arg)) (Poly_row.variant t scope ~tag ~arg:arg_ty)
    , bindings )
  | Poly_record { fields; rest } ->
    Poly_row.report_duplicate_labels scope fields;
    let fields =
      List.map fields ~f:(fun (label, pattern) ->
        ( label.txt
        , pattern_type
            t
            scope
            ~pattern
            ?expected:(row_entry expected `Record ~label:label.txt) ))
    in
    let bindings =
      List.concat_map fields ~f:(fun (label, (_, bindings)) ->
        Pattern_var.under (Part (Label label, Modality.id)) bindings)
    in
    let fields =
      List.map fields ~f:(fun (label, (pattern, _)) -> label, pattern)
    in
    let tail, rest, rest_bindings =
      match rest with
      | Closed -> Type.REmpty, Typed.Pattern.Rest.Closed, []
      | Open -> Scope.fresh_row t scope, Open, []
      | Bind binder ->
        let tail = Scope.fresh_row t scope in
        let ident, var = bind binder (Fresh.record t tail) in
        tail, Bind ident, [ var ]
    in
    let row =
      Poly_row.present
        (List.map fields ~f:(fun (label, (pattern : Typed.Pattern.t)) ->
           label, pattern.ty))
        ~tail
    in
    ( node (Poly_record { fields; rest }) (Fresh.record t row)
    , bindings @ rest_bindings )
  | Exception pattern ->
    fail (Typecheck_error.create ~loc Misplaced_exception_pattern) [ pattern ]
  | Or (left, right) ->
    let left, left_vars = pattern_type t scope ~pattern:left ?expected in
    let right_alternative = Alternative.create left_vars in
    let right, right_vars =
      pattern_type'
        ~alternative:(Some right_alternative)
        t
        scope
        ~pattern:right
        ?expected
    in
    Alternative.report_unmatched right_alternative scope;
    let right =
      if Scope.unifies t scope ~ty1:left.ty ~ty2:right.ty ~loc:right.loc
      then right
      else Typed.Pattern.error [ right ] ~loc:right.loc
    in
    ( node (Or (left, right)) left.ty
    , Pattern_var.alternatives left_vars right_vars )
;;

let pattern_type ?expected t scope ~pattern =
  pattern_type' ?expected ~alternative:None t scope ~pattern
;;
