open! Import

let constructor_ref (constructor : Type_env.Constructor.t) =
  { Typed.Constructor_ref.type_name = constructor.type_name
  ; name = constructor.name
  ; index = constructor.index
  }
;;

let field_ref (field : Type_env.Field.t) =
  { Typed.Field_ref.record = field.record_type; index = field.index }
;;

module Known = struct
  type 'member t =
    { type_name : string
    ; members : 'member list
    }

  let find
    (scope : Scope.t)
    (ty : Type.ty option)
    ~(members : Type_env.t -> string -> ('m list option, _) Result.t)
    =
    match Option.map ty ~f:Type.canonical_ty with
    | Some ty when Type.is_error ty ->
      (* What did not check has no members, nor any errors from them. *)
      Error Typecheck_error.already_reported
    | Some (TCon (type_name, _)) ->
      let%map.Result members = members scope.program.type_env type_name in
      Option.map members ~f:(fun members -> { type_name; members })
    | Some _ | None -> Ok None
  ;;

  let record scope ty = find scope ty ~members:Type_env.record_fields
  let variant scope ty = find scope ty ~members:Type_env.variant_constructors

  (* Only an unqualified name is looked up in the known type: a qualified one or
     a ref names its declaration. *)
  let unqualified : Syntax.Decl_path.t Located.t -> string option = function
    | { txt = Path path; loc = _ } ->
      (match (path :> string list) with
       | [ name ] -> Some name
       | _ -> None)
    | { txt = Root _ | Resolved _; loc = _ } -> None
  ;;

  let unqualified_field : Source.Field.t -> string option = function
    | Path path -> unqualified path
    | Ref _ -> None
  ;;

  let has t name ~member_name =
    List.exists t.members ~f:(fun member ->
      String.equal (Prelude_path.short_name (member_name member)) name)
  ;;

  let member t ~name ~member_name ~loc ~missing =
    match
      List.find t.members ~f:(fun member ->
        String.equal (Prelude_path.short_name (member_name member)) name)
    with
    | Some member -> Ok member
    | None -> Typecheck_error.error ~loc (missing ~type_name:t.type_name name)
  ;;
end

let find_constructor (scope : Scope.t) ~loc ~known : Source.Constructor.t -> _
  = function
  | Path path ->
    (match Known.unqualified path, known with
     | Some name, Some (known : _ Known.t) ->
       Known.member
         known
         ~name
         ~member_name:(fun (constructor : Type_env.Constructor.t) ->
           constructor.name)
         ~loc:path.loc
         ~missing:(fun ~type_name constructor ->
           No_such_constructor { type_name; constructor })
       |> Result.map ~f:(fun (constructor : Type_env.Constructor.t) ->
         Type_env.record_member_reference
           scope.program.type_env
           constructor.name
           ~loc:path.loc;
         constructor)
     | _ -> Type_env.find_constructor scope.program.type_env scope.module_ path)
  | Ref { name; _ } -> Type_env.constructor scope.program.type_env name ~loc
;;

(* Generated code's fields are at [loc], its node's. *)
let field_loc ~loc : Source.Field.t -> Loc.t = function
  | Path path -> path.loc
  | Ref _ -> loc
;;

let find_field (scope : Scope.t) ~loc ~known (field : Source.Field.t) =
  match field with
  | Path path ->
    (match Known.unqualified path, known with
     | Some name, Some (known : _ Known.t) ->
       Known.member
         known
         ~name
         ~member_name:Type_env.Field.name
         ~loc:path.loc
         ~missing:(fun ~type_name field -> No_such_field { type_name; field })
       |> Result.map ~f:(fun field ->
         Type_env.record_member_reference
           scope.program.type_env
           (Type_env.Field.name field)
           ~loc:path.loc;
         field)
     | _ -> Type_env.find_field scope.program.type_env scope.module_ path)
  | Ref field ->
    Type_env.field scope.program.type_env (Typed.Field_ref.name field) ~loc
;;

let unify_known t ~expected ty ~loc =
  match Option.map expected ~f:Type.canonical_ty, Type.canonical_ty ty with
  | Some (TCon (known, _) as expected), TCon (name, _)
    when String.equal known name ->
    Unify.unify_ty t ~ty1:ty ~ty2:expected |> Typecheck_error.lift ~loc
  | _ -> Ok ()
;;

let constructor_types
  t
  (scope : Scope.t)
  constructor
  ~args
  ~tuple
  ~expected
  ~loc
  =
  let%bind.Result known = Known.variant scope expected in
  let%bind.Result (constructor : Type_env.Constructor.t) =
    find_constructor scope constructor ~loc ~known
  in
  let args =
    match args, constructor.args with
    | _ :: _ :: _, [ _ ] -> [ tuple args ]
    | _ -> args
  in
  match List.zip args constructor.args with
  | Unequal_lengths ->
    Typecheck_error.error
      ~loc
      (Constructor_arity_mismatch
         { constructor = constructor.name
         ; expected = List.length constructor.args
         ; got = List.length args
         })
  | Ok args ->
    let inst = Scope.instantiate t scope in
    let result = inst constructor.result in
    let%map.Result () = unify_known t ~expected result ~loc in
    ( constructor_ref constructor
    , List.map2_exn args constructor.modalities ~f:(fun (arg, ty) modality ->
        arg, inst ty, Type_env.modality modality ~of_:result)
    , result )
;;

(* The record type [fields] are of: [expected] if it is known to be a record
   type, else that of the first qualified field (or ref), else the first of the
   unqualified fields' types in scope that has all of them. *)
let record_type (scope : Scope.t) fields_ne ~expected ~loc =
  let fields = Nonempty_list.to_list fields_ne in
  let of_field field =
    let%bind.Result (field : Type_env.Field.t) =
      find_field scope field ~loc ~known:None
    in
    let%map.Result known = Known.record scope (Some field.record) in
    Option.value_exn known
  in
  match%bind.Result Known.record scope expected with
  | Some known -> Ok known
  | None ->
    (match
       List.find fields ~f:(fun (field, _) ->
         Option.is_none (Known.unqualified_field field))
     with
     | Some (field, _) -> of_field field
     | None ->
       let names =
         List.filter_map fields ~f:(fun (field, _) ->
           Known.unqualified_field field)
       in
       let has_all known =
         List.for_all
           names
           ~f:(Known.has known ~member_name:Type_env.Field.name)
       in
       (match
          List.find_map fields ~f:(fun (field, _) ->
            match of_field field with
            | Ok known when has_all known -> Some known
            | Ok _ | Error _ -> None)
        with
        | Some known -> Ok known
        | None -> of_field (fst (Nonempty_list.hd fields_ne))))
;;

let is_given fields (member : Type_env.Field.t) =
  List.exists fields ~f:(fun ((field : Type_env.Field.t), _, _) ->
    field.index = member.index)
;;

module Resolved = struct
  type 'a t =
    { known : Type_env.Field.t Known.t
    ; declared : Type.ty
    ; record_type : Typed.Record_type.t
    ; fields : (Type_env.Field.t * Loc.t * 'a) list
    ; complete : bool
    }
end

let resolve_record_fields (scope : Scope.t) fields ~all_required ~expected ~loc =
  let%bind.Result first, rest =
    match fields with
    | [] -> Typecheck_error.error ~loc Empty_record
    | first :: rest -> Ok (first, rest)
  in
  let%bind.Result (known : Type_env.Field.t Known.t) =
    record_type scope (first :: rest) ~expected ~loc
  in
  let find (field, x) =
    let%bind.Result (found : Type_env.Field.t) =
      find_field scope field ~loc ~known:(Some known)
    in
    let loc = field_loc field ~loc in
    if String.equal found.record_type.name known.type_name
    then Ok (found, loc, x)
    else
      Typecheck_error.error
        ~loc
        (Fields_of_different_types
           { field = Type_env.Field.name found
           ; expected_type = known.type_name
           ; actual_type = found.record_type.name
           })
  in
  (* A qualified field of another type is the error, rather than an unqualified
     one the type lacks. *)
  let%bind.Result (_ : _ list) =
    List.filter fields ~f:(fun (field, _) ->
      Option.is_none (Known.unqualified_field field))
    |> List.map ~f:find
    |> Result.all
  in
  let%bind.Result ((first, (_ : Loc.t), _) as found) = find first in
  let%bind.Result rest = List.map rest ~f:find |> Result.all in
  let fields = found :: rest in
  let%bind.Result () =
    Duplicates.check
      (List.map fields ~f:(fun (field, loc, _) ->
         { Located.txt = Prelude_path.short_name (Type_env.Field.name field)
         ; loc
         }))
      ~error:(fun name -> Repeated_field name)
  in
  let complete =
    match List.find known.members ~f:(Fn.non (is_given fields)) with
    | Some member when all_required ->
      Scope.report
        scope
        (Typecheck_error.create
           ~loc
           (Missing_field (Type_env.Field.name member)));
      false
    | Some _ | None -> true
  in
  Ok
    { Resolved.known
    ; declared = first.record
    ; record_type = first.record_type
    ; fields
    ; complete
    }
;;

let record_fields t (scope : Scope.t) fields ~all_required ~expected ~loc =
  let%bind.Result { Resolved.declared
                  ; record_type
                  ; fields
                  ; complete
                  ; known = _
                  }
    =
    resolve_record_fields scope fields ~all_required ~expected ~loc
  in
  let inst = Scope.instantiate t scope in
  let record = inst declared in
  let%map.Result () = unify_known t ~expected record ~loc in
  let fields =
    List.map fields ~f:(fun ((field : Type_env.Field.t), _, x) ->
      field_ref field, (x, inst field.ty))
  in
  record, record_type, fields, complete
;;

let field_type t (scope : Scope.t) field ~record ~loc =
  let%bind.Result known = Known.record scope record in
  let%map.Result (field : Type_env.Field.t) =
    find_field scope field ~loc ~known
  in
  let inst = Scope.instantiate t scope in
  let ty = inst field.ty in
  field_ref field, inst field.record, ty
;;
