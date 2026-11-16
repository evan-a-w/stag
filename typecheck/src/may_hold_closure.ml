open! Import

let safe_builtins = [ "int"; "bool"; "string"; "char"; "unit"; Type.error_name ]

let ty type_env ty =
  let visited = String.Hash_set.create () in
  let rec ty_ ~in_decl (ty : Type.ty) =
    match Type.canonical_ty ty with
    | TArrow _ -> true
    | TCode _ -> false
    | TTuple tys -> List.exists tys ~f:(ty_ ~in_decl)
    | TRecord { row; _ } | TVariant { row; _ } -> row_ ~in_decl row
    | TVar { state = Generic; _ } -> not in_decl
    | TVar _ -> true
    | TCon (name, args) ->
      List.exists args ~f:(function
        | ATy ty -> ty_ ~in_decl ty
        | ARow row -> row_ ~in_decl row)
      || decl name
  and row_ ~in_decl row =
    match Type.canonical_row row with
    | REmpty -> false
    | RExtend (_, presence, _, ty, rest) ->
      (match Type.canonical_presence presence with
       | Absent -> false
       | Present | PVar _ -> ty_ ~in_decl ty)
      || row_ ~in_decl rest
    | RVar { state = Generic; _ } -> not in_decl
    | RVar _ -> true
  (* Its parameters are checked as the arguments, so are safe here. *)
  and decl name =
    if List.mem safe_builtins name ~equal:String.equal
       || Hash_set.mem visited name
    then false
    else (
      Hash_set.add visited name;
      match Type_env.check type_env name with
      | Error _ -> true
      | Ok { kind; _ } ->
        (match kind with
         | Abstract -> true
         | Alias ty -> ty_ ~in_decl:true ty
         | Record fields ->
           List.exists fields ~f:(fun (field : Signature.Field.t) ->
             ty_ ~in_decl:true field.ty)
         | Variant constructors ->
           List.exists
             constructors
             ~f:(fun (constructor : Signature.Constructor.t) ->
               List.exists constructor.args ~f:(ty_ ~in_decl:true))))
  in
  ty_ ~in_decl:false ty
;;
