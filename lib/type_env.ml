open! Core
open Syntax

module Constructor = struct
  type t =
    { name : string
    ; args : Type.ty list
    ; result : Type.ty
    }
  [@@deriving sexp_of]
end

module Field = struct
  type t =
    { name : string
    ; type_name : string
    ; record : Type.ty
    ; ty : Type.ty
    ; all_fields : string list
    }
  [@@deriving sexp_of]
end

module Decl = struct
  type t =
    { decl : Type_decl.t
    ; name : string
    ; module_ : Module_scope.t
    ; params : Type.arg list
    }

  let param_map t =
    List.zip_exn (List.map t.decl.params ~f:Type_decl.Param.name) t.params
    |> String.Map.of_alist_exn
  ;;
end

type t =
  { types : Decl.t String.Map.t
  ; constructors : Constructor.t String.Map.t
  ; fields : Field.t String.Map.t
  ; checked : Signature.Type_decl.t String.Map.t
  }

let builtins =
  List.map [ "int"; "bool"; "string"; "char"; "unit" ] ~f:(fun name ->
    { Type_decl.name; params = []; kind = Variant [] })
;;

let find_in map module_ namespace path ~unbound
  : (_, Typecheck_error.t) Result.t
  =
  match%bind.Result Module_scope.find module_ namespace path with
  | None -> Error (unbound path)
  | Some name -> Ok (Map.find_exn map name)
;;

let find_type t module_ path =
  find_in t.types module_ Type path ~unbound:(fun path -> Unbound_type path)
;;

let find_constructor t module_ path =
  find_in t.constructors module_ Constructor path ~unbound:(fun path ->
    Unbound_constructor path)
;;

let find_field t module_ path =
  find_in t.fields module_ Field path ~unbound:(fun path -> Unbound_field path)
;;

let find_decl_exn t name = Map.find_exn t.checked name

module Annotation_vars = struct
  type t =
    { fresh : Fresh.t
    ; rank : Type.rank
    ; tys : Type.ty String.Table.t
    ; rows : Type.row String.Table.t
    }

  let create fresh ~rank =
    { fresh; rank; tys = String.Table.create (); rows = String.Table.create () }
  ;;

  let ty t name =
    Hashtbl.find_or_add t.tys name ~default:(fun () ->
      Type.TVar (Fresh.var t.fresh ~rank:t.rank))
  ;;

  let row t name =
    Hashtbl.find_or_add t.rows name ~default:(fun () ->
      Type.RVar (Fresh.var t.fresh ~rank:t.rank))
  ;;
end

module Vars = struct
  type t =
    | Params of Type.arg String.Map.t
    | Annotation of Annotation_vars.t

  let ty t name : (Type.ty, Typecheck_error.t) Result.t =
    match t with
    | Annotation vars -> Ok (Annotation_vars.ty vars name)
    | Params params ->
      (match Map.find params name with
       | Some (ATy ty) -> Ok ty
       | Some (ARow _) -> Error (Type_variable_kind_mismatch name)
       | None -> Error (Unbound_type_variable name))
  ;;

  let row t name : (Type.row, Typecheck_error.t) Result.t =
    match t with
    | Annotation vars -> Ok (Annotation_vars.row vars name)
    | Params params ->
      (match Map.find params name with
       | Some (ARow row) -> Ok row
       | Some (ATy _) -> Error (Type_variable_kind_mismatch name)
       | None -> Error (Unbound_type_variable name))
  ;;
end

let rec elaborate' t module_ vars (expr : Type_expr.t)
  : (Type.ty, Typecheck_error.t) Result.t
  =
  match expr with
  | Var name -> Vars.ty vars name
  | Con (path, args) -> elaborate_con t module_ vars path args
  | Arrow (ty1, ty2) ->
    let%bind.Result ty1 = elaborate' t module_ vars ty1 in
    let%map.Result ty2 = elaborate' t module_ vars ty2 in
    Type.TArrow (ty1, ty2)
  | Tuple tys ->
    let%map.Result tys =
      List.map tys ~f:(elaborate' t module_ vars) |> Result.all
    in
    Type.TTuple tys
  | Code { inner; context } ->
    let%bind.Result inner = elaborate' t module_ vars inner in
    let%map.Result context = elaborate_row t module_ vars context in
    Type.TCode { inner; context }
  | Record row ->
    let%map.Result row = elaborate_row t module_ vars row in
    Type.TRecord row
  | Variant row ->
    let%map.Result row = elaborate_row t module_ vars row in
    Type.TVariant row

and elaborate_row t module_ vars (row : Type_expr.row)
  : (Type.row, Typecheck_error.t) Result.t
  =
  let rec go (row : Type_expr.row) ~seen =
    match row with
    | Empty -> Ok Type.REmpty
    | Extend (name, _, _) when Set.mem seen name ->
      Error (Typecheck_error.Duplicate_row_entry name)
    | Extend (name, ty, rest) ->
      let%bind.Result ty = elaborate' t module_ vars ty in
      let%map.Result rest = go rest ~seen:(Set.add seen name) in
      Type.RExtend (name, Present, ty, rest)
    | Var name -> Vars.row vars name
  in
  go row ~seen:String.Set.empty

and elaborate_con t module_ vars path args =
  let%bind.Result decl = find_type t module_ path in
  let expected = List.length decl.params in
  let got = List.length args in
  let%bind.Result () =
    if expected = got
    then Ok ()
    else
      Error
        (Typecheck_error.Type_arity_mismatch { type_name = path; expected; got })
  in
  let%bind.Result args =
    List.map2_exn decl.decl.params args ~f:(fun param arg ->
      match param, arg with
      | Ty _, Ty ty ->
        elaborate' t module_ vars ty |> Result.map ~f:(fun ty -> Type.ATy ty)
      | Row _, Row row ->
        elaborate_row t module_ vars row
        |> Result.map ~f:(fun row -> Type.ARow row)
      | Ty _, Row _ | Row _, Ty _ ->
        Error
          (Typecheck_error.Type_arg_kind_mismatch { type_name = path; param }))
    |> Result.all
  in
  match decl.decl.kind with
  | Alias body ->
    elaborate'
      t
      decl.module_
      (Params (Decl.param_map { decl with params = args }))
      body
  | Variant _ | Record _ -> Ok (Type.TCon (decl.name, args))
;;

let elaborate t module_ vars expr = elaborate' t module_ (Annotation vars) expr

let rec referenced_types (expr : Type_expr.t) =
  match expr with
  | Var _ -> []
  | Con (path, args) ->
    path
    :: List.concat_map args ~f:(function
      | Ty ty -> referenced_types ty
      | Row row -> row_referenced_types row)
  | Arrow (ty1, ty2) -> referenced_types ty1 @ referenced_types ty2
  | Tuple tys -> List.concat_map tys ~f:referenced_types
  | Code { inner; context } ->
    referenced_types inner @ row_referenced_types context
  | Record row | Variant row -> row_referenced_types row

and row_referenced_types (row : Type_expr.row) =
  match row with
  | Empty | Var _ -> []
  | Extend (_, ty, rest) -> referenced_types ty @ row_referenced_types rest
;;

let check_alias_cycles t =
  let finished = String.Hash_set.create () in
  let rec visit (decl : Decl.t) ~visiting =
    match decl.decl.kind with
    | Variant _ | Record _ -> Ok ()
    | Alias _ when Hash_set.mem finished decl.name -> Ok ()
    | Alias _ when Set.mem visiting decl.name ->
      Error (Typecheck_error.Cyclic_type_alias decl.name)
    | Alias body ->
      let visiting = Set.add visiting decl.name in
      let%map.Result () =
        List.filter_map (referenced_types body) ~f:(fun path ->
          find_type t decl.module_ path |> Result.ok)
        |> List.fold_result ~init:() ~f:(fun () decl -> visit decl ~visiting)
      in
      Hash_set.add finished decl.name
  in
  Map.data t.types
  |> List.fold_result ~init:() ~f:(fun () decl ->
    visit decl ~visiting:String.Set.empty)
;;

let declare module_ (decl : Type_decl.t) ~fresh =
  let%map.Result () =
    match
      List.map decl.params ~f:Type_decl.Param.name
      |> List.find_a_dup ~compare:String.compare
    with
    | None -> Ok ()
    | Some param ->
      Error
        (Typecheck_error.Duplicate_type_parameter
           { type_name = Module_scope.canonical_name module_ decl.name; param })
  in
  let params =
    List.map decl.params ~f:(function
      | Ty _ -> Type.ATy (TVar (Type.Var.create (Fresh.id fresh) Generic))
      | Row _ -> ARow (RVar (Type.Var.create (Fresh.id fresh) Generic)))
  in
  { Decl.decl
  ; name = Module_scope.canonical_name module_ decl.name
  ; module_
  ; params
  }
;;

let define t (decl : Decl.t) =
  let elaborate = elaborate' t decl.module_ (Params (Decl.param_map decl)) in
  let canonical_name = Module_scope.canonical_name decl.module_ in
  let%map.Result kind =
    match decl.decl.kind with
    | Alias body ->
      let%map.Result body = elaborate body in
      Signature.Type_decl.Kind.Alias body
    | Variant constructors ->
      let%map.Result constructors =
        List.map constructors ~f:(fun (name, args) ->
          let%map.Result args = List.map args ~f:elaborate |> Result.all in
          { Signature.Constructor.name; args })
        |> Result.all
      in
      Signature.Type_decl.Kind.Variant constructors
    | Record fields ->
      let%map.Result fields =
        List.map fields ~f:(fun (name, ty) ->
          let%map.Result ty = elaborate ty in
          { Signature.Field.name; ty })
        |> Result.all
      in
      Signature.Type_decl.Kind.Record fields
  in
  let checked =
    Map.add_exn
      t.checked
      ~key:decl.name
      ~data:
        { Signature.Type_decl.name = decl.decl.name
        ; params = decl.params
        ; kind
        }
  in
  let t = { t with checked } in
  match kind with
  | Alias _ -> t
  | Variant constructors ->
    let result = Type.TCon (decl.name, decl.params) in
    let constructors =
      List.fold
        constructors
        ~init:t.constructors
        ~f:(fun constructors { name; args } ->
          let name = canonical_name name in
          Map.add_exn
            constructors
            ~key:name
            ~data:{ Constructor.name; args; result })
    in
    { t with constructors }
  | Record fields ->
    let all_fields =
      List.map fields ~f:(fun (field : Signature.Field.t) ->
        canonical_name field.name)
    in
    let fields =
      List.fold fields ~init:t.fields ~f:(fun fields { name; ty } ->
        let name = canonical_name name in
        Map.add_exn
          fields
          ~key:name
          ~data:
            { Field.name
            ; type_name = decl.name
            ; record = TCon (decl.name, decl.params)
            ; ty
            ; all_fields
            })
    in
    { t with fields }
;;

let create root fresh =
  let%bind.Result decls =
    Module_scope.toplevels root
    |> List.filter_map ~f:(fun (module_, (toplevel : Toplevel.t)) ->
      match toplevel with
      | Type decl -> Some (declare module_ decl ~fresh)
      | Let _ | Module _ -> None)
    |> Result.all
  in
  let t =
    { types =
        List.map decls ~f:(fun (decl : Decl.t) -> decl.name, decl)
        |> String.Map.of_alist_exn
    ; constructors = String.Map.empty
    ; fields = String.Map.empty
    ; checked = String.Map.empty
    }
  in
  let%bind.Result () = check_alias_cycles t in
  List.fold_result decls ~init:t ~f:define
;;
