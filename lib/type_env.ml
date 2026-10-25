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
    ; params : Type.arg list
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

module Annotation_vars = struct
  type t =
    { fresh_id : unit -> Type.id
    ; rank : Type.rank
    ; tys : Type.ty String.Table.t
    ; contexts : Type.context String.Table.t
    }

  let create ~fresh_id ~rank =
    { fresh_id
    ; rank
    ; tys = String.Table.create ()
    ; contexts = String.Table.create ()
    }
  ;;

  let ty t name =
    Hashtbl.find_or_add t.tys name ~default:(fun () ->
      Type.TVar (ref (Type.Unbound (t.fresh_id (), t.rank))))
  ;;

  let context t name =
    Hashtbl.find_or_add t.contexts name ~default:(fun () ->
      Type.CVar (ref (Type.CUnbound (t.fresh_id (), t.rank))))
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
       | Some (AContext _) -> Error (Type_variable_kind_mismatch name)
       | None -> Error (Unbound_type_variable name))
  ;;

  let context t name : (Type.context, Typecheck_error.t) Result.t =
    match t with
    | Annotation vars -> Ok (Annotation_vars.context vars name)
    | Params params ->
      (match Map.find params name with
       | Some (AContext context) -> Ok context
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
    let%map.Result context = elaborate_context t module_ vars context in
    Type.TCode { inner; context }

and elaborate_context t module_ vars (context : Type_expr.context)
  : (Type.context, Typecheck_error.t) Result.t
  =
  let rec go (context : Type_expr.context) ~seen =
    match context with
    | Empty -> Ok Type.CEmpty
    | Extend (name, _, _) when Set.mem seen name ->
      Error (Typecheck_error.Duplicate_context_entry name)
    | Extend (name, ty, rest) ->
      let%bind.Result ty = elaborate' t module_ vars ty in
      let%map.Result rest = go rest ~seen:(Set.add seen name) in
      Type.CExtend (name, Present, ty, rest)
    | Var name -> Vars.context vars name
  in
  go context ~seen:String.Set.empty

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
      | Context _, Context context ->
        elaborate_context t module_ vars context
        |> Result.map ~f:(fun context -> Type.AContext context)
      | Ty _, Context _ | Context _, Ty _ ->
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
      | Context context -> context_referenced_types context)
  | Arrow (ty1, ty2) -> referenced_types ty1 @ referenced_types ty2
  | Tuple tys -> List.concat_map tys ~f:referenced_types
  | Code { inner; context } ->
    referenced_types inner @ context_referenced_types context

and context_referenced_types (context : Type_expr.context) =
  match context with
  | Empty | Var _ -> []
  | Extend (_, ty, rest) -> referenced_types ty @ context_referenced_types rest
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

let declare module_ (decl : Type_decl.t) ~fresh_id =
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
      | Ty _ -> Type.ATy (TVar (ref (Type.Generic (fresh_id ()))))
      | Context _ -> AContext (CVar (ref (Type.CGeneric (fresh_id ())))))
  in
  { Decl.decl
  ; name = Module_scope.canonical_name module_ decl.name
  ; module_
  ; params
  }
;;

let define t (decl : Decl.t) =
  let vars = Vars.Params (Decl.param_map decl) in
  let elaborate = elaborate' t decl.module_ vars in
  let canonical_name = Module_scope.canonical_name decl.module_ in
  match decl.decl.kind with
  | Alias body ->
    let%map.Result (_ : Type.ty) = elaborate body in
    t
  | Variant constructors ->
    let%map.Result constructors =
      List.fold_result
        constructors
        ~init:t.constructors
        ~f:(fun constructors (name, args) ->
          let%map.Result args = List.map args ~f:elaborate |> Result.all in
          let name = canonical_name name in
          Map.add_exn
            constructors
            ~key:name
            ~data:
              { Constructor.name
              ; args
              ; result = Type.TCon (decl.name, decl.params)
              })
    in
    { t with constructors }
  | Record fields ->
    let all_fields =
      List.map fields ~f:(fun (name, _) -> canonical_name name)
    in
    let%map.Result fields =
      List.fold_result fields ~init:t.fields ~f:(fun fields (name, ty) ->
        let%map.Result ty = elaborate ty in
        let name = canonical_name name in
        Map.add_exn
          fields
          ~key:name
          ~data:
            { Field.name
            ; type_name = decl.name
            ; params = decl.params
            ; ty
            ; all_fields
            })
    in
    { t with fields }
;;

let create root ~fresh_id =
  let%bind.Result decls =
    Module_scope.toplevels root
    |> List.filter_map ~f:(fun (module_, (toplevel : Toplevel.t)) ->
      match toplevel with
      | Type decl -> Some (declare module_ decl ~fresh_id)
      | Let _ | Module _ -> None)
    |> Result.all
  in
  let t =
    { types =
        List.map decls ~f:(fun (decl : Decl.t) -> decl.name, decl)
        |> String.Map.of_alist_exn
    ; constructors = String.Map.empty
    ; fields = String.Map.empty
    }
  in
  let%bind.Result () = check_alias_cycles t in
  List.fold_result decls ~init:t ~f:define
;;
