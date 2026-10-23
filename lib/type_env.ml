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
    ; ty : Type.ty
    ; record : Type.ty
    ; all_fields : string list
    }
  [@@deriving sexp_of]
end

module Decl = struct
  type t =
    { decl : Type_decl.t
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

let empty =
  { types = String.Map.empty
  ; constructors = String.Map.empty
  ; fields = String.Map.empty
  }
;;

(* Modules will make these lexical lookups. *)
let find_in map (path : Path.t) =
  match (path :> string list) with
  | [ name ] -> Map.find map name
  | _ -> None
;;

let find_type t path = find_in t.types path

let find_constructor t path =
  find_in t.constructors path
  |> Result.of_option ~error:(Typecheck_error.Unbound_constructor path)
;;

let find_field t path =
  find_in t.fields path
  |> Result.of_option ~error:(Typecheck_error.Unbound_field path)
;;

let builtins =
  String.Map.of_alist_exn
    [ "int", Type.int
    ; "bool", Type.bool
    ; "string", Type.string
    ; "char", Type.char
    ; "unit", Type.unit
    ]
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
       | Some (AContext _) | None -> Error (Unbound_type_variable name))
  ;;

  let context t name : (Type.context, Typecheck_error.t) Result.t =
    match t with
    | Annotation vars -> Ok (Annotation_vars.context vars name)
    | Params params ->
      (match Map.find params name with
       | Some (AContext context) -> Ok context
       | Some (ATy _) | None -> Error (Unbound_type_variable name))
  ;;
end

let rec elaborate' t vars (expr : Type_expr.t)
  : (Type.ty, Typecheck_error.t) Result.t
  =
  match expr with
  | Var name -> Vars.ty vars name
  | Con (path, args) -> elaborate_con t vars path args
  | Arrow (ty1, ty2) ->
    let%bind.Result ty1 = elaborate' t vars ty1 in
    let%map.Result ty2 = elaborate' t vars ty2 in
    Type.TArrow (ty1, ty2)
  | Tuple tys ->
    let%map.Result tys = List.map tys ~f:(elaborate' t vars) |> Result.all in
    Type.TTuple tys
  | Code { inner; context } ->
    let%bind.Result inner = elaborate' t vars inner in
    let%map.Result context = elaborate_context t vars context in
    Type.TCode { inner; context }

and elaborate_context t vars (context : Type_expr.context)
  : (Type.context, Typecheck_error.t) Result.t
  =
  match context with
  | Empty -> Ok CEmpty
  | Extend (name, ty, rest) ->
    let%bind.Result ty = elaborate' t vars ty in
    let%map.Result rest = elaborate_context t vars rest in
    Type.CExtend (name, Present, ty, rest)
  | Var name -> Vars.context vars name

and elaborate_con t vars path args =
  let check_arity ~expected =
    let got = List.length args in
    if expected = got
    then Ok ()
    else
      Error
        (Typecheck_error.Type_arity_mismatch { type_name = path; expected; got })
  in
  match find_type t path, (path :> string list) with
  | None, [ name ] when Map.mem builtins name ->
    let%map.Result () = check_arity ~expected:0 in
    Map.find_exn builtins name
  | None, _ -> Error (Unbound_type path)
  | Some decl, _ ->
    let%bind.Result () = check_arity ~expected:(List.length decl.params) in
    let%bind.Result args =
      List.map2_exn decl.decl.params args ~f:(fun param arg ->
        match param, arg with
        | Ty _, Ty ty ->
          elaborate' t vars ty |> Result.map ~f:(fun ty -> Type.ATy ty)
        | Context _, Context context ->
          elaborate_context t vars context
          |> Result.map ~f:(fun context -> Type.AContext context)
        | Ty _, Context _ | Context _, Ty _ ->
          Error
            (Typecheck_error.Type_arg_kind_mismatch { type_name = path; param }))
      |> Result.all
    in
    (match decl.decl.kind with
     | Alias body ->
       elaborate' t (Params (Decl.param_map { decl with params = args })) body
     | Variant _ | Record _ -> Ok (Type.TCon (decl.decl.name, args)))
;;

let elaborate t vars expr = elaborate' t (Annotation vars) expr

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
    | Alias _ when Hash_set.mem finished decl.decl.name -> Ok ()
    | Alias _ when Set.mem visiting decl.decl.name ->
      Error (Typecheck_error.Cyclic_type_alias decl.decl.name)
    | Alias body ->
      let visiting = Set.add visiting decl.decl.name in
      let%map.Result () =
        List.filter_map (referenced_types body) ~f:(find_type t)
        |> List.fold_result ~init:() ~f:(fun () decl -> visit decl ~visiting)
      in
      Hash_set.add finished decl.decl.name
  in
  Map.data t.types
  |> List.fold_result ~init:() ~f:(fun () decl ->
    visit decl ~visiting:String.Set.empty)
;;

let add map ~key ~data ~duplicate =
  match Map.add map ~key ~data with
  | `Ok map -> Ok map
  | `Duplicate -> Error (duplicate key)
;;

let declare_types decls ~fresh_id =
  List.fold_result
    decls
    ~init:String.Map.empty
    ~f:(fun types (decl : Type_decl.t) ->
      let%bind.Result () =
        match
          List.map decl.params ~f:Type_decl.Param.name
          |> List.find_a_dup ~compare:String.compare
        with
        | None -> Ok ()
        | Some param ->
          Error
            (Typecheck_error.Duplicate_type_parameter
               { type_name = decl.name; param })
      in
      let params =
        List.map decl.params ~f:(function
          | Ty _ -> Type.ATy (TVar (ref (Type.Generic (fresh_id ()))))
          | Context _ -> AContext (CVar (ref (Type.CGeneric (fresh_id ())))))
      in
      add
        types
        ~key:decl.name
        ~data:{ Decl.decl; params }
        ~duplicate:(fun name -> Typecheck_error.Duplicate_type name))
;;

let define t (decl : Decl.t) =
  let vars = Vars.Params (Decl.param_map decl) in
  let result = Type.TCon (decl.decl.name, decl.params) in
  match decl.decl.kind with
  | Alias body ->
    let%map.Result (_ : Type.ty) = elaborate' t vars body in
    t
  | Variant constructors ->
    let%map.Result constructors =
      List.fold_result
        constructors
        ~init:t.constructors
        ~f:(fun constructors (name, args) ->
          let%bind.Result args =
            List.map args ~f:(elaborate' t vars) |> Result.all
          in
          add
            constructors
            ~key:name
            ~data:{ Constructor.name; args; result }
            ~duplicate:(fun name -> Typecheck_error.Duplicate_constructor name))
    in
    { t with constructors }
  | Record fields ->
    let all_fields = List.map fields ~f:fst in
    let%map.Result fields =
      List.fold_result fields ~init:t.fields ~f:(fun fields (name, ty) ->
        let%bind.Result ty = elaborate' t vars ty in
        add
          fields
          ~key:name
          ~data:
            { Field.name
            ; type_name = decl.decl.name
            ; ty
            ; record = result
            ; all_fields
            }
          ~duplicate:(fun name -> Typecheck_error.Duplicate_field name))
    in
    { t with fields }
;;

let create decls ~fresh_id =
  let%bind.Result types = declare_types decls ~fresh_id in
  let t = { empty with types } in
  let%bind.Result () = check_alias_cycles t in
  List.fold_result decls ~init:t ~f:(fun t (decl : Type_decl.t) ->
    define t (Map.find_exn types decl.name))
;;
