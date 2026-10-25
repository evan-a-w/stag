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
  { fresh : Fresh.t
  ; types : Decl.t String.Map.t
  ; constructors : Constructor.t String.Map.t
  ; fields : Field.t String.Map.t
  ; checked : Signature.Type_decl.t String.Map.t
  }

let builtins =
  List.map [ "int"; "bool"; "string"; "char"; "unit" ] ~f:(fun name ->
    { Type_decl.name; params = []; kind = Abstract })
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

let is_empty_variant t name =
  match (find_decl_exn t name).kind with
  | Variant [] -> true
  | Variant (_ :: _) | Record _ | Alias _ | Abstract -> false
;;

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

(* An alias being expanded, with the variable tying the knot if its body
   refers back to it. *)
module Expansion = struct
  type t =
    { name : string
    ; args : Type.arg list
    ; mutable knot : (Type.ty, Nothing.t) Type.Var.t option
    }

  let knot t ~fresh =
    match t.knot with
    | Some knot -> knot
    | None ->
      let knot = Type.Var.create (Fresh.id fresh) Generic in
      t.knot <- Some knot;
      knot
  ;;

  let same_args t args =
    List.equal
      (fun (arg1 : Type.arg) (arg2 : Type.arg) ->
         match arg1, arg2 with
         | ATy ty1, ATy ty2 -> phys_equal ty1 ty2
         | ARow row1, ARow row2 -> phys_equal row1 row2
         | ATy _, ARow _ | ARow _, ATy _ -> false)
      t.args
      args
  ;;
end

(* [expanding] are the aliases being expanded, innermost first, each with
   whether a record or variant has been entered since: a reference back to one
   is then guarded, so closes a cycle. *)
let rec elaborate' t module_ vars ~expanding (expr : Type_expr.t)
  : (Type.ty, Typecheck_error.t) Result.t
  =
  let elaborate = elaborate' t module_ vars ~expanding in
  let guarded = List.map expanding ~f:(fun (expansion, _) -> expansion, true) in
  match expr with
  | Var name -> Vars.ty vars name
  | Con (path, args) -> elaborate_con t module_ vars ~expanding path args
  | Arrow (ty1, ty2) ->
    let%bind.Result ty1 = elaborate ty1 in
    let%map.Result ty2 = elaborate ty2 in
    Type.TArrow (ty1, ty2)
  | Tuple tys ->
    let%map.Result tys = List.map tys ~f:elaborate |> Result.all in
    Type.TTuple tys
  | Code { inner; context } ->
    let%bind.Result inner = elaborate inner in
    let%map.Result context = elaborate_row t module_ vars ~expanding context in
    Type.TCode { inner; context }
  | Record row ->
    let%map.Result row = elaborate_row t module_ vars ~expanding:guarded row in
    Type.TRecord row
  | Variant row ->
    let%map.Result row = elaborate_row t module_ vars ~expanding:guarded row in
    Type.TVariant row

and elaborate_row t module_ vars ~expanding (row : Type_expr.row)
  : (Type.row, Typecheck_error.t) Result.t
  =
  let rec go (row : Type_expr.row) ~seen =
    match row with
    | Empty -> Ok Type.REmpty
    | Extend (name, _, _) when Set.mem seen name ->
      Error (Typecheck_error.Duplicate_row_entry name)
    | Extend (name, ty, rest) ->
      let%bind.Result ty = elaborate' t module_ vars ~expanding ty in
      let%map.Result rest = go rest ~seen:(Set.add seen name) in
      Type.RExtend (name, Present, ty, rest)
    | Var name -> Vars.row vars name
  in
  go row ~seen:String.Set.empty

and elaborate_con t module_ vars ~expanding path args =
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
        elaborate' t module_ vars ~expanding ty
        |> Result.map ~f:(fun ty -> Type.ATy ty)
      | Row _, Row row ->
        elaborate_row t module_ vars ~expanding row
        |> Result.map ~f:(fun row -> Type.ARow row)
      | Ty _, Row _ | Row _, Ty _ ->
        Error
          (Typecheck_error.Type_arg_kind_mismatch { type_name = path; param }))
    |> Result.all
  in
  match decl.decl.kind with
  | Variant _ | Record _ | Abstract -> Ok (Type.TCon (decl.name, args))
  | Alias body ->
    (match
       List.find expanding ~f:(fun ((expansion : Expansion.t), _) ->
         String.equal expansion.name decl.name)
     with
     | Some (_, false) -> Error (Typecheck_error.Cyclic_type_alias decl.name)
     | Some (expansion, true) ->
       if Expansion.same_args expansion args
       then Ok (Type.TVar (Expansion.knot expansion ~fresh:t.fresh))
       else Error (Typecheck_error.Non_regular_recursive_alias decl.name)
     | None -> expand t decl ~args ~expanding body)

and expand t (decl : Decl.t) ~args ~expanding body =
  let expansion = { Expansion.name = decl.name; args; knot = None } in
  let%map.Result body =
    elaborate'
      t
      decl.module_
      (Params (Decl.param_map { decl with params = args }))
      ~expanding:((expansion, false) :: expanding)
      body
  in
  Option.iter expansion.knot ~f:(fun knot -> knot.state <- Link body);
  body
;;

let elaborate t module_ vars expr =
  elaborate' t module_ (Annotation vars) ~expanding:[] expr
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
  let elaborate =
    elaborate' t decl.module_ (Params (Decl.param_map decl)) ~expanding:[]
  in
  let canonical_name = Module_scope.canonical_name decl.module_ in
  let%map.Result kind =
    match decl.decl.kind with
    | Alias body ->
      let%map.Result body =
        expand t decl ~args:decl.params ~expanding:[] body
      in
      Signature.Type_decl.Kind.Alias body
    | Abstract -> Ok Signature.Type_decl.Kind.Abstract
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
  | Alias _ | Abstract -> t
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
    { fresh
    ; types =
        List.map decls ~f:(fun (decl : Decl.t) -> decl.name, decl)
        |> String.Map.of_alist_exn
    ; constructors = String.Map.empty
    ; fields = String.Map.empty
    ; checked = String.Map.empty
    }
  in
  List.fold_result decls ~init:t ~f:define
;;
