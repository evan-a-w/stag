open! Import
open Syntax

module Constructor = struct
  type t =
    { name : string
    ; type_name : string
    ; index : int
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
    ; index : int
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
    ; params : Type.arg list Lazy.t
    ; wrap_error : Typecheck_error.t -> Typecheck_error.t
    ; mutable checked : Signature.Type_decl.t option
    }

  let params t = force t.params

  let param_map t ~params =
    List.zip_exn
      (List.map t.decl.params ~f:(fun param -> Type_decl.Param.name param.txt))
      params
    |> String.Map.of_alist_exn
  ;;
end

type t =
  { fresh : Fresh.t
  ; types : Decl.t String.Table.t
  ; constructors : Constructor.t String.Table.t
  ; fields : Field.t String.Table.t
  ; constructor_types : string String.Table.t
  ; field_types : string String.Table.t
    (** The type declaring each constructor and field, whose entry in
        [constructors] or [fields] is added once it is checked. *)
  }

let create fresh =
  { fresh
  ; types = String.Table.create ()
  ; constructors = String.Table.create ()
  ; fields = String.Table.create ()
  ; constructor_types = String.Table.create ()
  ; field_types = String.Table.create ()
  }
;;

let builtins =
  List.map [ "int"; "bool"; "string"; "char"; "unit" ] ~f:(fun name ->
    { Type_decl.name = Located.create name; params = []; kind = Abstract })
;;

let find_resolved t module_ namespace path ~loc ~unbound ~find =
  let%bind.Result name =
    Module_scope.find module_ namespace path ~loc ~unbound
  in
  find t name ~loc
;;

let resolve_type module_ (path : Decl_path.t) ~loc =
  match path with
  | Resolved name -> Ok name
  | Path path ->
    Module_scope.find module_ Type path ~loc ~unbound:(fun path ->
      Unbound_type path)
;;

let find_type t module_ path ~loc =
  let%bind.Result name = resolve_type module_ path ~loc in
  match Hashtbl.find t.types name with
  | Some decl -> Ok decl
  | None ->
    Error (Typecheck_error.create ~loc (Unbound_type (Path.of_string name)))
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

  let error ~loc kind = Error (Typecheck_error.create ~loc kind)

  let ty t name ~loc : (Type.ty, Typecheck_error.t) Result.t =
    match t with
    | Annotation vars -> Ok (Annotation_vars.ty vars name)
    | Params params ->
      (match Map.find params name with
       | Some (ATy ty) -> Ok ty
       | Some (ARow _) -> error ~loc (Type_variable_kind_mismatch name)
       | None -> error ~loc (Unbound_type_variable name))
  ;;

  let row t name ~loc : (Type.row, Typecheck_error.t) Result.t =
    match t with
    | Annotation vars -> Ok (Annotation_vars.row vars name)
    | Params params ->
      (match Map.find params name with
       | Some (ARow row) -> Ok row
       | Some (ATy _) -> error ~loc (Type_variable_kind_mismatch name)
       | None -> error ~loc (Unbound_type_variable name))
  ;;
end

(* An expansion of an alias: [knot], linked to the alias's body once
   elaborated, stands for the expansion. Each alias is expanded once per
   (physical) arguments during one elaboration, so references to it share the
   knot, also while it is being expanded, which closes a cycle. As the knot is
   a [Link], traversals of the expansion's shared occurrences take it once.
   It is [Generic] until linked, but only reachable from the elaboration,
   which stops at its first error and is then dropped. *)
module Expansion = struct
  type t =
    { args : Type.arg list
    ; knot : (Type.ty, Nothing.t) Type.Var.t
    ; mutable expanded : bool
    ; mutable reached_back : bool
    }

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

  (* Whether the cycle the knot closes passes through a record or variant. *)
  let guarded body ~knot =
    let unguarded ~guarded var =
      if (not guarded) && phys_equal var knot then Error () else Ok ()
    in
    let ok ~guarded:_ _ = Ok () in
    Type.iter_ty { ty = unguarded; row = ok; presence = ok } body
    |> Result.is_ok
  ;;
end

(* Like [List.map] then [Result.all], but stops at the first error. *)
let map_result xs ~f =
  List.fold_result xs ~init:[] ~f:(fun ys x ->
    let%map.Result y = f x in
    y :: ys)
  |> Result.map ~f:List.rev
;;

let rec elaborate' t module_ vars ~expansions (expr : Type_expr.t)
  : (Type.ty, Typecheck_error.t) Result.t
  =
  let elaborate = elaborate' t module_ vars ~expansions in
  let loc = expr.loc in
  match expr.txt with
  | Var name -> Vars.ty vars name ~loc
  | Con (path, args) -> elaborate_con t module_ vars ~expansions path args ~loc
  | Arrow (ty1, ty2) ->
    let%bind.Result ty1 = elaborate ty1 in
    let%map.Result ty2 = elaborate ty2 in
    Type.TArrow (ty1, ty2)
  | Tuple tys ->
    let%map.Result tys = map_result tys ~f:elaborate in
    Type.TTuple tys
  | Code { inner; context } ->
    let%bind.Result inner = elaborate inner in
    let%map.Result context = elaborate_row t module_ vars ~expansions context in
    Type.TCode { inner; context }
  | Record row ->
    let%map.Result row = elaborate_row t module_ vars ~expansions row in
    Fresh.record t.fresh row
  | Variant row ->
    let%map.Result row = elaborate_row t module_ vars ~expansions row in
    Fresh.variant t.fresh row

and elaborate_row t module_ vars ~expansions (row : Type_expr.row)
  : (Type.row, Typecheck_error.t) Result.t
  =
  let rec go ({ txt = row; loc = _ } : Type_expr.row) ~seen =
    match row with
    | Empty -> Ok Type.REmpty
    | Extend ({ txt = name; loc }, _, _) when Set.mem seen name ->
      Error (Typecheck_error.create ~loc (Duplicate_row_entry name))
    | Extend ({ txt = name; loc = _ }, ty, rest) ->
      let%bind.Result ty = elaborate' t module_ vars ~expansions ty in
      let%map.Result rest = go rest ~seen:(Set.add seen name) in
      Type.RExtend (name, Present, ty, rest)
    | Var { txt = name; loc } -> Vars.row vars name ~loc
  in
  go row ~seen:String.Set.empty

and elaborate_con
      t
      module_
      vars
      ~expansions
      ({ txt = path; loc = path_loc } : Decl_path.t Located.t)
      args
      ~loc
  =
  let error ~loc kind = Error (Typecheck_error.create ~loc kind) in
  let%bind.Result decl = find_type t module_ path ~loc:path_loc in
  let expected = List.length decl.decl.params in
  let got = List.length args in
  let%bind.Result () =
    if expected = got
    then Ok ()
    else
      error
        ~loc
        (Type_arity_mismatch
           { type_name = Decl_path.to_path path; expected; got })
  in
  let%bind.Result args =
    map_result (List.zip_exn decl.decl.params args) ~f:(fun (param, arg) ->
      match param.txt, arg with
      | Ty _, Ty ty ->
        elaborate' t module_ vars ~expansions ty
        |> Result.map ~f:(fun ty -> Type.ATy ty)
      | Row _, Row row ->
        elaborate_row t module_ vars ~expansions row
        |> Result.map ~f:(fun row -> Type.ARow row)
      (* [{...}] parses as a row, but is a record type for a type parameter. *)
      | Ty _, Row row ->
        elaborate_row t module_ vars ~expansions row
        |> Result.map ~f:(fun row -> Type.ATy (Fresh.record t.fresh row))
      | Row _, Ty { loc; _ } ->
        error
          ~loc
          (Type_arg_kind_mismatch
             { type_name = Decl_path.to_path path; param = param.txt }))
  in
  match decl.decl.kind with
  | Variant _ | Record _ | Abstract -> Ok (Type.TCon (decl.name, args))
  | Alias body ->
    let instances = Hashtbl.find_multi expansions decl.name in
    (match
       List.find instances ~f:(fun expansion ->
         Expansion.same_args expansion args)
     with
     | Some expansion ->
       if not expansion.expanded then expansion.reached_back <- true;
       Ok (Type.TVar expansion.knot)
     | None ->
       if List.exists instances ~f:(fun expansion -> not expansion.expanded)
       then error ~loc (Non_regular_recursive_alias decl.name)
       else expand t decl ~args ~expansions body)

and expand t (decl : Decl.t) ~args ~expansions body =
  let knot = Type.Var.create (Fresh.id t.fresh) Generic in
  let expansion =
    { Expansion.args; knot; expanded = false; reached_back = false }
  in
  Hashtbl.add_multi expansions ~key:decl.name ~data:expansion;
  let%bind.Result body =
    elaborate'
      t
      decl.module_
      (Params (Decl.param_map decl ~params:args))
      ~expansions
      body
  in
  if expansion.reached_back && not (Expansion.guarded body ~knot)
  then
    Error
      (Typecheck_error.create
         ~loc:decl.decl.name.loc
         (Cyclic_type_alias decl.name))
  else (
    knot.state <- Link body;
    expansion.expanded <- true;
    Ok (Type.TVar knot))
;;

let elaborate t module_ vars expr =
  elaborate'
    t
    module_
    (Annotation vars)
    ~expansions:(String.Table.create ())
    expr
;;

let declare
      ?(lazily = false)
      ?(wrap_error = Fn.id)
      t
      module_
      (decl : Type_decl.t)
  =
  let name = Module_scope.canonical_name module_ decl.name.txt in
  let params =
    lazy
      (List.map decl.params ~f:(fun param ->
         match param.txt with
         | Ty _ -> Type.ATy (TVar (Type.Var.create (Fresh.id t.fresh) Generic))
         | Row _ -> ARow (RVar (Type.Var.create (Fresh.id t.fresh) Generic))))
  in
  if not lazily then ignore (force params : Type.arg list);
  Hashtbl.add_exn
    t.types
    ~key:name
    ~data:{ Decl.decl; name; module_; params; wrap_error; checked = None };
  let declares names table =
    List.iter names ~f:(fun ((member : Name.t), _) ->
      Hashtbl.add_exn
        table
        ~key:(Module_scope.canonical_name module_ member.txt)
        ~data:name)
  in
  match decl.kind with
  | Alias _ | Abstract -> ()
  | Variant constructors -> declares constructors t.constructor_types
  | Record fields -> declares fields t.field_types
;;

let define t (decl : Decl.t) =
  let expansions = String.Table.create () in
  let elaborate =
    elaborate'
      t
      decl.module_
      (Params (Decl.param_map decl ~params:(Decl.params decl)))
      ~expansions
  in
  let canonical_name = Module_scope.canonical_name decl.module_ in
  let%map.Result kind =
    match decl.decl.kind with
    | Alias body ->
      let%map.Result body =
        expand t decl ~args:(Decl.params decl) ~expansions body
      in
      Signature.Type_decl.Kind.Alias body
    | Abstract -> Ok Signature.Type_decl.Kind.Abstract
    | Variant constructors ->
      let%map.Result constructors =
        map_result constructors ~f:(fun ((name : Name.t), args) ->
          let%map.Result args = map_result args ~f:elaborate in
          { Signature.Constructor.name = name.txt; args })
      in
      Signature.Type_decl.Kind.Variant constructors
    | Record fields ->
      let%map.Result fields =
        map_result fields ~f:(fun ((name : Name.t), ty) ->
          let%map.Result ty = elaborate ty in
          { Signature.Field.name = name.txt; ty })
      in
      Signature.Type_decl.Kind.Record fields
  in
  let checked =
    { Signature.Type_decl.name = decl.decl.name.txt
    ; params = Decl.params decl
    ; kind
    }
  in
  decl.checked <- Some checked;
  (match kind with
   | Alias _ | Abstract -> ()
   | Variant constructors ->
     let result = Type.TCon (decl.name, Decl.params decl) in
     List.iteri constructors ~f:(fun index { name; args } ->
       let name = canonical_name name in
       Hashtbl.add_exn
         t.constructors
         ~key:name
         ~data:{ Constructor.name; type_name = decl.name; index; args; result })
   | Record fields ->
     let all_fields =
       List.map fields ~f:(fun (field : Signature.Field.t) ->
         canonical_name field.name)
     in
     List.iteri fields ~f:(fun index { name; ty } ->
       let name = canonical_name name in
       Hashtbl.add_exn
         t.fields
         ~key:name
         ~data:
           { Field.name
           ; type_name = decl.name
           ; index
           ; record = TCon (decl.name, Decl.params decl)
           ; ty
           ; all_fields
           }));
  checked
;;

let check t name =
  let decl = Hashtbl.find_exn t.types name in
  match decl.checked with
  | Some checked -> Ok checked
  | None -> define t decl |> Result.map_error ~f:decl.wrap_error
;;

let find_decl_exn t name =
  Option.value_exn (Hashtbl.find_exn t.types name).checked
;;

let is_empty_variant t name =
  let%map.Result checked = check t name in
  match checked.kind with
  | Variant [] -> true
  | Variant (_ :: _) | Record _ | Alias _ | Abstract -> false
;;

(* The entry of the constructor or field of canonical name [name], checking
   its type. *)
let find_member t ~types ~members name ~loc ~unbound =
  match Hashtbl.find types name with
  | None -> Error (Typecheck_error.create ~loc (unbound (Path.of_string name)))
  | Some type_name ->
    let%map.Result (_ : Signature.Type_decl.t) = check t type_name in
    Hashtbl.find_exn members name
;;

let constructor t name ~loc =
  find_member
    t
    ~types:t.constructor_types
    ~members:t.constructors
    name
    ~loc
    ~unbound:(fun path -> Unbound_constructor path)
;;

let field t name ~loc =
  find_member
    t
    ~types:t.field_types
    ~members:t.fields
    name
    ~loc
    ~unbound:(fun path -> Unbound_field path)
;;

let find_constructor t module_ ({ txt = path; loc } : Decl_path.t Located.t) =
  match path with
  | Resolved name -> constructor t name ~loc
  | Path path ->
    find_resolved
    t
    module_
    Constructor
    path
    ~loc
    ~unbound:(fun path -> Unbound_constructor path)
    ~find:constructor
;;

let find_field t module_ ({ txt = path; loc } : Decl_path.t Located.t) =
  match path with
  | Resolved name -> field t name ~loc
  | Path path ->
    find_resolved
    t
    module_
    Field
    path
    ~loc
    ~unbound:(fun path -> Unbound_field path)
    ~find:field
;;

let rec resolve t module_ (expr : Type_expr.t) =
  let resolve = resolve t module_ in
  let%map.Result txt =
    match expr.txt with
    | Var _ as desc -> Ok desc
    | Con (path, args) ->
      let%bind.Result name = resolve_type module_ path.txt ~loc:path.loc in
      let%map.Result args =
        map_result args ~f:(function
          | Ty ty -> Result.map (resolve ty) ~f:(fun ty -> Type_expr.Ty ty)
          | Row row ->
            Result.map (resolve_row t module_ row) ~f:(fun row ->
              Type_expr.Row row))
      in
      Type_expr.Con ({ path with txt = Resolved name }, args)
    | Arrow (a, b) ->
      let%bind.Result a = resolve a in
      let%map.Result b = resolve b in
      Type_expr.Arrow (a, b)
    | Tuple tys ->
      let%map.Result tys = map_result tys ~f:resolve in
      Type_expr.Tuple tys
    | Code { inner; context } ->
      let%bind.Result inner = resolve inner in
      let%map.Result context = resolve_row t module_ context in
      Type_expr.Code { inner; context }
    | Record row ->
      let%map.Result row = resolve_row t module_ row in
      Type_expr.Record row
    | Variant row ->
      let%map.Result row = resolve_row t module_ row in
      Type_expr.Variant row
  in
  { expr with txt }

and resolve_row t module_ (row : Type_expr.row) =
  let%map.Result txt =
    match row.txt with
    | (Empty | Var _) as desc -> Ok desc
    | Extend (label, ty, rest) ->
      let%bind.Result ty = resolve t module_ ty in
      let%map.Result rest = resolve_row t module_ rest in
      Type_expr.Extend (label, ty, rest)
  in
  { row with txt }
;;

let reflect t name =
  let%bind.Result (_ : Signature.Type_decl.t) = check t name in
  let decl = Hashtbl.find_exn t.types name in
  let resolve ty =
    resolve t decl.module_ ty |> Result.map_error ~f:decl.wrap_error
  in
  let%map.Result kind =
    match decl.decl.kind with
    | Abstract -> Ok None
    | Alias ty ->
      let%map.Result ty = resolve ty in
      Some (Type_decl.Kind.Alias ty)
    | Variant constructors ->
      let%map.Result constructors =
        map_result constructors ~f:(fun (name, args) ->
          let%map.Result args = map_result args ~f:resolve in
          name, args)
      in
      Some (Type_decl.Kind.Variant constructors)
    | Record fields ->
      let%map.Result fields =
        map_result fields ~f:(fun (name, ty) ->
          let%map.Result ty = resolve ty in
          name, ty)
      in
      Some (Type_decl.Kind.Record fields)
  in
  Option.map kind ~f:(fun kind -> { decl.decl with kind })
;;
