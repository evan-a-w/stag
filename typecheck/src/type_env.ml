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
    { record_type : Typed.Record_type.t
    ; record : Type.ty
    ; index : int
    ; ty : Type.ty
    }
  [@@deriving sexp_of]

  let name t = t.record_type.fields.(t.index)
end

module Decl = struct
  type t =
    { mutable decl : Type_decl.t (* Its definition once computed, if spliced. *)
    ; name : string
    ; module_ : Module_scope.t
    ; params : Type.arg list Lazy.t
    ; mutable wrap_error : Typecheck_error.t -> Typecheck_error.t
    ; mutable used_while_defined : bool
    ; mutable checked : Signature.Type_decl.t option
    ; mutable reflected : Reflected_decl.t option
      (** Once checked, unless abstract. *)
    }

  let params t = force t.params

  let param_map t ~params =
    List.zip_exn
      (List.map t.decl.params ~f:(fun param -> Type_decl.Param.name param.txt))
      params
    |> String.Map.of_alist_exn
  ;;
end

module Exception_decl = struct
  type t =
    { args : Type_expr.t list
    ; module_ : Module_scope.t
    ; wrap_error : Typecheck_error.t -> Typecheck_error.t
    ; mutable checked : Constructor.t option
    }
end

module Type_splice = struct
  module State = struct
    type t =
      | Evaluating
      | Evaluated of (Type_expr.t, Typecheck_error.t) Result.t
  end

  type t =
    { expr : Expr.t
    ; mutable state : State.t
    }
end

type t =
  { fresh : Fresh.t
  ; mutable evaluate_splice :
      Module_scope.t
      -> Expr.t
      -> loc:Loc.t
      -> (Type_expr.t, Typecheck_error.t) Result.t
  ; mutable type_splices : Type_splice.t list
    (* Physically keyed: each splice is evaluated once. *)
  ; types : Decl.t String.Table.t
  ; exceptions : Exception_decl.t String.Table.t
  ; constructors : Constructor.t String.Table.t
  ; fields : Field.t String.Table.t
  ; constructor_types : string String.Table.t
  ; field_types : string String.Table.t
    (** The type declaring each constructor and field, whose entry in
        [constructors] or [fields] is added once it is checked. *)
  }

let create fresh =
  { fresh
  ; evaluate_splice =
      (fun _ _ ~loc:_ -> raise_s [%message "Type_env: no type splice evaluator"])
  ; type_splices = []
  ; types = String.Table.create ()
  ; exceptions = String.Table.create ()
  ; constructors = String.Table.create ()
  ; fields = String.Table.create ()
  ; constructor_types = String.Table.create ()
  ; field_types = String.Table.create ()
  }
;;

let builtins =
  List.map
    [ "int"; "bool"; "string"; "char"; "unit"; Type.exn_name ]
    ~f:(fun name ->
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
  | Path _ | Root _ ->
    Module_scope.find module_ Type path ~loc ~unbound:(fun path ->
      Unbound_type path)
;;

let set_evaluate_splice t f = t.evaluate_splice <- f

let evaluate_splice t module_ expr ~loc =
  match
    List.find t.type_splices ~f:(fun splice -> phys_equal splice.expr expr)
  with
  | Some { state = Evaluated result; _ } -> result
  | Some { state = Evaluating; _ } ->
    Error (Typecheck_error.create ~loc Type_splice_cycle)
  | None ->
    let splice = { Type_splice.expr; state = Evaluating } in
    t.type_splices <- splice :: t.type_splices;
    let result = t.evaluate_splice module_ expr ~loc in
    splice.state <- Evaluated result;
    result
;;

let find_type t module_ path ~loc =
  let%bind.Result name = resolve_type module_ path ~loc in
  match Hashtbl.find t.types name with
  | Some decl -> Ok decl
  | None ->
    Error
      (Typecheck_error.create ~loc (Unbound_type (Path (Path.of_string name))))
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

(* Each elaborates to the type and the syntax with each type it names by its
   canonical name ([Resolved]), so that it names the same types wherever it
   is. *)
let rec elaborate' t module_ vars ~splices ~expansions (expr : Type_expr.t)
  : (Type.ty * Type_expr.t, Typecheck_error.t) Result.t
  =
  let elaborate = elaborate' t module_ vars ~splices ~expansions in
  let elaborate_row = elaborate_row t module_ vars ~splices ~expansions in
  let loc = expr.loc in
  let%map.Result ty, txt =
    match expr.txt with
    | Var name ->
      let%map.Result ty = Vars.ty vars name ~loc in
      ty, expr.txt
    | Con (path, args) ->
      elaborate_con t module_ vars ~splices ~expansions path args ~loc
    | Arrow (ty1, ty2) ->
      let%bind.Result ty1, syntax1 = elaborate ty1 in
      let%map.Result ty2, syntax2 = elaborate ty2 in
      Type.TArrow (ty1, ty2), Type_expr.Arrow (syntax1, syntax2)
    | Tuple tys ->
      let%map.Result tys = map_result tys ~f:elaborate in
      let tys, syntax = List.unzip tys in
      Type.TTuple tys, Type_expr.Tuple syntax
    | Code { inner; context } ->
      let%bind.Result inner, inner_syntax = elaborate inner in
      let%map.Result context, context_syntax = elaborate_row context in
      ( Type.TCode { inner; context }
      , Type_expr.Code { inner = inner_syntax; context = context_syntax } )
    | Record row ->
      let%map.Result row, syntax = elaborate_row row in
      Fresh.record t.fresh row, Type_expr.Record syntax
    | Variant row ->
      let%map.Result row, syntax = elaborate_row row in
      Fresh.variant t.fresh row, Type_expr.Variant syntax
    | Splice e when splices ->
      let%bind.Result generated = evaluate_splice t module_ e ~loc in
      let%map.Result ty, syntax =
        elaborate generated
        |> Result.map_error ~f:(fun error ->
          Typecheck_error.create ~loc (In_type_splice { generated; error = error.kind }))
      in
      ty, syntax.txt
    | Splice _ -> Error (Typecheck_error.create ~loc Misplaced_type_splice)
  in
  ty, { expr with txt }

and elaborate_row t module_ vars ~splices ~expansions (row : Type_expr.row)
  : (Type.row * Type_expr.row, Typecheck_error.t) Result.t
  =
  let rec go (row : Type_expr.row) ~seen =
    let%map.Result elaborated, txt =
      match row.txt with
      | Empty -> Ok (Type.REmpty, row.txt)
      | Extend ({ txt = name; loc }, _, _) when Set.mem seen name ->
        Error (Typecheck_error.create ~loc (Duplicate_row_entry name))
      | Extend (({ txt = name; loc = _ } as label), ty, rest) ->
        let%bind.Result ty, ty_syntax =
          elaborate' t module_ vars ~splices ~expansions ty
        in
        let%map.Result rest, rest_syntax = go rest ~seen:(Set.add seen name) in
        ( Type.RExtend (name, Present, ty, rest)
        , Type_expr.Extend (label, ty_syntax, rest_syntax) )
      | Var { txt = name; loc } ->
        let%map.Result var = Vars.row vars name ~loc in
        var, row.txt
    in
    elaborated, { row with txt }
  in
  go row ~seen:String.Set.empty

and elaborate_con
      t
      module_
      vars
      ~splices
      ~expansions
      ({ txt = path; loc = path_loc } as located_path : Decl_path.t Located.t)
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
    else error ~loc (Type_arity_mismatch { type_name = path; expected; got })
  in
  let%bind.Result args =
    map_result (List.zip_exn decl.decl.params args) ~f:(fun (param, arg) ->
      match param.txt, arg with
      | Ty _, Ty ty ->
        let%map.Result ty, syntax = elaborate' t module_ vars ~splices ~expansions ty in
        Type.ATy ty, Type_expr.Ty syntax
      | Row _, Row row ->
        let%map.Result row, syntax =
          elaborate_row t module_ vars ~splices ~expansions row
        in
        Type.ARow row, Type_expr.Row syntax
      (* [{...}] parses as a row, but is a record type for a type parameter. *)
      | Ty _, Row row ->
        let%map.Result row, syntax =
          elaborate_row t module_ vars ~splices ~expansions row
        in
        Type.ATy (Fresh.record t.fresh row), Type_expr.Row syntax
      | Row _, Ty { loc; _ } ->
        error
          ~loc
          (Type_arg_kind_mismatch { type_name = path; param = param.txt }))
  in
  let args, args_syntax = List.unzip args in
  let syntax =
    Type_expr.Con ({ located_path with txt = Resolved decl.name }, args_syntax)
  in
  let%bind.Result kind = definition decl ~loc in
  let%map.Result ty =
    match kind with
    | Variant _ | Record _ | Abstract | Splice _ ->
      Ok (Type.TCon (decl.name, args))
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
         else Result.map (expand t decl ~args ~expansions body) ~f:fst)
  in
  ty, syntax

(* Its definition, computed if spliced. While being computed, it is taken to
   be nominal, which [set_definition] checks. *)
and definition (decl : Decl.t) ~loc =
  match decl.decl.kind with
  | Splice _ when Module_scope.defining decl.module_ decl.decl.name.txt ->
    decl.used_while_defined <- true;
    Ok decl.decl.kind
  | Splice _ ->
    let%map.Result () =
      Module_scope.force_definition decl.module_ decl.decl.name.txt ~loc
    in
    decl.decl.kind
  | Variant _ | Record _ | Alias _ | Abstract -> Ok decl.decl.kind

and expand t (decl : Decl.t) ~args ~expansions body =
  let knot = Type.Var.create (Fresh.id t.fresh) Generic in
  let expansion =
    { Expansion.args; knot; expanded = false; reached_back = false }
  in
  Hashtbl.add_multi expansions ~key:decl.name ~data:expansion;
  let%bind.Result body, syntax =
    elaborate'
      t
      decl.module_
      (Params (Decl.param_map decl ~params:args))
      ~splices:true
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
    Ok (Type.TVar knot, syntax))
;;

let elaborate t module_ vars expr ~splices =
  elaborate'
    t
    module_
    (Annotation vars)
    ~splices
    ~expansions:(String.Table.create ())
    expr
;;

let declare_members t module_ (decl : Type_decl.t) ~name =
  let declares names table =
    List.iter names ~f:(fun ((member : Name.t), _) ->
      Hashtbl.add_exn
        table
        ~key:(Module_scope.canonical_name module_ member.txt)
        ~data:name)
  in
  match decl.kind with
  | Alias _ | Abstract | Splice _ -> ()
  | Variant constructors -> declares constructors t.constructor_types
  | Record fields -> declares fields t.field_types
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
    ~data:
      { Decl.decl
      ; name
      ; module_
      ; params
      ; wrap_error
      ; used_while_defined = false
      ; checked = None
      ; reflected = None
      };
  declare_members t module_ decl ~name
;;

let set_definition t name (kind : Type_decl.Kind.t) ~wrap_error ~loc =
  let decl = Hashtbl.find_exn t.types name in
  match kind with
  | Alias _ when decl.used_while_defined ->
    Error
      (Typecheck_error.create
         ~loc
         (Type_definition_cycle { name; splice = loc }))
  | Splice e ->
    Error (Typecheck_error.create ~loc:e.loc Nested_definition_splice)
  | Alias _ | Variant _ | Record _ | Abstract ->
    decl.decl <- { decl.decl with kind };
    decl.wrap_error <- Fn.compose decl.wrap_error wrap_error;
    declare_members t decl.module_ decl.decl ~name;
    Ok ()
;;

let declare_exception ?(wrap_error = Fn.id) t module_ (name : Name.t) args =
  Hashtbl.add_exn
    t.exceptions
    ~key:(Module_scope.canonical_name module_ name.txt)
    ~data:{ Exception_decl.args; module_; wrap_error; checked = None }
;;

(* Its arguments have no type variables, which nothing would bind. *)
let check_exception t name =
  let decl = Hashtbl.find_exn t.exceptions name in
  match decl.checked with
  | Some checked -> Ok checked
  | None ->
    Result.map_error ~f:decl.wrap_error
    @@
    let%map.Result args =
      map_result decl.args ~f:(fun arg ->
        elaborate'
          t
          decl.module_
          (Params String.Map.empty)
          ~splices:true
          ~expansions:(String.Table.create ())
          arg
        |> Result.map ~f:fst)
    in
    let { Typed.Constructor_ref.type_name; index; name = _ } =
      Typed.Constructor_ref.exception_ name
    in
    let checked =
      { Constructor.name; type_name; index; args; result = Type.exn }
    in
    decl.checked <- Some checked;
    checked
;;

let define t (decl : Decl.t) =
  let expansions = String.Table.create () in
  let elaborate =
    elaborate'
      t
      decl.module_
      (Params (Decl.param_map decl ~params:(Decl.params decl)))
      ~splices:true
      ~expansions
  in
  let canonical_name = Module_scope.canonical_name decl.module_ in
  let%map.Result kind, reflected =
    match decl.decl.kind with
    | Alias body ->
      let%map.Result body, syntax =
        expand t decl ~args:(Decl.params decl) ~expansions body
      in
      ( Signature.Type_decl.Kind.Alias body
      , Some (Reflected_decl.Kind.Alias syntax) )
    | Abstract -> Ok (Signature.Type_decl.Kind.Abstract, None)
    | Splice _ ->
      raise_s [%message "Type_env.define: definition not computed" decl.name]
    | Variant constructors ->
      let%map.Result constructors =
        map_result constructors ~f:(fun ((name : Name.t), args) ->
          let%map.Result args = map_result args ~f:elaborate in
          let args, syntax = List.unzip args in
          ( { Signature.Constructor.name = name.txt; args }
          , (canonical_name name.txt, syntax) ))
      in
      let constructors, reflected = List.unzip constructors in
      ( Signature.Type_decl.Kind.Variant constructors
      , Some (Reflected_decl.Kind.Variant reflected) )
    | Record fields ->
      let%map.Result fields =
        map_result fields ~f:(fun ((name : Name.t), ty) ->
          let%map.Result ty, syntax = elaborate ty in
          ( { Signature.Field.name = name.txt; ty }
          , (canonical_name name.txt, syntax) ))
      in
      let fields, reflected = List.unzip fields in
      ( Signature.Type_decl.Kind.Record fields
      , Some (Reflected_decl.Kind.Record reflected) )
  in
  decl.reflected
  <- Option.map reflected ~f:(fun kind ->
       { Reflected_decl.params = decl.decl.params; kind });
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
     let record_type =
       { Typed.Record_type.name = decl.name
       ; fields =
           Array.of_list_map fields ~f:(fun (field : Signature.Field.t) ->
             canonical_name field.name)
       }
     in
     List.iteri fields ~f:(fun index { name = _; ty } ->
       Hashtbl.add_exn
         t.fields
         ~key:record_type.fields.(index)
         ~data:
           { Field.record_type
           ; index
           ; record = TCon (decl.name, Decl.params decl)
           ; ty
           }));
  checked
;;

let check t name =
  let decl = Hashtbl.find_exn t.types name in
  match decl.checked with
  | Some checked -> Ok checked
  | None ->
    let%bind.Result () =
      match decl.decl.kind with
      | Splice _ ->
        Module_scope.force_definition
          decl.module_
          decl.decl.name.txt
          ~loc:decl.decl.name.loc
        |> Result.map_error ~f:decl.wrap_error
      | Variant _ | Record _ | Alias _ | Abstract -> Ok ()
    in
    (match define t decl with
     | Ok checked -> Ok checked
     | Error error -> Error (decl.wrap_error error))
;;

let find_decl_exn t name =
  Option.value_exn (Hashtbl.find_exn t.types name).checked
;;

let members t type_name ~kind ~members =
  match Hashtbl.find t.types type_name with
  | None -> Ok None
  | Some decl ->
    let%map.Result checked = check t type_name in
    kind checked.kind
    |> Option.map ~f:(fun names ->
      List.map names ~f:(fun name ->
        Hashtbl.find_exn members (Module_scope.canonical_name decl.module_ name)))
;;

let record_fields t type_name =
  members t type_name ~members:t.fields ~kind:(function
    | Record fields ->
      Some (List.map fields ~f:(fun (field : Signature.Field.t) -> field.name))
    | Variant _ | Alias _ | Abstract -> None)
;;

let record_type t type_name =
  let%map.Result fields = record_fields t type_name in
  match fields with
  | Some (field :: _) -> field.record_type
  | Some [] | None ->
    raise_s [%message "Not a record type" (type_name : string)]
;;

let variant_constructors t type_name =
  members t type_name ~members:t.constructors ~kind:(function
    | Variant constructors ->
      Some
        (List.map
           constructors
           ~f:(fun (constructor : Signature.Constructor.t) -> constructor.name))
    | Record _ | Alias _ | Abstract -> None)
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
  | None ->
    Error
      (Typecheck_error.create
         ~loc
         (unbound (Decl_path.Path (Path.of_string name))))
  | Some type_name ->
    let%map.Result (_ : Signature.Type_decl.t) = check t type_name in
    Hashtbl.find_exn members name
;;

let constructor t name ~loc =
  if Hashtbl.mem t.exceptions name
  then check_exception t name
  else
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
  | Path _ | Root _ ->
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
  | Path _ | Root _ ->
    find_resolved
      t
      module_
      Field
      path
      ~loc
      ~unbound:(fun path -> Unbound_field path)
      ~find:field
;;

let reflect t name =
  let%map.Result (_ : Signature.Type_decl.t) = check t name in
  (Hashtbl.find_exn t.types name).reflected
;;
