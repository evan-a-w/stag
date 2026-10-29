open Ppxlib
open Ast_builder.Default

let shared =
  Attribute.declare
    "rust.shared"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let skip =
  Attribute.declare
    "rust.skip"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let error ~loc fmt = Location.raise_errorf ~loc ("[@@@@deriving rust]: " ^^ fmt)
let witness_name name = "rust_" ^ name
let lid ~loc txt = { txt = Longident.parse txt; loc }

let runtime ~loc name arg =
  pexp_construct ~loc (lid ~loc ("Ppx_rust_runtime." ^ name)) arg
;;

let type_expr_ ~loc name arg = runtime ~loc ("Type_expr." ^ name) arg

let rec module_named (m : longident) name =
  match m with
  | Lident n | Ldot (_, n) -> String.equal n name
  | Lapply (_, m) -> module_named m name
;;

(** [unchecked] are the names of the group a [type nonrec] declaration may
    not refer to: [u] written there is the [u] of before the group, while
    the [u] the derived code sees is the group's. *)
let rec type_expr ~unchecked (ty : core_type) =
  let loc = ty.ptyp_loc in
  let type_expr = type_expr ~unchecked in
  match ty.ptyp_desc with
  | Ptyp_var name -> type_expr_ ~loc "Var" (Some (estring ~loc name))
  | Ptyp_tuple tys ->
    type_expr_ ~loc "Tuple" (Some (elist ~loc (List.map type_expr tys)))
  | Ptyp_constr ({ txt; loc = path_loc }, args) ->
    let rec check_path : longident -> unit = function
      | Lident _ -> ()
      | Ldot (m, _) -> check_path m
      | Lapply _ -> error ~loc:path_loc "functor applications are unsupported"
    in
    check_path txt;
    (match builtin ~loc ~type_expr txt args with
     | Some (canonical, desc) ->
       [%expr
         let (_ : [%t ty] -> [%t canonical]) = Stdlib.Fun.id in
         [%e desc]]
     | None ->
       let witness : longident =
         match txt with
         | Lident name -> Lident (witness_name name)
         | Ldot (m, name) -> Ldot (m, witness_name name)
         | Lapply _ -> assert false
       in
       let witness = pexp_ident ~loc { txt = witness; loc = path_loc } in
       let witness =
         match txt with
         | Lident name when List.mem name unchecked -> witness
         | _ ->
           let ty =
             ptyp_constr
               ~loc
               { txt; loc = path_loc }
               (List.map (fun _ -> ptyp_any ~loc) args)
           in
           [%expr ([%e witness] : [%t ty] Ppx_rust_runtime.Witness.t)]
       in
       type_expr_
         ~loc
         "Decl"
         (Some
            (pexp_tuple
               ~loc
               [ [%expr [%e witness].Ppx_rust_runtime.Witness.key]
               ; elist ~loc (List.map type_expr args)
               ])))
  | _ -> error ~loc "unsupported type (only variables, constructors and tuples)"

(** The builtin [txt] stands for, if any: its canonical type, which the type
    written must be, and its description. *)
and builtin ~loc ~type_expr (txt : longident) args =
  let nullary =
    [ "int", "Int"
    ; "bool", "Bool"
    ; "char", "Char"
    ; "unit", "Unit"
    ; "string", "String"
    ]
  in
  let unary = [ "list", "List"; "array", "Array"; "option", "Option" ] in
  let named names =
    List.find_map
      (fun (lower, upper) ->
         match txt with
         | Lident n when String.equal n lower -> Some upper
         | Ldot (m, "t") when module_named m upper -> Some upper
         | _ -> None)
      names
  in
  let stdlib upper = Longident.parse ("Stdlib." ^ upper ^ ".t") in
  let in_ m name = ptyp_constr ~loc { txt = Ldot (m, name); loc } [] in
  match named nullary, named unary, txt, args with
  | Some upper, _, _, [] ->
    Some
      ( ptyp_constr ~loc { txt = stdlib upper; loc } []
      , type_expr_ ~loc upper None )
  | _, Some upper, _, [ arg ] ->
    Some
      ( ptyp_constr ~loc { txt = stdlib upper; loc } [ ptyp_any ~loc ]
      , type_expr_ ~loc upper (Some (type_expr arg)) )
  | _, _, Ldot (m, "t"), [] when module_named m "Nothing" ->
    Some ([%type: Base.Nothing.t], type_expr_ ~loc "Nothing" None)
  | _, _, Ldot (Ldot (k, "Map"), "t"), [ data ] ->
    let key = in_ k "t" in
    Some
      ( [%type: ([%t key], _, [%t in_ k "comparator_witness"]) Base.Map.t]
      , type_expr_
          ~loc
          "Map"
          (Some (pexp_tuple ~loc [ type_expr key; type_expr data ])) )
  | _, _, Ldot (Ldot (k, "Set"), "t"), [] ->
    let elt = in_ k "t" in
    Some
      ( [%type: ([%t elt], [%t in_ k "comparator_witness"]) Base.Set.t]
      , type_expr_ ~loc "Set" (Some (type_expr elt)) )
  | _ -> None
;;

let field ~unchecked (label : label_declaration) =
  let loc = label.pld_loc in
  let skip = Option.is_some (Attribute.get skip label) in
  [%expr
    { Ppx_rust_runtime.Field.name = [%e estring ~loc label.pld_name.txt]
    ; ty =
        [%e
          if skip
          then type_expr_ ~loc "Tuple" (Some [%expr []])
          else type_expr ~unchecked label.pld_type]
    ; mutable_ =
        [%e
          ebool
            ~loc
            (match label.pld_mutable with
             | Mutable -> true
             | Immutable -> false)]
    ; skip = [%e ebool ~loc skip]
    }]
;;

let constructor ~unchecked (cd : constructor_declaration) =
  let loc = cd.pcd_loc in
  if Option.is_some cd.pcd_res || not (List.is_empty cd.pcd_vars)
  then error ~loc "GADT constructors are unsupported";
  let args =
    match cd.pcd_args with
    | Pcstr_tuple args ->
      runtime
        ~loc
        "Constructor.Args.Tuple"
        (Some (elist ~loc (List.map (type_expr ~unchecked) args)))
    | Pcstr_record labels ->
      runtime
        ~loc
        "Constructor.Args.Record"
        (Some (elist ~loc (List.map (field ~unchecked) labels)))
  in
  [%expr
    { Ppx_rust_runtime.Constructor.name = [%e estring ~loc cd.pcd_name.txt]
    ; args = [%e args]
    }]
;;

let key ~code_path (td : type_declaration) =
  let modules =
    List.filter
      (fun m -> not (String.equal m "T"))
      (Code_path.submodule_path code_path)
  in
  (Code_path.main_module_name code_path :: modules) @ [ td.ptype_name.txt ]
;;

let params (td : type_declaration) =
  List.map
    (fun ((param : core_type), _) ->
       match param.ptyp_desc with
       | Ptyp_var name -> name
       | _ -> error ~loc:param.ptyp_loc "parameters must be named")
    td.ptype_params
;;

let decl ~code_path ~unchecked (td : type_declaration) =
  let loc = td.ptype_loc in
  if not (List.is_empty td.ptype_cstrs)
  then error ~loc "constraints are unsupported";
  if
    List.exists
      (fun (attr : attribute) ->
         match attr.attr_name.txt with
         | "unboxed" | "ocaml.unboxed" -> true
         | _ -> false)
      td.ptype_attributes
  then error ~loc "[@@@@unboxed] is unsupported";
  let shared = Option.is_some (Attribute.get shared td) in
  let kind =
    match td.ptype_manifest, td.ptype_kind with
    | Some manifest, _ ->
      if shared
      then
        error
          ~loc
          "[@@@@rust.shared] on an alias is unsupported: put it on the \
           declaration it aliases";
      runtime ~loc "Kind.Alias" (Some (type_expr ~unchecked manifest))
    | None, Ptype_record labels ->
      runtime
        ~loc
        "Kind.Record"
        (Some (elist ~loc (List.map (field ~unchecked) labels)))
    | None, Ptype_variant cds ->
      if
        shared
        && List.for_all
             (fun (cd : constructor_declaration) ->
                match cd.pcd_args with
                | Pcstr_tuple [] -> true
                | Pcstr_tuple _ | Pcstr_record _ -> false)
             cds
      then
        error
          ~loc
          "[@@@@rust.shared] on a type without blocks is unsupported: its \
           values are immediates";
      runtime
        ~loc
        "Kind.Variant"
        (Some (elist ~loc (List.map (constructor ~unchecked) cds)))
    | None, Ptype_abstract -> error ~loc "abstract types are unsupported"
    | None, Ptype_open -> error ~loc "extensible variants are unsupported"
  in
  [%expr
    { Ppx_rust_runtime.Decl.key =
        [%e elist ~loc (List.map (estring ~loc) (key ~code_path td))]
    ; params = [%e elist ~loc (List.map (estring ~loc) (params td))]
    ; kind = [%e kind]
    ; shared = [%e ebool ~loc shared]
    }]
;;

let witness_type ~loc (td : type_declaration) =
  ptyp_constr
    ~loc
    { txt = Lident td.ptype_name.txt; loc }
    (List.map fst td.ptype_params)
;;

let str_type_decl =
  Deriving.Generator.V2.make_noarg (fun ~ctxt (rec_flag, tds) ->
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    let code_path = Expansion_context.Deriver.code_path ctxt in
    let names = List.map (fun td -> td.ptype_name.txt) tds in
    let unchecked =
      match rec_flag with
      | Nonrecursive -> names
      | Recursive -> []
    in
    let register =
      [%stri
        let () =
          Ppx_rust_runtime.register
            [%e elist ~loc (List.map (decl ~code_path ~unchecked) tds)]
        ;;]
    in
    let witnesses =
      List.map
        (fun td ->
           let key = elist ~loc (List.map (estring ~loc) (key ~code_path td)) in
           [%stri
             let [%p pvar ~loc (witness_name td.ptype_name.txt)]
               : [%t witness_type ~loc td] Ppx_rust_runtime.Witness.t
               =
               { Ppx_rust_runtime.Witness.key = [%e key] }
             ;;])
        tds
    in
    (* The declarations of a [type nonrec] group refer to the witnesses of
       before it. *)
    match rec_flag with
    | Recursive -> witnesses @ [ register ]
    | Nonrecursive -> register :: witnesses)
;;

let sig_type_decl =
  Deriving.Generator.V2.make_noarg (fun ~ctxt (_, tds) ->
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    List.map
      (fun td ->
         psig_value
           ~loc
           (value_description
              ~loc
              ~name:{ txt = witness_name td.ptype_name.txt; loc }
              ~type_:
                [%type: [%t witness_type ~loc td] Ppx_rust_runtime.Witness.t]
              ~prim:[]))
      tds)
;;

let rust = Deriving.add "rust" ~str_type_decl ~sig_type_decl
