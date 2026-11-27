open! Import

let of_toplevels text toplevels =
  let range = Text.range_of_loc text in
  let symbol
    ?(children = [])
    ?detail
    ~kind
    ~(loc : Loc.t)
    ~(name_loc : Loc.t)
    name
    =
    Lsp_types.DocumentSymbol.create
      ~name
      ?detail
      ~kind
      ~range:(range (Loc.merge loc name_loc))
      ~selectionRange:(range name_loc)
      ~children
      ()
  in
  let rec of_toplevel (toplevel : Syntax.Toplevel.t) =
    let loc = toplevel.loc in
    match toplevel.txt with
    | Let (name, bindings, _, body) | Let_nonrec (name, bindings, _, body) ->
      let is_function =
        (not (List.is_empty bindings))
        ||
        match body.txt with
        | Fun _ -> true
        | _ -> false
      in
      [ symbol
          ~kind:(if is_function then Function else Variable)
          ~loc
          ~name_loc:name.loc
          name.txt
      ]
    | Let_mut (name, _, _) ->
      [ symbol ~kind:Variable ~loc ~name_loc:name.loc name.txt ]
    | External { name; _ } ->
      [ symbol
          ~detail:"external"
          ~kind:Function
          ~loc
          ~name_loc:name.loc
          name.txt
      ]
    | Type { name; params = _; kind } ->
      let member ~kind (name : Syntax.Name.t) =
        symbol ~kind ~loc:name.loc ~name_loc:name.loc name.txt
      in
      let rec symbols : Syntax.Type_decl.Kind.t -> _ = function
        | Malformed kind -> symbols kind
        | Variant constructors ->
          ( Lsp_types.SymbolKind.Enum
          , List.map constructors ~f:(fun (name, _) ->
              member ~kind:EnumMember name) )
        | Record fields ->
          Struct, List.map fields ~f:(fun (name, _) -> member ~kind:Field name)
        | Alias _ | Abstract | Splice _ -> Class, []
      in
      let symbol_kind, children = symbols kind in
      [ symbol ~children ~kind:symbol_kind ~loc ~name_loc:name.loc name.txt ]
    | Exception (name, _) | Malformed_exception (name, _) ->
      [ symbol
          ~detail:"exception"
          ~kind:Constructor
          ~loc
          ~name_loc:name.loc
          name.txt
      ]
    | Module (name, toplevels) ->
      [ symbol
          ~children:(List.concat_map toplevels ~f:of_toplevel)
          ~kind:Module
          ~loc
          ~name_loc:name.loc
          name.txt
      ]
    | Module_splice (name, _, expansion) ->
      [ symbol
          ~children:(of_expansion expansion)
          ~detail:"splice"
          ~kind:Module
          ~loc
          ~name_loc:name.loc
          name.txt
      ]
    | Expect_test { name; body = _ } ->
      [ symbol
          ~detail:"expect test"
          ~kind:Event
          ~loc
          ~name_loc:name.loc
          name.txt
      ]
    | Bench { name; param = _; body = _ } ->
      [ symbol ~detail:"bench" ~kind:Event ~loc ~name_loc:name.loc name.txt ]
    | Include (_, expansion) -> of_expansion expansion
    | Attributed (toplevel, _) -> of_toplevel toplevel
    | Attribute _ -> []
  and of_expansion expansion =
    Option.value_map expansion ~default:[] ~f:(fun expansion ->
      List.concat_map expansion.txt ~f:of_toplevel)
  in
  List.concat_map toplevels ~f:of_toplevel
;;
