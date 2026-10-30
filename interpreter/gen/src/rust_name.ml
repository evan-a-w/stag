open! Import

let keywords =
  String.Set.of_list
    [ "as"
    ; "async"
    ; "await"
    ; "box"
    ; "break"
    ; "const"
    ; "continue"
    ; "crate"
    ; "dyn"
    ; "else"
    ; "enum"
    ; "extern"
    ; "false"
    ; "fn"
    ; "for"
    ; "if"
    ; "impl"
    ; "in"
    ; "let"
    ; "loop"
    ; "macro"
    ; "match"
    ; "mod"
    ; "move"
    ; "mut"
    ; "priv"
    ; "pub"
    ; "ref"
    ; "return"
    ; "self"
    ; "static"
    ; "struct"
    ; "super"
    ; "trait"
    ; "true"
    ; "try"
    ; "type"
    ; "typeof"
    ; "union"
    ; "unsafe"
    ; "unsized"
    ; "use"
    ; "virtual"
    ; "where"
    ; "while"
    ; "yield"
    ]
;;

let escape name = if Set.mem keywords name then name ^ "_" else name
let module_ name = escape (String.lowercase name)
let field = escape

let camel name =
  String.split name ~on:'_'
  |> List.map ~f:String.capitalize
  |> String.concat
  |> fun camel -> if String.is_empty camel then "T" else camel
;;

let decl ~module_path ~name =
  match name, List.last module_path with
  | "t", Some last ->
    List.map (List.drop_last_exn module_path) ~f:module_, camel last
  | _ -> List.map module_path ~f:module_, camel name
;;

let path ~from (modules, name) =
  let modules =
    if List.is_prefix modules ~prefix:from ~equal:String.equal
    then List.drop modules (List.length from)
    else "crate" :: modules
  in
  String.concat ~sep:"::" (modules @ [ name ])
;;

let snake name =
  String.concat_map name ~f:(fun c ->
    if Char.is_uppercase c
    then "_" ^ String.of_char (Char.lowercase c)
    else String.of_char c)
  |> String.chop_prefix_if_exists ~prefix:"_"
;;

let crate_types =
  String.Set.of_list
    [ "Arena"
    ; "ArenaBuilder"
    ; "Builder"
    ; "Construct"
    ; "Error"
    ; "FromOcaml"
    ; "GcStress"
    ; "Indexed"
    ; "Map"
    ; "Nothing"
    ; "OcamlKey"
    ; "Pending"
    ; "Pool"
    ; "Ref"
    ; "Set"
    ; "Slot"
    ; "Store"
    ; "Stored"
    ; "Str"
    ; "ToOcaml"
    ]
;;

let crate_modules =
  String.Set.of_list
    [ "arena"; "conv"; "ostring"; "raw"; "test_heap"; "tests"; "to_ocaml" ]
;;
