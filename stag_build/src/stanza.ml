open! Import
module Cst = Parsexp.Cst

module Kind = struct
  type t =
    | Library
    | Executable
    | Test
  [@@deriving sexp_of, equal, enumerate]

  let to_string = function
    | Library -> "library"
    | Executable -> "executable"
    | Test -> "test"
  ;;

  let to_string_with_article t =
    match t with
    | Library | Test -> "a " ^ to_string t
    | Executable -> "an executable"
  ;;

  let of_string string =
    List.find all ~f:(fun t -> String.equal (to_string t) string)
  ;;
end

type t =
  { kind : Kind.t
  ; name : string Located.t
  ; libraries : string Located.t list
  ; compile_time_c_calls : bool
  ; loc : Loc.t
  }
[@@deriving sexp_of]

let module_name t = String.capitalize t.name.txt

let loc_of_pos ({ line; col; offset } : Parsexp.Positions.pos) : Loc.Pos.t =
  #{ line; col; offset }
;;

let loc ~path ({ start_pos; end_pos } : Parsexp.Positions.range) : Loc.t =
  Loc.create ~file:path ~start:(loc_of_pos start_pos) ~stop:(loc_of_pos end_pos)
;;

let cst_loc ~path : Cst.t -> Loc.t = function
  | Atom { loc = range; _ } | List { loc = range; _ } -> loc ~path range
;;

let sexps elements =
  List.filter_map elements ~f:(function
    | Cst.Sexp sexp -> Some sexp
    | Comment _ -> None)
;;

let is_valid_name name =
  (not (String.is_empty name))
  && Char.is_lowercase name.[0]
  && String.for_all name ~f:(fun c -> Char.is_alphanum c || Char.equal c '_')
;;

let atom ~path (cst : Cst.t) =
  match cst with
  | Atom { atom; loc = range; _ } ->
    Some (Located.create ~loc:(loc ~path range) atom)
  | List _ -> None
;;

let name_atom ~path ~error ~what (cst : Cst.t) =
  match atom ~path cst with
  | None ->
    error (cst_loc ~path cst) (sprintf "Expected a %s name" what);
    None
  | Some name when not (is_valid_name name.txt) ->
    error
      name.loc
      (sprintf
         "Invalid %s name %s: it should be a lowercase letter followed by \
          letters, digits and _"
         what
         name.txt);
    None
  | Some name -> Some name
;;

let check_duplicate_libraries ~error libraries =
  ignore
    (List.fold
       libraries
       ~init:String.Set.empty
       ~f:(fun seen (library : string Located.t) ->
         if Set.mem seen library.txt
         then
           error
             library.loc
             (sprintf "The library %s is listed more than once" library.txt);
         Set.add seen library.txt)
     : String.Set.t)
;;

(* The fields of a stanza, each [Some] once given: the name [Some None] if
   invalid. *)
module Fields = struct
  type t =
    { mutable name : string Located.t option option
    ; mutable libraries : string Located.t list option
    ; mutable compile_time_c_calls : bool
    }
end

let field ~path ~error ~kind (fields : Fields.t) (cst : Cst.t) =
  let once (field_name : string Located.t) ~given ~set =
    if given
    then
      error
        field_name.loc
        (sprintf "The field %s is given more than once" field_name.txt)
    else set ()
  in
  let head_and_args =
    match cst with
    | Atom _ -> None
    | List { elements; _ } ->
      (match sexps elements with
       | [] -> None
       | head :: args ->
         Option.map (atom ~path head) ~f:(fun head -> head, args))
  in
  match head_and_args with
  | None ->
    error
      (cst_loc ~path cst)
      "Expected a field: (name ...), (libraries ...) or (compile_time_c_calls)"
  | Some (({ txt = "name"; _ } as field_name), args) ->
    let name =
      match args with
      | [ arg ] -> name_atom ~path ~error ~what:(Kind.to_string kind) arg
      | _ ->
        error
          (cst_loc ~path cst)
          "The field name expects one name, eg. (name foo)";
        None
    in
    once field_name ~given:(Option.is_some fields.name) ~set:(fun () ->
      fields.name <- Some name)
  | Some (({ txt = "libraries"; _ } as field_name), args) ->
    let libraries =
      List.filter_map args ~f:(name_atom ~path ~error ~what:"library")
    in
    check_duplicate_libraries ~error libraries;
    once field_name ~given:(Option.is_some fields.libraries) ~set:(fun () ->
      fields.libraries <- Some libraries)
  | Some (({ txt = "compile_time_c_calls"; _ } as field_name), args) ->
    if not (List.is_empty args)
    then
      error
        (cst_loc ~path cst)
        "The field compile_time_c_calls takes no arguments: \n\
        \         (compile_time_c_calls)";
    once field_name ~given:fields.compile_time_c_calls ~set:(fun () ->
      fields.compile_time_c_calls <- true)
  | Some (field_name, _) ->
    error
      field_name.loc
      (sprintf
         "Unknown field %s: expected name, libraries or compile_time_c_calls"
         field_name.txt)
;;

let stanza ~path ~error (cst : Cst.t) =
  let stanza_loc = cst_loc ~path cst in
  let head_and_fields =
    match cst with
    | Atom _ -> None
    | List { elements; _ } ->
      (match sexps elements with
       | [] -> None
       | head :: fields ->
         Option.map (atom ~path head) ~f:(fun head -> head, fields))
  in
  match head_and_fields with
  | None ->
    error
      stanza_loc
      "Expected a stanza: (library ...), (executable ...) or (test ...)";
    None
  | Some (kind_name, field_csts) ->
    (match Kind.of_string kind_name.txt with
     | None ->
       error
         kind_name.loc
         (sprintf
            "Unknown stanza %s: expected library, executable or test"
            kind_name.txt);
       None
     | Some kind ->
       let fields =
         { Fields.name = None; libraries = None; compile_time_c_calls = false }
       in
       List.iter field_csts ~f:(field ~path ~error ~kind fields);
       (match fields.name with
        | None ->
          error
            stanza_loc
            (sprintf
               "The %s stanza has no name: add (name ...)"
               (Kind.to_string kind));
          None
        | Some None -> None
        | Some (Some name) ->
          Some
            { kind
            ; name
            ; libraries = Option.value fields.libraries ~default:[]
            ; compile_time_c_calls = fields.compile_time_c_calls
            ; loc = stanza_loc
            }))
;;

let parse ~path contents =
  let errors = Queue.create () in
  let error loc message =
    Queue.enqueue errors (Diagnostic.create ~loc message)
  in
  let result =
    match Parsexp.Many_cst.parse_string contents with
    | Error parse_error ->
      let pos = loc_of_pos (Parsexp.Parse_error.position parse_error) in
      error
        (Loc.create ~file:path ~start:pos ~stop:pos)
        (String.capitalize (Parsexp.Parse_error.message parse_error));
      None
    | Ok elements ->
      (match sexps elements with
       | [] -> None
       | first :: rest ->
         List.iter rest ~f:(fun cst ->
           error
             (cst_loc ~path cst)
             "A stag file has at most one stanza; put the others in their own \
              directories");
         stanza ~path ~error first)
  in
  if Queue.is_empty errors then Ok result else Error (Queue.to_list errors)
;;
