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
  ; loc : Loc.t
  }
[@@deriving sexp_of]

let module_name t = String.capitalize t.name.txt

let loc_of_pos ({ line; col; offset } : Parsexp.Positions.pos) : Loc.Pos.t =
  { line; col; offset }
;;

let loc ~path ({ start_pos; end_pos } : Parsexp.Positions.range) : Loc.t =
  { file = path; start = loc_of_pos start_pos; stop = loc_of_pos end_pos }
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

let parse ~path contents =
  let errors = Queue.create () in
  let error loc message = Queue.enqueue errors { Diagnostic.loc; message } in
  let atom (cst : Cst.t) =
    match cst with
    | Atom { atom; loc = range; _ } ->
      Some (Located.create ~loc:(loc ~path range) atom)
    | List _ -> None
  in
  let name_atom ~what (cst : Cst.t) =
    match atom cst with
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
  in
  let stanza (cst : Cst.t) =
    let stanza_loc = cst_loc ~path cst in
    let expected_stanza () =
      error
        stanza_loc
        "Expected a stanza: (library ...), (executable ...) or (test ...)";
      None
    in
    match cst with
    | Atom _ -> expected_stanza ()
    | List { elements; _ } ->
      (match sexps elements with
       | [] -> expected_stanza ()
       | head :: fields ->
         (match atom head with
          | None -> expected_stanza ()
          | Some kind_name ->
            (match Kind.of_string kind_name.txt with
             | None ->
               error
                 kind_name.loc
                 (sprintf
                    "Unknown stanza %s: expected library, executable or test"
                    kind_name.txt);
               None
             | Some kind ->
               let name = ref None in
               let libraries = ref None in
               List.iter fields ~f:(fun field ->
                 let field_loc = cst_loc ~path field in
                 let set slot field_name value =
                   match !slot with
                   | Some _ ->
                     error
                       field_name.Located.loc
                       (sprintf
                          "The field %s is given more than once"
                          field_name.txt)
                   | None -> slot := Some value
                 in
                 match field with
                 | Atom _ | List { elements = []; _ } ->
                   error
                     field_loc
                     "Expected a field: (name ...) or (libraries ...)"
                 | List { elements; _ } ->
                   (match sexps elements with
                    | [] ->
                      error
                        field_loc
                        "Expected a field: (name ...) or (libraries ...)"
                    | head :: args ->
                      (match atom head with
                       | None ->
                         error
                           field_loc
                           "Expected a field: (name ...) or (libraries ...)"
                       | Some ({ txt = "name"; _ } as field_name) ->
                         (match args with
                          | [ arg ] ->
                            set
                              name
                              field_name
                              (name_atom ~what:(Kind.to_string kind) arg)
                          | _ ->
                            error
                              field_loc
                              "The field name expects one name, eg. (name foo)";
                            set name field_name None)
                       | Some ({ txt = "libraries"; _ } as field_name) ->
                         let names =
                           List.filter_map args ~f:(name_atom ~what:"library")
                         in
                         List.fold
                           names
                           ~init:String.Set.empty
                           ~f:(fun seen (library : string Located.t) ->
                             if Set.mem seen library.txt
                             then
                               error
                                 library.loc
                                 (sprintf
                                    "The library %s is listed more than once"
                                    library.txt);
                             Set.add seen library.txt)
                         |> (ignore : String.Set.t -> unit);
                         set libraries field_name names
                       | Some field_name ->
                         error
                           field_name.loc
                           (sprintf
                              "Unknown field %s: expected name or libraries"
                              field_name.txt))));
               (match !name with
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
                    ; libraries = Option.value !libraries ~default:[]
                    ; loc = stanza_loc
                    }))))
  in
  let result =
    match Parsexp.Many_cst.parse_string contents with
    | Error parse_error ->
      let pos = loc_of_pos (Parsexp.Parse_error.position parse_error) in
      error
        { file = path; start = pos; stop = pos }
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
         stanza first)
  in
  if Queue.is_empty errors then Ok result else Error (Queue.to_list errors)
;;
