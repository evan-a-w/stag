open! Import

let path = "<prelude>"
let core_dir = "<core>"

module Sources = struct
  type t =
    { prelude : Source_file.t
    ; core : Source_file.t list
    }

  let embedded =
    { prelude =
        (match Stag_prelude_source.files with
         | [ (_, contents) ] -> { path; contents }
         | files ->
           raise_s
             [%message
               "Expected one prelude file"
                 ~files:(List.map files ~f:fst : string list)])
    ; core =
        List.map Stag_core_source.files ~f:(fun (name, contents) ->
          { Source_file.path = Filename.concat core_dir name; contents })
    }
  ;;
end

let parse ({ prelude = prelude_file; core = core_files } : Sources.t) =
  let%bind.Result prelude =
    Parser.program ~file:prelude_file.path prelude_file.contents
    |> Result.map_error ~f:(fun error -> Source_file.Error.Parse error)
  in
  let%map.Result core =
    List.map core_files ~f:Source_file.parse |> Result.all
  in
  let loc =
    Loc.start_of_file
      (Loc.File.of_string
         (match core_files with
          | first :: _ -> first.path
          | [] -> prelude_file.path))
  in
  prelude
  @ [ Located.create
        ~loc
        (Syntax.Toplevel.Module (Located.create ~loc "Core", core))
    ]
;;

let source file =
  List.find_map
    (Sources.embedded.prelude :: Sources.embedded.core)
    ~f:(fun { path; contents } ->
      Option.some_if (String.equal file path) contents)
;;

let toplevels =
  lazy
    (match parse Sources.embedded with
     | Ok toplevels -> toplevels
     | Error error ->
       raise_s
         [%message "Prelude failed to parse" (error : Source_file.Error.t)])
;;
