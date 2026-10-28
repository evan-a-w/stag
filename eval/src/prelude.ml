open! Import

let path = "<prelude>"
let core_dir = "<core>"
let checkout_prelude = "prelude/prelude.stag"
let checkout_core_dir = "core"

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

let parse ({ prelude; core } : Sources.t) =
  let%bind.Result prelude =
    Parser.program ~file:prelude.path prelude.contents
    |> Result.map_error ~f:(fun error -> Source_file.Error.Parse error)
  in
  let%map.Result core = List.map core ~f:Source_file.parse |> Result.all in
  let loc = Loc.start_of_file core_dir in
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

let checkout_path file =
  if String.equal file path
  then Some checkout_prelude
  else (
    let%bind.Option name = String.chop_prefix file ~prefix:(core_dir ^ "/") in
    let%map.Option _ = source file in
    Filename.concat checkout_core_dir name)
;;

let toplevels =
  lazy
    (match parse Sources.embedded with
     | Ok toplevels -> toplevels
     | Error error ->
       raise_s
         [%message "Prelude failed to parse" (error : Source_file.Error.t)])
;;
