open! Import

let path = "<prelude>"
let core_dir = "<core>"

let prelude () =
  let source =
    match Prelude_source.files with
    | [ (_, source) ] -> source
    | files ->
      raise_s
        [%message
          "Expected one prelude file"
            ~files:(List.map files ~f:fst : string list)]
  in
  match Parser.program ~file:path source with
  | Ok toplevels -> toplevels
  | Error error ->
    raise_s [%message "Prelude failed to parse" (error : Parser.Error.t)]
;;

let core () : Syntax.Toplevel.t =
  let files =
    List.map Stag_core_source.files ~f:(fun (name, contents) ->
      match
        Source_file.parse { path = Filename.concat core_dir name; contents }
      with
      | Ok file -> file
      | Error error ->
        raise_s [%message "Core failed to parse" (error : Source_file.Error.t)])
  in
  let loc = Loc.start_of_file core_dir in
  Located.create
    ~loc
    (Syntax.Toplevel.Module (Located.create ~loc "Core", files))
;;

let toplevels = lazy (prelude () @ [ core () ])
