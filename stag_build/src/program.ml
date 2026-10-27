open! Import

let file_modules (target : Project.Target.t) =
  List.map target.files ~f:(fun file ->
    Source_file.parse file
    |> Result.map_error ~f:Diagnostic.of_source_file_error)
  |> Result.combine_errors
;;

let wrapped (library : Project.Target.t) =
  let%map.Result modules = file_modules library in
  let loc = library.stanza.name.loc in
  [ Located.create
      ~loc
      (Syntax.Toplevel.Module
         (Located.create ~loc (Stanza.module_name library.stanza), modules))
  ]
;;

let modules project (target : Project.Target.t) =
  let own =
    match target.stanza.kind with
    | Library -> wrapped target
    | Executable | Test -> file_modules target
  in
  List.map (Project.closure project target) ~f:wrapped @ [ own ]
  |> Result.combine_errors
  |> Result.map ~f:List.concat
  |> Result.map_error ~f:List.concat
;;

let check ?output project target =
  let%bind.Result modules = modules project target in
  Driver.check_modules ?output modules
  |> Result.map_error ~f:(fun error -> [ Diagnostic.of_typecheck_error error ])
;;
