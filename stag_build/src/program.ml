open! Import

let files (target : Project.Target.t) =
  List.map target.files ~f:(fun file ->
    Source_file.parse file
    |> Result.map ~f:(fun file -> Driver.Module_group.File file)
    |> Result.map_error ~f:Diagnostic.of_source_file_error)
  |> Result.combine_errors
;;

let wrapped (library : Project.Target.t) =
  let%map.Result files = files library in
  [ Driver.Module_group.Group
      ( Located.create
          ~loc:library.stanza.name.loc
          (Stanza.module_name library.stanza)
      , files )
  ]
;;

let parse project (target : Project.Target.t) =
  let own =
    match target.stanza.kind with
    | Library -> wrapped target
    | Executable | Test -> files target
  in
  List.map (Project.closure project target) ~f:wrapped @ [ own ]
  |> Result.combine_errors
  |> Result.map ~f:List.concat
  |> Result.map_error ~f:List.concat
;;

let check ?output project target =
  let%bind.Result groups = parse project target in
  Driver.check_modules ?output groups
  |> Result.map_error ~f:(fun error -> [ Diagnostic.of_typecheck_error error ])
;;
