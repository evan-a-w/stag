open! Import

let files (target : Project.Target.t) =
  let parsed = List.map target.files ~f:Source_file.parse_recovering in
  ( List.map parsed ~f:(fun (file, _) ->
      Option.map file ~f:(fun file -> Driver.Module_group.File file))
    |> Option.all
  , List.concat_map parsed ~f:(fun (_, errors) ->
      List.map errors ~f:Diagnostic.of_source_file_error) )
;;

let wrapped (library : Project.Target.t) =
  let files, errors = files library in
  ( Option.map files ~f:(fun files ->
      [ Driver.Module_group.Group
          ( Located.create
              ~loc:library.stanza.name.loc
              (Stanza.module_name library.stanza)
          , files )
      ])
  , errors )
;;

let parse_recovering project (target : Project.Target.t) =
  let own =
    match target.stanza.kind with
    | Library -> wrapped target
    | Executable | Test -> files target
  in
  let parsed = List.map (Project.closure project target) ~f:wrapped @ [ own ] in
  ( List.map parsed ~f:fst |> Option.all |> Option.map ~f:List.concat
  , List.concat_map parsed ~f:snd )
;;

let parse project target =
  match parse_recovering project target with
  | Some groups, [] -> Ok groups
  | _, errors -> Error errors
;;

let check ?output project target =
  let groups, errors = parse_recovering project target in
  match groups with
  | None -> Error (Diagnostic.sort errors)
  | Some groups ->
    (match Driver.check_modules ?output groups, errors with
     | Ok driver, [] -> Ok driver
     | Ok _, errors -> Error (Diagnostic.sort errors)
     | Error { errors = type_errors; partial = _ }, errors ->
       Error
         (Diagnostic.sort
            (errors @ List.map type_errors ~f:Diagnostic.of_typecheck_error)))
;;
