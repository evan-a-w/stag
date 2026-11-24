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

let compile_time_c_calls project (target : Project.Target.t) =
  let modules (target : Project.Target.t) =
    match target.stanza.kind with
    | Library -> [ Stanza.module_name target.stanza ]
    | Executable | Test ->
      List.filter_map target.files ~f:(fun file ->
        Source_file.module_name file.path |> Result.ok)
  in
  Project.closure project target @ [ target ]
  |> List.concat_map ~f:(fun (target : Project.Target.t) ->
    let library =
      { Compile_time_c_calls.Library.name = target.stanza.name.txt
      ; allowed = target.stanza.compile_time_c_calls
      }
    in
    List.map (modules target) ~f:(fun module_ -> module_, library))
  |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  |> Compile_time_c_calls.of_libraries
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
    (match
       ( Driver.check_modules
           ~compile_time_c_calls:(compile_time_c_calls project target)
           ?output
           groups
       , errors )
     with
     | Ok driver, [] -> Ok driver
     | Ok _, errors -> Error (Diagnostic.sort errors)
     | Error { errors = type_errors; partial = _ }, errors ->
       Error
         (Diagnostic.sort
            (errors @ List.map type_errors ~f:Diagnostic.of_typecheck_error)))
;;
