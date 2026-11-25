open! Import

let files (target : Project.Target.t) = Pipeline.parse_files target.files

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

let paths project target =
  List.concat_map
    (Project.closure project target @ [ target ])
    ~f:(fun (target : Project.Target.t) ->
      List.map target.files ~f:(fun source -> Project.root project ^/ source.path))
;;

let config ~limits project target =
  Pipeline_config.create ~limits (paths project target)
;;

let check ?output ~limits project target =
  match
    Pipeline.check
      ~compile_time_c_calls:(compile_time_c_calls project target)
      ?output
      (config ~limits project target)
      (parse_recovering project target)
  with
  | Error diagnostic -> Error [ diagnostic ]
  | Ok checked -> Pipeline.Checked.to_result checked
;;
