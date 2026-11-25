open! Import

module Checked = struct
  type t =
    { groups : Driver.Module_group.t list option
    ; checked : Typecheck.Checked.t option
    ; driver : Driver.t option
    ; diagnostics : Diagnostic.t list
    }

  let to_result t =
    match t.driver with
    | Some driver -> Ok driver
    | None -> Error t.diagnostics
  ;;
end

let parse_files files =
  let parsed = List.map files ~f:Source_file.parse_recovering in
  ( List.map parsed ~f:(fun (file, _) ->
      Option.map file ~f:(fun file -> Driver.Module_group.File file))
    |> Option.all
  , List.concat_map parsed ~f:(fun (_, errors) ->
      List.map errors ~f:Diagnostic.of_source_file_error) )
;;

let check_sources ?compile_time_c_calls ?output ?prelude ~limits (groups, errors)
  =
  match groups with
  | None ->
    { Checked.groups
    ; checked = None
    ; driver = None
    ; diagnostics = Diagnostic.sort errors
    }
  | Some modules ->
    let driver, checked, type_errors =
      match
        Driver.check_modules
          ?compile_time_c_calls
          ~limits
          ?output
          ?prelude
          modules
      with
      | Ok driver -> Some driver, Driver.checked driver, []
      | Error { errors; partial } -> None, partial, errors
    in
    let diagnostics =
      Diagnostic.sort
        (errors @ List.map type_errors ~f:Diagnostic.of_typecheck_error)
    in
    { groups
    ; checked = Some checked
    ; driver = Option.filter driver ~f:(fun _ -> List.is_empty diagnostics)
    ; diagnostics
    }
;;

let check
  ?compile_time_c_calls
  ?output
  ?contents
  ({ prelude; limits } : Pipeline_config.t)
  sources
  =
  let%map.Result prelude =
    Pipeline_config.Prelude_choice.prelude ?contents prelude
  in
  check_sources ?compile_time_c_calls ?output ?prelude ~limits sources
;;
