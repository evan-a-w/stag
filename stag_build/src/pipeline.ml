open! Import

module Checked = struct
  type t =
    { groups : Driver.Module_group.t list option
    ; prelude : Syntax.Toplevel.t list option
    ; checked : Typecheck.Checked.t option
    ; driver : Driver.t option
    ; diagnostics : Diagnostic.t list
    }

  let to_result t =
    match t.driver with
    | Some driver -> Ok driver
    | None -> Error t.diagnostics
  ;;

  let of_result ~groups ~prelude ~errors result =
    let driver, checked, type_errors =
      match result with
      | Ok driver -> Some driver, Driver.checked driver, []
      | Error { Typecheck.Failed.errors; partial } -> None, partial, errors
    in
    let diagnostics =
      Diagnostic.sort
        (errors @ List.map type_errors ~f:Diagnostic.of_typecheck_error)
    in
    { groups
    ; prelude
    ; checked = Some checked
    ; driver = Option.filter driver ~f:(fun _ -> List.is_empty diagnostics)
    ; diagnostics
    }
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

let check_sources
  ?compile_time_c_calls
  ?record_references
  ?output
  ?prelude
  ~limits
  (groups, errors)
  =
  match groups with
  | None ->
    { Checked.groups
    ; prelude
    ; checked = None
    ; driver = None
    ; diagnostics = Diagnostic.sort errors
    }
  | Some modules ->
    Driver.check_modules
      ?compile_time_c_calls
      ?record_references
      ~limits
      ?output
      ?prelude
      modules
    |> Checked.of_result ~groups ~prelude ~errors
;;

let check
  ?compile_time_c_calls
  ?record_references
  ?output
  ?contents
  ({ prelude; limits } : Pipeline_config.t)
  sources
  =
  let%map.Result prelude =
    Pipeline_config.Prelude_choice.prelude ?contents prelude
  in
  check_sources
    ?compile_time_c_calls
    ?record_references
    ?output
    ?prelude
    ~limits
    sources
;;

let check_prelude ?record_references ?output ~limits prelude =
  Driver.check_prelude ?record_references ~limits ?output prelude
  |> Checked.of_result
       ~groups:
         (Some
            [ Driver.Module_group.Group
                ( Located.create ~loc:Loc.none Prelude_path.root
                , List.map prelude ~f:(fun toplevel ->
                    Driver.Module_group.File toplevel) )
            ])
       ~prelude:None
       ~errors:[]
;;
