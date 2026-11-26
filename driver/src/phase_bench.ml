open! Import

let to_error diagnostics =
  Error.of_string
    (String.concat
       (List.map
          diagnostics
          ~f:(Diagnostic.to_string_hum ?contents:None)))
;;

let benches ?(prefix = "") ?compile_time_c_calls ~prelude parse =
  let parse () = parse () |> Result.map_error ~f:to_error in
  let check groups =
    Driver.check_modules ?compile_time_c_calls ?prelude ~output:ignore groups
    |> Result.map_error
         ~f:(fun ({ errors; partial = _ } : Typecheck.Failed.t) ->
           to_error (List.map errors ~f:Diagnostic.of_typecheck_error))
  in
  let bench name routine =
    Measure.Suite.Bench.Single { name = prefix ^ name; routine }
  in
  let parsed = lazy (parse ()) in
  [ bench "parse" (Plain (fun () -> parse () |> Or_error.ignore_m))
  ; bench
      "typecheck"
      (Plain
         (fun () ->
           let%bind.Or_error groups = force parsed in
           check groups |> Or_error.ignore_m))
  ; bench
      "eval"
      (Batched
         { setup =
             (fun () ->
               let%bind.Or_error groups = force parsed in
               check groups)
         ; run = Driver.eval ?limits:None
         })
  ]
;;

let parse_files files () =
  List.map files ~f:(fun file ->
    Source_file.parse file
    |> Result.map ~f:(fun file -> Driver.Module_group.File file)
    |> Result.map_error ~f:Diagnostic.of_source_file_error)
  |> Result.combine_errors
;;
