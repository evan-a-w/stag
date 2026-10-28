open! Import

let to_error diagnostics =
  Error.of_string
    (String.concat
       (List.map
          diagnostics
          ~f:(Diagnostic.to_string_hum ?contents:None ?warning:None)))
;;

let benches ?(prefix = "") parse =
  let parse () = parse () |> Result.map_error ~f:to_error in
  let check groups =
    Driver.check_modules ~output:ignore groups
    |> Result.map_error ~f:(fun error ->
      to_error [ Diagnostic.of_typecheck_error error ])
  in
  let bench name routine =
    { Measure.Suite.Bench.name = prefix ^ name; elements = None; routine }
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
