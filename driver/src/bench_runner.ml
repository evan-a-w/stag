open! Import

let benches ?files driver =
  let interpreter = Driver.interpreter driver in
  Treewalk.set_output interpreter ignore;
  let in_files (bench : Typed.Bench.t) =
    Option.for_all files ~f:(fun files -> Set.mem files bench.name.loc.file)
  in
  let routine bindings body : Measure.Routine.t =
    Plain
      (fun () ->
        Treewalk.eval_expr interpreter ~bindings body |> Or_error.ignore_m)
  in
  let cases (bench : Typed.Bench.t) =
    match bench.param with
    | None -> Ok [ bench.name.txt, routine [] bench.body ]
    | Some (ident, args) ->
      List.map args ~f:(fun arg ->
        let%map.Result value =
          Treewalk.eval_expr interpreter arg
          |> Result.map_error ~f:Diagnostic.of_runtime_error
        in
        ( bench.name.txt ^ "/" ^ Value.to_string_hum value
        , routine [ ident, value ] bench.body ))
      |> Result.all
  in
  let seen = String.Table.create () in
  (Driver.checked driver).program.benches
  |> List.filter ~f:in_files
  |> List.map ~f:(fun (bench : Typed.Bench.t) ->
    let%bind.Result cases = cases bench in
    List.map cases ~f:(fun (name, routine) ->
      match Hashtbl.find seen name with
      | Some (previous : Loc.t) ->
        Error
          (Diagnostic.create
             ~loc:bench.name.loc
             ~notes:[ { loc = previous; message = "The other is here" } ]
             (sprintf "Two benchmarks are named %S" name))
      | None ->
        Hashtbl.set seen ~key:name ~data:bench.name.loc;
        Ok { Measure.Suite.Bench.name; elements = None; routine })
    |> Result.all)
  |> Result.all
  |> Result.map ~f:List.concat
;;
