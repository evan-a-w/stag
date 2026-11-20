open! Import

let max_label_length = 40

let label string =
  if String.length string <= max_label_length
  then string
  else String.prefix string (max_label_length - 3) ^ "..."
;;

let benches ?files driver =
  let interpreter = Driver.interpreter driver in
  Interpreter.set_output interpreter ignore;
  Interpreter.set_limits interpreter Limits.unlimited;
  let (T ((module I), interpreter)) = interpreter in
  let in_files (bench : Typed.Bench.t) =
    Option.for_all files ~f:(fun files ->
      Set.mem files (Loc.file_name bench.name.loc))
  in
  let routine bindings body : Measure.Routine.t =
    Plain
      (fun () -> I.eval_expr interpreter ~bindings body |> Or_error.ignore_m)
  in
  let seen = String.Table.create () in
  let claim name ~(loc : Loc.t) =
    match Hashtbl.find seen name with
    | Some (previous : Loc.t) -> Error previous
    | None ->
      Hashtbl.set seen ~key:name ~data:loc;
      Ok ()
  in
  let duplicate name ~loc ~previous =
    Diagnostic.create
      ~loc
      ~notes:[ { loc = previous; message = "The other is here" } ]
      (sprintf "Two benchmarks are named %S" name)
  in
  let benches =
    (Driver.checked driver).program.benches |> List.filter ~f:in_files
  in
  let%map.Result () =
    List.filter_map benches ~f:(fun (bench : Typed.Bench.t) ->
      match bench.param with
      | Some _ -> None
      | None ->
        let name = bench.name.txt
        and loc = bench.name.loc in
        Some
          (claim name ~loc
           |> Result.map_error ~f:(fun previous ->
             duplicate name ~loc ~previous)))
    |> Result.all_unit
  in
  List.map benches ~f:(fun (bench : Typed.Bench.t) : Measure.Suite.Bench.t ->
    let name = bench.name.txt in
    match bench.param with
    | None -> Single { name; routine = routine [] bench.body }
    | Some (ident, args) ->
      Parameterised
        { name
        ; args =
            List.map args ~f:(fun arg () ->
              let%bind.Or_error value = I.eval_expr interpreter arg in
              let label = label (I.Value.to_string_hum value) in
              let loc = bench.name.loc in
              let%map.Or_error () =
                claim (name ^ "/" ^ label) ~loc
                |> Result.map_error ~f:(fun previous ->
                  Runtime_error.to_error
                    (Runtime_error.create
                       ~loc
                       (sprintf
                          "Two benchmarks are named %S, the other at %s"
                          (name ^ "/" ^ label)
                          (Loc.to_string previous))))
              in
              label, routine [ ident, value ] bench.body)
        })
;;
