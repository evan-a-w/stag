open! Import

let corrections_dir = "_stag"

let report project diagnostics =
  let contents path =
    let path = Project.root project ^/ path in
    Option.try_with (fun () -> In_channel.read_all path)
  in
  Out_channel.flush stdout;
  List.iter diagnostics ~f:(fun diagnostic ->
    eprintf "%s%!" (Diagnostic.to_string_hum ~contents diagnostic))
;;

let error message =
  Out_channel.flush stdout;
  eprintf "Error: %s\n%!" message;
  false
;;

let build project =
  let status = String.Table.create () in
  let rec ok (target : Project.Target.t) =
    let name = Project.Target.name target in
    match Hashtbl.find status name with
    | Some ok -> ok
    | None ->
      let deps_ok = List.for_all (Project.closure project target) ~f:ok in
      let ok =
        deps_ok
        &&
        match Program.check ~output:ignore project target with
        | Ok (_ : Driver.t) -> true
        | Error diagnostics ->
          report project diagnostics;
          false
      in
      Hashtbl.set status ~key:name ~data:ok;
      ok
  in
  List.map (Project.targets project) ~f:ok |> List.for_all ~f:Fn.id
;;

let check_and_eval project target =
  match Program.check project target with
  | Error diagnostics ->
    report project diagnostics;
    None
  | Ok driver ->
    (match Driver.eval driver with
     | Ok () -> Some driver
     | Error error ->
       report project [ Diagnostic.of_runtime_error error ];
       None)
;;

let exec project name =
  match Project.find project name with
  | None -> error (sprintf "Unknown executable %s" name)
  | Some ({ stanza = { kind = Executable; _ }; _ } as target) ->
    let ok = Option.is_some (check_and_eval project target) in
    Out_channel.flush stdout;
    ok
  | Some { stanza = { kind = (Library | Test) as kind; _ }; _ } ->
    error
      (sprintf
         "%s is a %s, not an executable"
         name
         (Stanza.Kind.to_string kind))
;;

let correction_path path = corrections_dir ^/ path ^ ".corrected"

let test_target project (target : Project.Target.t) =
  match check_and_eval project target with
  | None -> false
  | Some driver ->
    let paths =
      List.map target.files ~f:(fun file -> file.path) |> String.Set.of_list
    in
    let results =
      Expect_test_runner.run driver
      |> List.filter ~f:(fun (result : Expect_test_runner.Test_result.t) ->
        Set.mem paths result.name.loc.file)
    in
    report
      project
      (List.filter_map results ~f:(fun result ->
         Option.map result.error ~f:Diagnostic.of_runtime_error));
    List.iter target.files ~f:(fun { path; contents } ->
      let correction = correction_path path in
      let absolute = Project.root project ^/ correction in
      match Expect_test_runner.corrected ~path ~contents results with
      | None ->
        if Sys_unix.file_exists_exn absolute then Sys_unix.remove absolute
      | Some corrected ->
        Option.iter
          (Text_diff.unified
             ~old_name:path
             ~new_name:correction
             ~old:contents
             ~new_:corrected)
          ~f:print_string;
        Core_unix.mkdir_p (Filename.dirname absolute);
        Out_channel.write_all absolute ~data:corrected);
    let failed =
      List.count results ~f:(Fn.non Expect_test_runner.Test_result.passed)
    in
    Out_channel.flush stdout;
    if failed > 0
    then
      eprintf
        "%s: %d of %d test(s) failed\n%!"
        (Project.Target.name target)
        failed
        (List.length results);
    failed = 0
;;

let test project names =
  let has_tests (target : Project.Target.t) =
    match target.stanza.kind with
    | Library | Test -> true
    | Executable -> false
  in
  let targets =
    match names with
    | [] -> Ok (List.filter (Project.targets project) ~f:has_tests)
    | names ->
      List.map names ~f:(fun name ->
        match Project.find project name with
        | None -> Error (sprintf "Unknown test or library %s" name)
        | Some target when has_tests target -> Ok target
        | Some _ ->
          Error (sprintf "%s is an executable, whose tests are not run" name))
      |> Result.combine_errors
  in
  match targets with
  | Error messages ->
    List.iter messages ~f:(fun message -> ignore (error message : bool));
    false
  | Ok targets ->
    List.map targets ~f:(test_target project) |> List.for_all ~f:Fn.id
;;

let promote ~root =
  let rec walk dir =
    let absolute = root ^/ dir in
    Sys_unix.readdir absolute
    |> Array.to_list
    |> List.sort ~compare:String.compare
    |> List.iter ~f:(fun name ->
      let path = dir ^/ name in
      if Sys_unix.is_directory_exn (root ^/ path)
      then walk path
      else
        Option.iter
          (String.chop_suffix path ~suffix:".corrected")
          ~f:(fun source ->
            let source =
              String.chop_prefix_exn source ~prefix:(corrections_dir ^ "/")
            in
            eprintf "Promoting %s to %s.\n%!" path source;
            Out_channel.write_all
              (root ^/ source)
              ~data:(In_channel.read_all (root ^/ path));
            Sys_unix.remove (root ^/ path)))
  in
  if Sys_unix.is_directory_exn (root ^/ corrections_dir)
  then walk corrections_dir
;;
