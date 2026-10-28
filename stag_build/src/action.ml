open! Import

let corrections_dir = "_stag"

let contents project path =
  Option.try_with (fun () -> In_channel.read_all (Project.root project ^/ path))
;;

let report project diagnostics =
  Out_channel.flush stdout;
  List.iter diagnostics ~f:(fun diagnostic ->
    eprintf
      "%s%!"
      (Diagnostic.to_string_hum ~contents:(contents project) diagnostic))
;;

let warn project diagnostic =
  Out_channel.flush stdout;
  eprintf
    "%s%!"
    (Diagnostic.to_string_hum
       ~contents:(contents project)
       ~warning:true
       diagnostic)
;;

let error message =
  Out_channel.flush stdout;
  eprintf "Error: %s\n%!" message;
  false
;;

let all list ~f = List.map list ~f |> List.for_all ~f:Fn.id

let files (target : Project.Target.t) =
  List.map target.files ~f:(fun file -> file.path) |> String.Set.of_list
;;

let warn_unrun_tests project (target : Project.Target.t) driver =
  match target.stanza.kind with
  | Library | Test -> ()
  | Executable ->
    let files = files target in
    List.iter
      (Driver.checked driver).program.expect_tests
      ~f:(fun (test : Typed.Expect_test.t) ->
        if Set.mem files test.name.loc.file
        then
          warn
            project
            (Diagnostic.create
               ~loc:test.name.loc
               (sprintf
                  "This expect test is in the executable %s, whose tests are \
                   never run"
                  (Project.Target.name target))))
;;

let stale_expansions (target : Project.Target.t) driver =
  let files = files target in
  List.filter (Inline_expansion.stale driver) ~f:(fun expansion ->
    Set.mem files expansion.loc.file)
;;

(* The program of a target, checked once, reporting its errors once, and its
   stale expansions as warnings unless [stale_are_errors]. A target is not
   checked if a library it depends on fails. Output at compile time is
   discarded. *)
let checker ?(stale_are_errors = false) project =
  let status = String.Table.create () in
  let rec check (target : Project.Target.t) =
    let name = Project.Target.name target in
    match Hashtbl.find status name with
    | Some driver -> driver
    | None ->
      let deps_ok =
        all (Project.closure project target) ~f:(fun dep ->
          Option.is_some (check dep))
      in
      let driver =
        if not deps_ok
        then None
        else (
          match Program.check ~output:ignore project target with
          | Ok driver ->
            warn_unrun_tests project target driver;
            if not stale_are_errors
            then
              List.iter (stale_expansions target driver) ~f:(fun expansion ->
                warn project (Inline_expansion.diagnostic expansion));
            Some driver
          | Error diagnostics ->
            report project diagnostics;
            None)
      in
      Hashtbl.set status ~key:name ~data:driver;
      driver
  in
  check
;;

let build project =
  let check = checker project in
  all (Project.targets project) ~f:(fun target -> Option.is_some (check target))
;;

let eval project driver =
  Treewalk.set_output (Driver.interpreter driver) print_string;
  match Driver.eval driver with
  | Ok () -> true
  | Error error ->
    report project [ Diagnostic.of_runtime_error error ];
    false
;;

let exec project name =
  match Project.find project name with
  | None -> error (sprintf "Unknown executable %s" name)
  | Some ({ stanza = { kind = Executable; _ }; _ } as target) ->
    let ok =
      match checker project target with
      | None -> false
      | Some driver -> eval project driver
    in
    Out_channel.flush stdout;
    ok
  | Some { stanza = { kind = (Library | Test) as kind; _ }; _ } ->
    error
      (sprintf
         "%s is %s, not an executable"
         name
         (Stanza.Kind.to_string_with_article kind))
;;

(* The correction of [source] is [corrections_dir/source.corrected], beside the
   digest of the version of [source] it corrects. *)
module Correction = struct
  let path source = corrections_dir ^/ source ^ ".corrected"
  let digest_path source = corrections_dir ^/ source ^ ".digest"
  let digest contents = Md5.to_hex (Md5.digest_string contents)

  let source_of_path path =
    let%bind.Option path =
      String.chop_prefix path ~prefix:(corrections_dir ^ "/")
    in
    String.chop_suffix path ~suffix:".corrected"
  ;;

  let rec remove_empty_dirs ~root dir =
    if String.is_prefix dir ~prefix:(corrections_dir ^ "/")
    then (
      match Sys_unix.readdir (root ^/ dir) with
      | [||] ->
        Core_unix.rmdir (root ^/ dir);
        remove_empty_dirs ~root (Filename.dirname dir)
      | _ | (exception _) -> ())
  ;;

  let remove ~root source =
    Fs.try_ ~path:(path source) (fun () ->
      List.iter
        [ path source; digest_path source ]
        ~f:(fun path ->
          if Sys_unix.file_exists_exn (root ^/ path)
          then Sys_unix.remove (root ^/ path));
      remove_empty_dirs ~root (Filename.dirname (path source)))
  ;;

  let write ~root ({ path = source; contents } : Source_file.t) ~corrected =
    Fs.try_ ~path:(path source) (fun () ->
      Core_unix.mkdir_p (root ^/ Filename.dirname (path source));
      Out_channel.write_all (root ^/ path source) ~data:corrected;
      Out_channel.write_all
        (root ^/ digest_path source)
        ~data:(digest contents ^ "\n"))
  ;;

  (* Whether [contents] is the version of [source] the correction corrects. *)
  let corrects ~root source ~contents =
    match In_channel.read_all (root ^/ digest_path source) with
    | recorded -> String.equal (String.strip recorded) (digest contents)
    | exception _ -> false
  ;;
end

let test_target project ~check (target : Project.Target.t) =
  let root = Project.root project in
  let checked =
    Option.map (check target) ~f:(fun driver ->
      let expansions = stale_expansions target driver in
      report project (List.map expansions ~f:Inline_expansion.diagnostic);
      let results =
        if not (eval project driver)
        then None
        else (
          let results = Expect_test_runner.run ~files:(files target) driver in
          report project (List.concat_map results ~f:Diagnostic.of_test_result);
          Some results)
      in
      results, expansions)
  in
  let corrections =
    List.map target.files ~f:(fun ({ path; contents } as file) ->
      match
        Option.bind checked ~f:(fun (results, expansions) ->
          Corrected_file.create
            ~path
            ~contents
            ~tests:(Option.value results ~default:[])
            ~expansions)
      with
      | None -> Correction.remove ~root path
      | Some corrected ->
        Option.iter
          (Text_diff.unified
             ~old_name:path
             ~new_name:(Correction.path path)
             ~old:contents
             ~new_:corrected)
          ~f:print_string;
        Correction.write ~root file ~corrected)
    |> Result.combine_errors_unit
  in
  Out_channel.flush stdout;
  let corrections_ok =
    match corrections with
    | Ok () -> true
    | Error diagnostics ->
      report project diagnostics;
      false
  in
  match checked with
  | None -> false
  | Some (results, expansions) ->
    let failed =
      Option.value_map results ~default:1 ~f:(fun results ->
        let failed =
          List.count results ~f:(Fn.non Expect_test_runner.Test_result.passed)
        in
        if failed > 0
        then
          eprintf
            "%s: %d of %d test(s) failed\n%!"
            (Project.Target.name target)
            failed
            (List.length results);
        failed)
    in
    let stale = List.length expansions in
    if stale > 0
    then
      eprintf
        "%s: %d inline expansion(s) out of date\n%!"
        (Project.Target.name target)
        stale;
    corrections_ok && failed = 0 && stale = 0
;;

(* Whether [corrections_dir] exists, erring if it is not a directory. *)
let corrections_dir_exists project =
  match Fs.Kind.of_path (Project.root project ^/ corrections_dir) with
  | Directory -> Ok true
  | File | Other -> Error ()
  | exception Core_unix.Unix_error (ENOENT, _, _) -> Ok false
;;

let not_a_directory () = error (sprintf "%s is not a directory" corrections_dir)

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
      List.stable_dedup names ~compare:String.compare
      |> List.map ~f:(fun name ->
        match Project.find project name with
        | None -> Error (sprintf "Unknown test or library %s" name)
        | Some target when has_tests target -> Ok target
        | Some _ ->
          Error (sprintf "%s is an executable, whose tests are not run" name))
      |> Result.combine_errors
  in
  match targets, corrections_dir_exists project with
  | Error messages, _ ->
    List.iter messages ~f:(fun message -> ignore (error message : bool));
    false
  | Ok _, Error () -> not_a_directory ()
  | Ok targets, Ok (_ : bool) ->
    all
      targets
      ~f:(test_target project ~check:(checker ~stale_are_errors:true project))
;;

let bench_dir = corrections_dir ^/ "bench"

let bench project options ~filters ~phases =
  let check = checker project in
  let benches (target : Project.Target.t) =
    let name = Project.Target.name target in
    let prefix = name ^ "/" in
    if phases
    then
      Option.map (check target) ~f:(fun (_ : Driver.t) ->
        Phase_bench.benches ~prefix (fun () -> Program.parse project target))
    else (
      let%bind.Option driver = check target in
      Treewalk.set_output (Driver.interpreter driver) ignore;
      match
        let%bind.Result () =
          Driver.eval driver |> Result.map_error ~f:Diagnostic.of_runtime_error
        in
        Bench_runner.benches ~files:(files target) driver
      with
      | Ok benches ->
        Some
          (List.map benches ~f:(fun bench ->
             Measure.Suite.Bench.add_prefix bench prefix))
      | Error diagnostic ->
        report project [ diagnostic ];
        None)
  in
  let benches = List.map (Project.targets project) ~f:benches in
  let ok =
    Bench_command.run
      ~filters
      options
      ~baseline_dir:(Project.root project ^/ bench_dir)
      ~report:(fun diagnostic -> report project [ diagnostic ])
      (List.concat_map benches ~f:(Option.value ~default:[]))
  in
  ok && List.for_all benches ~f:Option.is_some
;;

let corrections ~root ~error =
  let rec walk dir =
    Fs.read_dir ~error ~root dir
    |> List.concat_map ~f:(fun (name, (kind : Fs.Kind.t)) ->
      let path = dir ^/ name in
      match kind with
      | Directory -> walk path
      | File -> Option.to_list (Correction.source_of_path path)
      | Other -> [])
  in
  walk corrections_dir
;;

let promote_one project ~sources source =
  let root = Project.root project in
  let correction = Correction.path source in
  let refuse reason =
    error (sprintf "Not promoting %s: %s" correction reason)
  in
  match Map.find sources source with
  | None ->
    if Sys_unix.file_exists_exn (root ^/ source)
    then refuse (sprintf "%s is not a source of the project" source)
    else refuse (sprintf "%s does not exist" source)
  | Some contents when not (Correction.corrects ~root source ~contents) ->
    refuse
      (sprintf
         "%s has changed since its tests ran; run stag-build test again"
         source)
  | Some _ ->
    eprintf "Promoting %s to %s.\n%!" correction source;
    (match
       let%bind.Result corrected =
         Fs.try_ ~path:correction (fun () ->
           In_channel.read_all (root ^/ correction))
       in
       let%bind.Result () =
         Fs.try_ ~path:source (fun () ->
           Out_channel.write_all (root ^/ source) ~data:corrected)
       in
       Correction.remove ~root source
     with
     | Ok () -> true
     | Error diagnostic ->
       report project [ diagnostic ];
       false)
;;

let promote project =
  match corrections_dir_exists project with
  | Error () -> not_a_directory ()
  | Ok false -> true
  | Ok true ->
    let sources =
      List.concat_map (Project.targets project) ~f:(fun target -> target.files)
      |> List.map ~f:(fun (file : Source_file.t) -> file.path, file.contents)
      |> String.Map.of_alist_exn
    in
    let errors = Queue.create () in
    let corrections =
      corrections ~root:(Project.root project) ~error:(Queue.enqueue errors)
    in
    report project (Queue.to_list errors);
    let promoted = all corrections ~f:(promote_one project ~sources) in
    promoted && Queue.is_empty errors
;;

let fmt project ~(mode : Format.Mode.t) =
  let { Format.Summary.unformatted; errors } =
    Format.run
      ~root:(Project.root project)
      ~mode
      (List.concat_map (Project.targets project) ~f:(fun target -> target.files))
  in
  List.iter unformatted ~f:(fun unformatted ->
    print_string (Format.Unformatted.diff unformatted));
  report project errors;
  let unformatted_fails =
    match mode with
    | Check -> not (List.is_empty unformatted)
    | Inplace -> false
  in
  List.is_empty errors && not unformatted_fails
;;
