open! Core
open Stag

let canonical path =
  Option.value (Option.try_with (fun () -> Filename_unix.realpath path)) ~default:path
;;

let read_files paths =
  List.stable_dedup paths ~compare:(Comparable.lift String.compare ~f:canonical)
  |> List.map ~f:(fun path ->
    match In_channel.read_all path with
    | contents -> { Source_file.path; contents }
    | exception Sys_error message ->
      eprintf "Error: %s\n%!" message;
      exit 2)
;;

let report files diagnostic =
  let contents path =
    List.find_map files ~f:(fun (file : Source_file.t) ->
      Option.some_if (String.equal file.path path) file.contents)
  in
  Out_channel.flush stdout;
  eprintf "%s%!" (Diagnostic.to_string_hum ~contents diagnostic)
;;

let check files =
  match Driver.check_files files with
  | Ok driver -> driver
  | Error error ->
    report files (Diagnostic.of_driver_error error);
    exit 1
;;

let eval files driver =
  match Driver.eval driver with
  | Ok () -> ()
  | Error error ->
    report files (Diagnostic.of_runtime_error error);
    exit 1
;;

let files_param =
  Command.Param.(anon (non_empty_sequence_as_list ("FILE" %: string)))
;;

let check_command =
  Command.basic
    ~summary:"Typecheck the program of the files and print its signature"
    (let%map_open.Command paths = files_param in
     fun () ->
       let files = read_files paths in
       let driver = check files in
       print_string (Signature.to_string_hum (Driver.checked driver).signature))
;;

let run_command =
  Command.basic
    ~summary:"Typecheck and evaluate the program of the files"
    (let%map_open.Command paths = files_param in
     fun () ->
       let files = read_files paths in
       eval files (check files))
;;

let test_command =
  Command.basic
    ~summary:"Run the expect tests of the program of the files"
    ~readme:(fun () ->
      "Each failing FILE is corrected into FILE.corrected, or in place with \
       -promote, and the diff printed.")
    (let%map_open.Command paths = files_param
     and promote =
       flag
         "promote"
         ~aliases:[ "--promote" ]
         no_arg
         ~doc:" correct the files in place"
     in
     fun () ->
       let files = read_files paths in
       let driver = check files in
       eval files driver;
       let results = Expect_test_runner.run driver in
       List.iter results ~f:(fun result ->
         List.iter (Diagnostic.of_test_result result) ~f:(report files));
       let mismatched =
         List.filter_map files ~f:(fun { path; contents } ->
           let corrected_path = path ^ ".corrected" in
           match Expect_test_runner.corrected ~path ~contents results with
           | None ->
             if Sys_unix.file_exists_exn corrected_path
             then Sys_unix.remove corrected_path;
             None
           | Some corrected ->
             Option.iter
               (Text_diff.unified
                  ~old_name:path
                  ~new_name:corrected_path
                  ~old:contents
                  ~new_:corrected)
               ~f:print_string;
             if promote
             then (
               Out_channel.write_all path ~data:corrected;
               if Sys_unix.file_exists_exn corrected_path
               then Sys_unix.remove corrected_path)
             else Out_channel.write_all corrected_path ~data:corrected;
             Some path)
       in
       let errors =
         List.count results ~f:(fun result ->
           Option.is_some result.error
           || not (List.is_empty result.different_outputs))
       in
       let total = List.length results in
       let failed =
         List.count results ~f:(Fn.non Expect_test_runner.Test_result.passed)
       in
       Out_channel.flush stdout;
       if failed > 0
       then
         eprintf
           "%d of %d test(s) failed%s\n%!"
           failed
           total
           (if promote && not (List.is_empty mismatched)
            then " (expectations promoted)"
            else "");
       if errors > 0 || ((not promote) && failed > 0) then exit 1)
;;

let () =
  Command.group
    ~summary:"The stag language"
    [ "check", check_command; "run", run_command; "test", test_command ]
  |> Command_unix.run
;;
