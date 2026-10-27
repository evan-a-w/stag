open! Import

let report (file : Source_file.t) diagnostic =
  let contents path =
    Option.some_if (String.equal path file.path) file.contents
  in
  eprintf "%s%!" (Diagnostic.to_string_hum ~contents diagnostic)
;;

let fail file diagnostics =
  List.iter diagnostics ~f:(report file);
  exit 1
;;

let run_command =
  Command.basic
    ~summary:"Run the expect tests of FILE, printing it corrected"
    (let%map_open.Command path = anon ("FILE" %: string) in
     fun () ->
       let file = { Source_file.path; contents = In_channel.read_all path } in
       let driver =
         match Driver.check_files ~output:ignore [ file ] with
         | Ok driver -> driver
         | Error error -> fail file [ Diagnostic.of_driver_error error ]
       in
       (match Driver.eval driver with
        | Ok () -> ()
        | Error error -> fail file [ Diagnostic.of_runtime_error error ]);
       let results = Expect_test_runner.run driver in
       (match List.concat_map results ~f:Diagnostic.of_test_result with
        | [] -> ()
        | diagnostics -> fail file diagnostics);
       print_string
         (Option.value
            (Expect_test_runner.corrected ~path ~contents:file.contents results)
            ~default:file.contents))
;;

let rule ~path =
  let basename = Filename.basename path in
  match Source_file.module_name basename with
  | Error message ->
    eprintf "Error: %s\n%!" message;
    exit 1
  | Ok _ ->
    sprintf
      {|(rule
 (deps ../%s)
 (action
  (with-stdout-to
   %s.corrected
   (chdir
    %%{workspace_root}
    (run %%{exe:../runner/stag_expect.exe} run tests/%s)))))

(rule
 (alias runtest)
 (action
  (diff ../%s %s.corrected)))
|}
      basename
      basename
      basename
      basename
      basename
;;

let dune_rules_command =
  Command.basic
    ~summary:
      "Print the dune rules running the tests of the .stag FILEs in tests/"
    (let%map_open.Command paths = anon (sequence ("FILE" %: string)) in
     fun () ->
       List.map paths ~f:(fun path -> rule ~path)
       |> String.concat ~sep:"\n"
       |> print_string)
;;

let () =
  Command.group
    ~summary:"Expect tests written as .stag files"
    [ "run", run_command; "dune-rules", dune_rules_command ]
  |> Command_unix.run
;;
