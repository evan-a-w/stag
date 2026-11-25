open! Import
open Stag_build

let find_root () =
  match Project.find_root (Sys_unix.getcwd ()) with
  | Ok root -> root
  | Error error ->
    eprintf "Error: %s\n%!" (Error.to_string_hum error);
    exit 1
;;

let load () =
  let root = find_root () in
  match Project.load ~root () with
  | Ok project -> project
  | Error diagnostics ->
    let contents path =
      Option.try_with (fun () -> In_channel.read_all (root ^/ path))
    in
    List.iter diagnostics ~f:(fun diagnostic ->
      eprintf "%s%!" (Diagnostic.to_string_hum ~contents diagnostic));
    exit 1
;;

let exit_unless ok = if not ok then exit 1

let build_command =
  Command.basic
    ~summary:"Typecheck every library, executable and test"
    ~readme:(fun () -> Pipeline_config.limits_readme ~phases:"compile time")
    (let%map_open.Command limits = Pipeline_config.limits_param () in
     fun () -> exit_unless (Action.build ~limits (load ())))
;;

let exec_command =
  Command.basic
    ~summary:"Typecheck and evaluate an executable"
    ~readme:(fun () ->
      "Arguments after -- are not supported yet. "
      ^ Pipeline_config.limits_readme ~phases:"compile time and evaluation")
    (let%map_open.Command name = anon ("NAME" %: string)
     and limits = Pipeline_config.limits_param ()
     and args =
       flag "--" escape ~doc:"ARGS arguments for the program (unsupported)"
     in
     fun () ->
       let project = load () in
       match args with
       | Some (_ :: _) ->
         eprintf "Error: executables cannot take arguments yet\n%!";
         exit 1
       | None | Some [] -> exit_unless (Action.exec ~limits project name))
;;

let test_command =
  Command.basic
    ~summary:"Run the expect tests of the named libraries and tests, or all"
    ~readme:(fun () ->
      sprintf
        "The corrected version of each file whose tests fail is written to \
         %s/, mirroring its path, and the diff printed. stag-build promote \
         moves them over the sources."
        Action.corrections_dir
      ^ " "
      ^ Pipeline_config.limits_readme
          ~phases:"compile time, evaluation and each test")
    (let%map_open.Command names = anon (sequence ("NAME" %: string))
     and limits = Pipeline_config.limits_param () in
     fun () -> exit_unless (Action.test ~limits (load ()) names))
;;

let promote_command =
  Command.basic
    ~summary:"Move the corrections of failed expect tests over their sources"
    (let%map_open.Command () = return () in
     fun () -> exit_unless (Action.promote (load ())))
;;

let bench_command ~fake_clock =
  Command.basic
    ~summary:"Run the benchmarks of every target, or those matching FILTERs"
    ~readme:(fun () ->
      sprintf
        "Runs the let%%bench benchmarks of each library, executable and test, \
         named TARGET/NAME, whose names contain any FILTER (all without), or \
         with -phases benchmarks the interpreter on the program of each \
         target: TARGET/parse, TARGET/typecheck and TARGET/eval. Baselines are \
         saved in %s/. Benchmarks are unlimited. "
        Action.bench_dir
      ^ Pipeline_config.limits_readme ~phases:"compile time and evaluation")
    (let%map_open.Command filters = anon (sequence ("FILTER" %: string))
     and options = Bench_command.Options.param ~fake_clock
     and limits = Pipeline_config.limits_param ()
     and phases =
       flag "phases" no_arg ~doc:" benchmark the phases of the interpreter"
     in
     fun () ->
       exit_unless (Action.bench ~limits (load ()) options ~filters ~phases))
;;

let fmt_command =
  Command.basic
    ~summary:"Format every source of the project"
    ~readme:(fun () ->
      "Rewrites each source that is not formatted, printing its diff. With \
       -check, writes nothing, printing the diffs, and fails if there is one.")
    (let%map_open.Command check =
       flag
         "check"
         ~aliases:[ "--check" ]
         no_arg
         ~doc:" print the diffs without writing them, failing if any"
     in
     fun () ->
       let mode : Format.Mode.t = if check then Check else Inplace in
       exit_unless (Action.fmt (load ()) ~mode))
;;

let () =
  let argv, fake_clock =
    Bench_command.take_fake_clock (Array.to_list (Sys.get_argv ()))
  in
  Command.group
    ~summary:"Build stag projects"
    [ "build", build_command
    ; "exec", exec_command
    ; "test", test_command
    ; "promote", promote_command
    ; "bench", bench_command ~fake_clock
    ; "fmt", fmt_command
    ]
  |> Command_unix.run ~argv
;;
