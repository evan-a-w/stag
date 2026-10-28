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
    (let%map_open.Command () = return () in
     fun () -> exit_unless (Action.build (load ())))
;;

let exec_command =
  Command.basic
    ~summary:"Typecheck and evaluate an executable"
    ~readme:(fun () -> "Arguments after -- are not supported yet.")
    (let%map_open.Command name = anon ("NAME" %: string)
     and args =
       flag "--" escape ~doc:"ARGS arguments for the program (unsupported)"
     in
     fun () ->
       let project = load () in
       match args with
       | Some (_ :: _) ->
         eprintf "Error: executables cannot take arguments yet\n%!";
         exit 1
       | None | Some [] -> exit_unless (Action.exec project name))
;;

let test_command =
  Command.basic
    ~summary:"Run the expect tests of the named libraries and tests, or all"
    ~readme:(fun () ->
      sprintf
        "The corrected version of each file whose tests fail is written to \
         %s/, mirroring its path, and the diff printed. stag-build promote \
         moves them over the sources."
        Action.corrections_dir)
    (let%map_open.Command names = anon (sequence ("NAME" %: string)) in
     fun () -> exit_unless (Action.test (load ()) names))
;;

let promote_command =
  Command.basic
    ~summary:"Move the corrections of failed expect tests over their sources"
    (let%map_open.Command () = return () in
     fun () -> exit_unless (Action.promote (load ())))
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
  Command.group
    ~summary:"Build stag projects"
    [ "build", build_command
    ; "exec", exec_command
    ; "test", test_command
    ; "promote", promote_command
    ; "fmt", fmt_command
    ]
  |> Command_unix.run
;;
