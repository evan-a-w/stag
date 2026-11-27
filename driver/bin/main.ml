open! Import

let write_stderr string =
  try
    Out_channel.output_string stderr string;
    Out_channel.flush stderr
  with
  | Sys_error _ -> ()
;;

(* Before writing to stderr, so the two keep their order. A failure is left to
   [exit_flushing]. *)
let flush_before_stderr () =
  try Out_channel.flush stdout with
  | Sys_error _ -> ()
;;

(* Exits with [code], or fails if stdout can't be written. *)
let exit_flushing code =
  match Out_channel.flush stdout with
  | () ->
    (* If a write to stderr failed earlier, [exit] would flush it again and
       raise. *)
    (match Out_channel.flush stderr with
     | () -> exit code
     | exception Sys_error _ -> Core_unix.exit_immediately code)
  | exception Sys_error message ->
    write_stderr (sprintf "Error: Writing to stdout failed: %s\n" message);
    (* [exit] would flush stdout again, raising. *)
    Core_unix.exit_immediately (Int.max code 1)
;;

let dedup paths =
  List.stable_dedup
    paths
    ~compare:(Comparable.lift String.compare ~f:Fs.canonical)
;;

let read_files paths =
  dedup paths
  |> List.map ~f:(fun path ->
    match In_channel.read_all path with
    | contents -> { Source_file.path; contents }
    | exception Sys_error message ->
      write_stderr (sprintf "Error: %s\n" message);
      exit_flushing 2)
;;

(* Files under the current directory, eg. the prelude's sources in a checkout,
   which are absolute, are named relative to it. *)
let relative_loc =
  let cwd = lazy (Sys_unix.getcwd () ^ "/") in
  fun (loc : Loc.t) ->
    match String.chop_prefix (Loc.file_name loc) ~prefix:(force cwd) with
    | Some file -> { loc with file = Loc.File.of_string file }
    | None -> loc
;;

let contents files path =
  match
    List.find_map files ~f:(fun (file : Source_file.t) ->
      Option.some_if (String.equal file.path path) file.contents)
  with
  | Some contents -> Some contents
  | None -> Option.try_with (fun () -> In_channel.read_all path)
;;

let report ?warning files diagnostic =
  flush_before_stderr ();
  write_stderr
    (Diagnostic.to_string_hum
       ~contents:(contents files)
       ?warning
       (Diagnostic.relocate diagnostic ~f:relative_loc))
;;

let report_all files diagnostics =
  flush_before_stderr ();
  write_stderr
    (Diagnostic.list_to_string_hum
       ~contents:(contents files)
       (List.map diagnostics ~f:(Diagnostic.relocate ~f:relative_loc)))
;;

let warn_stale files driver =
  List.iter (Inline_expansion.stale driver) ~f:(fun expansion ->
    report ~warning:true files (Inline_expansion.diagnostic expansion))
;;

(* The driver of the program, and the prelude it sees. *)
let check_with_prelude ?output config files =
  match Pipeline.check ?output config (Pipeline.parse_files files) with
  | Error diagnostic ->
    report [] diagnostic;
    exit_flushing 1
  | Ok (prelude, checked) ->
    (match Pipeline.Checked.to_result checked with
     | Ok driver -> driver, prelude
     | Error diagnostics ->
       report_all files diagnostics;
       exit_flushing 1)
;;

let check ?output config files = fst (check_with_prelude ?output config files)

let eval ?argv ~limits files driver =
  match Driver.eval ?argv ~limits driver with
  | Ok () -> ()
  | Error error ->
    (match Stag_eval.Runtime_error.exit_code error with
     | Some code -> exit_flushing code
     | None ->
       report files (Diagnostic.of_runtime_error error);
       exit_flushing 1)
;;

let files_param =
  Command.Param.(anon (non_empty_sequence_as_list ("FILE" %: string)))
;;

(* The sources of the prelude among [paths] are checked as the prelude of their
   checkout, then the others as a program that sees it. *)
let check_with_prelude_sources ~limits ~modes ~sources ~program =
  let checkout = snd (List.hd_exn sources) in
  List.iter sources ~f:(fun (path, other) ->
    if not
         (String.equal
            (Stdlib_checkout.root other)
            (Stdlib_checkout.root checkout))
    then (
      write_stderr
        (sprintf
           "Error: %s and %s are sources of the preludes of different \
            checkouts, which are not checked together\n"
           (fst (List.hd_exn sources))
           path);
      exit_flushing 1));
  let prelude =
    match Stdlib_checkout.prelude checkout with
    | Ok prelude -> prelude
    | Error diagnostic ->
      report [] diagnostic;
      exit_flushing 1
  in
  match Pipeline.Checked.to_result (Pipeline.check_prelude ~limits prelude) with
  | Error diagnostics ->
    report_all [] diagnostics;
    exit_flushing 1
  | Ok driver ->
    warn_stale [] driver;
    let signature = (Driver.checked driver).signature in
    List.iter sources ~f:(fun (path, _) ->
      match
        Stdlib_checkout.signature_of_source
          checkout
          signature
          (Fs.canonical path)
      with
      | Ok signature ->
        print_string (Signature.to_string_hum ~of_prelude:true ~modes signature)
      | Error diagnostic ->
        report [] diagnostic;
        exit_flushing 1);
    if not (List.is_empty program)
    then (
      let files = read_files program in
      let driver =
        match
          Pipeline.check_sources
            ~prelude:(Parsed prelude)
            ~limits
            (Pipeline.parse_files files)
          |> Pipeline.Checked.to_result
        with
        | Ok driver -> driver
        | Error diagnostics ->
          report_all files diagnostics;
          exit_flushing 1
      in
      warn_stale files driver;
      print_string
        (Signature.to_string_hum ~modes (Driver.checked driver).signature))
;;

let check_command =
  Command.basic
    ~summary:"Typecheck the program of the files and print its signature"
    ~readme:(fun () ->
      Pipeline_config.limits_readme ~phases:"compile time and evaluation"
      ^ sprintf
          "\n\n\
           Files of the prelude in a stag checkout (a directory with a %s \
           file), %s and %s/*.stag, are checked as the prelude, and their \
           signatures printed, then the other files as a program that sees it. \
           A program in a checkout sees the prelude of its sources, not the \
           embedded one."
          Stdlib_checkout.marker_file
          Stdlib_checkout.prelude_file
          Stdlib_checkout.core_dir)
    (let%map_open.Command paths = files_param
     and limits = Pipeline_config.limits_param ()
     and modes =
       flag
         "modes"
         no_arg
         ~doc:
           " print the modes of the signature in full: each mode written, and \
            the constraints between them"
     in
     fun () ->
       let sources, program =
         List.partition_map (dedup paths) ~f:(fun path ->
           match Stdlib_checkout.of_source (Fs.canonical path) with
           | Some checkout -> First (path, checkout)
           | None -> Second path)
       in
       (match sources with
        | _ :: _ -> check_with_prelude_sources ~limits ~modes ~sources ~program
        | [] ->
          let files = read_files paths in
          let driver = check (Pipeline_config.create ~limits paths) files in
          warn_stale files driver;
          print_string
            (Signature.to_string_hum ~modes (Driver.checked driver).signature));
       exit_flushing 0)
;;

let run_command =
  Command.basic
    ~summary:"Typecheck and evaluate the program of the files"
    ~readme:(fun () ->
      Pipeline_config.limits_readme ~phases:"compile time and evaluation")
    (let%map_open.Command paths = files_param
     and limits = Pipeline_config.limits_param ()
     and argv =
       flag
         "--"
         escape
         ~doc:"ARG... the program's arguments, as [argv ()] returns them"
     in
     fun () ->
       let files = read_files paths in
       let driver = check (Pipeline_config.create ~limits paths) files in
       warn_stale files driver;
       eval ?argv ~limits files driver;
       exit_flushing 0)
;;

let test_command =
  Command.basic
    ~summary:"Run the expect tests of the program of the files"
    ~readme:(fun () ->
      "Each failing FILE is corrected into FILE.corrected, or in place with \
       -promote, and the diff printed. With -print-corrected, the program is \
       one FILE, printed to stdout corrected (or as it is if it passes), \
       without writing any file, eg. for a dune rule diffing it with FILE. "
      ^ "Stale inline expansions ([include%inline] and [module%inline]) are \
         corrected likewise. "
      ^ Pipeline_config.limits_readme
          ~phases:"compile time, evaluation and each test")
    (let%map_open.Command paths = files_param
     and limits = Pipeline_config.limits_param ()
     and mode =
       choose_one
         ~if_nothing_chosen:(Default_to `Write_corrected)
         [ flag
             "promote"
             ~aliases:[ "--promote" ]
             (no_arg_some `Promote)
             ~doc:" correct the files in place"
         ; flag
             "print-corrected"
             (no_arg_some `Print_corrected)
             ~doc:" print the corrected FILE to stdout"
         ]
     in
     fun () ->
       let files = read_files paths in
       (match mode, files with
        | `Print_corrected, _ :: _ :: _ ->
          eprintf "Error: -print-corrected takes one FILE\n%!";
          exit 2
        | _ -> ());
       let driver = check (Pipeline_config.create ~limits paths) files in
       let outcome =
         Test_and_correct.run
           ~limits
           ~report:(report files)
           ~write:(fun { path; contents } ~corrected ->
             let corrected_path = path ^ ".corrected" in
             Fs.try_ ~path (fun () ->
               let remove_corrected () =
                 if Sys_unix.file_exists_exn corrected_path
                 then Sys_unix.remove corrected_path
               in
               match mode, corrected with
               | `Print_corrected, _ ->
                 print_string (Option.value corrected ~default:contents)
               | (`Promote | `Write_corrected), None -> remove_corrected ()
               | ((`Promote | `Write_corrected) as mode), Some corrected ->
                 Option.iter
                   (Text_diff.unified
                      ~old_name:path
                      ~new_name:corrected_path
                      ~old:contents
                      ~new_:corrected)
                   ~f:print_string;
                 (match mode with
                  | `Promote ->
                    Out_channel.write_all path ~data:corrected;
                    remove_corrected ()
                  | `Write_corrected ->
                    Out_channel.write_all corrected_path ~data:corrected)))
           ~files
           driver
       in
       Test_and_correct.Outcome.print_summary
         ~promoted:
           (match mode with
            | `Promote -> true
            | `Print_corrected | `Write_corrected -> false)
         outcome;
       if not
            (Test_and_correct.Outcome.passed
               ~mismatches_fail:
                 (match mode with
                  | `Write_corrected -> true
                  | `Promote | `Print_corrected -> false)
               outcome)
       then exit 1)
;;

let bench_command ~fake_clock =
  Command.basic
    ~summary:"Run the benchmarks of the program of the files"
    ~readme:(fun () ->
      "Runs the let%bench benchmarks whose names contain any -filter (all \
       without), or with -phases benchmarks the interpreter on the program: \
       parsing, typechecking and evaluating it. Output of the program is \
       discarded. Baselines are saved in -baseline-dir. Benchmarks are \
       unlimited; limits apply to compile time and evaluation.")
    (let%map_open.Command paths = files_param
     and limits = Pipeline_config.limits_param ()
     and options = Bench_command.Options.param ~fake_clock
     and filters =
       flag
         "filter"
         (listed string)
         ~doc:"SUBSTRING run only the benchmarks whose names contain it"
     and phases =
       flag "phases" no_arg ~doc:" benchmark the phases of the interpreter"
     and baseline_dir =
       flag
         "baseline-dir"
         (optional_with_default "_stag/bench" string)
         ~doc:"DIR where baselines are saved (default: _stag/bench)"
     in
     fun () ->
       let files = read_files paths in
       let driver, prelude =
         check_with_prelude
           ~output:ignore
           (Pipeline_config.create ~limits paths)
           files
       in
       warn_stale files driver;
       let benches =
         if phases
         then
           Phase_bench.benches
             ~prelude:(Pipeline_config.Loaded_prelude.toplevels prelude)
             (Phase_bench.parse_files files)
         else (
           eval ~limits files driver;
           match Bench_runner.benches driver with
           | Ok benches -> benches
           | Error diagnostic ->
             report files diagnostic;
             exit 1)
       in
       if not
            (Bench_command.run
               ~filters
               options
               ~baseline_dir
               ~report:(report files)
               benches)
       then exit 1)
;;

let fmt_command =
  Command.basic
    ~summary:"Format the files"
    ~readme:(fun () ->
      "With one FILE and no flag, prints it formatted to stdout. With \
       -inplace, rewrites each FILE that is not formatted. With -check, writes \
       nothing, lists each FILE that is not formatted on stdout, and fails if \
       there is one. Several FILEs need -inplace or -check.")
    (let%map_open.Command paths = files_param
     and mode =
       choose_one
         ~if_nothing_chosen:(Default_to `Print)
         [ flag
             "inplace"
             ~aliases:[ "--inplace" ]
             (no_arg_some (`Run Format.Mode.Inplace))
             ~doc:" rewrite the files that are not formatted"
         ; flag
             "check"
             ~aliases:[ "--check" ]
             (no_arg_some (`Run Format.Mode.Check))
             ~doc:" list the files that are not formatted, writing nothing"
         ]
     in
     fun () ->
       let files = read_files paths in
       match mode, files with
       | `Print, [ file ] ->
         (match Format.file file with
          | Ok formatted -> print_string formatted
          | Error diagnostic ->
            report files diagnostic;
            exit 1)
       | `Print, _ ->
         eprintf "Error: several FILEs need -inplace or -check\n%!";
         exit 2
       | `Run mode, files ->
         let { Format.Summary.unformatted; errors } = Format.run ~mode files in
         List.iter errors ~f:(report files);
         (match mode with
          | Inplace -> ()
          | Check ->
            List.iter unformatted ~f:(fun { file; formatted = _ } ->
              print_endline file.path));
         let unformatted_fails =
           match mode with
           | Check -> not (List.is_empty unformatted)
           | Inplace -> false
         in
         if unformatted_fails || not (List.is_empty errors) then exit 1)
;;

let dump_tokens_command =
  Command.basic
    ~summary:"Print the tokens of a file with their byte offsets"
    ~readme:(fun () ->
      "For tests of editor grammars against the lexer: one (kind start stop) \
       per token and comment.")
    (let%map_open.Command path = anon ("FILE" %: string) in
     fun () ->
       match read_files [ path ] with
       | [ file ] -> Dump_tokens.print file.contents
       | _ -> assert false)
;;

let lsp_command =
  Command.basic
    ~summary:"Serve the Language Server Protocol on stdin and stdout"
    ~readme:(fun () ->
      "Each check of a document is limited (by default to a nesting of 100000 \
       and 5s; `none` lifts a limit), so a compile-time splice cannot hang the \
       editor.")
    (let%map_open.Command limits =
       Pipeline_config.limits_param ~default:Stag_lsp.Server.default_limits ()
     in
     fun () ->
       Stag_lsp.Server.run ~limits (Stag_lsp.Transport.of_stdio ()) |> exit)
;;

let () =
  let argv, fake_clock =
    Bench_command.take_fake_clock (Array.to_list (Sys.get_argv ()))
  in
  Command.group
    ~summary:"The stag language"
    [ "check", check_command
    ; "run", run_command
    ; "test", test_command
    ; "bench", bench_command ~fake_clock
    ; "fmt", fmt_command
    ; "dump-tokens", dump_tokens_command
    ; "lsp", lsp_command
    ]
  |> Command_unix.run ~argv
;;
