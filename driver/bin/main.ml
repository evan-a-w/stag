open! Import

let canonical path =
  Option.value
    (Option.try_with (fun () -> Filename_unix.realpath path))
    ~default:path
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

let report ?warning files diagnostic =
  let contents path =
    List.find_map files ~f:(fun (file : Source_file.t) ->
      Option.some_if (String.equal file.path path) file.contents)
  in
  Out_channel.flush stdout;
  eprintf "%s%!" (Diagnostic.to_string_hum ~contents ?warning diagnostic)
;;

let report_stale ?warning files expansions =
  List.iter expansions ~f:(fun expansion ->
    report ?warning files (Inline_expansion.diagnostic expansion))
;;

let check ?output ~limits files =
  match Driver.check_files ?output ~limits files with
  | Ok driver -> driver
  | Error error ->
    report files (Diagnostic.of_driver_error error);
    exit 1
;;

let eval ~limits files driver =
  match Driver.eval ~limits driver with
  | Ok () -> ()
  | Error error ->
    report files (Diagnostic.of_runtime_error error);
    exit 1
;;

let files_param =
  Command.Param.(anon (non_empty_sequence_as_list ("FILE" %: string)))
;;

let limits_param ?(default = Limits.unlimited) () =
  let limit name ~arg ~doc ~of_string ~to_string default =
    let%map_open.Command value =
      flag
        name
        (optional
           (Command.Arg_type.create (function
              | "none" -> None
              | string -> Some (of_string string))))
        ~doc:
          (sprintf
             "%s|none %s (default: %s)"
             arg
             doc
             (Option.value_map default ~default:"none" ~f:to_string))
    in
    Option.value value ~default
  in
  let%map_open.Command fuel =
    limit
      "fuel"
      ~arg:"N"
      ~doc:"evaluation steps allowed in each phase"
      ~of_string:Int.of_string
      ~to_string:Int.to_string
      default.fuel
  and max_depth =
    limit
      "max-depth"
      ~arg:"N"
      ~doc:"nesting of evaluation allowed"
      ~of_string:Int.of_string
      ~to_string:Int.to_string
      default.max_depth
  and time_limit =
    limit
      "time-limit"
      ~arg:"SPAN"
      ~doc:"time allowed for each phase, eg. 10s"
      ~of_string:Time_ns.Span.of_string
      ~to_string:Time_ns.Span.to_string
      default.time_limit
  in
  { Limits.fuel; max_depth; time_limit }
;;

let limits_readme ~phases =
  sprintf "Limits apply to each phase of evaluation on its own: %s." phases
;;

(* The signature of [path], a source of the prelude of [checkout]: of the
   prelude, but [Core], or of a module of [Core]. *)
let prelude_source_signature checkout (signature : Signature.t) path =
  if
    String.equal
      (canonical path)
      (Stdlib_checkout.root checkout ^/ Prelude.checkout_prelude)
  then
    List.filter signature ~f:(function
      | Module { name = "Core"; _ } -> false
      | Module _ | Value _ | Type _ | Exception _ -> true)
  else (
    let module_name = Source_file.module_name path |> Result.ok in
    List.filter_map signature ~f:(function
      | Module { name = "Core"; items } ->
        Some
          (Signature.Item.Module
             { name = "Core"
             ; items =
                 List.filter items ~f:(function
                   | Module { name; _ } ->
                     Option.equal String.equal (Some name) module_name
                   | Value _ | Type _ | Exception _ -> false)
             })
      | Module _ | Value _ | Type _ | Exception _ -> None))
;;

(* Sources of the prelude in a stag checkout are checked as the prelude, with
   their contents on disk. *)
let check_prelude_sources ~limits checkout paths =
  let cwd = Sys_unix.getcwd () ^ "/" in
  let relative (file : Source_file.t) =
    { file with
      path =
        Option.value
          (String.chop_prefix file.path ~prefix:cwd)
          ~default:file.path
    }
  in
  let fail ?(files = []) diagnostic =
    report files diagnostic;
    exit 1
  in
  match Stdlib_checkout.sources checkout with
  | Error diagnostic -> fail diagnostic
  | Ok sources ->
    let sources =
      { Prelude.Sources.prelude = relative sources.prelude
      ; core = List.map sources.core ~f:relative
      }
    in
    let fail = fail ~files:(sources.prelude :: sources.core) in
    (match Prelude.parse sources with
     | Error error -> fail (Diagnostic.of_source_file_error error)
     | Ok prelude ->
       (match Driver.check_prelude ~limits prelude with
        | Error error -> fail (Diagnostic.of_typecheck_error error)
        | Ok driver ->
          let signature = (Driver.checked driver).signature in
          List.iter paths ~f:(fun path ->
            print_string
              (Signature.to_string_hum
                 (prelude_source_signature checkout signature path)))))
;;

let check_command =
  Command.basic
    ~summary:"Typecheck the program of the files and print its signature"
    ~readme:(fun () ->
      limits_readme ~phases:"compile time and evaluation"
      ^ sprintf
          "\n\n\
           Files of the prelude in a stag checkout (a directory with a %s \
           file), %s and %s/*.stag, are checked as the prelude."
          Stdlib_checkout.marker_file
          Prelude.checkout_prelude
          Prelude.checkout_core_dir)
    (let%map_open.Command paths = files_param
     and limits = limits_param () in
     fun () ->
       let checkouts =
         List.map paths ~f:(fun path ->
           Stdlib_checkout.of_source (canonical path))
       in
       match List.filter_opt checkouts with
       | checkout :: _ ->
         if
           not
             (List.for_all checkouts ~f:(function
                | Some other ->
                  String.equal
                    (Stdlib_checkout.root other)
                    (Stdlib_checkout.root checkout)
                | None -> false))
         then (
           eprintf
             "Error: sources of the prelude are checked only with each other\n\
              %!";
           exit 2);
         check_prelude_sources ~limits checkout paths
       | [] ->
         let files = read_files paths in
         let driver = check ~limits files in
         report_stale ~warning:true files (Inline_expansion.stale driver);
         print_string
           (Signature.to_string_hum (Driver.checked driver).signature))
;;

let run_command =
  Command.basic
    ~summary:"Typecheck and evaluate the program of the files"
    ~readme:(fun () -> limits_readme ~phases:"compile time and evaluation")
    (let%map_open.Command paths = files_param
     and limits = limits_param () in
     fun () ->
       let files = read_files paths in
       let driver = check ~limits files in
       report_stale ~warning:true files (Inline_expansion.stale driver);
       eval ~limits files driver)
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
      ^ limits_readme ~phases:"compile time, evaluation and each test")
    (let%map_open.Command paths = files_param
     and limits = limits_param ()
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
       let driver = check ~limits files in
       let expansions = Inline_expansion.stale driver in
       report_stale files expansions;
       let evaluated =
         match Driver.eval ~limits driver with
         | Ok () -> true
         | Error error ->
           report files (Diagnostic.of_runtime_error error);
           false
       in
       let results =
         if evaluated then Expect_test_runner.run ~limits driver else []
       in
       List.iter results ~f:(fun result ->
         List.iter (Diagnostic.of_test_result result) ~f:(report files));
       let mismatched =
         List.filter_map files ~f:(fun { path; contents } ->
           let corrected_path = path ^ ".corrected" in
           let remove_corrected () =
             if Sys_unix.file_exists_exn corrected_path
             then Sys_unix.remove corrected_path
           in
           let corrected =
             Corrected_file.create ~path ~contents ~tests:results ~expansions
           in
           match mode with
           | `Print_corrected ->
             print_string (Option.value corrected ~default:contents);
             Option.map corrected ~f:(fun _ -> path)
           | (`Promote | `Write_corrected) as mode ->
             (match corrected with
              | None ->
                remove_corrected ();
                None
              | Some corrected ->
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
                   Out_channel.write_all corrected_path ~data:corrected);
                Some path))
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
       let stale = List.length expansions in
       Out_channel.flush stdout;
       if stale > 0
       then
         eprintf
           "%d inline expansion(s) out of date%s\n%!"
           stale
           (match mode with
            | `Promote -> " (promoted)"
            | `Print_corrected | `Write_corrected -> "");
       if failed > 0
       then
         eprintf
           "%d of %d test(s) failed%s\n%!"
           failed
           total
           (match mode with
            | `Promote when not (List.is_empty mismatched) ->
              " (expectations promoted)"
            | `Promote | `Print_corrected | `Write_corrected -> "");
       let failed_on_mismatch =
         match mode with
         | `Write_corrected -> true
         | `Promote | `Print_corrected -> false
       in
       if
         (not evaluated)
         || errors > 0
         || (failed_on_mismatch && (failed > 0 || stale > 0))
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
     and limits = limits_param ()
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
       let driver = check ~output:ignore ~limits files in
       let benches =
         if phases
         then Phase_bench.benches (Phase_bench.parse_files files)
         else (
           eval ~limits files driver;
           match Bench_runner.benches driver with
           | Ok benches -> benches
           | Error diagnostic ->
             report files diagnostic;
             exit 1)
       in
       if
         not
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
       limits_param ~default:Stag_lsp.Server.default_limits ()
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
