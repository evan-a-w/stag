open! Import
open Measure

module Options = struct
  type t =
    { config : Config.t
    ; format : Report.Format.t
    ; alloc : bool
    ; baseline : string option
    ; save_baseline : string option
    }

  let baseline_name =
    Command.Arg_type.create (fun name ->
      Baseline.validate_name name |> Or_error.ok_exn;
      name)
  ;;

  let param =
    let%map_open.Command quick =
      flag "quick" no_arg ~doc:" shorter warm-up and measurement, fewer samples"
    and samples =
      flag "samples" (optional int) ~doc:"N samples to take (default: 100)"
    and warm_up =
      flag
        "warm-up"
        (optional Time_ns.Span.arg_type)
        ~doc:"SPAN time to run before measuring (default: 1s)"
    and measurement_time =
      flag
        "time"
        (optional Time_ns.Span.arg_type)
        ~doc:"SPAN time the samples of each bench should take (default: 3s)"
    and noise_threshold =
      flag
        "noise-threshold"
        (optional float)
        ~doc:"PERCENT changes smaller than this are noise (default: 2)"
    and no_alloc =
      flag "no-alloc" no_arg ~doc:" do not measure allocation and collections"
    and format =
      choose_one
        ~if_nothing_chosen:(Default_to Report.Format.Human)
        [ flag
            "format"
            (optional
               (Arg_type.of_alist_exn
                  (List.map Report.Format.all ~f:(fun format ->
                     Report.Format.to_string format, format))))
            ~doc:"FORMAT human (default), sexp or json"
        ; flag "json" (no_arg_some Report.Format.Json) ~doc:" -format json"
        ; flag "sexp" (no_arg_some Report.Format.Sexp) ~doc:" -format sexp"
        ]
    and baseline =
      flag
        "baseline"
        (optional baseline_name)
        ~doc:"NAME compare with the baseline saved as NAME"
    and save_baseline =
      flag
        "save-baseline"
        (optional baseline_name)
        ~doc:"NAME save the results as the baseline NAME"
    in
    let base = if quick then Config.quick else Config.default in
    let samples = Option.value samples ~default:base.samples in
    let config =
      { base with
        samples
      ; min_samples = Int.min samples base.min_samples
      ; warm_up = Option.value warm_up ~default:base.warm_up
      ; measurement_time =
          Option.value measurement_time ~default:base.measurement_time
      ; noise_threshold =
          Option.value_map
            noise_threshold
            ~default:base.noise_threshold
            ~f:(fun percent -> percent /. 100.)
      }
    in
    { config; format; alloc = not no_alloc; baseline; save_baseline }
  ;;
end

let clock () =
  match Sys.getenv "STAG_BENCH_FAKE_CLOCK" with
  | None -> Clock.monotonic ()
  | Some tick ->
    Clock.Fake.clock (Clock.Fake.create ~tick:(Time_ns.Span.of_string tick) ())
;;

let error_diagnostic error =
  Diagnostic.create ~loc:Loc.none (Error.to_string_hum error)
;;

let run
      ?filters
      { Options.config; format; alloc; baseline; save_baseline }
      clock
      ~baseline_dir
      ~report
      benches
  =
  let load name = Baseline.load ~path:(Baseline.path ~dir:baseline_dir ~name) in
  let baseline =
    match baseline with
    | None -> Ok None
    | Some name -> load name |> Or_error.map ~f:Option.some
  in
  match baseline with
  | Error error ->
    report (error_diagnostic error);
    false
  | Ok baseline ->
    let on_outcome ({ name; entry } : Suite.Outcome.t) =
      match entry with
      | Ok entry ->
        (match format with
         | Human -> print_string (Report.human config entry)
         | Sexp | Json -> ())
      | Error error ->
        report
          (let diagnostic = Diagnostic.of_runtime_error error in
           { diagnostic with
             message = sprintf "Benchmark %s failed: %s" name diagnostic.message
           })
    in
    let outcomes =
      Suite.run
        ?alloc:(Option.some_if alloc Alloc.read)
        ?baseline
        ?filters
        ~on_outcome
        config
        clock
        benches
    in
    let entries =
      List.filter_map outcomes ~f:(fun outcome -> Result.ok outcome.entry)
    in
    (match format with
     | Human -> ()
     | Sexp | Json -> print_string (Report.machine format entries));
    Out_channel.flush stdout;
    let saved =
      match save_baseline with
      | None -> true
      | Some name ->
        let path = Baseline.path ~dir:baseline_dir ~name in
        let saved =
          let%bind.Or_error previous =
            if Stdlib.Sys.file_exists path then load name else Ok Baseline.empty
          in
          Baseline.save
            (Baseline.update
               previous
               (List.map entries ~f:(fun entry -> entry.result)))
            ~path
        in
        (match saved with
         | Ok () -> true
         | Error error ->
           report (error_diagnostic error);
           false)
    in
    saved
    && List.for_all outcomes ~f:(fun outcome -> Result.is_ok outcome.entry)
;;
