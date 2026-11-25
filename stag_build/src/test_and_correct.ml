open! Import

module Outcome = struct
  type t =
    { evaluated : bool
    ; tests : int
    ; failed : int
    ; errors : int
    ; stale : int
    ; corrected : int
    ; write_failed : bool
    }

  let print_summary ?target ?(promoted = false) t =
    Out_channel.flush stdout;
    let prefix =
      Option.value_map target ~default:"" ~f:(fun name -> name ^ ": ")
    in
    if t.stale > 0
    then
      eprintf
        "%s%d inline expansion(s) out of date%s\n%!"
        prefix
        t.stale
        (if promoted then " (promoted)" else "");
    if t.failed > 0
    then
      eprintf
        "%s%d of %d test(s) failed%s\n%!"
        prefix
        t.failed
        t.tests
        (if promoted && t.corrected > 0 then " (expectations promoted)" else "")
  ;;

  let passed ~mismatches_fail t =
    t.evaluated
    && t.errors = 0
    && (not t.write_failed)
    && ((not mismatches_fail) || (t.failed = 0 && t.stale = 0))
  ;;
end

let run ~limits ~report ~write ~files driver =
  let paths =
    List.map files ~f:(fun (file : Source_file.t) -> file.path)
    |> String.Set.of_list
  in
  let expansions =
    List.filter (Inline_expansion.stale driver) ~f:(fun expansion ->
      Set.mem paths (Loc.file_name expansion.loc))
  in
  List.iter expansions ~f:(fun expansion ->
    report (Inline_expansion.diagnostic expansion));
  let evaluated =
    match Driver.eval ~limits driver with
    | Ok () -> true
    | Error error ->
      report (Diagnostic.of_runtime_error error);
      false
  in
  let results =
    if evaluated then Expect_test_runner.run ~limits ~files:paths driver else []
  in
  List.iter results ~f:(fun result ->
    List.iter (Diagnostic.of_test_result result) ~f:report);
  let written =
    List.map files ~f:(fun ({ path; contents } as file) ->
      let corrected =
        Corrected_file.create ~path ~contents ~tests:results ~expansions
      in
      match write file ~corrected with
      | Ok () -> Ok (Option.is_some corrected)
      | Error diagnostic ->
        report diagnostic;
        Error ())
  in
  Out_channel.flush stdout;
  { Outcome.evaluated
  ; tests = List.length results
  ; failed =
      List.count results ~f:(Fn.non Expect_test_runner.Test_result.passed)
  ; errors =
      List.count results ~f:(fun result ->
        Option.is_some result.error
        || not (List.is_empty result.different_outputs))
  ; stale = List.length expansions
  ; corrected =
      List.count written ~f:(fun written ->
        Result.equal Bool.equal Unit.equal written (Ok true))
  ; write_failed = List.exists written ~f:Result.is_error
  }
;;
