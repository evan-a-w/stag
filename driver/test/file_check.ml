open! Import

let check_files ?(run = false) ?(filter_diagnostic = Fn.id) files =
  let files =
    List.map files ~f:(fun (path, contents) -> { Source_file.path; contents })
  in
  let contents path =
    List.find_map files ~f:(fun (file : Source_file.t) ->
      Option.some_if (String.equal file.path path) file.contents)
  in
  let print_diagnostic diagnostic =
    print_string (filter_diagnostic (Diagnostic.to_string_hum ~contents diagnostic))
  in
  match Driver.check_files files with
  | Error error -> print_diagnostic (Diagnostic.of_driver_error error)
  | Ok driver ->
    print_string (Signature.to_string_hum (Driver.checked driver).signature);
    if run
    then (
      let output = Buffer.create 64 in
      Treewalk.set_output (Driver.interpreter driver) (Buffer.add_string output);
      (match Driver.eval driver with
       | Ok () -> ()
       | Error error -> print_diagnostic (Diagnostic.of_runtime_error error));
      printf "--- output ---\n%s" (Buffer.contents output);
      let results = Expect_test_runner.run driver in
      List.iter results ~f:(fun result ->
        if not (Expect_test_runner.Test_result.passed result)
        then print_s [%sexp (result : Expect_test_runner.Test_result.t)]);
      List.iter files ~f:(fun { path; contents } ->
        Option.iter
          (Corrected_file.create
             ~path
             ~contents
             ~tests:results
             ~expansions:(Inline_expansion.stale driver))
          ~f:(printf "--- corrected ---\n%s")))
;;

let check ?run ?filter_diagnostic contents =
  check_files ?run ?filter_diagnostic [ "a.stag", contents ]
;;
