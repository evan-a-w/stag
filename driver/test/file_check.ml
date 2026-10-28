open! Import

let check ?(run = false) contents =
  let file = { Source_file.path = "a.stag"; contents } in
  let print_diagnostic diagnostic =
    print_string
      (Diagnostic.to_string_hum ~contents:(fun _ -> Some contents) diagnostic)
  in
  match Driver.check_files [ file ] with
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
      Option.iter
        (Expect_test_runner.corrected ~path:file.path ~contents results)
        ~f:(printf "--- corrected ---\n%s"))
;;
