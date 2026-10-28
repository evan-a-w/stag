open! Import

let create ~path ~contents ~tests ~expansions =
  let edits =
    Expect_test_runner.edits ~path ~contents tests
    @ List.filter_map expansions ~f:(fun (expansion : Inline_expansion.t) ->
      Option.some_if
        (String.equal expansion.loc.file path)
        (Inline_expansion.edit expansion))
  in
  if List.is_empty edits then None else Some (Source_edit.apply contents edits)
;;
