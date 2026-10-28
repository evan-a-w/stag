open! Import

let create ~path ~contents ~tests ~expansions =
  let edits =
    Expect_test_runner.edits ~path ~contents tests
    @ List.filter_map expansions ~f:(fun (expansion : Expansion.t) ->
      if String.equal expansion.loc.file path
      then Inline_expansion.edit expansion ~contents
      else None)
  in
  if List.is_empty edits then None else Some (Source_edit.apply contents edits)
;;
