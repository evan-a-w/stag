open! Import

(** [contents] of the file [path] with the mismatches of [tests] (see
    [Expect_test_runner.edits]) and the stale [expansions] in it corrected, if
    any. *)
val create
  :  path:string
  -> contents:string
  -> tests:Expect_test_runner.Test_result.t list
  -> expansions:Inline_expansion.t list
  -> string option
