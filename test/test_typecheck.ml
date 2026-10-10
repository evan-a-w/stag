open! Core

let%expect_test "trivial unification" =
  let open Stag.Type in
  let ty1 = TCon ("int", []) in
  let ty2 = TCon ("int", []) in

  let result = unify ty1 ty2 in
  print_s [%sexp (result : unit)];
  [%expect {| () |}]
;;
