open! Core
open Stag

let%expect_test "trivial unification" =
  let open Type in
  let ty1 = TCon ("int", []) in
  let ty2 = TCon ("int", []) in
  let typecheck = Typecheck.create () in
  let result = Typecheck.unify_ty typecheck ~ty1 ~ty2 in
  print_s [%sexp (result : (Type.ty, Typecheck.Unification_error.t) Result.t)];
  [%expect {| (Ok (TCon int ())) |}]
;;

let%expect_test "trivial fail unification" =
  let open Type in
  let ty1 = TCon ("int", []) in
  let ty2 = TCon ("string", []) in
  let typecheck = Typecheck.create () in
  let result = Typecheck.unify_ty typecheck ~ty1 ~ty2 in
  print_s [%sexp (result : (Type.ty, Typecheck.Unification_error.t) Result.t)];
  [%expect
    {|
    (Error
     (Type_mismatch (TCon int ()) (TCon string ())
      (Base_types_not_equal int string)))
    |}]
;;

let%expect_test "list unification" =
  let open Type in
  let typecheck = Typecheck.create () in
  let ty1 = TCon ("list", [ ATy (TCon ("int", [])) ]) in
  let ty2 =
    TCon ("list", [ ATy (TVar (Typecheck.fresh_tvar typecheck ~rank:0)) ])
  in
  let result = Typecheck.unify_ty typecheck ~ty1 ~ty2 in
  print_s [%sexp (result : (Type.ty, Typecheck.Unification_error.t) Result.t)];
  [%expect
    {| (Ok (TCon list ((ATy (TCon int ()))))) |}]
;;

let%expect_test "list fail unification" =
  let open Type in
  let ty1 = TCon ("list", [ ATy (TCon ("int", [])) ]) in
  let ty2 = TCon ("list", [ ATy (TCon ("string", [])) ]) in
  let typecheck = Typecheck.create () in
  let result = Typecheck.unify_ty typecheck ~ty1 ~ty2 in
  print_s [%sexp (result : (Type.ty, Typecheck.Unification_error.t) Result.t)];
  [%expect
    {|
    (Error
     (Type_mismatch (TCon int ()) (TCon string ())
      (Base_types_not_equal int string)))
    |}]
;;
