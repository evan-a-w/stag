open! Import

let ptr ty = Type.TCon ("ptr", [ ATy ty ])
let var fresh = Type.TVar (Fresh.var fresh ~rank:1)

let bind fresh f =
  let knot = var fresh in
  Unify.unify_ty fresh ~ty1:knot ~ty2:(f knot)
;;

let show = function
  | Ok () -> print_endline "finite layout"
  | Error (Unification_error.Occurs_check_failed _) ->
    print_endline "infinite layout"
  | Error error -> print_s [%sexp (error : Unification_error.t)]
;;

let%expect_test "structural occurs check visits every by-value path" =
  let fresh = Fresh.conservative () in
  let field ty = Type.RExtend ("next", Present, Absent, ty, REmpty) in
  show (bind fresh (fun t -> Fresh.record fresh (field t)));
  show (bind fresh (fun t -> Fresh.variant fresh (field t)));
  show (bind fresh (fun t -> Fresh.record fresh (field (ptr t))));
  show (bind fresh (fun t -> Fresh.variant fresh (field (ptr t))));
  show
    (bind fresh (fun t ->
       let shared = Fresh.record fresh (field t) in
       TTuple [ ptr shared; shared ]));
  let row = Type.RVar (Fresh.var fresh ~rank:1) in
  show (Unify.unify_row fresh ~row1:row ~row2:(field (Fresh.record fresh row)));
  let row = Type.RVar (Fresh.var fresh ~rank:1) in
  show
    (Unify.unify_row
       fresh
       ~row1:row
       ~row2:(field (ptr (Fresh.record fresh row))));
  [%expect
    {|
    infinite layout
    infinite layout
    finite layout
    finite layout
    infinite layout
    infinite layout
    finite layout
    |}]
;;

let%expect_test "cycles consisting only of pointers can print and unify" =
  let fresh = Fresh.conservative () in
  let a = var fresh in
  let b = var fresh in
  show (Unify.unify_ty fresh ~ty1:a ~ty2:(ptr (ptr a)));
  show (Unify.unify_ty fresh ~ty1:b ~ty2:(ptr (ptr b)));
  print_endline (Type.to_string_hum a);
  print_s [%sexp (a : Type.ty)];
  let shifted = ptr b in
  print_s [%sexp (Type.same_ty a shifted : bool)];
  show (Unify.unify_ty fresh ~ty1:a ~ty2:shifted);
  [%expect
    {|
    finite layout
    finite layout
    (ptr[ptr['a]] as 'a)
    (TVar
     (Cycle 0 (Link (TCon ptr ((ATy (TCon ptr ((ATy (TVar (Cycle_ref 0)))))))))))
    true
    finite layout
    |}]
;;
