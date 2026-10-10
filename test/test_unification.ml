open! Core
open Stag

open Type

let int = TCon ("int", [])
let bool = TCon ("bool", [])
let string = TCon ("string", [])
let tvar typecheck ~rank = TVar (Typecheck.fresh_tvar typecheck ~rank)
let cvar typecheck ~rank = CVar (Typecheck.fresh_cvar typecheck ~rank)

let unify_ty typecheck ~ty1 ~ty2 =
  let result = Typecheck.unify_ty typecheck ~ty1 ~ty2 in
  print_s [%sexp (result : (Type.ty, Typecheck.Unification_error.t) Result.t)]
;;

let unify_context typecheck ~context1 ~context2 =
  let result = Typecheck.unify_context typecheck ~context1 ~context2 in
  print_s
    [%sexp (result : (Type.context, Typecheck.Unification_error.t) Result.t)]
;;

let print_ty ty = print_s [%sexp (ty : Type.ty)]
let print_context context = print_s [%sexp (context : Type.context)]

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
  [%expect {| (Ok (TCon list ((ATy (TCon int ()))))) |}]
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


let%expect_test "constructor arity mismatch" =
  let typecheck = Typecheck.create () in
  unify_ty
    typecheck
    ~ty1:(TCon ("pair", [ ATy int ]))
    ~ty2:(TCon ("pair", [ ATy int; ATy bool ]));
  [%expect
    {|
    (Error
     (Type_mismatch (TCon pair ((ATy (TCon int ()))))
      (TCon pair ((ATy (TCon int ())) (ATy (TCon bool ()))))
      (Args_length_mismatch 1 2)))
    |}]
;;

let%expect_test "constructor arg kind mismatch" =
  let typecheck = Typecheck.create () in
  unify_ty
    typecheck
    ~ty1:(TCon ("t", [ ATy int ]))
    ~ty2:(TCon ("t", [ AContext CEmpty ]));
  [%expect
    {|
    (Error
     (Type_mismatch (TCon t ((ATy (TCon int ())))) (TCon t ((AContext CEmpty)))
      (Mismatch_arg_kind (ATy (TCon int ())) (AContext CEmpty))))
    |}]
;;

let%expect_test "constructor with context args" =
  let typecheck = Typecheck.create () in
  let a = tvar typecheck ~rank:1 in
  let ty1 = TCon ("t", [ AContext (CExtend ("x", a, CEmpty)) ]) in
  let ty2 = TCon ("t", [ AContext (CExtend ("x", int, CEmpty)) ]) in
  unify_ty typecheck ~ty1 ~ty2;
  print_ty a;
  [%expect
    {|
    (Ok (TCon t ((AContext (CExtend x (TVar (Link (TCon int ()))) CEmpty)))))
    (TVar (Link (TCon int ())))
    |}]
;;

let%expect_test "arrows" =
  let typecheck = Typecheck.create () in
  let a = tvar typecheck ~rank:1 in
  let b = tvar typecheck ~rank:1 in
  unify_ty typecheck ~ty1:(TArrow (a, int)) ~ty2:(TArrow (bool, b));
  print_ty a;
  print_ty b;
  [%expect
    {|
    (Ok (TArrow (TCon bool ()) (TCon int ())))
    (TVar (Link (TCon bool ())))
    (TVar (Link (TCon int ())))
    |}]
;;

let%expect_test "tuples" =
  let typecheck = Typecheck.create () in
  let a = tvar typecheck ~rank:1 in
  unify_ty typecheck ~ty1:(TTuple [ a; int ]) ~ty2:(TTuple [ bool; int ]);
  print_ty a;
  [%expect
    {|
    (Ok (TTuple ((TCon bool ()) (TCon int ()))))
    (TVar (Link (TCon bool ())))
    |}]
;;

let%expect_test "tuple length mismatch" =
  let typecheck = Typecheck.create () in
  unify_ty typecheck ~ty1:(TTuple [ int; int ]) ~ty2:(TTuple [ int ]);
  [%expect
    {|
    (Error
     (Type_mismatch (TTuple ((TCon int ()) (TCon int ())))
      (TTuple ((TCon int ()))) (Args_length_mismatch 2 1)))
    |}]
;;

let%expect_test "structural mismatch" =
  let typecheck = Typecheck.create () in
  unify_ty typecheck ~ty1:(TArrow (int, int)) ~ty2:(TTuple [ int; int ]);
  unify_ty typecheck ~ty1:int ~ty2:(TArrow (int, int));
  [%expect
    {|
    (Error
     (Type_mismatch (TArrow (TCon int ()) (TCon int ()))
      (TTuple ((TCon int ()) (TCon int ()))) Structural_mismatch))
    (Error
     (Type_mismatch (TCon int ()) (TArrow (TCon int ()) (TCon int ()))
      Structural_mismatch))
    |}]
;;

let%expect_test "code types unify inner type and context" =
  let typecheck = Typecheck.create () in
  let a = tvar typecheck ~rank:1 in
  let r = cvar typecheck ~rank:1 in
  unify_ty
    typecheck
    ~ty1:(TCode { inner = a; context = r })
    ~ty2:(TCode { inner = int; context = CExtend ("x", bool, CEmpty) });
  print_ty a;
  print_context r;
  [%expect
    {|
    (Ok
     (TCode (inner (TCon int ())) (context (CExtend x (TCon bool ()) CEmpty))))
    (TVar (Link (TCon int ())))
    (CVar (CLink (CExtend x (TCon bool ()) CEmpty)))
    |}]
;;

let%expect_test "variable with itself" =
  let typecheck = Typecheck.create () in
  let a = tvar typecheck ~rank:1 in
  unify_ty typecheck ~ty1:a ~ty2:a;
  [%expect {| (Ok (TVar (Unbound 0 1))) |}]
;;

let%expect_test "variable binds on either side" =
  let typecheck = Typecheck.create () in
  let a = tvar typecheck ~rank:1 in
  let b = tvar typecheck ~rank:1 in
  unify_ty typecheck ~ty1:a ~ty2:int;
  unify_ty typecheck ~ty1:int ~ty2:b;
  print_ty a;
  print_ty b;
  [%expect
    {|
    (Ok (TCon int ()))
    (Ok (TCon int ()))
    (TVar (Link (TCon int ())))
    (TVar (Link (TCon int ())))
    |}]
;;

let%expect_test "link chains" =
  let typecheck = Typecheck.create () in
  let a = tvar typecheck ~rank:1 in
  let b = tvar typecheck ~rank:1 in
  unify_ty typecheck ~ty1:a ~ty2:b;
  unify_ty typecheck ~ty1:b ~ty2:int;
  print_ty a;
  print_ty (Type.canonical_ty a);
  unify_ty typecheck ~ty1:a ~ty2:bool;
  [%expect
    {|
    (Ok (TVar (Unbound 1 1)))
    (Ok (TCon int ()))
    (TVar (Link (TVar (Link (TCon int ())))))
    (TCon int ())
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "occurs check" =
  let typecheck = Typecheck.create () in
  let a = tvar typecheck ~rank:1 in
  unify_ty typecheck ~ty1:a ~ty2:(TArrow (a, int));
  unify_ty typecheck ~ty1:a ~ty2:(TTuple [ a; int ]);
  unify_ty typecheck ~ty1:a ~ty2:(TCon ("list", [ ATy a ]));
  [%expect
    {|
    (Error (Occurs_check_failed (TVar (Unbound 0 1))))
    (Error (Occurs_check_failed (TVar (Unbound 0 1))))
    (Error (Occurs_check_failed (TVar (Unbound 0 1))))
    |}]
;;

let%expect_test "occurs check through context" =
  let typecheck = Typecheck.create () in
  let a = tvar typecheck ~rank:1 in
  unify_ty
    typecheck
    ~ty1:a
    ~ty2:(TCode { inner = int; context = CExtend ("x", a, CEmpty) });
  [%expect {| (Error (Occurs_check_failed (TVar (Unbound 0 1)))) |}]
;;

let%expect_test "binding lowers ranks of variables in the bound type" =
  let typecheck = Typecheck.create () in
  let a = tvar typecheck ~rank:1 in
  let b = tvar typecheck ~rank:3 in
  let c = tvar typecheck ~rank:2 in
  let r = cvar typecheck ~rank:3 in
  let d = tvar typecheck ~rank:0 in
  let ty = TTuple [ TArrow (b, c); TCode { inner = d; context = r } ] in
  unify_ty typecheck ~ty1:a ~ty2:ty;
  print_ty ty;
  [%expect
    {|
    (Ok
     (TTuple
      ((TArrow (TVar (Unbound 1 1)) (TVar (Unbound 2 1)))
       (TCode (inner (TVar (Unbound 4 0))) (context (CVar (CUnbound 3 1)))))))
    (TTuple
     ((TArrow (TVar (Unbound 1 1)) (TVar (Unbound 2 1)))
      (TCode (inner (TVar (Unbound 4 0))) (context (CVar (CUnbound 3 1))))))
    |}]
;;

let%expect_test "binding variable to variable lowers rank" =
  let typecheck = Typecheck.create () in
  let a = tvar typecheck ~rank:1 in
  let b = tvar typecheck ~rank:3 in
  unify_ty typecheck ~ty1:a ~ty2:b;
  print_ty b;
  [%expect
    {|
    (Ok (TVar (Unbound 1 1)))
    (TVar (Unbound 1 1))
    |}]
;;

let%expect_test "bindings are not undone on failure" =
  let typecheck = Typecheck.create () in
  let a = tvar typecheck ~rank:1 in
  unify_ty typecheck ~ty1:(TArrow (a, int)) ~ty2:(TArrow (bool, string));
  print_ty a;
  [%expect
    {|
    (Error
     (Type_mismatch (TCon int ()) (TCon string ())
      (Base_types_not_equal int string)))
    (TVar (Link (TCon bool ())))
    |}]
;;

let%expect_test "empty contexts" =
  let typecheck = Typecheck.create () in
  unify_context typecheck ~context1:CEmpty ~context2:CEmpty;
  unify_context
    typecheck
    ~context1:CEmpty
    ~context2:(CExtend ("x", int, CEmpty));
  unify_context
    typecheck
    ~context1:(CExtend ("x", int, CEmpty))
    ~context2:CEmpty;
  [%expect
    {|
    (Ok CEmpty)
    (Error
     (Context_mismatch CEmpty (CExtend x (TCon int ()) CEmpty)
      (Empty_context_has_variable x)))
    (Error
     (Context_mismatch (CExtend x (TCon int ()) CEmpty) CEmpty
      (Empty_context_has_variable x)))
    |}]
;;

let%expect_test "context order does not matter" =
  let typecheck = Typecheck.create () in
  unify_context
    typecheck
    ~context1:(CExtend ("x", int, CExtend ("y", bool, CEmpty)))
    ~context2:(CExtend ("y", bool, CExtend ("x", int, CEmpty)));
  [%expect
    {| (Ok (CExtend x (TCon int ()) (CExtend y (TCon bool ()) CEmpty))) |}]
;;

let%expect_test "context label types are unified" =
  let typecheck = Typecheck.create () in
  let a = tvar typecheck ~rank:1 in
  unify_context
    typecheck
    ~context1:(CExtend ("x", a, CEmpty))
    ~context2:(CExtend ("x", int, CEmpty));
  print_ty a;
  unify_context
    typecheck
    ~context1:(CExtend ("x", int, CEmpty))
    ~context2:(CExtend ("x", bool, CEmpty));
  [%expect
    {|
    (Ok (CExtend x (TVar (Link (TCon int ()))) CEmpty))
    (TVar (Link (TCon int ())))
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "missing label in closed context" =
  let typecheck = Typecheck.create () in
  unify_context
    typecheck
    ~context1:(CExtend ("x", int, CEmpty))
    ~context2:(CExtend ("y", int, CEmpty));
  [%expect
    {|
    (Error
     (Context_mismatch (CExtend x (TCon int ()) CEmpty)
      (CExtend y (TCon int ()) CEmpty) (Empty_context_has_variable x)))
    |}]
;;

let%expect_test "open context against closed context" =
  let typecheck = Typecheck.create () in
  let r = cvar typecheck ~rank:1 in
  unify_context
    typecheck
    ~context1:(CExtend ("x", int, r))
    ~context2:(CExtend ("x", int, CExtend ("y", bool, CEmpty)));
  print_context r;
  [%expect
    {|
    (Ok
     (CExtend x (TCon int ()) (CVar (CLink (CExtend y (TCon bool ()) CEmpty)))))
    (CVar (CLink (CExtend y (TCon bool ()) CEmpty)))
    |}]
;;

let%expect_test "two open contexts share a fresh tail" =
  let typecheck = Typecheck.create () in
  let r1 = cvar typecheck ~rank:1 in
  let r2 = cvar typecheck ~rank:3 in
  unify_context
    typecheck
    ~context1:(CExtend ("x", int, r1))
    ~context2:(CExtend ("y", bool, r2));
  print_context r1;
  print_context r2;
  [%expect
    {|
    (Ok
     (CExtend x (TCon int ())
      (CVar (CLink (CExtend y (TCon bool ()) (CVar (CUnbound 3 1)))))))
    (CVar (CLink (CExtend y (TCon bool ()) (CVar (CUnbound 3 1)))))
    (CVar (CLink (CExtend x (TVar (Link (TCon int ()))) (CVar (CUnbound 3 1)))))
    |}]
;;

let%expect_test "context variables" =
  let typecheck = Typecheck.create () in
  let r = cvar typecheck ~rank:1 in
  unify_context typecheck ~context1:r ~context2:r;
  unify_context typecheck ~context1:r ~context2:(CExtend ("x", int, CEmpty));
  print_context r;
  [%expect
    {|
    (Ok (CVar (CUnbound 0 1)))
    (Ok (CExtend x (TCon int ()) CEmpty))
    (CVar (CLink (CExtend x (TCon int ()) CEmpty)))
    |}]
;;

let%expect_test "context occurs check" =
  let typecheck = Typecheck.create () in
  let r = cvar typecheck ~rank:1 in
  unify_context typecheck ~context1:r ~context2:(CExtend ("x", int, r));
  [%expect {| (Error (Occurs_check_failed (CVar (CUnbound 0 1)))) |}]
;;

let%expect_test "cyclic context" =
  let typecheck = Typecheck.create () in
  let r = cvar typecheck ~rank:1 in
  unify_context
    typecheck
    ~context1:(CExtend ("x", int, r))
    ~context2:(CExtend ("y", bool, r));
  [%expect
    {|
    (Error
     (Context_mismatch
      (CExtend x (TCon int ())
       (CVar (CLink (CExtend x (TVar (Unbound 1 1)) (CVar (CUnbound 2 1))))))
      (CExtend y (TCon bool ())
       (CVar (CLink (CExtend x (TVar (Unbound 1 1)) (CVar (CUnbound 2 1))))))
      Cyclic_context))
    |}]
;;

let%expect_test "duplicate labels are scoped" =
  let typecheck = Typecheck.create () in
  let context1 = CExtend ("x", int, CExtend ("x", bool, CEmpty)) in
  unify_context
    typecheck
    ~context1
    ~context2:(CExtend ("x", int, CExtend ("x", bool, CEmpty)));
  unify_context
    typecheck
    ~context1
    ~context2:(CExtend ("x", bool, CExtend ("x", int, CEmpty)));
  [%expect
    {|
    (Ok (CExtend x (TCon int ()) (CExtend x (TCon bool ()) CEmpty)))
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "linked context variable in tail position" =
  let typecheck = Typecheck.create () in
  let r = cvar typecheck ~rank:1 in
  unify_context typecheck ~context1:r ~context2:(CExtend ("z", int, CEmpty));
  unify_context
    typecheck
    ~context1:(CExtend ("x", int, CEmpty))
    ~context2:(CExtend ("y", bool, r));
  unify_context
    typecheck
    ~context1:(CExtend ("z", int, CExtend ("y", bool, CEmpty)))
    ~context2:(CExtend ("y", bool, r));
  [%expect
    {|
    (Ok (CExtend z (TCon int ()) CEmpty))
    (Error
     (Context_mismatch (CExtend x (TCon int ()) CEmpty)
      (CExtend y (TCon bool ()) (CVar (CLink (CExtend z (TCon int ()) CEmpty))))
      (Empty_context_has_variable x)))
    (Ok (CExtend z (TCon int ()) (CExtend y (TCon bool ()) CEmpty)))
    |}]
;;
