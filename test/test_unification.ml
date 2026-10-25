open! Core
open Stag
open Type

let int = TCon ("int", [])
let bool = TCon ("bool", [])
let string = TCon ("string", [])
let con ?(args = []) name = TCon (name, args)
let code ?(inner = int) context = TCode { inner; context }
let tvar fresh ~rank = TVar (Fresh.var fresh ~rank)
let cvar fresh ~rank = CVar (Fresh.var fresh ~rank)
let pvar fresh ~rank = PVar (Fresh.var fresh ~rank)
let ext ?(presence = Present) name ty rest = CExtend (name, presence, ty, rest)

(** Present entries, in order, before [tail]. *)
let context ?(tail = CEmpty) entries =
  List.fold_right entries ~init:tail ~f:(fun (name, ty) rest ->
    ext name ty rest)
;;

let unify_ty fresh ~ty1 ~ty2 =
  let result = Unify.unify_ty fresh ~ty1 ~ty2 in
  print_s [%sexp (result : (unit, Unification_error.t) Result.t)]
;;

let unify_context fresh ~context1 ~context2 =
  let result = Unify.unify_context fresh ~context1 ~context2 in
  print_s [%sexp (result : (unit, Unification_error.t) Result.t)]
;;

let constrain fresh ~context ~bound =
  let result =
    Unify.constrain_context fresh ~context ~bound ~shadowed:String.Set.empty
  in
  print_s [%sexp (result : (unit, Unification_error.t) Result.t)]
;;

let print_ty ty = print_s [%sexp (ty : Type.ty)]
let print_context context = print_s [%sexp (context : Type.context)]

let%expect_test "trivial unification" =
  let fresh = Fresh.create () in
  unify_ty fresh ~ty1:int ~ty2:int;
  [%expect {| (Ok ()) |}]
;;

let%expect_test "trivial fail unification" =
  let fresh = Fresh.create () in
  unify_ty fresh ~ty1:int ~ty2:string;
  [%expect
    {|
    (Error
     (Type_mismatch (TCon int ()) (TCon string ())
      (Base_types_not_equal int string)))
    |}]
;;

let%expect_test "list unification" =
  let fresh = Fresh.create () in
  let list ty = con "list" ~args:[ ATy ty ] in
  let a = tvar fresh ~rank:0 in
  unify_ty fresh ~ty1:(list int) ~ty2:(list a);
  print_ty a;
  unify_ty fresh ~ty1:(list int) ~ty2:(list string);
  [%expect
    {|
    (Ok ())
    (TVar (Link (TCon int ())))
    (Error
     (Type_mismatch (TCon int ()) (TCon string ())
      (Base_types_not_equal int string)))
    |}]
;;

let%expect_test "constructor arity mismatch" =
  let fresh = Fresh.create () in
  unify_ty
    fresh
    ~ty1:(con "pair" ~args:[ ATy int ])
    ~ty2:(con "pair" ~args:[ ATy int; ATy bool ]);
  [%expect
    {|
    (Error
     (Type_mismatch (TCon pair ((ATy (TCon int ()))))
      (TCon pair ((ATy (TCon int ())) (ATy (TCon bool ()))))
      (Args_length_mismatch 1 2)))
    |}]
;;

let%expect_test "constructor arg kind mismatch" =
  let fresh = Fresh.create () in
  unify_ty
    fresh
    ~ty1:(con "t" ~args:[ ATy int ])
    ~ty2:(con "t" ~args:[ AContext CEmpty ]);
  [%expect
    {|
    (Error
     (Type_mismatch (TCon t ((ATy (TCon int ())))) (TCon t ((AContext CEmpty)))
      (Mismatch_arg_kind (ATy (TCon int ())) (AContext CEmpty))))
    |}]
;;

let%expect_test "constructor with context args" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  unify_ty
    fresh
    ~ty1:(con "t" ~args:[ AContext (context [ "x", a ]) ])
    ~ty2:(con "t" ~args:[ AContext (context [ "x", int ]) ]);
  print_ty a;
  [%expect
    {|
    (Ok ())
    (TVar (Link (TCon int ())))
    |}]
;;

let%expect_test "arrows" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  let b = tvar fresh ~rank:1 in
  unify_ty fresh ~ty1:(TArrow (a, int)) ~ty2:(TArrow (bool, b));
  print_ty a;
  print_ty b;
  [%expect
    {|
    (Ok ())
    (TVar (Link (TCon bool ())))
    (TVar (Link (TCon int ())))
    |}]
;;

let%expect_test "tuples" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  unify_ty fresh ~ty1:(TTuple [ a; int ]) ~ty2:(TTuple [ bool; int ]);
  print_ty a;
  [%expect
    {|
    (Ok ())
    (TVar (Link (TCon bool ())))
    |}]
;;

let%expect_test "tuple length mismatch" =
  let fresh = Fresh.create () in
  unify_ty fresh ~ty1:(TTuple [ int; int ]) ~ty2:(TTuple [ int ]);
  [%expect
    {|
    (Error
     (Type_mismatch (TTuple ((TCon int ()) (TCon int ())))
      (TTuple ((TCon int ()))) (Args_length_mismatch 2 1)))
    |}]
;;

let%expect_test "structural mismatch" =
  let fresh = Fresh.create () in
  unify_ty fresh ~ty1:(TArrow (int, int)) ~ty2:(TTuple [ int; int ]);
  unify_ty fresh ~ty1:int ~ty2:(TArrow (int, int));
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
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  let r = cvar fresh ~rank:1 in
  unify_ty fresh ~ty1:(code ~inner:a r) ~ty2:(code (context [ "x", bool ]));
  print_ty a;
  print_context r;
  [%expect
    {|
    (Ok ())
    (TVar (Link (TCon int ())))
    (CVar (Link (CExtend x Present (TCon bool ()) CEmpty)))
    |}]
;;

let%expect_test "variable with itself" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  unify_ty fresh ~ty1:a ~ty2:a;
  print_ty a;
  [%expect
    {|
    (Ok ())
    (TVar (Unbound 0 1))
    |}]
;;

let%expect_test "variable binds on either side" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  let b = tvar fresh ~rank:1 in
  unify_ty fresh ~ty1:a ~ty2:int;
  unify_ty fresh ~ty1:int ~ty2:b;
  print_ty a;
  print_ty b;
  [%expect
    {|
    (Ok ())
    (Ok ())
    (TVar (Link (TCon int ())))
    (TVar (Link (TCon int ())))
    |}]
;;

let%expect_test "link chains" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  let b = tvar fresh ~rank:1 in
  unify_ty fresh ~ty1:a ~ty2:b;
  unify_ty fresh ~ty1:b ~ty2:int;
  print_ty a;
  print_ty (Type.canonical_ty a);
  unify_ty fresh ~ty1:a ~ty2:bool;
  [%expect
    {|
    (Ok ())
    (Ok ())
    (TVar (Link (TVar (Link (TCon int ())))))
    (TCon int ())
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "occurs check" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  unify_ty fresh ~ty1:a ~ty2:(TArrow (a, int));
  unify_ty fresh ~ty1:a ~ty2:(TTuple [ a; int ]);
  unify_ty fresh ~ty1:a ~ty2:(con "list" ~args:[ ATy a ]);
  [%expect
    {|
    (Error (Occurs_check_failed (ATy (TVar (Unbound 0 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 0 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 0 1)))))
    |}]
;;

let%expect_test "occurs check through context" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  unify_ty fresh ~ty1:a ~ty2:(code (context [ "x", a ]));
  [%expect {| (Error (Occurs_check_failed (ATy (TVar (Unbound 0 1))))) |}]
;;

let%expect_test "binding lowers ranks of variables in the bound type" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  let b = tvar fresh ~rank:3 in
  let c = tvar fresh ~rank:2 in
  let r = cvar fresh ~rank:3 in
  let d = tvar fresh ~rank:0 in
  let ty = TTuple [ TArrow (b, c); code ~inner:d r ] in
  unify_ty fresh ~ty1:a ~ty2:ty;
  print_ty ty;
  [%expect
    {|
    (Ok ())
    (TTuple
     ((TArrow (TVar (Unbound 1 1)) (TVar (Unbound 2 1)))
      (TCode (inner (TVar (Unbound 4 0))) (context (CVar (Unbound 3 1))))))
    |}]
;;

let%expect_test "binding variable to variable lowers rank" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  let b = tvar fresh ~rank:3 in
  unify_ty fresh ~ty1:a ~ty2:b;
  print_ty b;
  [%expect
    {|
    (Ok ())
    (TVar (Unbound 1 1))
    |}]
;;

let%expect_test "bindings are not undone on failure" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  unify_ty fresh ~ty1:(TArrow (a, int)) ~ty2:(TArrow (bool, string));
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
  let fresh = Fresh.create () in
  unify_context fresh ~context1:CEmpty ~context2:CEmpty;
  unify_context fresh ~context1:CEmpty ~context2:(context [ "x", int ]);
  unify_context fresh ~context1:(context [ "x", int ]) ~context2:CEmpty;
  [%expect
    {|
    (Ok ())
    (Error
     (Context_mismatch CEmpty (CExtend x Present (TCon int ()) CEmpty)
      (Empty_context_has_variable x)))
    (Error
     (Context_mismatch (CExtend x Present (TCon int ()) CEmpty) CEmpty
      (Empty_context_has_variable x)))
    |}]
;;

let%expect_test "context order does not matter" =
  let fresh = Fresh.create () in
  unify_context
    fresh
    ~context1:(context [ "x", int; "y", bool ])
    ~context2:(context [ "y", bool; "x", int ]);
  [%expect {| (Ok ()) |}]
;;

let%expect_test "context label types are unified" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  unify_context
    fresh
    ~context1:(context [ "x", a ])
    ~context2:(context [ "x", int ]);
  print_ty a;
  unify_context
    fresh
    ~context1:(context [ "x", int ])
    ~context2:(context [ "x", bool ]);
  [%expect
    {|
    (Ok ())
    (TVar (Link (TCon int ())))
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "missing label in closed context" =
  let fresh = Fresh.create () in
  unify_context
    fresh
    ~context1:(context [ "x", int ])
    ~context2:(context [ "y", int ]);
  [%expect
    {|
    (Error
     (Context_mismatch (CExtend x Present (TCon int ()) CEmpty)
      (CExtend y Present (TCon int ()) CEmpty) (Empty_context_has_variable x)))
    |}]
;;

let%expect_test "open context against closed context" =
  let fresh = Fresh.create () in
  let r = cvar fresh ~rank:1 in
  unify_context
    fresh
    ~context1:(context [ "x", int ] ~tail:r)
    ~context2:(context [ "x", int; "y", bool ]);
  print_context r;
  [%expect
    {|
    (Ok ())
    (CVar (Link (CExtend y Present (TCon bool ()) CEmpty)))
    |}]
;;

let%expect_test "two open contexts share a fresh tail" =
  let fresh = Fresh.create () in
  let r1 = cvar fresh ~rank:1 in
  let r2 = cvar fresh ~rank:3 in
  unify_context
    fresh
    ~context1:(context [ "x", int ] ~tail:r1)
    ~context2:(context [ "y", bool ] ~tail:r2);
  print_context r1;
  print_context r2;
  [%expect
    {|
    (Ok ())
    (CVar (Link (CExtend y Present (TCon bool ()) (CVar (Unbound 4 1)))))
    (CVar
     (Link
      (CExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
       (CVar (Unbound 4 1)))))
    |}]
;;

let%expect_test "context variables" =
  let fresh = Fresh.create () in
  let r = cvar fresh ~rank:1 in
  unify_context fresh ~context1:r ~context2:r;
  unify_context fresh ~context1:r ~context2:(context [ "x", int ]);
  print_context r;
  [%expect
    {|
    (Ok ())
    (Ok ())
    (CVar (Link (CExtend x Present (TCon int ()) CEmpty)))
    |}]
;;

let%expect_test "context occurs check" =
  let fresh = Fresh.create () in
  let r = cvar fresh ~rank:1 in
  unify_context fresh ~context1:r ~context2:(context [ "x", int ] ~tail:r);
  [%expect {| (Error (Occurs_check_failed (AContext (CVar (Unbound 0 1))))) |}]
;;

let%expect_test "cyclic context" =
  let fresh = Fresh.create () in
  let r = cvar fresh ~rank:1 in
  unify_context
    fresh
    ~context1:(context [ "x", int ] ~tail:r)
    ~context2:(context [ "y", bool ] ~tail:r);
  [%expect
    {|
    (Error
     (Context_mismatch
      (CExtend x Present (TCon int ())
       (CVar
        (Link
         (CExtend x (PVar (Unbound 1 1)) (TVar (Unbound 2 1))
          (CVar (Unbound 3 1))))))
      (CExtend y Present (TCon bool ())
       (CVar
        (Link
         (CExtend x (PVar (Unbound 1 1)) (TVar (Unbound 2 1))
          (CVar (Unbound 3 1))))))
      Cyclic_context))
    |}]
;;

let%expect_test "duplicate labels are scoped" =
  let fresh = Fresh.create () in
  let context1 = context [ "x", int; "x", bool ] in
  unify_context fresh ~context1 ~context2:(context [ "x", int; "x", bool ]);
  unify_context fresh ~context1 ~context2:(context [ "x", bool; "x", int ]);
  [%expect
    {|
    (Ok ())
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "linked context variable in tail position" =
  let fresh = Fresh.create () in
  let r = cvar fresh ~rank:1 in
  unify_context fresh ~context1:r ~context2:(context [ "z", int ]);
  unify_context
    fresh
    ~context1:(context [ "x", int ])
    ~context2:(context [ "y", bool ] ~tail:r);
  unify_context
    fresh
    ~context1:(context [ "z", int; "y", bool ])
    ~context2:(context [ "y", bool ] ~tail:r);
  [%expect
    {|
    (Ok ())
    (Error
     (Context_mismatch (CExtend x Present (TCon int ()) CEmpty)
      (CExtend y Present (TCon bool ())
       (CVar (Link (CExtend z Present (TCon int ()) CEmpty))))
      (Empty_context_has_variable x)))
    (Ok ())
    |}]
;;

let%expect_test "presence" =
  let fresh = Fresh.create () in
  let maybe name ty rest = ext name ty rest ~presence:(pvar fresh ~rank:1) in
  let maybe_x = maybe "x" int CEmpty in
  unify_context fresh ~context1:maybe_x ~context2:CEmpty;
  print_context maybe_x;
  let maybe_x = maybe "x" int CEmpty in
  unify_context fresh ~context1:(context [ "x", int ]) ~context2:maybe_x;
  print_context maybe_x;
  let maybe_x = maybe "x" int (context [ "y", bool ]) in
  unify_context fresh ~context1:maybe_x ~context2:(context [ "y", bool ]);
  print_context maybe_x;
  unify_context
    fresh
    ~context1:(ext "x" int CEmpty ~presence:Absent)
    ~context2:(context [ "x", int ]);
  unify_context
    fresh
    ~context1:(ext "x" int CEmpty ~presence:Absent)
    ~context2:CEmpty;
  [%expect
    {|
    (Ok ())
    (CExtend x (PVar (Link Absent)) (TCon int ()) CEmpty)
    (Ok ())
    (CExtend x (PVar (Link Present)) (TCon int ()) CEmpty)
    (Ok ())
    (CExtend x (PVar (Link Absent)) (TCon int ())
     (CExtend y Present (TCon bool ()) CEmpty))
    (Error
     (Context_mismatch (CExtend x Absent (TCon int ()) CEmpty)
      (CExtend x Present (TCon int ()) CEmpty) (Presence_mismatch x)))
    (Ok ())
    |}]
;;

let%expect_test "absent entries need not agree on type" =
  let fresh = Fresh.create () in
  let r = cvar fresh ~rank:1 in
  unify_context
    fresh
    ~context1:(ext "x" int CEmpty ~presence:Absent)
    ~context2:r;
  print_context r;
  unify_context
    fresh
    ~context1:(ext "x" int CEmpty ~presence:Absent)
    ~context2:(context [ "y", bool ]);
  [%expect
    {|
    (Ok ())
    (CVar
     (Link
      (CExtend x (PVar (Link Absent)) (TVar (Link (TCon int ())))
       (CVar (Link CEmpty)))))
    (Error
     (Context_mismatch CEmpty (CExtend y Present (TCon bool ()) CEmpty)
      (Empty_context_has_variable y)))
    |}]
;;

let%expect_test "presence variables are generalized and their rank lowered" =
  let fresh = Fresh.create () in
  let inner = Fresh.var fresh ~rank:2 in
  let outer = pvar fresh ~rank:1 in
  let r = cvar fresh ~rank:1 in
  unify_context
    fresh
    ~context1:r
    ~context2:(ext "x" int CEmpty ~presence:(PVar inner));
  print_s [%sexp (inner : (presence, Nothing.t) Var.t)];
  let context =
    ext
      "y"
      int
      ~presence:outer
      (ext "z" int CEmpty ~presence:(pvar fresh ~rank:2))
  in
  print_context (Scheme.generalize_context ~rank:1 context);
  [%expect
    {|
    (Ok ())
    (Unbound 0 1)
    (CExtend y (PVar (Unbound 1 1)) (TCon int ())
     (CExtend z (PVar (Generic 3)) (TCon int ()) CEmpty))
    |}]
;;

let%expect_test "context bounds" =
  let fresh = Fresh.create () in
  let r = cvar fresh ~rank:1 in
  let bound = cvar fresh ~rank:1 in
  constrain fresh ~context:r ~bound;
  unify_context
    fresh
    ~context1:r
    ~context2:(context [ "x", int ] ~tail:(cvar fresh ~rank:1));
  print_context bound;
  (* An entry already in the bound is reused, rather than duplicated. *)
  unify_context
    fresh
    ~context1:r
    ~context2:(context [ "x", int; "y", bool ] ~tail:(cvar fresh ~rank:1));
  print_context bound;
  unify_context
    fresh
    ~context1:r
    ~context2:(context [ "y", int ] ~tail:(cvar fresh ~rank:1));
  [%expect
    {|
    (Ok ())
    (Ok ())
    (CVar
     (Link
      (CExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
       (CVar (Unbound 5 1)))))
    (Ok ())
    (CVar
     (Link
      (CExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
       (CVar
        (Link
         (CExtend y (PVar (Link Present)) (TVar (Link (TCon bool ())))
          (CVar (Unbound 9 1))))))))
    (Error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
    |}]
;;

let%expect_test "context bounds and presence" =
  let fresh = Fresh.create () in
  let constrain = constrain fresh in
  let maybe name ty rest = ext name ty rest ~presence:(pvar fresh ~rank:1) in
  (* Unused entries need not be in the bound. *)
  let maybe_x = maybe "x" int CEmpty in
  constrain ~context:maybe_x ~bound:CEmpty;
  print_context maybe_x;
  constrain ~context:(ext "x" int CEmpty ~presence:Absent) ~bound:CEmpty;
  constrain ~context:(context [ "x", int ]) ~bound:CEmpty;
  (* A used entry makes the bound's entry used. *)
  let bound = maybe "x" int CEmpty in
  constrain ~context:(context [ "x", int ]) ~bound;
  print_context bound;
  (* Shadowed entries are not constrained. *)
  constrain
    ~context:(context [ "x", int; "x", bool ])
    ~bound:(context [ "x", int ]);
  [%expect
    {|
    (Ok ())
    (CExtend x (PVar (Link Absent)) (TCon int ()) CEmpty)
    (Ok ())
    (Error
     (Context_mismatch (CExtend x Present (TCon int ()) CEmpty) CEmpty
      (Empty_context_has_variable x)))
    (Ok ())
    (CExtend x (PVar (Link Present)) (TCon int ()) CEmpty)
    (Ok ())
    |}]
;;

let%expect_test "bounds are kept by generalization and instantiation" =
  let fresh = Fresh.create () in
  let r = cvar fresh ~rank:2 in
  let bound = cvar fresh ~rank:2 in
  ignore
    (Unify.constrain_context fresh ~context:r ~bound ~shadowed:String.Set.empty
     : _ Result.t);
  let ty = TArrow (code r, code bound) in
  let print ty = print_endline (Type.to_string_hum ty) in
  print ty;
  let ty = Scheme.generalize_ty ~rank:1 ty in
  print ty;
  [%expect
    {|
    <int>{..'_a} -> <int>{..'_b} where ..'_a <= {..'_b}
    <int>{..'a} -> <int>{..'b} where ..'a <= {..'b}
    |}]
;;

let%expect_test "bounds lower ranks" =
  let fresh = Fresh.create () in
  let r = cvar fresh ~rank:2 in
  let bound = cvar fresh ~rank:2 in
  ignore
    (Unify.constrain_context fresh ~context:r ~bound ~shadowed:String.Set.empty
     : _ Result.t);
  unify_context fresh ~context1:r ~context2:(cvar fresh ~rank:1);
  print_context r;
  print_context bound;
  [%expect
    {|
    (Ok ())
    (CVar (Link (CVar (Unbound 2 1))))
    (CVar (Unbound 1 1))
    |}]
;;

let%expect_test "an absent entry shadows later entries with its name" =
  let fresh = Fresh.create () in
  let constrain = constrain fresh in
  let absent_then_x = ext "x" int (context [ "x", bool ]) ~presence:Absent in
  (* The shadowed [x] is unreachable, so needs no entry in the bound. *)
  constrain ~context:absent_then_x ~bound:CEmpty;
  let maybe_then_x =
    ext "x" int (context [ "x", bool ]) ~presence:(pvar fresh ~rank:1)
  in
  constrain ~context:maybe_then_x ~bound:CEmpty;
  print_context maybe_then_x;
  (* Unification never finds the shadowed [x] in place of the absent one. *)
  unify_context
    fresh
    ~context1:(context [ "x", bool ] ~tail:(cvar fresh ~rank:1))
    ~context2:absent_then_x;
  let r = cvar fresh ~rank:1 in
  constrain
    ~context:r
    ~bound:
      (ext
         "x"
         int
         (context [ "x", bool ] ~tail:(cvar fresh ~rank:1))
         ~presence:Absent);
  unify_context fresh ~context1:r ~context2:(context [ "x", bool ]);
  (* Shadowed entries still take part in unification, which is stricter than
     needed. *)
  unify_context fresh ~context1:CEmpty ~context2:absent_then_x;
  [%expect
    {|
    (Ok ())
    (Ok ())
    (CExtend x (PVar (Link Absent)) (TCon int ())
     (CExtend x Present (TCon bool ()) CEmpty))
    (Error
     (Context_mismatch (CExtend x Present (TCon bool ()) (CVar (Unbound 1 1)))
      (CExtend x Absent (TCon int ()) (CExtend x Present (TCon bool ()) CEmpty))
      (Presence_mismatch x)))
    (Ok ())
    (Error
     (Context_mismatch (CExtend x Present (TCon bool ()) CEmpty)
      (CExtend x Absent (TCon int ())
       (CExtend x Present (TCon bool ()) (CVar (Unbound 3 1))))
      (Presence_mismatch x)))
    (Error
     (Context_mismatch CEmpty (CExtend x Present (TCon bool ()) CEmpty)
      (Empty_context_has_variable x)))
    |}]
;;

let%expect_test "cyclic bounds" =
  let fresh = Fresh.create () in
  let constrain = constrain fresh in
  let print contexts =
    TTuple (List.map contexts ~f:code) |> Type.to_string_hum |> print_endline
  in
  let cycle () =
    let a = cvar fresh ~rank:1 in
    let b = cvar fresh ~rank:1 in
    let c = cvar fresh ~rank:1 in
    constrain ~context:a ~bound:b;
    constrain ~context:b ~bound:c;
    constrain ~context:c ~bound:a;
    a, b, c
  in
  let a, b, c = cycle () in
  print [ a; b; c ];
  (* An entry of any member reaches every member. *)
  unify_context
    fresh
    ~context1:a
    ~context2:(context [ "x", int ] ~tail:(cvar fresh ~rank:1));
  print [ a; b; c ];
  unify_context
    fresh
    ~context1:c
    ~context2:(context [ "y", bool ] ~tail:(cvar fresh ~rank:1));
  print [ a; b; c ];
  unify_context
    fresh
    ~context1:b
    ~context2:(context [ "x", bool ] ~tail:(cvar fresh ~rank:1));
  (* Once one member is closed, no member can get entries. *)
  let a, b, c = cycle () in
  unify_context fresh ~context1:c ~context2:CEmpty;
  print [ a; b; c ];
  unify_context
    fresh
    ~context1:a
    ~context2:(context [ "x", int ] ~tail:(cvar fresh ~rank:1));
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Ok ())
    <int>{..'_a} * <int>{..'_b} * <int>{..'_c} where ..'_a <= {..'_b}, ..'_b <= {..'_c}, ..'_c <= {..'_a}
    (Ok ())
    <int>{x : int; ..'_a} * <int>{x : int; ..'_b} * <int>{x : int; ..'_c} where ..'_a <= {x : int; ..'_b}, ..'_b <= {x : int; ..'_c}, ..'_c <= {x : int; ..'_a}
    (Ok ())
    <int>{x : int; y : bool; ..'_a} * <int>{x : int; y : bool; ..'_b} * <int>{x : int; y : bool; ..'_c} where ..'_a <= {x : int; y : bool; ..'_b}, ..'_b <= {x : int; y : bool; ..'_c}, ..'_c <= {x : int; y : bool; ..'_a}
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Ok ())
    (Ok ())
    (Ok ())
    (Ok ())
    <int>{..'_a} * <int>{..'_b} * <int>{} where ..'_a <= {..'_b}, ..'_b <= {}
    (Error
     (Context_mismatch (CExtend x Present (TCon int ()) (CVar (Unbound 24 1)))
      (CVar
       (Link
        (CExtend x (PVar (Link Absent)) (TVar (Unbound 26 1))
         (CVar (Unbound 27 1)))))
      (Presence_mismatch x)))
    |}]
;;

let%expect_test "bounds referring to the bounded variable" =
  let fresh = Fresh.create () in
  let print context = print_endline (Type.to_string_hum (code context)) in
  let s = cvar fresh ~rank:1 in
  (* A bound that is the variable itself constrains nothing. *)
  constrain fresh ~context:s ~bound:s;
  print s;
  constrain fresh ~context:s ~bound:(context [ "x", int ] ~tail:s);
  print s;
  unify_context
    fresh
    ~context1:s
    ~context2:(context [ "y", bool ] ~tail:(cvar fresh ~rank:1));
  print s;
  unify_context fresh ~context1:s ~context2:(context [ "x", int; "y", bool ]);
  print s;
  [%expect
    {|
    (Ok ())
    <int>{..'_a}
    (Ok ())
    <int>{..'_a} where ..'_a <= {x : int; ..'_a}
    (Ok ())
    <int>{y : bool; ..'_a} where ..'_a <= {x : int; y : bool; ..'_a}
    (Ok ())
    <int>{x : int; y : bool}
    |}]
;;

let%expect_test "bounds do not constrain shadowed entries" =
  let fresh = Fresh.create () in
  let r = cvar fresh ~rank:1 in
  let bound = context [ "x", int ] ~tail:(cvar fresh ~rank:1) in
  constrain fresh ~context:(context [ "x", int ] ~tail:r) ~bound;
  (* [r]'s [x] is hidden by the [x : int] before it. *)
  unify_context
    fresh
    ~context1:r
    ~context2:(context [ "x", bool; "y", bool ] ~tail:(cvar fresh ~rank:1));
  print_context bound;
  (* Shadowing is kept as bounds move to the rest of [r]. *)
  unify_context
    fresh
    ~context1:r
    ~context2:(context [ "x", bool; "y", bool; "x", string; "y", string ]);
  print_context bound;
  [%expect
    {|
    (Ok ())
    (Ok ())
    (CExtend x Present (TCon int ())
     (CVar
      (Link
       (CExtend y (PVar (Link Present)) (TVar (Link (TCon bool ())))
        (CVar (Unbound 5 1))))))
    (Ok ())
    (CExtend x Present (TCon int ())
     (CVar
      (Link
       (CExtend y (PVar (Link Present)) (TVar (Link (TCon bool ())))
        (CVar (Unbound 5 1))))))
    |}]
;;
