open! Core
open Stag
open Type

let int = TCon ("int", [])
let bool = TCon ("bool", [])
let string = TCon ("string", [])
let con ?(args = []) name = TCon (name, args)
let code ?(inner = int) context = TCode { inner; context }
let tvar fresh ~rank = TVar (Fresh.var fresh ~rank)
let rvar fresh ~rank = RVar (Fresh.var fresh ~rank)
let pvar fresh ~rank = PVar (Fresh.var fresh ~rank)
let ext ?(presence = Present) name ty rest = RExtend (name, presence, ty, rest)

(** Present entries, in order, before [tail]. *)
let row ?(tail = REmpty) entries =
  List.fold_right entries ~init:tail ~f:(fun (name, ty) rest ->
    ext name ty rest)
;;

let unify_ty fresh ~ty1 ~ty2 =
  let result = Unify.unify_ty fresh ~ty1 ~ty2 in
  print_s [%sexp (result : (unit, Unification_error.t) Result.t)]
;;

let unify_row fresh ~row1 ~row2 =
  let result = Unify.unify_row fresh ~row1 ~row2 in
  print_s [%sexp (result : (unit, Unification_error.t) Result.t)]
;;

let constrain fresh ~row ~bound =
  let result =
    Unify.constrain_row fresh ~row ~bound ~shadowed:String.Set.empty
  in
  print_s [%sexp (result : (unit, Unification_error.t) Result.t)]
;;

let print_ty ty = print_s [%sexp (ty : Type.ty)]
let print_row row = print_s [%sexp (row : Type.row)]

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
    ~ty2:(con "t" ~args:[ ARow REmpty ]);
  [%expect
    {|
    (Error
     (Type_mismatch (TCon t ((ATy (TCon int ())))) (TCon t ((ARow REmpty)))
      (Mismatch_arg_kind (ATy (TCon int ())) (ARow REmpty))))
    |}]
;;

let%expect_test "constructor with context args" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  unify_ty
    fresh
    ~ty1:(con "t" ~args:[ ARow (row [ "x", a ]) ])
    ~ty2:(con "t" ~args:[ ARow (row [ "x", int ]) ]);
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
  let r = rvar fresh ~rank:1 in
  unify_ty fresh ~ty1:(code ~inner:a r) ~ty2:(code (row [ "x", bool ]));
  print_ty a;
  print_row r;
  [%expect
    {|
    (Ok ())
    (TVar (Link (TCon int ())))
    (RVar (Link (RExtend x Present (TCon bool ()) REmpty)))
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
  unify_ty fresh ~ty1:a ~ty2:(code (row [ "x", a ]));
  [%expect {| (Error (Occurs_check_failed (ATy (TVar (Unbound 0 1))))) |}]
;;

let%expect_test "binding lowers ranks of variables in the bound type" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  let b = tvar fresh ~rank:3 in
  let c = tvar fresh ~rank:2 in
  let r = rvar fresh ~rank:3 in
  let d = tvar fresh ~rank:0 in
  let ty = TTuple [ TArrow (b, c); code ~inner:d r ] in
  unify_ty fresh ~ty1:a ~ty2:ty;
  print_ty ty;
  [%expect
    {|
    (Ok ())
    (TTuple
     ((TArrow (TVar (Unbound 1 1)) (TVar (Unbound 2 1)))
      (TCode (inner (TVar (Unbound 4 0))) (context (RVar (Unbound 3 1))))))
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
  unify_row fresh ~row1:REmpty ~row2:REmpty;
  unify_row fresh ~row1:REmpty ~row2:(row [ "x", int ]);
  unify_row fresh ~row1:(row [ "x", int ]) ~row2:REmpty;
  [%expect
    {|
    (Ok ())
    (Error
     (Row_mismatch REmpty (RExtend x Present (TCon int ()) REmpty)
      (Missing_label x)))
    (Error
     (Row_mismatch (RExtend x Present (TCon int ()) REmpty) REmpty
      (Missing_label x)))
    |}]
;;

let%expect_test "context order does not matter" =
  let fresh = Fresh.create () in
  unify_row
    fresh
    ~row1:(row [ "x", int; "y", bool ])
    ~row2:(row [ "y", bool; "x", int ]);
  [%expect {| (Ok ()) |}]
;;

let%expect_test "context label types are unified" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  unify_row fresh ~row1:(row [ "x", a ]) ~row2:(row [ "x", int ]);
  print_ty a;
  unify_row fresh ~row1:(row [ "x", int ]) ~row2:(row [ "x", bool ]);
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
  unify_row fresh ~row1:(row [ "x", int ]) ~row2:(row [ "y", int ]);
  [%expect
    {|
    (Error
     (Row_mismatch (RExtend x Present (TCon int ()) REmpty)
      (RExtend y Present (TCon int ()) REmpty) (Missing_label x)))
    |}]
;;

let%expect_test "open context against closed context" =
  let fresh = Fresh.create () in
  let r = rvar fresh ~rank:1 in
  unify_row
    fresh
    ~row1:(row [ "x", int ] ~tail:r)
    ~row2:(row [ "x", int; "y", bool ]);
  print_row r;
  [%expect
    {|
    (Ok ())
    (RVar (Link (RExtend y Present (TCon bool ()) REmpty)))
    |}]
;;

let%expect_test "two open contexts share a fresh tail" =
  let fresh = Fresh.create () in
  let r1 = rvar fresh ~rank:1 in
  let r2 = rvar fresh ~rank:3 in
  unify_row
    fresh
    ~row1:(row [ "x", int ] ~tail:r1)
    ~row2:(row [ "y", bool ] ~tail:r2);
  print_row r1;
  print_row r2;
  [%expect
    {|
    (Ok ())
    (RVar (Link (RExtend y Present (TCon bool ()) (RVar (Unbound 4 1)))))
    (RVar
     (Link
      (RExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
       (RVar (Unbound 4 1)))))
    |}]
;;

let%expect_test "context variables" =
  let fresh = Fresh.create () in
  let r = rvar fresh ~rank:1 in
  unify_row fresh ~row1:r ~row2:r;
  unify_row fresh ~row1:r ~row2:(row [ "x", int ]);
  print_row r;
  [%expect
    {|
    (Ok ())
    (Ok ())
    (RVar (Link (RExtend x Present (TCon int ()) REmpty)))
    |}]
;;

let%expect_test "context occurs check" =
  let fresh = Fresh.create () in
  let r = rvar fresh ~rank:1 in
  unify_row fresh ~row1:r ~row2:(row [ "x", int ] ~tail:r);
  [%expect {| (Error (Occurs_check_failed (ARow (RVar (Unbound 0 1))))) |}]
;;

let%expect_test "cyclic context" =
  let fresh = Fresh.create () in
  let r = rvar fresh ~rank:1 in
  unify_row
    fresh
    ~row1:(row [ "x", int ] ~tail:r)
    ~row2:(row [ "y", bool ] ~tail:r);
  [%expect
    {|
    (Error
     (Row_mismatch
      (RExtend x Present (TCon int ())
       (RVar
        (Link
         (RExtend x (PVar (Unbound 1 1)) (TVar (Unbound 2 1))
          (RVar (Unbound 3 1))))))
      (RExtend y Present (TCon bool ())
       (RVar
        (Link
         (RExtend x (PVar (Unbound 1 1)) (TVar (Unbound 2 1))
          (RVar (Unbound 3 1))))))
      Cyclic_row))
    |}]
;;

let%expect_test "duplicate labels are scoped" =
  let fresh = Fresh.create () in
  let row1 = row [ "x", int; "x", bool ] in
  unify_row fresh ~row1 ~row2:(row [ "x", int; "x", bool ]);
  unify_row fresh ~row1 ~row2:(row [ "x", bool; "x", int ]);
  [%expect
    {|
    (Ok ())
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "linked context variable in tail position" =
  let fresh = Fresh.create () in
  let r = rvar fresh ~rank:1 in
  unify_row fresh ~row1:r ~row2:(row [ "z", int ]);
  unify_row fresh ~row1:(row [ "x", int ]) ~row2:(row [ "y", bool ] ~tail:r);
  unify_row
    fresh
    ~row1:(row [ "z", int; "y", bool ])
    ~row2:(row [ "y", bool ] ~tail:r);
  [%expect
    {|
    (Ok ())
    (Error
     (Row_mismatch (RExtend x Present (TCon int ()) REmpty)
      (RExtend y Present (TCon bool ())
       (RVar (Link (RExtend z Present (TCon int ()) REmpty))))
      (Missing_label x)))
    (Ok ())
    |}]
;;

let%expect_test "presence" =
  let fresh = Fresh.create () in
  let maybe name ty rest = ext name ty rest ~presence:(pvar fresh ~rank:1) in
  let maybe_x = maybe "x" int REmpty in
  unify_row fresh ~row1:maybe_x ~row2:REmpty;
  print_row maybe_x;
  let maybe_x = maybe "x" int REmpty in
  unify_row fresh ~row1:(row [ "x", int ]) ~row2:maybe_x;
  print_row maybe_x;
  let maybe_x = maybe "x" int (row [ "y", bool ]) in
  unify_row fresh ~row1:maybe_x ~row2:(row [ "y", bool ]);
  print_row maybe_x;
  unify_row
    fresh
    ~row1:(ext "x" int REmpty ~presence:Absent)
    ~row2:(row [ "x", int ]);
  unify_row fresh ~row1:(ext "x" int REmpty ~presence:Absent) ~row2:REmpty;
  [%expect
    {|
    (Ok ())
    (RExtend x (PVar (Link Absent)) (TCon int ()) REmpty)
    (Ok ())
    (RExtend x (PVar (Link Present)) (TCon int ()) REmpty)
    (Ok ())
    (RExtend x (PVar (Link Absent)) (TCon int ())
     (RExtend y Present (TCon bool ()) REmpty))
    (Error
     (Row_mismatch (RExtend x Absent (TCon int ()) REmpty)
      (RExtend x Present (TCon int ()) REmpty) (Presence_mismatch x)))
    (Ok ())
    |}]
;;

let%expect_test "absent entries need not agree on type" =
  let fresh = Fresh.create () in
  let r = rvar fresh ~rank:1 in
  unify_row fresh ~row1:(ext "x" int REmpty ~presence:Absent) ~row2:r;
  print_row r;
  unify_row
    fresh
    ~row1:(ext "x" int REmpty ~presence:Absent)
    ~row2:(row [ "y", bool ]);
  [%expect
    {|
    (Ok ())
    (RVar
     (Link
      (RExtend x (PVar (Link Absent)) (TVar (Link (TCon int ())))
       (RVar (Link REmpty)))))
    (Error
     (Row_mismatch REmpty (RExtend y Present (TCon bool ()) REmpty)
      (Missing_label y)))
    |}]
;;

let%expect_test "presence variables are generalized and their rank lowered" =
  let fresh = Fresh.create () in
  let inner = Fresh.var fresh ~rank:2 in
  let outer = pvar fresh ~rank:1 in
  let r = rvar fresh ~rank:1 in
  unify_row fresh ~row1:r ~row2:(ext "x" int REmpty ~presence:(PVar inner));
  print_s [%sexp (inner : (presence, Nothing.t) Var.t)];
  let context =
    ext
      "y"
      int
      ~presence:outer
      (ext "z" int REmpty ~presence:(pvar fresh ~rank:2))
  in
  Scheme.generalize_row ~rank:1 context;
  print_row context;
  [%expect
    {|
    (Ok ())
    (Unbound 0 1)
    (RExtend y (PVar (Unbound 1 1)) (TCon int ())
     (RExtend z (PVar (Generic 3)) (TCon int ()) REmpty))
    |}]
;;

let%expect_test "context bounds" =
  let fresh = Fresh.create () in
  let r = rvar fresh ~rank:1 in
  let bound = rvar fresh ~rank:1 in
  constrain fresh ~row:r ~bound;
  unify_row fresh ~row1:r ~row2:(row [ "x", int ] ~tail:(rvar fresh ~rank:1));
  print_row bound;
  (* An entry already in the bound is reused, rather than duplicated. *)
  unify_row
    fresh
    ~row1:r
    ~row2:(row [ "x", int; "y", bool ] ~tail:(rvar fresh ~rank:1));
  print_row bound;
  unify_row fresh ~row1:r ~row2:(row [ "y", int ] ~tail:(rvar fresh ~rank:1));
  [%expect
    {|
    (Ok ())
    (Ok ())
    (RVar
     (Link
      (RExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
       (RVar (Unbound 5 1)))))
    (Ok ())
    (RVar
     (Link
      (RExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
       (RVar
        (Link
         (RExtend y (PVar (Link Present)) (TVar (Link (TCon bool ())))
          (RVar (Unbound 9 1))))))))
    (Error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
    |}]
;;

let%expect_test "context bounds and presence" =
  let fresh = Fresh.create () in
  let constrain = constrain fresh in
  let maybe name ty rest = ext name ty rest ~presence:(pvar fresh ~rank:1) in
  (* Unused entries need not be in the bound. *)
  let maybe_x = maybe "x" int REmpty in
  constrain ~row:maybe_x ~bound:REmpty;
  print_row maybe_x;
  constrain ~row:(ext "x" int REmpty ~presence:Absent) ~bound:REmpty;
  constrain ~row:(row [ "x", int ]) ~bound:REmpty;
  (* A used entry makes the bound's entry used. *)
  let bound = maybe "x" int REmpty in
  constrain ~row:(row [ "x", int ]) ~bound;
  print_row bound;
  (* Shadowed entries are not constrained. *)
  constrain ~row:(row [ "x", int; "x", bool ]) ~bound:(row [ "x", int ]);
  [%expect
    {|
    (Ok ())
    (RExtend x (PVar (Link Absent)) (TCon int ()) REmpty)
    (Ok ())
    (Error
     (Row_mismatch (RExtend x Present (TCon int ()) REmpty) REmpty
      (Missing_label x)))
    (Ok ())
    (RExtend x (PVar (Link Present)) (TCon int ()) REmpty)
    (Ok ())
    |}]
;;

let%expect_test "bounds are kept by generalization and instantiation" =
  let fresh = Fresh.create () in
  let r = rvar fresh ~rank:2 in
  let bound = rvar fresh ~rank:2 in
  ignore
    (Unify.constrain_row fresh ~row:r ~bound ~shadowed:String.Set.empty
     : _ Result.t);
  let ty = TArrow (code r, code bound) in
  let print ty = print_endline (Type.to_string_hum ty) in
  print ty;
  Scheme.generalize_ty ~rank:1 ty;
  print ty;
  [%expect
    {|
    <int>{..'_a} -> <int>{..'_b} where ..'_a <= {..'_b}
    <int>{..'a} -> <int>{..'b} where ..'a <= {..'b}
    |}]
;;

let%expect_test "bounds lower ranks" =
  let fresh = Fresh.create () in
  let r = rvar fresh ~rank:2 in
  let bound = rvar fresh ~rank:2 in
  ignore
    (Unify.constrain_row fresh ~row:r ~bound ~shadowed:String.Set.empty
     : _ Result.t);
  unify_row fresh ~row1:r ~row2:(rvar fresh ~rank:1);
  print_row r;
  print_row bound;
  [%expect
    {|
    (Ok ())
    (RVar (Link (RVar (Unbound 2 1))))
    (RVar (Unbound 1 1))
    |}]
;;

let%expect_test "an absent entry shadows later entries with its name" =
  let fresh = Fresh.create () in
  let constrain = constrain fresh in
  let absent_then_x = ext "x" int (row [ "x", bool ]) ~presence:Absent in
  (* The shadowed [x] is unreachable, so needs no entry in the bound. *)
  constrain ~row:absent_then_x ~bound:REmpty;
  let maybe_then_x =
    ext "x" int (row [ "x", bool ]) ~presence:(pvar fresh ~rank:1)
  in
  constrain ~row:maybe_then_x ~bound:REmpty;
  print_row maybe_then_x;
  (* Unification never finds the shadowed [x] in place of the absent one. *)
  unify_row
    fresh
    ~row1:(row [ "x", bool ] ~tail:(rvar fresh ~rank:1))
    ~row2:absent_then_x;
  let r = rvar fresh ~rank:1 in
  constrain
    ~row:r
    ~bound:
      (ext
         "x"
         int
         (row [ "x", bool ] ~tail:(rvar fresh ~rank:1))
         ~presence:Absent);
  unify_row fresh ~row1:r ~row2:(row [ "x", bool ]);
  (* Shadowed entries still take part in unification, which is stricter than
     needed. *)
  unify_row fresh ~row1:REmpty ~row2:absent_then_x;
  [%expect
    {|
    (Ok ())
    (Ok ())
    (RExtend x (PVar (Link Absent)) (TCon int ())
     (RExtend x Present (TCon bool ()) REmpty))
    (Error
     (Row_mismatch (RExtend x Present (TCon bool ()) (RVar (Unbound 1 1)))
      (RExtend x Absent (TCon int ()) (RExtend x Present (TCon bool ()) REmpty))
      (Presence_mismatch x)))
    (Ok ())
    (Error
     (Row_mismatch (RExtend x Present (TCon bool ()) REmpty)
      (RExtend x Absent (TCon int ())
       (RExtend x Present (TCon bool ()) (RVar (Unbound 3 1))))
      (Presence_mismatch x)))
    (Error
     (Row_mismatch REmpty (RExtend x Present (TCon bool ()) REmpty)
      (Missing_label x)))
    |}]
;;

let%expect_test "cyclic bounds" =
  let fresh = Fresh.create () in
  let constrain = constrain fresh in
  let print contexts =
    TTuple (List.map contexts ~f:code) |> Type.to_string_hum |> print_endline
  in
  let cycle () =
    let a = rvar fresh ~rank:1 in
    let b = rvar fresh ~rank:1 in
    let c = rvar fresh ~rank:1 in
    constrain ~row:a ~bound:b;
    constrain ~row:b ~bound:c;
    constrain ~row:c ~bound:a;
    a, b, c
  in
  let a, b, c = cycle () in
  print [ a; b; c ];
  (* An entry of any member reaches every member. *)
  unify_row fresh ~row1:a ~row2:(row [ "x", int ] ~tail:(rvar fresh ~rank:1));
  print [ a; b; c ];
  unify_row fresh ~row1:c ~row2:(row [ "y", bool ] ~tail:(rvar fresh ~rank:1));
  print [ a; b; c ];
  unify_row fresh ~row1:b ~row2:(row [ "x", bool ] ~tail:(rvar fresh ~rank:1));
  (* Once one member is closed, no member can get entries. *)
  let a, b, c = cycle () in
  unify_row fresh ~row1:c ~row2:REmpty;
  print [ a; b; c ];
  unify_row fresh ~row1:a ~row2:(row [ "x", int ] ~tail:(rvar fresh ~rank:1));
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
     (Row_mismatch (RExtend x Present (TCon int ()) (RVar (Unbound 24 1)))
      (RVar
       (Link
        (RExtend x (PVar (Link Absent)) (TVar (Unbound 26 1))
         (RVar (Unbound 27 1)))))
      (Presence_mismatch x)))
    |}]
;;

let%expect_test "bounds referring to the bounded variable" =
  let fresh = Fresh.create () in
  let print context = print_endline (Type.to_string_hum (code context)) in
  let s = rvar fresh ~rank:1 in
  (* A bound that is the variable itself constrains nothing. *)
  constrain fresh ~row:s ~bound:s;
  print s;
  constrain fresh ~row:s ~bound:(row [ "x", int ] ~tail:s);
  print s;
  unify_row fresh ~row1:s ~row2:(row [ "y", bool ] ~tail:(rvar fresh ~rank:1));
  print s;
  unify_row fresh ~row1:s ~row2:(row [ "x", int; "y", bool ]);
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
  let r = rvar fresh ~rank:1 in
  let bound = row [ "x", int ] ~tail:(rvar fresh ~rank:1) in
  constrain fresh ~row:(row [ "x", int ] ~tail:r) ~bound;
  (* [r]'s [x] is hidden by the [x : int] before it. *)
  unify_row
    fresh
    ~row1:r
    ~row2:(row [ "x", bool; "y", bool ] ~tail:(rvar fresh ~rank:1));
  print_row bound;
  (* Shadowing is kept as bounds move to the rest of [r]. *)
  unify_row
    fresh
    ~row1:r
    ~row2:(row [ "x", bool; "y", bool; "x", string; "y", string ]);
  print_row bound;
  [%expect
    {|
    (Ok ())
    (Ok ())
    (RExtend x Present (TCon int ())
     (RVar
      (Link
       (RExtend y (PVar (Link Present)) (TVar (Link (TCon bool ())))
        (RVar (Unbound 5 1))))))
    (Ok ())
    (RExtend x Present (TCon int ())
     (RVar
      (Link
       (RExtend y (PVar (Link Present)) (TVar (Link (TCon bool ())))
        (RVar (Unbound 5 1))))))
    |}]
;;
