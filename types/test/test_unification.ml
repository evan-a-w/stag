open! Import
open Type

let int = TCon ("int", [])
let bool = TCon ("bool", [])
let string = TCon ("string", [])
let con ?(args = []) name = TCon (name, args)
let code ?(inner = int) context = TCode { inner; context }
let tvar fresh ~rank = TVar (Fresh.var fresh ~rank)
let rvar fresh ~rank = RVar (Fresh.var fresh ~rank)
let pvar fresh ~rank = PVar (Fresh.var fresh ~rank)

let ext ?(presence = Present) ?(assigned = Absent) name ty rest =
  RExtend (name, presence, assigned, ty, rest)
;;

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
  [%expect {|
    (Ok ())
    (TVar (Link (TCon int ())))
    |}]
;;

let%expect_test "arrows" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  let b = tvar fresh ~rank:1 in
  unify_ty fresh ~ty1:(Type.arrow a int) ~ty2:(Type.arrow bool b);
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
  [%expect {|
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

let%expect_test "tuples unify every element, reporting the first error" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  let b = tvar fresh ~rank:1 in
  unify_ty
    fresh
    ~ty1:(TTuple [ int; a; string; b ])
    ~ty2:(TTuple [ bool; int; int; bool ]);
  print_ty a;
  print_ty b;
  [%expect
    {|
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (TVar (Link (TCon int ())))
    (TVar (Link (TCon bool ())))
    |}]
;;

let%expect_test "constructor arguments stop at the first error" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  unify_ty
    fresh
    ~ty1:(con "pair" ~args:[ ATy int; ATy a ])
    ~ty2:(con "pair" ~args:[ ATy bool; ATy int ]);
  print_ty a;
  [%expect
    {|
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (TVar (Unbound 0 1))
    |}]
;;

let%expect_test "length mismatches unify no element" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  unify_ty fresh ~ty1:(TTuple [ a; int ]) ~ty2:(TTuple [ int ]);
  unify_ty
    fresh
    ~ty1:(con "pair" ~args:[ ATy a; ATy int ])
    ~ty2:(con "pair" ~args:[ ATy int ]);
  print_ty a;
  [%expect
    {|
    (Error
     (Type_mismatch (TTuple ((TVar (Unbound 0 1)) (TCon int ())))
      (TTuple ((TCon int ()))) (Args_length_mismatch 2 1)))
    (Error
     (Type_mismatch (TCon pair ((ATy (TVar (Unbound 0 1))) (ATy (TCon int ()))))
      (TCon pair ((ATy (TCon int ())))) (Args_length_mismatch 2 1)))
    (TVar (Unbound 0 1))
    |}]
;;

let%expect_test "structural mismatch" =
  let fresh = Fresh.create () in
  unify_ty fresh ~ty1:(Type.arrow int int) ~ty2:(TTuple [ int; int ]);
  unify_ty fresh ~ty1:int ~ty2:(Type.arrow int int);
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
  [%expect {|
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
  unify_ty fresh ~ty1:a ~ty2:(TTuple [ a; int ]);
  unify_ty fresh ~ty1:a ~ty2:(con "list" ~args:[ ATy a ]);
  unify_ty fresh ~ty1:a ~ty2:(Type.arrow a int);
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
  let ty = TTuple [ Type.arrow b c; code ~inner:d r ] in
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
  [%expect {|
    (Ok ())
    (TVar (Unbound 1 1))
    |}]
;;

let%expect_test "bindings are not undone on failure" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  unify_ty fresh ~ty1:(Type.arrow a int) ~ty2:(Type.arrow bool string);
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
    (RVar (Link (RExtend y Present (TCon bool ()) (RVar (Unbound 3 1)))))
    (RVar
     (Link (RExtend x (PVar (Link Present)) (TCon int ()) (RVar (Unbound 3 1)))))
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
         (RExtend x (PVar (Unbound 1 1)) (TCon int ()) (RVar (Unbound 2 1))))))
      (RExtend y Present (TCon bool ())
       (RVar
        (Link
         (RExtend x (PVar (Unbound 1 1)) (TCon int ()) (RVar (Unbound 2 1))))))
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
     (Link (RExtend x (PVar (Link Absent)) (TCon int ()) (RVar (Link REmpty)))))
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
  print_s [%sexp (inner : (presence, presence_bound) Var.t)];
  let context =
    ext
      "y"
      int
      ~presence:outer
      (ext "z" int REmpty ~presence:(pvar fresh ~rank:2))
  in
  Scheme.generalize fresh ~rank:1 [ Fresh.record fresh context ];
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
       (RVar (Unbound 6 1)))))
    (Ok ())
    (RVar
     (Link
      (RExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
       (RVar
        (Link
         (RExtend y (PVar (Link Present)) (TVar (Link (TCon bool ())))
          (RVar (Unbound 11 1))))))))
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

(* Contexts with an entry [x] of each presence, as code types, so their presence
   bounds are printed. *)
let print_presences presences =
  print_endline
    (Type.to_string_hum
       (TTuple
          (List.map presences ~f:(fun presence ->
             code (ext "x" int REmpty ~presence)))))
;;

let presence_error = Unification_error.Row_mismatch (REmpty, REmpty, Cyclic_row)

let force fresh presence ~to_ =
  let result =
    Unify.unify_row
      fresh
      ~row1:(ext "x" int REmpty ~presence)
      ~row2:(ext "x" int REmpty ~presence:to_)
    |> Result.map_error ~f:ignore
  in
  print_s [%sexp (result : (unit, unit) Result.t)]
;;

let at_most fresh ~presence ~bound =
  constrain
    fresh
    ~row:(ext "x" int REmpty ~presence)
    ~bound:(ext "x" int REmpty ~presence:bound)
;;

let%expect_test "unknown presences are bounded, not unified" =
  let fresh = Fresh.create () in
  let bounded () =
    let p = pvar fresh ~rank:1 in
    let q = pvar fresh ~rank:1 in
    at_most fresh ~presence:p ~bound:q;
    p, q
  in
  let p, q = bounded () in
  print_presences [ p; q ];
  (* Either may be used or not, as long as [p] implies [q]. *)
  force fresh p ~to_:Absent;
  print_presences [ p; q ];
  let p, q = bounded () in
  force fresh q ~to_:Present;
  print_presences [ p; q ];
  let p, q = bounded () in
  force fresh p ~to_:Present;
  print_presences [ p; q ];
  let p, q = bounded () in
  force fresh q ~to_:Absent;
  print_presences [ p; q ];
  [%expect
    {|
    (Ok ())
    <int>{x?'_a : immut int} * <int>{x?'_b : immut int} where '_a <= '_b
    (Ok ())
    <int>{} * <int>{x?'_a : immut int}
    (Ok ())
    (Ok ())
    <int>{x?'_a : immut int} * <int>{x : immut int}
    (Ok ())
    (Ok ())
    <int>{x : immut int} * <int>{x : immut int}
    (Ok ())
    (Ok ())
    <int>{} * <int>{}
    |}]
;;

let if_present fresh ~presence ~ty1 ~ty2 =
  let result = Unify.unify_if_present fresh ~presence ~ty1 ~ty2 in
  print_s [%sexp (result : (unit, Unification_error.t) Result.t)]
;;

(* An entry [x] of each presence and type, so their equalities are printed. *)
let print_entries entries =
  print_endline
    (Type.to_string_hum
       (TTuple
          (List.map entries ~f:(fun (presence, ty) ->
             code (ext "x" ty REmpty ~presence)))))
;;

let%expect_test "equalities wait for their presence" =
  let fresh = Fresh.create () in
  let pending () =
    let p = pvar fresh ~rank:1 in
    let a = tvar fresh ~rank:1 in
    if_present fresh ~presence:p ~ty1:a ~ty2:int;
    p, a
  in
  let p, a = pending () in
  print_entries [ p, a ];
  force fresh p ~to_:Present;
  print_entries [ p, a ];
  let p, a = pending () in
  force fresh p ~to_:Absent;
  print_entries [ p, a ];
  (* Known presences decide at once. *)
  let a = tvar fresh ~rank:1 in
  if_present fresh ~presence:Absent ~ty1:a ~ty2:int;
  if_present fresh ~presence:Present ~ty1:a ~ty2:bool;
  print_ty a;
  if_present fresh ~presence:Present ~ty1:a ~ty2:int;
  (* Conflicting equalities are fine until the presence is known. *)
  let p, a = pending () in
  if_present fresh ~presence:p ~ty1:a ~ty2:bool;
  print_entries [ p, a ];
  force fresh p ~to_:Present;
  [%expect
    {|
    (Ok ())
    <int>{x?'_a : immut '_b} where '_a => '_b = int
    (Ok ())
    <int>{x : immut int}
    (Ok ())
    (Ok ())
    <int>{}
    (Ok ())
    (Ok ())
    (TVar (Link (TCon bool ())))
    (Error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
    (Ok ())
    (Ok ())
    <int>{x?'_a : immut '_b} where '_a => '_b = bool, '_a => '_b = int
    (Error ())
    |}]
;;

let%expect_test "types clash only if no instance of their variables unifies \
                 them"
  =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:1 in
  let b = tvar fresh ~rank:1 in
  let cyclic f =
    let var = Fresh.var fresh ~rank:1 in
    var.state <- Link (f (TVar var));
    TVar var
  in
  let rec nest n ty = if n = 0 then ty else Type.arrow int (nest (n - 1) ty) in
  let ints = cyclic (fun self -> Type.arrow int self) in
  let rs = rvar fresh ~rank:1 in
  List.iter
    [ "int, bool", int, bool
    ; "int, 'a", int, a
    ; "'a, 'b", a, b
    ; "int * 'a, bool * 'b", TTuple [ int; a ], TTuple [ bool; b ]
    ; "int * 'a, int * 'b", TTuple [ int; a ], TTuple [ int; b ]
    ; "int * int, int", TTuple [ int; int ], TTuple [ int ]
    ; "int -> int, int * int", Type.arrow int int, TTuple [ int; int ]
    ; ( "int list, bool list"
      , con "list" ~args:[ ATy int ]
      , con "list" ~args:[ ATy bool ] )
    ; ( "'a list, bool list"
      , con "list" ~args:[ ATy a ]
      , con "list" ~args:[ ATy bool ] )
    ; "int list, int", con "list" ~args:[ ATy int ], con "list"
    ; ( "<int>, <bool>"
      , code ~inner:int REmpty
      , code ~inner:bool (row [ "x", int ]) )
    ; ( "<int>{x : int}, <int>{x : bool}"
      , code (row [ "x", int ])
      , code (row [ "x", bool ]) )
    ; ( "{x : int}, {x : bool} (records)"
      , Fresh.record fresh (row [ "x", int ])
      , Fresh.record fresh (row [ "x", bool ]) )
    ; "record, int", Fresh.record fresh (row [ "x", int ]), int
    ; ( "rows as arguments"
      , con "r" ~args:[ ARow (row [ "x", int ]) ]
      , con "r" ~args:[ ARow (row [ "x", bool ] ~tail:rs) ] )
    ; "int -> int -> ..., same", ints, cyclic (fun self -> Type.arrow int self)
    ; "int -> int -> ..., int -> int -> bool", ints, nest 2 bool
    ; "int -> int -> ..., 20 arrows to bool", ints, nest 20 bool
    ]
    ~f:(fun (what, ty1, ty2) ->
      print_s [%message what ~clash:(Unify.clash ty1 ty2 : bool)]);
  [%expect
    {|
    ("int, bool" (clash true))
    ("int, 'a" (clash false))
    ("'a, 'b" (clash false))
    ("int * 'a, bool * 'b" (clash true))
    ("int * 'a, int * 'b" (clash false))
    ("int * int, int" (clash true))
    ("int -> int, int * int" (clash true))
    ("int list, bool list" (clash true))
    ("'a list, bool list" (clash false))
    ("int list, int" (clash true))
    ("<int>, <bool>" (clash true))
    ("<int>{x : int}, <int>{x : bool}" (clash false))
    ("{x : int}, {x : bool} (records)" (clash false))
    ("record, int" (clash true))
    ("rows as arguments" (clash false))
    ("int -> int -> ..., same" (clash false))
    ("int -> int -> ..., int -> int -> bool" (clash true))
    ("int -> int -> ..., 20 arrows to bool" (clash false))
    |}]
;;

let%expect_test "an equality between clashing types makes its presence Absent" =
  let fresh = Fresh.create () in
  let p = pvar fresh ~rank:1 in
  let q = pvar fresh ~rank:1 in
  at_most fresh ~presence:q ~bound:p;
  if_present fresh ~presence:p ~ty1:int ~ty2:bool;
  print_presences [ p; q ];
  (* A presence that must be [Present] gets the mismatch. *)
  let p = pvar fresh ~rank:1 in
  at_most fresh ~presence:Present ~bound:p;
  if_present
    fresh
    ~presence:p
    ~ty1:(TTuple [ int; tvar fresh ~rank:1 ])
    ~ty2:(TTuple [ bool; int ]);
  (* Types that become clashing later are decided at generalization, for a
     presence no other one is bounded by. *)
  let generalized ~lower =
    let p = pvar fresh ~rank:2 in
    let q = pvar fresh ~rank:2 in
    let a = tvar fresh ~rank:2 in
    if lower then at_most fresh ~presence:q ~bound:p;
    if_present fresh ~presence:p ~ty1:a ~ty2:int;
    unify_ty fresh ~ty1:a ~ty2:bool;
    let ty =
      TTuple
        [ code (ext "x" a REmpty ~presence:p)
        ; code (ext "x" int REmpty ~presence:q)
        ]
    in
    print_endline (Type.to_string_hum ty);
    Scheme.generalize fresh ~rank:1 [ ty ];
    print_endline (Type.to_string_hum ty)
  in
  generalized ~lower:false;
  generalized ~lower:true;
  [%expect
    {|
    (Ok ())
    (Ok ())
    <int>{} * <int>{}
    (Ok ())
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Ok ())
    (Ok ())
    <int>{x?'_a : immut bool} * <int>{x?'_b : immut int} where '_a => bool = int
    <int>{} * <int>{x?'a : immut int}
    (Ok ())
    (Ok ())
    (Ok ())
    <int>{x?'_a : immut bool} * <int>{x?'_b : immut int} where '_a => bool = int, '_b <= '_a
    <int>{x?'a : immut bool} * <int>{x?'b : immut int} where 'a => bool = int, 'b <= 'a
    |}]
;;

let%expect_test "unifying presences merges their equalities" =
  let fresh = Fresh.create () in
  let p = pvar fresh ~rank:1 in
  let q = pvar fresh ~rank:1 in
  let a = tvar fresh ~rank:1 in
  let b = tvar fresh ~rank:1 in
  if_present fresh ~presence:p ~ty1:a ~ty2:int;
  if_present fresh ~presence:q ~ty1:b ~ty2:bool;
  force fresh p ~to_:q;
  print_entries [ p, a; q, b ];
  force fresh q ~to_:Present;
  print_entries [ p, a; q, b ];
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Ok ())
    <int>{x?'_a : immut '_b} * <int>{x?'_a : immut '_c} where '_a => '_b = int, '_a => '_c = bool
    (Ok ())
    <int>{x : immut int} * <int>{x : immut bool}
    |}]
;;

let%expect_test "equalities follow presence bounds" =
  let fresh = Fresh.create () in
  let bounded () =
    let p = pvar fresh ~rank:1 in
    let q = pvar fresh ~rank:1 in
    let a = tvar fresh ~rank:1 in
    at_most fresh ~presence:p ~bound:q;
    if_present fresh ~presence:q ~ty1:a ~ty2:int;
    p, q, a
  in
  (* [p] present makes [q] present, so its equality holds. *)
  let p, q, a = bounded () in
  force fresh p ~to_:Present;
  print_entries [ p, a; q, a ];
  (* [q] absent makes [p] absent, dropping it. *)
  let p, q, a = bounded () in
  force fresh q ~to_:Absent;
  print_entries [ p, a; q, a ];
  (* [p] absent says nothing of [q]. *)
  let p, q, a = bounded () in
  force fresh p ~to_:Absent;
  print_entries [ p, a; q, a ];
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Ok ())
    <int>{x : immut int} * <int>{x : immut int}
    (Ok ())
    (Ok ())
    (Ok ())
    <int>{} * <int>{}
    (Ok ())
    (Ok ())
    (Ok ())
    <int>{} * <int>{x?'_a : immut '_b} where '_a => '_b = int
    |}]
;;

let%expect_test "a row bound equates entry types if present" =
  let fresh = Fresh.create () in
  let p = pvar fresh ~rank:1 in
  let q = pvar fresh ~rank:1 in
  let a = tvar fresh ~rank:1 in
  constrain
    fresh
    ~row:(ext "x" a REmpty ~presence:p)
    ~bound:(ext "x" bool REmpty ~presence:q);
  print_entries [ p, a; q, bool ];
  (* A row unified with another having [x?p : int] *)
  let r = rvar fresh ~rank:1 in
  let b = tvar fresh ~rank:1 in
  unify_row fresh ~row1:r ~row2:(ext "x" int REmpty ~presence:p);
  unify_row fresh ~row1:r ~row2:(ext "x" b REmpty ~presence:p);
  print_entries [ p, a; p, b ];
  force fresh p ~to_:Absent;
  print_entries [ p, a; p, b; q, bool ];
  [%expect
    {|
    (Ok ())
    <int>{x?'_a : immut '_b} * <int>{x?'_c : immut bool} where '_a <= '_c, '_a => '_b = bool
    (Ok ())
    (Ok ())
    <int>{x?'_a : immut '_b} * <int>{x?'_a : immut '_c} where '_a <= '_d, '_a => int = '_c, '_a => '_b = bool
    (Ok ())
    <int>{} * <int>{} * <int>{x?'_a : immut bool}
    |}]
;;

let%expect_test "equalities are generalized and instantiated with their \
                 presence"
  =
  let fresh = Fresh.create () in
  let p = pvar fresh ~rank:2 in
  let a = tvar fresh ~rank:2 in
  let outer = pvar fresh ~rank:1 in
  let o = tvar fresh ~rank:1 in
  let g = tvar fresh ~rank:2 in
  if_present fresh ~presence:p ~ty1:a ~ty2:(Type.arrow o int);
  (* [outer] is not generalized, but its equality is, with [g]. *)
  if_present fresh ~presence:outer ~ty1:g ~ty2:bool;
  (* [a] and [g] are also used where their presences don't matter. *)
  let ty =
    TTuple
      [ code (ext "x" a REmpty ~presence:p)
      ; code (ext "x" g REmpty ~presence:outer)
      ; a
      ; g
      ]
  in
  Scheme.generalize fresh ~rank:1 [ ty ];
  print_endline (Type.to_string_hum ty);
  print_entries [ outer, o ];
  let instance () =
    match Scheme.instantiate fresh ~rank:1 ty with
    | TTuple
        [ TCode { context = RExtend (_, p, _, a, _); _ }
        ; TCode { context = RExtend (_, _, _, g, _); _ }
        ; _
        ; _
        ] -> p, a, g
    | ty -> raise_s [%message "unexpected instance" (ty : ty)]
  in
  let p1, a1, g1 = instance () in
  let p2, a2, g2 = instance () in
  force fresh p1 ~to_:Present;
  print_entries [ p1, a1; p2, a2; outer, g1; outer, g2 ];
  force fresh outer ~to_:Present;
  print_entries [ p1, a1; p2, a2; outer, g1; outer, g2 ];
  [%expect
    {|
    (Ok ())
    (Ok ())
    <int>{x?'a : immut 'b} * <int>{x?'_c : immut 'd} * 'b * 'd where 'a => 'b = '_e -> int, '_c => '_f = bool, '_c => 'd = bool
    <int>{x?'_a : immut '_b} where '_a => '_c = bool
    (Ok ())
    <int>{x : immut '_a -> int} * <int>{x?'_b : immut '_c} * <int>{x?'_d : immut '_e} * <int>{x?'_d : immut '_f} where '_b => '_c = '_a -> int, '_d => '_f = bool, '_d => '_e = bool, '_d => '_g = bool
    (Ok ())
    <int>{x : immut '_a -> int} * <int>{x?'_b : immut '_c} * <int>{x : immut bool} * <int>{x : immut bool} where '_b => '_c = '_a -> int
    |}]
;;

let%expect_test "entry types that only matter where their presence is present" =
  let fresh = Fresh.create () in
  let p = pvar fresh ~rank:2 in
  let q = pvar fresh ~rank:2 in
  let a = tvar fresh ~rank:2 in
  let b = tvar fresh ~rank:2 in
  let c = tvar fresh ~rank:2 in
  let outer = pvar fresh ~rank:1 in
  let g = tvar fresh ~rank:2 in
  (* [a] is only the type of entries of [p], so is [int]; [b] is also that of an
     entry of [q], which may be present while [p] is absent, unless [q <= p];
     [c] is used elsewhere. *)
  if_present fresh ~presence:p ~ty1:a ~ty2:int;
  if_present fresh ~presence:p ~ty1:b ~ty2:int;
  if_present fresh ~presence:p ~ty1:c ~ty2:int;
  if_present fresh ~presence:outer ~ty1:g ~ty2:bool;
  let ty () =
    TTuple
      [ code (ext "x" a REmpty ~presence:p)
      ; code (ext "x" b (ext "y" b REmpty ~presence:q) ~presence:p)
      ; code (ext "x" c REmpty ~presence:p)
      ; Type.arrow c c
      ; code (ext "x" g REmpty ~presence:outer)
      ]
  in
  let generalize ty =
    Scheme.generalize fresh ~rank:1 [ ty ];
    print_endline (Type.to_string_hum ty)
  in
  generalize (ty ());
  let p' = pvar fresh ~rank:2 in
  let q' = pvar fresh ~rank:2 in
  let b' = tvar fresh ~rank:2 in
  Unify.presence_at_most fresh ~presence:q' ~bound:p' ~error:presence_error
  |> Result.map_error ~f:ignore
  |> [%sexp_of: (unit, unit) Result.t]
  |> print_s;
  if_present fresh ~presence:p' ~ty1:b' ~ty2:int;
  generalize (code (ext "x" b' (ext "y" b' REmpty ~presence:q') ~presence:p'));
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Ok ())
    (Ok ())
    <int>{x?'a : immut int} * <int>{x?'a : immut 'b; y?'c : immut 'b} * <int>{x?'a : immut 'd} * ('d -> 'd) * <int>{x?'_e : immut bool} where 'a => 'd = int, 'a => 'b = int
    (Ok ())
    (Ok ())
    <int>{x?'a : immut int; y?'b : immut int} where 'b <= 'a
    |}]
;;

let rec permutations = function
  | [] -> [ [] ]
  | xs ->
    List.concat_mapi xs ~f:(fun i x ->
      List.filteri xs ~f:(fun j _ -> j <> i)
      |> permutations
      |> List.map ~f:(fun rest -> x :: rest))
;;

(* Runs [steps] on [x?'p] and [x?'q] in every order, printing their results and
   the presences, which are the same whatever the order. *)
let in_every_order steps =
  let fresh = Fresh.create () in
  List.iter (permutations steps) ~f:(fun steps ->
    let p = pvar fresh ~rank:1 in
    let q = pvar fresh ~rank:1 in
    let result step : (unit, unit) Result.t =
      match step with
      | `Bound ->
        Unify.constrain_row
          fresh
          ~row:(ext "x" int REmpty ~presence:p)
          ~bound:(ext "x" int REmpty ~presence:q)
          ~shadowed:String.Set.empty
        |> Result.ignore_m
        |> Result.map_error ~f:ignore
      | `P to_ ->
        Unify.unify_row
          fresh
          ~row1:(ext "x" int REmpty ~presence:p)
          ~row2:(ext "x" int REmpty ~presence:to_)
        |> Result.map_error ~f:ignore
      | `Q to_ ->
        Unify.unify_row
          fresh
          ~row1:(ext "x" int REmpty ~presence:q)
          ~row2:(ext "x" int REmpty ~presence:to_)
        |> Result.map_error ~f:ignore
    in
    let results = List.map steps ~f:result in
    print_s
      [%sexp
        (List.zip_exn
           (List.map steps ~f:(function
             | `Bound -> "p <= q"
             | `P to_ -> "p = " ^ Sexp.to_string [%sexp (to_ : presence)]
             | `Q to_ -> "q = " ^ Sexp.to_string [%sexp (to_ : presence)]))
           results
         : (string * (unit, unit) Result.t) list)];
    print_presences [ p; q ])
;;

let%expect_test "presence bounds are solved in any order" =
  in_every_order [ `Bound; `P Absent; `Q Present ];
  [%expect
    {|
    (("p <= q" (Ok ())) ("p = Absent" (Ok ())) ("q = Present" (Ok ())))
    <int>{} * <int>{x : immut int}
    (("p <= q" (Ok ())) ("q = Present" (Ok ())) ("p = Absent" (Ok ())))
    <int>{} * <int>{x : immut int}
    (("p = Absent" (Ok ())) ("p <= q" (Ok ())) ("q = Present" (Ok ())))
    <int>{} * <int>{x : immut int}
    (("p = Absent" (Ok ())) ("q = Present" (Ok ())) ("p <= q" (Ok ())))
    <int>{} * <int>{x : immut int}
    (("q = Present" (Ok ())) ("p <= q" (Ok ())) ("p = Absent" (Ok ())))
    <int>{} * <int>{x : immut int}
    (("q = Present" (Ok ())) ("p = Absent" (Ok ())) ("p <= q" (Ok ())))
    <int>{} * <int>{x : immut int}
    |}];
  in_every_order [ `Bound; `P Present ];
  [%expect
    {|
    (("p <= q" (Ok ())) ("p = Present" (Ok ())))
    <int>{x : immut int} * <int>{x : immut int}
    (("p = Present" (Ok ())) ("p <= q" (Ok ())))
    <int>{x : immut int} * <int>{x : immut int}
    |}];
  in_every_order [ `Bound; `Q Absent ];
  [%expect
    {|
    (("p <= q" (Ok ())) ("q = Absent" (Ok ())))
    <int>{} * <int>{}
    (("q = Absent" (Ok ())) ("p <= q" (Ok ())))
    <int>{} * <int>{}
    |}];
  (* A conflict is found whichever constraint comes last. *)
  in_every_order [ `Bound; `P Present; `Q Absent ];
  [%expect
    {|
    (("p <= q" (Ok ())) ("p = Present" (Ok ())) ("q = Absent" (Error ())))
    <int>{x : immut int} * <int>{x : immut int}
    (("p <= q" (Ok ())) ("q = Absent" (Ok ())) ("p = Present" (Error ())))
    <int>{} * <int>{}
    (("p = Present" (Ok ())) ("p <= q" (Ok ())) ("q = Absent" (Error ())))
    <int>{x : immut int} * <int>{x : immut int}
    (("p = Present" (Ok ())) ("q = Absent" (Ok ())) ("p <= q" (Error ())))
    <int>{x : immut int} * <int>{}
    (("q = Absent" (Ok ())) ("p <= q" (Ok ())) ("p = Present" (Error ())))
    <int>{} * <int>{}
    (("q = Absent" (Ok ())) ("p = Present" (Ok ())) ("p <= q" (Error ())))
    <int>{x : immut int} * <int>{}
    |}]
;;

let%expect_test "presence bounds through chains and cycles" =
  let fresh = Fresh.create () in
  let chain n =
    let presences = List.init n ~f:(fun _ -> pvar fresh ~rank:1) in
    List.iter2_exn
      (List.drop_last_exn presences)
      (List.tl_exn presences)
      ~f:(fun presence bound -> at_most fresh ~presence ~bound);
    presences
  in
  let cycle n =
    let presences = chain n in
    at_most
      fresh
      ~presence:(List.last_exn presences)
      ~bound:(List.hd_exn presences);
    presences
  in
  let presences = chain 3 in
  print_presences presences;
  force fresh (List.hd_exn presences) ~to_:Present;
  print_presences presences;
  let presences = chain 3 in
  force fresh (List.last_exn presences) ~to_:Absent;
  print_presences presences;
  let presences = cycle 3 in
  print_presences presences;
  force fresh (List.nth_exn presences 1) ~to_:Present;
  print_presences presences;
  let presences = cycle 3 in
  force fresh (List.nth_exn presences 1) ~to_:Absent;
  print_presences presences;
  [%expect
    {|
    (Ok ())
    (Ok ())
    <int>{x?'_a : immut int} * <int>{x?'_b : immut int} * <int>{x?'_c : immut int} where '_a <= '_b, '_b <= '_c
    (Ok ())
    <int>{x : immut int} * <int>{x : immut int} * <int>{x : immut int}
    (Ok ())
    (Ok ())
    (Ok ())
    <int>{} * <int>{} * <int>{}
    (Ok ())
    (Ok ())
    (Ok ())
    <int>{x?'_a : immut int} * <int>{x?'_b : immut int} * <int>{x?'_c : immut int} where '_a <= '_b, '_b <= '_c, '_c <= '_a
    (Ok ())
    <int>{x : immut int} * <int>{x : immut int} * <int>{x : immut int}
    (Ok ())
    (Ok ())
    (Ok ())
    (Ok ())
    <int>{} * <int>{} * <int>{}
    |}]
;;

let%expect_test "unifying bounded presences merges their bounds" =
  let fresh = Fresh.create () in
  let p = pvar fresh ~rank:1 in
  let q = pvar fresh ~rank:1 in
  let r = pvar fresh ~rank:1 in
  let s = pvar fresh ~rank:1 in
  at_most fresh ~presence:p ~bound:q;
  at_most fresh ~presence:r ~bound:s;
  at_most fresh ~presence:s ~bound:q;
  print_presences [ p; q; r; s ];
  force fresh q ~to_:r;
  print_presences [ p; q; r; s ];
  (* [q] and [r] now bound each other through [s]. *)
  force fresh p ~to_:Present;
  print_presences [ p; q; r; s ];
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Ok ())
    <int>{x?'_a : immut int} * <int>{x?'_b : immut int} * <int>{x?'_c : immut int} * <int>{x?'_d : immut int} where '_a <= '_b, '_c <= '_d, '_d <= '_b
    (Ok ())
    <int>{x?'_a : immut int} * <int>{x?'_b : immut int} * <int>{x?'_b : immut int} * <int>{x?'_c : immut int} where '_a <= '_b, '_b <= '_c, '_c <= '_b
    (Ok ())
    <int>{x : immut int} * <int>{x : immut int} * <int>{x : immut int} * <int>{x : immut int}
    |}]
;;

let%expect_test "presence bounds are generalized and instantiated together" =
  let fresh = Fresh.create () in
  let p = pvar fresh ~rank:2 in
  let q = pvar fresh ~rank:1 in
  at_most fresh ~presence:p ~bound:q;
  (* Bounds relate variables of any ranks: [p] is generalized, keeping its bound
     to [q], which no longer refers to [p]. *)
  let pair p q =
    TTuple
      [ code (ext "x" int REmpty ~presence:p)
      ; code (ext "x" int REmpty ~presence:q)
      ]
  in
  Scheme.generalize fresh ~rank:1 [ pair p p ];
  print_presences [ p; q ];
  print_presences [ q ];
  let p = pvar fresh ~rank:2 in
  let q = pvar fresh ~rank:2 in
  at_most fresh ~presence:p ~bound:q;
  (* Generalizing [p] generalizes [q], which only its bound refers to. *)
  Scheme.generalize fresh ~rank:1 [ pair p p ];
  print_presences [ p; q ];
  let instance () =
    match Scheme.instantiate fresh ~rank:1 (pair p q) with
    | TTuple
        [ TCode { context = RExtend (_, p, _, _, _); _ }
        ; TCode { context = RExtend (_, q, _, _, _); _ }
        ] -> p, q
    | ty -> raise_s [%message "unexpected instance" (ty : ty)]
  in
  let p1, q1 = instance () in
  let p2, q2 = instance () in
  force fresh p1 ~to_:Present;
  force fresh q2 ~to_:Absent;
  print_presences [ p1; q1; p2; q2; p; q ];
  [%expect
    {|
    (Ok ())
    <int>{x?'a : immut int} * <int>{x?'_b : immut int} where 'a <= '_b
    <int>{x?'_a : immut int}
    (Ok ())
    <int>{x?'a : immut int} * <int>{x?'b : immut int} where 'a <= 'b
    (Ok ())
    (Ok ())
    <int>{x : immut int} * <int>{x : immut int} * <int>{} * <int>{} * <int>{x?'a : immut int} * <int>{x?'b : immut int} where 'a <= 'b
    |}]
;;

(* [l1 <= g1 <= g2 <= g3 <= u], [g3 <= g2] and [l2 <= g3], generalizing [g1]:
   the other [g]s are only reached through bounds. *)
let bounded_through_generalized fresh =
  let l1 = pvar fresh ~rank:1 in
  let l2 = pvar fresh ~rank:1 in
  let u = pvar fresh ~rank:1 in
  let g1 = pvar fresh ~rank:2 in
  let g2 = pvar fresh ~rank:2 in
  let g3 = pvar fresh ~rank:2 in
  List.iter
    [ l1, g1; g1, g2; g2, g3; g3, g2; g3, u; l2, g3 ]
    ~f:(fun (presence, bound) ->
      match
        Unify.presence_at_most fresh ~presence ~bound ~error:presence_error
      with
      | Ok () -> ()
      | Error _ -> raise_s [%message "unexpected conflict"]);
  print_presences [ l1; l2; u ];
  print_presences [ g1 ];
  let ty = code (ext "x" int REmpty ~presence:g1) in
  Scheme.generalize fresh ~rank:1 [ ty ];
  print_endline (Type.to_string_hum ty);
  print_presences [ l1; l2; u ];
  let instance () =
    match Scheme.instantiate fresh ~rank:1 ty with
    | TCode { context = RExtend (_, presence, _, _, _); _ } -> presence
    | ty -> raise_s [%message "unexpected instance" (ty : ty)]
  in
  l1, l2, u, instance
;;

let%expect_test "generalization keeps the bounds that hold through generalized \
                 variables"
  =
  let fresh = Fresh.create () in
  let l1, l2, u, instance = bounded_through_generalized fresh in
  let i1 = instance () in
  let i2 = instance () in
  print_presences [ l1; l2; u; i1; i2 ];
  force fresh l1 ~to_:Present;
  print_presences [ l1; l2; u; i1; i2 ];
  [%expect
    {|
    <int>{x?'_a : immut int} * <int>{x?'_b : immut int} * <int>{x?'_c : immut int} where '_a <= '_d, '_b <= '_e, '_d <= '_f, '_e <= '_c, '_e <= '_f, '_f <= '_e
    <int>{x?'_a : immut int} where '_a <= '_b, '_b <= '_d, '_c <= '_a, '_d <= '_e, '_d <= '_b, '_f <= '_d
    <int>{x?'a : immut int} where 'a <= 'b, '_c <= 'a, 'b <= 'd, '_c <= '_e, 'd <= '_e, 'd <= 'b, '_f <= 'd, '_f <= '_e
    <int>{x?'_a : immut int} * <int>{x?'_b : immut int} * <int>{x?'_c : immut int} where '_a <= '_c, '_b <= '_c
    <int>{x?'_a : immut int} * <int>{x?'_b : immut int} * <int>{x?'_c : immut int} * <int>{x?'_d : immut int} * <int>{x?'_e : immut int} where '_a <= '_c, '_a <= '_d, '_a <= '_e, '_b <= '_c, '_b <= '_f, '_b <= '_g, '_d <= '_h, '_e <= '_i, '_f <= '_c, '_f <= '_h, '_g <= '_c, '_g <= '_i, '_h <= '_f, '_i <= '_g
    (Ok ())
    <int>{x : immut int} * <int>{x?'_a : immut int} * <int>{x : immut int} * <int>{x : immut int} * <int>{x : immut int}
    |}]
;;

let%expect_test "instances of a scheme get the presences already known" =
  let fresh = Fresh.create () in
  let l1, l2, u, instance = bounded_through_generalized fresh in
  force fresh u ~to_:Absent;
  print_presences [ l1; l2; u; instance () ];
  let l1, l2, u, instance = bounded_through_generalized fresh in
  force fresh l2 ~to_:Present;
  print_presences [ l1; l2; u; instance () ];
  [%expect
    {|
    <int>{x?'_a : immut int} * <int>{x?'_b : immut int} * <int>{x?'_c : immut int} where '_a <= '_d, '_b <= '_e, '_d <= '_f, '_e <= '_c, '_e <= '_f, '_f <= '_e
    <int>{x?'_a : immut int} where '_a <= '_b, '_b <= '_d, '_c <= '_a, '_d <= '_e, '_d <= '_b, '_f <= '_d
    <int>{x?'a : immut int} where 'a <= 'b, '_c <= 'a, 'b <= 'd, '_c <= '_e, 'd <= '_e, 'd <= 'b, '_f <= 'd, '_f <= '_e
    <int>{x?'_a : immut int} * <int>{x?'_b : immut int} * <int>{x?'_c : immut int} where '_a <= '_c, '_b <= '_c
    (Ok ())
    <int>{} * <int>{} * <int>{} * <int>{}
    <int>{x?'_a : immut int} * <int>{x?'_b : immut int} * <int>{x?'_c : immut int} where '_a <= '_d, '_b <= '_e, '_d <= '_f, '_e <= '_c, '_e <= '_f, '_f <= '_e
    <int>{x?'_a : immut int} where '_a <= '_b, '_b <= '_d, '_c <= '_a, '_d <= '_e, '_d <= '_b, '_f <= '_d
    <int>{x?'a : immut int} where 'a <= 'b, '_c <= 'a, 'b <= 'd, '_c <= '_e, 'd <= '_e, 'd <= 'b, '_f <= 'd, '_f <= '_e
    <int>{x?'_a : immut int} * <int>{x?'_b : immut int} * <int>{x?'_c : immut int} where '_a <= '_c, '_b <= '_c
    (Ok ())
    <int>{x?'_a : immut int} * <int>{x : immut int} * <int>{x : immut int} * <int>{x?'_b : immut int} where '_a <= '_b
    |}]
;;

let%expect_test "bounds are kept by generalization and instantiation" =
  let fresh = Fresh.create () in
  let r = rvar fresh ~rank:2 in
  let bound = rvar fresh ~rank:2 in
  ignore
    (Unify.constrain_row fresh ~row:r ~bound ~shadowed:String.Set.empty
     : _ Result.t);
  let ty = Type.arrow (code r) (code bound) in
  let print ty = print_endline (Type.to_string_hum ty) in
  print ty;
  Scheme.generalize fresh ~rank:1 [ ty ];
  print ty;
  [%expect
    {|
    <int>{..'_a} -> <int>{..'_b} where ..'_a <= {..'_b}
    <int>{..'a} -> <int>{..'b} where ..'a <= {..'b}
    |}]
;;

let%expect_test "bounds don't lower ranks" =
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
    (RVar (Unbound 1 2))
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
    <int>{x : immut int; ..'_a} * <int>{x : immut int; ..'_b} * <int>{x : immut int; ..'_c} where ..'_a <= {x : immut int; ..'_b}, ..'_b <= {x : immut int; ..'_c}, ..'_c <= {x : immut int; ..'_a}
    (Ok ())
    <int>{x : immut int; y : immut bool; ..'_a} * <int>{x : immut int; y : immut bool; ..'_b} * <int>{x : immut int; y : immut bool; ..'_c} where ..'_a <= {x : immut int; y : immut bool; ..'_b}, ..'_b <= {x : immut int; y : immut bool; ..'_c}, ..'_c <= {x : immut int; y : immut bool; ..'_a}
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Ok ())
    (Ok ())
    (Ok ())
    (Ok ())
    <int>{..'_a} * <int>{..'_b} * <int>{} where ..'_a <= {..'_b}, ..'_b <= {}
    (Error
     (Row_mismatch (RExtend x Present (TCon int ()) (RVar (Unbound 27 1)))
      (RVar
       (Link
        (RExtend x (PVar (Link Absent)) (TVar (Unbound 30 1))
         (RVar (Unbound 31 1)))))
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
    <int>{..'_a} where ..'_a <= {x : immut int; ..'_a}
    (Ok ())
    <int>{y : immut bool; ..'_a} where ..'_a <= {x : immut int; y : immut bool; ..'_a}
    (Ok ())
    <int>{x : immut int; y : immut bool}
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
        (RVar (Unbound 6 1))))))
    (Ok ())
    (RExtend x Present (TCon int ())
     (RVar
      (Link
       (RExtend y (PVar (Link Present)) (TVar (Link (TCon bool ())))
        (RVar (Unbound 6 1))))))
    |}]
;;

let print_rows rows =
  print_endline (Type.to_string_hum (TTuple (List.map rows ~f:code)))
;;

let print_in_bounds_of (var : _ Var.t) =
  print_s
    [%sexp
      (List.map var.in_bounds_of ~f:(function
         | Row_var var -> var.id
         | Presence_var var -> var.id)
       : int list)]
;;

let%expect_test "a row bound is recorded on the variables in it" =
  let fresh = Fresh.create () in
  let o = Fresh.var fresh ~rank:1 in
  let a = Fresh.var fresh ~rank:2 in
  let g = Fresh.var fresh ~rank:2 in
  constrain fresh ~row:(RVar o) ~bound:(ext "x" int (RVar g) ~presence:(PVar a));
  print_s [%sexp (o.id : int)];
  print_in_bounds_of a;
  print_in_bounds_of g;
  (* Binding a variable passes its back references on. *)
  let g' = Fresh.var fresh ~rank:2 in
  unify_row fresh ~row1:(RVar g) ~row2:(ext "y" int (RVar g'));
  print_in_bounds_of g';
  let a' = Fresh.var fresh ~rank:2 in
  force fresh (PVar a) ~to_:(PVar a');
  print_in_bounds_of a';
  [%expect
    {|
    (Ok ())
    0
    (0)
    (0)
    (Ok ())
    (0)
    (Ok ())
    (0)
    |}]
;;

let context_of = function
  | TCode { context; _ } -> context
  | ty -> raise_s [%message "not a code type" (ty : ty)]
;;

let%expect_test "a scheme keeps the bounds of outer rows to its variables" =
  let fresh = Fresh.create () in
  let o = rvar fresh ~rank:1 in
  let a = pvar fresh ~rank:2 in
  let g = rvar fresh ~rank:2 in
  constrain fresh ~row:o ~bound:(ext "x" int g ~presence:a);
  let ty = code (ext "x" int g ~presence:a) in
  Scheme.generalize fresh ~rank:1 [ ty ];
  print_endline (Type.to_string_hum ty);
  let instance () = context_of (Scheme.instantiate fresh ~rank:1 ty) in
  let i1 = instance () in
  let i2 = instance () in
  print_rows [ o; i1; i2 ];
  (* Each instance gets what [o] uses, and so does a later one. *)
  unify_row fresh ~row1:o ~row2:(row [ "x", int ] ~tail:(rvar fresh ~rank:1));
  print_rows [ o; i1; i2; instance () ];
  [%expect
    {|
    (Ok ())
    <int>{x?'a : immut int; ..'b} where ..'_c <= {x?'a : immut int; ..'b}, ..'_c <= {x?'_d : immut int; ..'_e}
    <int>{..'_a} * <int>{x?'_b : immut int; ..'_c} * <int>{x?'_d : immut int; ..'_e} where ..'_a <= {x?'_d : immut int; ..'_e}, ..'_a <= {x?'_b : immut int; ..'_c}, ..'_a <= {x?'_f : immut int; ..'_g}
    (Ok ())
    <int>{x : immut int; ..'_a} * <int>{x : immut int; ..'_b} * <int>{x : immut int; ..'_c} * <int>{x : immut int; ..'_d} where ..'_a <= {x : immut int; ..'_d}, ..'_a <= {x : immut int; ..'_e}, ..'_a <= {x : immut int; ..'_b}, ..'_a <= {x : immut int; ..'_c}
    |}]
;;

let%expect_test "generalization adds the outside bounds implied through a \
                 scheme"
  =
  let fresh = Fresh.create () in
  let o1 = rvar fresh ~rank:1 in
  let o2 = rvar fresh ~rank:1 in
  let g = rvar fresh ~rank:2 in
  constrain fresh ~row:o1 ~bound:g;
  constrain fresh ~row:g ~bound:o2;
  let ty = code g in
  Scheme.generalize fresh ~rank:1 [ ty ];
  print_endline (Type.to_string_hum ty);
  print_rows [ o1; o2 ];
  unify_row fresh ~row1:o1 ~row2:(row [ "y", int ] ~tail:(rvar fresh ~rank:1));
  print_rows [ o1; o2 ];
  unify_row fresh ~row1:o2 ~row2:REmpty;
  [%expect
    {|
    (Ok ())
    (Ok ())
    <int>{..'a} where ..'a <= {..'_b}, ..'_c <= {..'a}, ..'_c <= {..'_d}, ..'_d <= {..'_b}
    <int>{..'_a} * <int>{..'_b} where ..'_a <= {..'_c}, ..'_c <= {..'_b}
    (Ok ())
    <int>{y : immut int; ..'_a} * <int>{y : mut?'_b int; ..'_c} where ..'_a <= {y : int; ..'_d}, ..'_d <= {y : mut?'_b int; ..'_c}, '_e <= '_b
    (Error
     (Row_mismatch
      (RExtend y (PVar (Link Present)) (TVar (Link (TCon int ())))
       (RVar (Unbound 13 1)))
      REmpty (Missing_label y)))
    |}]
;;

let%expect_test "a bound of a variable out of scope is left in place" =
  let fresh = Fresh.create () in
  let dead = rvar fresh ~rank:2 in
  let g = rvar fresh ~rank:2 in
  constrain fresh ~row:dead ~bound:g;
  let ty = code g in
  Scheme.generalize fresh ~rank:1 [ ty ];
  print_endline (Type.to_string_hum ty);
  print_rows [ dead ];
  [%expect
    {|
    (Ok ())
    <int>{..'a}
    <int>{..'_a} where ..'_a <= {..'b}
    |}]
;;

let%expect_test "same_bound compares structure up to links, conservatively" =
  let fresh = Fresh.create () in
  let p = Fresh.var fresh ~rank:1 in
  let q = Fresh.var fresh ~rank:1 in
  let a = tvar fresh ~rank:1 in
  let tail = rvar fresh ~rank:1 in
  let linked_tail = RVar { (Fresh.var fresh ~rank:1) with state = Link tail } in
  let record = Fresh.record fresh (row [ "x", int ]) in
  let bound ?(shadowed = []) row =
    { row; shadowed = String.Set.of_list shadowed }
  in
  let x_y ?(px = PVar p) ?(tx = int) ?(rest = tail) () =
    ext "x" tx ~presence:px (ext "y" bool rest)
  in
  let cases =
    [ "same", bound (x_y ()), bound (x_y ())
    ; "tail through a link", bound (x_y ()), bound (x_y ~rest:linked_tail ())
    ; ( "presence through a link"
      , bound (x_y ())
      , bound
          (x_y
             ~px:(PVar { (Fresh.var fresh ~rank:1) with state = Link (PVar p) })
             ()) )
    ; ( "entries in another order"
      , bound (x_y ())
      , bound (ext "y" bool (ext "x" int ~presence:(PVar p) tail)) )
    ; "other presence variable", bound (x_y ()), bound (x_y ~px:(PVar q) ())
    ; "known presence", bound (x_y ()), bound (x_y ~px:Present ())
    ; "other entry type", bound (x_y ()), bound (x_y ~tx:a ())
    ; "other tail", bound (x_y ()), bound (x_y ~rest:REmpty ())
    ; "other shadowed", bound (x_y ()), bound ~shadowed:[ "x" ] (x_y ())
    ; "same record", bound (x_y ~tx:record ()), bound (x_y ~tx:record ())
    ; ( "equal records"
      , bound (x_y ~tx:record ())
      , bound (x_y ~tx:(Fresh.record fresh (row [ "x", int ])) ()) )
    ]
  in
  List.iter cases ~f:(fun (name, bound1, bound2) ->
    print_s
      [%sexp
        (name : string)
        , (Type.same_bound bound1 bound2 : bool)
        , (Type.same_bound bound2 bound1 : bool)]);
  (* [same_entry_ty] only decides entries of different types. *)
  let calls = ref [] in
  let same_entry_ty presence ty1 ty2 =
    calls := [%sexp (presence : presence), (ty1 : ty), (ty2 : ty)] :: !calls;
    true
  in
  let other_type =
    Type.same_bound ~same_entry_ty (bound (x_y ())) (bound (x_y ~tx:a ()))
  in
  let other_presence =
    Type.same_bound
      ~same_entry_ty
      (bound (x_y ()))
      (bound (x_y ~px:(PVar q) ~tx:a ()))
  in
  print_s
    [%sexp
      (other_type : bool)
      , (other_presence : bool)
      , (List.rev !calls : Sexp.t list)];
  [%expect
    {|
    (same true true)
    ("tail through a link" true true)
    ("presence through a link" true true)
    ("entries in another order" false false)
    ("other presence variable" false false)
    ("known presence" false false)
    ("other entry type" false false)
    ("other tail" false false)
    ("other shadowed" false false)
    ("same record" true true)
    ("equal records" false false)
    (true false (((PVar (Unbound 0 1)) (TCon int ()) (TVar (Unbound 2 1)))))
    |}]
;;

let%expect_test "iteration past the first steps visits cycles once and reports \
                 unguarded uses"
  =
  let fresh = Fresh.create () in
  let x = Fresh.var fresh ~rank:1 in
  let chain =
    List.fold (List.init 100 ~f:Fn.id) ~init:(TVar x) ~f:(fun target _ ->
      TVar { (Fresh.var fresh ~rank:1) with state = Link target })
  in
  let back = Fresh.var fresh ~rank:1 in
  let cyclic = Fresh.record fresh (row [ "self", TVar back; "x", chain ]) in
  back.state <- Link cyclic;
  let calls = ref [] in
  let add ~guarded (var : _ Var.t) =
    calls := (var.id, guarded) :: !calls;
    Ok ()
  in
  (match
     (Type.iter_ty
        { ty = add; row = add; presence = add; mode = (fun _ -> Ok ()) }
        (TTuple [ cyclic; chain ])
      : (unit, Nothing.t) Result.t)
   with
   | Ok () -> ()
   | Error _ -> .);
  print_s [%sexp (x.id : int), (List.rev !calls : (int * bool) list)];
  [%expect {| (0 ((0 true) (0 false))) |}]
;;

let%expect_test "a reopened scheme's presences are bounded by outside ones \
                 again"
  =
  let fresh = Fresh.create () in
  let outer = pvar fresh ~rank:1 in
  let p = pvar fresh ~rank:2 in
  at_most fresh ~presence:outer ~bound:p;
  let scheme = code (ext "x" int REmpty ~presence:p) in
  Scheme.generalize fresh ~rank:1 [ scheme ];
  print_presences [ p; outer ];
  print_s
    [%sexp
      (Scheme.reopen fresh ~rank:2 [ scheme ]
       : (unit, Unification_error.t) Result.t)];
  (* What [outer] becomes flows into the reopened [p]. *)
  at_most fresh ~presence:Present ~bound:outer;
  print_presences [ p; outer ];
  [%expect
    {|
    (Ok ())
    <int>{x?'a : immut int} * <int>{x?'_b : immut int} where '_b <= 'a
    (Ok ())
    (Ok ())
    <int>{x : immut int} * <int>{x : immut int}
    |}]
;;

let%expect_test "reopening errs if a kept bound no longer holds" =
  let fresh = Fresh.create () in
  let outer = rvar fresh ~rank:1 in
  let g = row [ "x", bool ] ~tail:(rvar fresh ~rank:2) in
  constrain fresh ~row:outer ~bound:g;
  let scheme = code g in
  Scheme.generalize fresh ~rank:1 [ scheme ];
  unify_row
    fresh
    ~row1:outer
    ~row2:(row [ "x", int ] ~tail:(rvar fresh ~rank:1));
  print_s
    [%sexp
      (Scheme.reopen fresh ~rank:2 [ scheme ]
       : (unit, Unification_error.t) Result.t)];
  [%expect
    {|
    (Ok ())
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "a reopened scheme is refined in place" =
  let fresh = Fresh.create () in
  let a = tvar fresh ~rank:2 in
  let outer = rvar fresh ~rank:1 in
  let g = rvar fresh ~rank:2 in
  constrain fresh ~row:outer ~bound:g;
  let scheme = TTuple [ a; a; code g ] in
  let both = TTuple [ scheme; code outer ] in
  let show () = print_endline (Type.to_string_hum both) in
  show ();
  Scheme.generalize fresh ~rank:1 [ scheme ];
  show ();
  let before = Scheme.instantiate fresh ~rank:2 scheme in
  print_s
    [%sexp
      (Scheme.reopen fresh ~rank:2 [ scheme ]
       : (unit, Unification_error.t) Result.t)];
  (* The bound of [outer] the scheme kept holds again. *)
  show ();
  unify_ty fresh ~ty1:a ~ty2:int;
  Scheme.generalize fresh ~rank:1 [ scheme ];
  show ();
  (* An instance made before is unaffected. *)
  print_endline (Type.to_string_hum (TTuple [ before; code outer ]));
  [%expect
    {|
    (Ok ())
    ('_a * '_a * <int>{..'_b}) * <int>{..'_c} where ..'_c <= {..'_b}
    ('a * 'a * <int>{..'b}) * <int>{..'_c} where ..'_c <= {..'b}
    (Ok ())
    ('_a * '_a * <int>{..'_b}) * <int>{..'_c} where ..'_c <= {..'_b}, ..'_c <= {..'_d}
    (Ok ())
    (int * int * <int>{..'a}) * <int>{..'_b} where ..'_b <= {..'a}, ..'_b <= {..'_c}
    ('_a * '_a * <int>{..'_b}) * <int>{..'_c} where ..'_c <= {..'_b}
    |}]
;;
