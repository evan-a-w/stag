open! Core
open Stag
open Syntax
open Helpers

(* Line [n] of [t], so a location in the output names the node built with
   [at n]. *)
let at line txt =
  let pos col = { Loc.Pos.line; col; offset = 0 } in
  Located.create ~loc:{ Loc.file = "t"; start = pos 0; stop = pos 1 } txt
;;

let path_at line name = at line (path name)
let e line desc : Expr.t = at line desc
let p line desc : Pattern.t = at line desc
let ty line desc : Type_expr.t = at line desc
let row line desc : Type_expr.row = at line desc
let int_at line n = e line (Lit (Int n))
let bool_at line b = e line (Lit (Bool b))
let var_at line name = e line (Var (path name, None))
let app_at line f x = e line (App (f, x))
let con_at line ?(args = []) name = ty line (Con (path_at line name, args))
let int_ty line = con_at line "int"

let top_at line name expr =
  at line (Toplevel.Let (at line name, [], None, expr))
;;

let type_at line name ?(params = []) kind =
  at line (Toplevel.Type { name = at line name; params; kind })
;;

let check_errors cases = List.iter cases ~f:check

let%expect_test "values" =
  check_errors
    [ [ top_at 1 "x" (var_at 2 "unbound") ]
    ; [ top_at 1 "x" (app_at 2 (var_at 3 "id") (var_at 4 "M.y")) ]
    ; (* Unification errors are at the expression checked against a type. *)
      [ top_at
          1
          "x"
          (app_at 2 (app_at 3 (var_at 4 "add") (bool_at 5 true)) (int_at 6 1))
      ]
    ; [ top_at 1 "x" (app_at 2 (int_at 3 1) (int_at 4 2)) ]
    ; [ at 1 (Toplevel.Let (at 2 "x", [], Some (int_ty 3), bool_at 4 true)) ]
    ; [ top_at
          1
          "x"
          (e
             2
             (Var (path "id", Some (ty 3 (Arrow (int_ty 4, con_at 5 "bool"))))))
      ]
    ; [ top_at 1 "x" (e 2 (Var (path "id", Some (con_at 3 "unbound")))) ]
    ; [ top_at 1 "x" (int_at 2 1)
      ; top_at 3 "y" (int_at 4 2)
      ; top_at 5 "x" (int_at 6 3)
      ]
    ; [ top_at
          1
          "x"
          (e
             2
             (Let
                ( at 3 "f"
                , [ at 4 "a", Some (con_at 5 "nope") ]
                , None
                , int_at 6 1
                , int_at 7 1 )))
      ]
    ; [ top_at
          1
          "x"
          (e
             2
             (Let (at 3 "f", [], Some (con_at 4 "bool"), int_at 5 1, int_at 6 1)))
      ]
    ];
  [%expect
    {|
    ((Not_in_scope unbound) @ t:2:0-1)
    ((Unbound_module M) @ t:4:0-1)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t:5:0-1)
    ((Unification_error
      (Type_mismatch (TArrow (TVar (Unbound 18 1)) (TVar (Unbound 19 1)))
       (TCon int ()) Structural_mismatch))
     @ t:3:0-1)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t:4:0-1)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t:2:0-1)
    ((Unbound_type unbound) @ t:3:0-1)
    ((Duplicate_toplevel x) @ t:5:0-1)
    ((Unbound_type nope) @ t:5:0-1)
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t:5:0-1)
    |}]
;;

let%expect_test "nominal types" =
  let decls =
    [ type_at 10 "t" (Variant [ at 11 "A", []; at 12 "B", [ int_ty 13 ] ])
    ; type_at 20 "r" (Record [ at 21 "x", int_ty 22; at 23 "y", int_ty 24 ])
    ; type_at 30 "s" (Record [ at 31 "z", int_ty 32 ])
    ]
  in
  check_errors
    (List.map
       ~f:(fun expr -> decls @ [ top_at 1 "v" expr ])
       [ e 2 (Construct (path_at 3 "C", []))
       ; e 2 (Construct (path_at 3 "M.C", []))
       ; e 2 (Construct (path_at 3 "B", []))
       ; e 2 (Construct (path_at 3 "B", [ bool_at 4 true ]))
       ; e 2 (Record [ path_at 3 "x", int_at 4 1; path_at 5 "w", int_at 6 2 ])
       ; e 2 (Record [ path_at 3 "x", int_at 4 1; path_at 5 "z", int_at 6 2 ])
       ; e
           2
           (Record
              [ path_at 3 "x", int_at 4 1
              ; path_at 5 "y", int_at 6 2
              ; path_at 7 "x", int_at 8 3
              ])
       ; e 2 (Record [ path_at 3 "x", int_at 4 1 ])
       ; e
           2
           (Record [ path_at 3 "x", int_at 4 1; path_at 5 "y", bool_at 6 true ])
       ; e 2 (Record [])
       ; e 2 (Field (int_at 3 1, path_at 4 "x"))
       ; e 2 (Field (int_at 3 1, path_at 4 "w"))
       ]);
  [%expect
    {|
    ((Unbound_constructor C) @ t:3:0-1)
    ((Unbound_module M) @ t:3:0-1)
    ((Constructor_arity_mismatch (constructor B) (expected 1) (got 0)) @ t:2:0-1)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t:4:0-1)
    ((Unbound_field w) @ t:5:0-1)
    ((Fields_of_different_types (field z) (expected_type r) (actual_type s)) @
     t:5:0-1)
    ((Duplicate_field x) @ t:7:0-1)
    ((Missing_field y) @ t:2:0-1)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t:6:0-1)
    (Empty_record @ t:2:0-1)
    ((Unification_error
      (Type_mismatch (TCon r ()) (TCon int ()) (Base_types_not_equal r int)))
     @ t:3:0-1)
    ((Unbound_field w) @ t:4:0-1)
    |}]
;;

let%expect_test "patterns" =
  let decls =
    [ type_at 10 "t" (Variant [ at 11 "A", []; at 12 "B", [ int_ty 13 ] ]) ]
  in
  let match_ cases = e 2 (Match (int_at 3 1, cases)) in
  check_errors
    (List.map
       ~f:(fun expr -> decls @ [ top_at 1 "v" expr ])
       [ match_ [ p 4 (Lit (Bool true)), int_at 5 1 ]
       ; match_ [ p 4 (Construct (path_at 5 "C", [])), int_at 6 1 ]
       ; match_ [ p 4 (Construct (path_at 5 "B", [])), int_at 6 1 ]
       ; match_ [ p 4 (Record [ path_at 5 "x", p 6 Any ]), int_at 7 1 ]
       ; match_ [ p 4 (Tuple [ p 5 (Var "a"); p 6 (Var "a") ]), int_at 7 1 ]
       ; match_
           [ ( p
                 4
                 (Poly_record
                    { fields = [ at 5 "x", p 6 Any; at 7 "x", p 8 Any ]
                    ; rest = Closed
                    })
             , int_at 9 1 )
           ]
       ; match_ [ p 4 Any, int_at 5 1; p 6 Any, bool_at 7 true ]
       ; (* Closing the scrutinee to the tags of the cases. *)
         match_ [ p 4 (Poly_variant (at 5 "A", None)), int_at 6 1 ]
       ]);
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t:4:0-1)
    ((Unbound_constructor C) @ t:5:0-1)
    ((Constructor_arity_mismatch (constructor B) (expected 1) (got 0)) @ t:4:0-1)
    ((Unbound_field x) @ t:5:0-1)
    ((Duplicate_pattern_variable a) @ t:6:0-1)
    ((Duplicate_row_entry x) @ t:7:0-1)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t:7:0-1)
    ((Unification_error
      (Type_mismatch (TCon int ())
       (TVariant (RExtend A Present (TCon unit ()) (RVar (Unbound 19 1))))
       Structural_mismatch))
     @ t:4:0-1)
    |}]
;;

let%expect_test "polymorphic records and variants" =
  check_errors
    (List.map
       ~f:(fun expr -> [ top_at 1 "v" expr ])
       [ e
           2
           (Poly_record
              [ at 3 "x", int_at 4 1
              ; at 5 "y", int_at 6 1
              ; at 7 "x", int_at 8 1
              ])
       ; e 2 (Poly_field (int_at 3 1, at 4 "x"))
       ; e 2 (Poly_field (e 3 (Poly_record [ at 4 "y", int_at 5 1 ]), at 6 "x"))
       ; e 2 (Poly_restrict (int_at 3 1, at 4 "x"))
       ; e
           2
           (Poly_extend
              ([ at 3 "x", int_at 4 1; at 5 "x", int_at 6 1 ], int_at 7 1))
       ; e 2 (Poly_extend ([ at 3 "x", int_at 4 1 ], int_at 5 1))
       ; e 2 (Poly_update (int_at 3 1, [ at 4 "x", int_at 5 1 ]))
       ; e
           2
           (Poly_update
              (int_at 3 1, [ at 4 "x", int_at 5 1; at 6 "x", int_at 7 1 ]))
       ; app_at
           2
           (int_at 3 1)
           (e 4 (Poly_variant (at 5 "A", Some (int_at 6 1))))
       ]);
  [%expect
    {|
    ((Duplicate_row_entry x) @ t:7:0-1)
    ((Unification_error
      (Type_mismatch
       (TRecord (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))))
       (TCon int ()) Structural_mismatch))
     @ t:3:0-1)
    ((Unification_error
      (Row_mismatch
       (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1)))
       (RExtend y Present (TCon int ()) REmpty) (Missing_label x)))
     @ t:3:0-1)
    ((Unification_error
      (Type_mismatch
       (TRecord (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))))
       (TCon int ()) Structural_mismatch))
     @ t:3:0-1)
    ((Duplicate_row_entry x) @ t:5:0-1)
    ((Unification_error
      (Type_mismatch (TRecord (RVar (Unbound 18 1))) (TCon int ())
       Structural_mismatch))
     @ t:5:0-1)
    ((Unification_error
      (Type_mismatch
       (TRecord (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))))
       (TCon int ()) Structural_mismatch))
     @ t:3:0-1)
    ((Duplicate_row_entry x) @ t:6:0-1)
    ((Unification_error
      (Type_mismatch (TArrow (TVar (Unbound 18 1)) (TVar (Unbound 19 1)))
       (TCon int ()) Structural_mismatch))
     @ t:3:0-1)
    |}]
;;

let%expect_test "type expressions" =
  let annotated annot =
    [ at 1 (Toplevel.Let (at 2 "x", [], Some annot, var_at 3 "x")) ]
  in
  let list =
    type_at 10 "list" ~params:[ at 11 (Type_decl.Param.Ty "a") ] Abstract
  in
  let rows =
    type_at 20 "rows" ~params:[ at 21 (Type_decl.Param.Row "r") ] Abstract
  in
  check_errors
    [ annotated (con_at 4 "unbound")
    ; annotated (ty 4 (Con (path_at 5 "M.t", [])))
    ; list :: annotated (ty 4 (Con (path_at 5 "list", [])))
    ; list :: annotated (ty 4 (Con (path_at 5 "list", [ Row (row 6 Empty) ])))
    ; rows :: annotated (ty 4 (Con (path_at 5 "rows", [ Ty (int_ty 6) ])))
    ; annotated
        (ty
           4
           (Record
              (row
                 5
                 (Extend
                    ( at 6 "x"
                    , int_ty 7
                    , row 8 (Extend (at 9 "x", int_ty 10, row 11 Empty)) )))))
    ];
  [%expect
    {|
    ((Unbound_type unbound) @ t:4:0-1)
    ((Unbound_module M) @ t:5:0-1)
    ((Type_arity_mismatch (type_name list) (expected 1) (got 0)) @ t:4:0-1)
    type list['a]
    val x : list[{}]
    ((Type_arg_kind_mismatch (type_name rows) (param (Row r))) @ t:6:0-1)
    ((Duplicate_row_entry x) @ t:9:0-1)
    |}]
;;

let%expect_test "declarations" =
  check_errors
    [ [ type_at 1 "t" (Alias (ty 2 (Var "a"))) ]
    ; [ type_at 1 "t" (Alias (ty 2 (Record (row 3 (Var (at 4 "r")))))) ]
    ; [ type_at
          1
          "t"
          ~params:[ at 2 (Type_decl.Param.Ty "a") ]
          (Alias (ty 3 (Record (row 4 (Var (at 5 "a"))))))
      ]
    ; [ type_at
          1
          "t"
          ~params:[ at 2 (Type_decl.Param.Row "r") ]
          (Alias (ty 3 (Var "r")))
      ]
    ; [ type_at 1 "t" Abstract; type_at 2 "u" Abstract; type_at 3 "t" Abstract ]
    ; [ type_at 1 "t" (Variant [ at 2 "A", [] ])
      ; type_at 3 "u" (Variant [ at 4 "B", []; at 5 "A", [] ])
      ]
    ; [ type_at 1 "t" (Record [ at 2 "x", int_ty 3; at 4 "x", int_ty 5 ]) ]
    ; [ type_at
          1
          "t"
          ~params:
            [ at 2 (Type_decl.Param.Ty "a")
            ; at 3 (Type_decl.Param.Ty "b")
            ; at 4 (Type_decl.Param.Row "a")
            ]
          Abstract
      ]
    ; [ type_at 1 "t" (Variant [ at 2 "A", [ con_at 3 "unbound" ] ]) ]
    ; [ type_at 1 "t" (Alias (ty 2 (Tuple [ con_at 3 "t"; int_ty 4 ]))) ]
    ; [ type_at 1 "u" (Alias (con_at 2 "t"))
      ; type_at 3 "t" (Alias (ty 4 (Tuple [ con_at 5 "u"; int_ty 6 ])))
      ]
    ; [ type_at
          1
          "t"
          ~params:[ at 2 (Type_decl.Param.Ty "a") ]
          (Alias
             (ty
                3
                (Arrow
                   ( ty 4 (Var "a")
                   , ty
                       5
                       (Con
                          ( path_at 6 "t"
                          , [ Ty
                                (ty
                                   7
                                   (Tuple [ ty 8 (Var "a"); ty 9 (Var "a") ]))
                            ] )) ))))
      ]
    ; [ at 1 (Toplevel.Module (at 2 "M", []))
      ; at 3 (Toplevel.Module (at 4 "N", []))
      ; at 5 (Toplevel.Module (at 6 "M", []))
      ]
    ; [ at
          1
          (Toplevel.Module
             (at 2 "M", [ top_at 3 "x" (int_at 4 1); top_at 5 "x" (int_at 6 1) ]))
      ]
    ];
  [%expect
    {|
    ((Unbound_type_variable a) @ t:2:0-1)
    ((Unbound_type_variable r) @ t:4:0-1)
    ((Type_variable_kind_mismatch a) @ t:5:0-1)
    ((Type_variable_kind_mismatch r) @ t:3:0-1)
    ((Duplicate_type t) @ t:3:0-1)
    ((Duplicate_constructor A) @ t:5:0-1)
    ((Duplicate_field x) @ t:4:0-1)
    ((Duplicate_type_parameter (type_name t) (param a)) @ t:4:0-1)
    ((Unbound_type unbound) @ t:3:0-1)
    ((Cyclic_type_alias t) @ t:1:0-1)
    ((Cyclic_type_alias u) @ t:1:0-1)
    ((Non_regular_recursive_alias t) @ t:5:0-1)
    ((Duplicate_module M) @ t:6:0-1)
    ((Duplicate_toplevel M.x) @ t:5:0-1)
    |}]
;;

let%expect_test "staging" =
  let splice ?subs line expr = e line (Splice { expr; subs }) in
  let quote line expr = e line (Quote expr) in
  check_errors
    [ [ top_at
          1
          "x"
          (quote
             2
             (e
                3
                (Let (at 4 "y", [], None, int_at 5 1, splice 6 (var_at 7 "y")))))
      ]
    ; [ top_at 1 "x" (splice 2 (var_at 3 "x")) ]
    ; [ top_at 1 "f" (splice 2 (app_at 3 (var_at 4 "id") (var_at 5 "f"))) ]
    ; [ top_at 1 "x" (splice 2 (int_at 3 1)) ]
    ; [ top_at 1 "x" (splice 2 (quote 3 (var_at 4 "y"))) ]
    ; [ top_at
          1
          "x"
          (quote
             2
             (splice
                3
                ~subs:
                  [ at 4 "a", Some (int_at 5 1)
                  ; at 6 "b", None
                  ; at 7 "a", None
                  ]
                (quote 8 (int_at 9 1))))
      ]
    ; [ top_at 1 "x" (splice 2 ~subs:[ at 3 "a", None ] (quote 4 (int_at 5 1)))
      ]
    ; [ top_at
          1
          "x"
          (splice
             2
             (e
                3
                (Match (int_at 4 1, [ p 5 (Lit (Int 2)), quote 6 (int_at 7 1) ]))))
      ]
    ];
  [%expect
    {|
    ((Variable_used_at_wrong_level (name y) (var_level 1) (used_at 0)) @ t:7:0-1)
    ((Compile_time_use_of_toplevel_being_checked x) @ t:3:0-1)
    ((Compile_time_use_of_toplevel_being_checked f) @ t:5:0-1)
    ((Unification_error
      (Type_mismatch
       (TCode (inner (TVar (Unbound 18 1))) (context (RVar (Unbound 19 1))))
       (TCon int ()) Structural_mismatch))
     @ t:3:0-1)
    ((Not_in_scope y) @ t:2:0-1)
    ((Duplicate_substitution a) @ t:7:0-1)
    ((Not_in_scope a) @ t:3:0-1)
    ((Splice_evaluation_failed
      (code
       (Match (Lit (Int 1) : int)
        (((Lit (Int 2) : int) (Quote (Lit (Int 1) : int) : <int>{}))) : <int>{}))
      (error ("Match failure: no case matches the value 1" @ t:4:0-1)))
     @ t:2:0-1)
    |}]
;;

(* The location of each node of the body of the last toplevel, and of its
   children. *)
let rec loc_tree (expr : Typed.Expr.t) =
  let head =
    match [%sexp (expr.desc : Typed.Expr.t Typed.Expr.Desc.t)] with
    | List (Atom head :: _) | Atom head -> head
    | List _ -> "?"
  in
  Sexp.List
    (Atom head
     :: Atom (Loc.to_string expr.loc)
     :: List.map (Typed.Expr.sub_exprs expr) ~f:loc_tree)
;;

let print_loc_trees program =
  match Typecheck.typecheck (typechecker ()) ~program:(prims @ program) with
  | Error error -> print_s [%sexp (error : Typecheck_error.t)]
  | Ok { program; _ } ->
    let last = List.last_exn program.sccs in
    List.iter last.members ~f:(fun (toplevel : Typed.Toplevel.t) ->
      print_s (loc_tree toplevel.body))
;;

let%expect_test "typed nodes keep their syntax's locations" =
  print_loc_trees
    [ top_at
        1
        "x"
        (e
           2
           (Let
              ( at 3 "y"
              , []
              , None
              , int_at 4 1
              , e
                  5
                  (Match
                     ( var_at 6 "y"
                     , [ p 7 Any, e 8 (Tuple [ var_at 9 "y"; bool_at 10 true ])
                       ] )) )))
    ];
  (* Generated code keeps the locations of the quote it came from; lets binding
     explicit substitutions are at the splice. *)
  print_loc_trees
    [ top_at
        1
        "x"
        (e
           2
           (Splice
              { expr = e 3 (Quote (app_at 4 (var_at 5 "a") (int_at 6 1)))
              ; subs = Some [ at 7 "a", Some (var_at 8 "id") ]
              }))
    ];
  [%expect
    {|
    (Let t:2:0-1 (Lit t:4:0-1)
     (Match t:5:0-1 (Var t:6:0-1) (Tuple t:8:0-1 (Var t:9:0-1) (Lit t:10:0-1))))
    (Toplevel_splice t:2:0-1
     (Quote t:3:0-1 (App t:4:0-1 (Var t:5:0-1) (Lit t:6:0-1)))
     (Let t:2:0-1 (Var t:8:0-1) (App t:4:0-1 (Var t:5:0-1) (Lit t:6:0-1))))
    |}]
;;
