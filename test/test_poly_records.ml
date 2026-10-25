open! Core
open Stag
open Syntax
open Helpers

let%expect_test "record operations" =
  check
    [ top "lit" [] (poly_record [ "x", int 1; "y", bool true ])
    ; top "empty" [] (poly_record [])
    ; top "get_x" [ "r" ] (dot (var "r") "x")
    ; top "extend" [ "r" ] (extend [ "z", int 1 ] (var "r"))
    ; top
        "extend_many"
        [ "r"; "a" ]
        (extend [ "y", var "a"; "z", int 1 ] (var "r"))
    ; top "restrict" [ "r" ] (restrict (var "r") "x")
    ; top "update" [ "r" ] (update (var "r") [ "x", bool true ])
    ; top "nested" [ "r" ] (dot (dot (var "r") "a") "b")
    ; top "lit_x" [] (dot (poly_record [ "x", int 1; "y", int 2 ]) "x")
    ; top
        "extend_lit"
        []
        (extend [ "z", Lit (Char 'c') ] (poly_record [ "x", int 1 ]))
    ; top
        "restrict_lit"
        []
        (restrict (poly_record [ "x", int 1; "y", int 2 ]) "x")
    ; top
        "two_fields"
        [ "r" ]
        (app (var "add") [ dot (var "r") "x"; dot (var "r") "y" ])
    ];
  [%expect
    {|
    val lit : {x : int; y : bool}
    val empty : {}
    val get_x : {x : 'a; ..'b} -> 'a
    val extend : {..'a} -> {z : int; ..'a}
    val extend_many : {..'a} -> 'b -> {y : 'b; z : int; ..'a}
    val restrict : {x : 'a; ..'b} -> {..'b}
    val update : {x : 'a; ..'b} -> {x : bool; ..'b}
    val nested : {a : {b : 'a; ..'b}; ..'c} -> 'a
    val lit_x : int
    val extend_lit : {x : int; z : char}
    val restrict_lit : {y : int}
    val two_fields : {x : int; y : int; ..'a} -> int
    |}]
;;

let%expect_test "extension shadows and restriction reveals" =
  let shadow = extend [ "x", bool true ] (poly_record [ "x", int 1 ]) in
  check
    [ top "shadow" [] shadow
    ; top "shadow_x" [] (dot shadow "x")
    ; top "revealed" [] (dot (restrict shadow "x") "x")
    ; top "update_shadow" [] (update shadow [ "x", Lit (Char 'c') ])
    ; top
        "update_shadow_inner"
        []
        (dot (restrict (update shadow [ "x", Lit (Char 'c') ]) "x") "x")
    ; top "restrict_twice" [] (restrict (restrict shadow "x") "x")
    ; top
        "extend_restrict"
        [ "r" ]
        (restrict (extend [ "x", int 1 ] (var "r")) "x")
    ; top "extend_get" [ "r" ] (dot (extend [ "x", int 1 ] (var "r")) "x")
    ; top
        "reveal"
        [ "r" ]
        (dot (restrict (extend [ "x", int 1 ] (var "r")) "x") "x")
    ];
  [%expect
    {|
    val shadow : {x : bool; x : int}
    val shadow_x : bool
    val revealed : int
    val update_shadow : {x : char; x : int}
    val update_shadow_inner : int
    val restrict_twice : {}
    val extend_restrict : {..'a} -> {..'a}
    val extend_get : {..'a} -> int
    val reveal : {x : 'a; ..'b} -> 'a
    |}]
;;

let%expect_test "update, restriction and access require the field" =
  check
    [ top
        "update_missing"
        []
        (update (poly_record [ "y", int 1 ]) [ "x", int 1 ])
    ];
  check
    [ top "restrict_missing" [] (restrict (poly_record [ "y", int 1 ]) "x") ];
  check [ top "access_missing" [] (dot (poly_record []) "x") ];
  check
    [ top "get_y" [ "r" ] (dot (var "r") "y")
    ; top "closed" [] (app (var "get_y") [ poly_record [ "x", int 1 ] ])
    ];
  check
    [ top
        "restrict_all"
        [ "r" ]
        (dot (restrict (extend [ "x", int 1 ] (poly_record [])) "x") "x")
    ];
  [%expect
    {|
    (Unification_error
     (Row_mismatch
      (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1)))
      (RExtend y Present (TCon int ()) REmpty) (Missing_label x)))
    (Unification_error
     (Row_mismatch
      (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1)))
      (RExtend y Present (TCon int ()) REmpty) (Missing_label x)))
    (Unification_error
     (Row_mismatch
      (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))) REmpty
      (Missing_label x)))
    (Unification_error
     (Row_mismatch
      (RExtend y Present (TVar (Unbound 25 1)) (RVar (Unbound 26 1)))
      (RExtend x Present (TCon int ()) REmpty) (Missing_label y)))
    (Unification_error
     (Row_mismatch
      (RExtend x Present (TVar (Unbound 19 1)) (RVar (Unbound 20 1))) REmpty
      (Missing_label x)))
    |}]
;;

let%expect_test "update may change field types" =
  check
    [ top
        "changed"
        []
        (update
           (poly_record [ "x", int 1; "y", int 2; "z", int 3 ])
           [ "x", bool true; "y", Lit (String "s") ])
    ; top "update_fn" [ "r"; "v" ] (update (var "r") [ "x", var "v" ])
    ];
  [%expect
    {|
    val changed : {x : bool; y : string; z : int}
    val update_fn : {x : 'a; ..'b} -> 'c -> {x : 'c; ..'b}
    |}]
;;

let%expect_test "duplicate labels" =
  check
    [ top "literal" [] (poly_record [ "x", int 1; "y", int 2; "x", int 2 ]) ];
  check
    [ top "extension" [ "r" ] (extend [ "x", int 1; "x", int 2 ] (var "r")) ];
  check [ top "update" [ "r" ] (update (var "r") [ "x", int 1; "x", int 2 ]) ];
  check
    [ top
        "pattern"
        [ "r" ]
        (match_
           (var "r")
           [ Pat.poly_record [ "x", Pat.var "a"; "x", Pat.var "b" ], var "a" ])
    ];
  check
    [ Toplevel.Let
        ( "annotation"
        , [ typed "r" (Ty.record [ "x", Ty.int; "x", Ty.bool ]) ]
        , None
        , var "r" )
    ];
  check [ Decl.alias "t" (Ty.record [ "x", Ty.int; "x", Ty.int ] ~tail:"r") ];
  [%expect
    {|
    (Duplicate_row_entry x)
    (Duplicate_row_entry x)
    (Duplicate_row_entry x)
    (Duplicate_row_entry x)
    (Duplicate_row_entry x)
    (Duplicate_row_entry x)
    |}]
;;

let%expect_test "record patterns" =
  check
    [ top
        "closed"
        [ "r" ]
        (match_
           (var "r")
           [ ( Pat.poly_record [ "x", Pat.var "a"; "y", Pat.var "b" ]
             , app (var "add") [ var "a"; var "b" ] )
           ])
    ; top
        "open"
        [ "r" ]
        (match_
           (var "r")
           [ Pat.poly_record ~rest:Open [ "x", Pat.var "a" ], var "a" ])
    ; top
        "rest"
        [ "r" ]
        (match_
           (var "r")
           [ ( Pat.poly_record ~rest:(Bind "rest") [ "x", Pat.var "a" ]
             , tuple [ var "a"; var "rest" ] )
           ])
    ; top
        "rest_reveals_shadowed"
        []
        (match_
           (extend [ "x", bool true ] (poly_record [ "x", int 1 ]))
           [ ( Pat.poly_record ~rest:(Bind "rest") [ "x", Pat.var "a" ]
             , tuple [ var "a"; dot (var "rest") "x" ] )
           ])
    ; top
        "nested"
        [ "r" ]
        (match_
           (var "r")
           [ ( Pat.poly_record
                 ~rest:Open
                 [ "p", Pat.poly_record ~rest:Open [ "x", Pat.var "a" ] ]
             , var "a" )
           ])
    ; top "empty" [ "r" ] (match_ (var "r") [ Pat.poly_record [], int 1 ])
    ; top
        "with_literals"
        [ "r" ]
        (match_
           (var "r")
           [ Pat.poly_record ~rest:Open [ "x", Pat.int 1 ], bool true
           ; Pat.any, bool false
           ])
    ];
  check
    [ top
        "closed_mismatch"
        []
        (match_
           (poly_record [ "x", int 1; "y", int 2 ])
           [ Pat.poly_record [ "x", Pat.var "a" ], var "a" ])
    ];
  check
    [ top
        "duplicate_rest"
        [ "r" ]
        (match_
           (var "r")
           [ Pat.poly_record ~rest:(Bind "a") [ "x", Pat.var "a" ], var "a" ])
    ];
  [%expect
    {|
    val closed : {x : int; y : int} -> int
    val open : {x : 'a; ..'b} -> 'a
    val rest : {x : 'a; ..'b} -> 'a * {..'b}
    val rest_reveals_shadowed : bool * int
    val nested : {p : {x : 'a; ..'b}; ..'c} -> 'a
    val empty : {} -> int
    val with_literals : {x : int; ..'a} -> bool
    (Unification_error
     (Row_mismatch (RExtend y Present (TCon int ()) REmpty) REmpty
      (Missing_label y)))
    (Duplicate_pattern_variable a)
    |}]
;;

let%expect_test "aliases" =
  check
    [ Decl.alias "point" (Ty.record [ "x", Ty.int; "y", Ty.int ])
    ; Decl.alias
        "with_x"
        ~params:[ Decl.row "r" ]
        (Ty.record [ "x", Ty.int ] ~tail:"r")
    ; Decl.alias
        "box"
        ~params:[ Decl.ty "a" ]
        (Ty.record [ "contents", Ty.var "a" ])
    ; Decl.record "holder" [ "p", Ty.con "point" ]
    ; Toplevel.Let
        ( "origin"
        , []
        , Some (Ty.con "point")
        , poly_record [ "x", int 0; "y", int 0 ] )
    ; Toplevel.Let
        ( "get"
        , [ typed "b" (Ty.con "box" ~args:[ Ty Ty.int ]) ]
        , None
        , dot (var "b") "contents" )
    ; Toplevel.Let
        ( "x_of"
        , [ typed "r" (Ty.con "with_x" ~args:[ Row (Var "r") ]) ]
        , None
        , dot (var "r") "x" )
    ; top "x_of_origin" [] (app (var "x_of") [ var "origin" ])
    ; top "hold" [] (record [ "p", var "origin" ])
    ; module_
        "M"
        [ Decl.variant "t" [ "A", [] ]
        ; Decl.alias "r" (Ty.record [ "f", Ty.con "t" ])
        ; Decl.alias
            "v"
            ~params:[ Decl.row "r" ]
            (Ty.variant [ "A", Ty.con "t"; "B", Ty.unit ] ~tail:"r")
        ]
    ; top "m" [ "r" ] (var "r" ~annot:(Ty.con "M.r"))
    ];
  check
    [ Toplevel.Let
        ("not_a_point", [], Some (Ty.con "point"), poly_record [ "x", int 0 ])
    ; Decl.alias "point" (Ty.record [ "x", Ty.int; "y", Ty.int ])
    ];
  [%expect
    {|
    type point = {x : int; y : int}
    type with_x[{..'a}] = {x : int; ..'a}
    type box['a] = {contents : 'a}
    type holder = { p : {x : int; y : int} }
    val origin : {x : int; y : int}
    val get : {contents : int} -> int
    val x_of : {x : int; ..'a} -> int
    val x_of_origin : int
    val hold : holder
    module M : sig
      type t = A
      type r = {f : t}
      type v[{..'a}] = [`A of t | `B; ..'a]
    end
    val m : {f : M.t} -> {f : M.t}
    (Unification_error
     (Row_mismatch (RExtend y Present (TCon int ()) REmpty) REmpty
      (Missing_label y)))
    |}]
;;

let%expect_test "generalization and the value restriction" =
  check
    [ top "r" [] (poly_record [ "f", var "id" ])
    ; top "weak" [] (poly_record [ "f", app (var "id") [ var "id" ] ])
    ; top "ext" [] (extend [ "g", var "id" ] (var "r"))
    ; top "ext_weak" [] (extend [ "g", var "id" ] (app (var "id") [ var "r" ]))
    ; top "field" [] (dot (var "r") "f")
    ; top "restricted" [] (restrict (var "ext") "g")
    ; top "updated" [] (update (var "r") [ "f", poly_record [] ])
    ; top
        "used_twice"
        []
        (tuple
           [ app (dot (var "r") "f") [ int 1 ]
           ; app (dot (var "r") "f") [ bool true ]
           ])
    ; top
        "local"
        []
        (let_
           "p"
           []
           (poly_record [ "f", var "id" ])
           (tuple
              [ app (dot (var "p") "f") [ int 1 ]
              ; app (dot (var "p") "f") [ bool true ]
              ]))
    ];
  [%expect
    {|
    val r : {f : 'a -> 'a}
    val weak : {f : '_a -> '_a}
    val ext : {f : 'a -> 'a; g : 'b -> 'b}
    val ext_weak : {f : '_a -> '_a; g : '_b -> '_b}
    val field : 'a -> 'a
    val restricted : {f : 'a -> 'a}
    val updated : {f : {}}
    val used_twice : int * bool
    val local : int * bool
    |}]
;;

let%expect_test "values through the new forms" =
  check
    [ top
        "recursive"
        []
        (let_
           "p"
           []
           (poly_record [ "a", var "id"; "b", dot (var "p") "a" ])
           (var "p"))
    ; top
        "rest_binds"
        []
        (let_
           "y"
           []
           (match_
              (poly_record [ "x", var "id" ])
              [ ( Pat.poly_record ~rest:(Bind "y") [ "x", Pat.any ]
                , tuple [ var "y"; var "id" ] )
              ])
           (var "y"))
    ; top
        "tag_binds"
        []
        (let_
           "y"
           []
           (match_
              (tag "A" ~arg:(var "id"))
              [ Pat.tag "A" ~arg:(Pat.var "y"), var "y" ])
           (var "y"))
    ; top
        "splice_value"
        []
        (quote (poly_record [ "f", splice (quote (var "id")) ]))
    ; top
        "splice_not_value"
        []
        (quote
           (extend
              [ "f", splice (app (var "id") [ quote (var "id") ]) ]
              (poly_record [])))
    ];
  [%expect
    {|
    val recursive : {a : '_a -> '_a; b : '_a -> '_a}
    val rest_binds : {} * ('a -> 'a)
    val tag_binds : 'a -> 'a
    val splice_value : <{f : 'a -> 'a}>{..'b}
    val splice_not_value : <{f : '_a -> '_a}>{..'_b}
    |}]
;;

let%expect_test "labels in quotes are not free variables" =
  check
    [ top "label" [] (quote (dot (poly_record [ "x", int 1 ]) "x"))
    ; top "var_and_label" [] (quote (poly_record [ "y", var "y" ]))
    ; top
        "outside"
        []
        (let_ "r" [] (poly_record [ "x", int 1 ]) (quote (dot (var "r") "x")))
    ; top
        "extend_inside"
        [ "c" ]
        (quote (restrict (extend [ "z", int 1 ] (splice (var "c"))) "z"))
    ; top
        "pattern_rest"
        []
        (quote
           (match_
              (var "r")
              [ ( Pat.poly_record ~rest:(Bind "rest") [ "x", Pat.var "x" ]
                , tuple [ var "x"; var "rest" ] )
              ]))
    ];
  [%expect
    {|
    val label : <int>{..'a}
    val var_and_label : <{y : 'a}>{y : 'a; ..'b}
    val outside : <'a>{r : {x : 'a; ..'b}; ..'c}
    val extend_inside : <{..'a}>{..'b} -> <{..'a}>{..'c} where ..'b <= {..'c}
    val pattern_rest : <'a * {..'b}>{r : {x : 'a; ..'b}; ..'c}
    |}]
;;

let%expect_test "rows shared between contexts and records" =
  check
    [ Toplevel.Let
        ( "same_row"
        , [ typed "c" (Ty.code Ty.int ~tail:"r")
          ; typed "r" (Ty.record [] ~tail:"r")
          ]
        , None
        , var "c" )
    ; top
        "ok"
        []
        (app (var "same_row") [ quote (var "x"); poly_record [ "x", int 1 ] ])
    ; Toplevel.Let
        ( "captured"
        , [ "c", None; typed "r" (Ty.record [] ~tail:"r") ]
        , None
        , seq
            (quote
               (let_
                  "y"
                  []
                  (int 1)
                  (splice (var "c" ~annot:(Ty.code Ty.int ~tail:"r")))))
            (var "r") )
    ];
  check
    [ Toplevel.Let
        ( "same_row"
        , [ typed "c" (Ty.code Ty.int ~tail:"r")
          ; typed "r" (Ty.record [] ~tail:"r")
          ]
        , None
        , var "c" )
    ; top
        "missing"
        []
        (app (var "same_row") [ quote (var "x"); poly_record [] ])
    ];
  [%expect
    {|
    val same_row : <int>{..'a} -> {..'a} -> <int>{..'a}
    val ok : <int>{x : int}
    val captured : <int>{y?'a : int; ..'b} -> {y?'a : int; ..'b} -> {y?'a : int; ..'b} where ..'b <= {..'c}
    (Unification_error
     (Row_mismatch
      (RExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
       (RVar (Unbound 30 1)))
      REmpty (Missing_label x)))
    |}]
;;

let%expect_test "unification errors" =
  check [ top "record_vs_variant" [] (dot (tag "A") "x") ];
  check [ top "record_vs_int" [] (dot (int 1) "x") ];
  check [ top "record_vs_code" [] (restrict (quote (int 1)) "x") ];
  check
    [ top
        "field_type"
        []
        (app (var "add") [ dot (poly_record [ "x", bool true ]) "x"; int 1 ])
    ];
  check
    [ top
        "different_rows"
        [ "r" ]
        (tuple
           [ match_ (var "r") [ Pat.poly_record [ "x", Pat.any ], int 1 ]
           ; match_ (var "r") [ Pat.poly_record [ "y", Pat.any ], int 1 ]
           ])
    ];
  [%expect
    {|
    (Unification_error
     (Type_mismatch
      (TRecord (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))))
      (TVariant (RExtend A Present (TCon unit ()) (RVar (Unbound 20 1))))
      Structural_mismatch))
    (Unification_error
     (Type_mismatch
      (TRecord (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))))
      (TCon int ()) Structural_mismatch))
    (Unification_error
     (Type_mismatch
      (TRecord (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))))
      (TCode (inner (TCon int ())) (context (RVar (Unbound 20 1))))
      Structural_mismatch))
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Unification_error
     (Row_mismatch (RExtend x Present (TVar (Unbound 20 1)) REmpty)
      (RExtend y Present (TVar (Unbound 22 1)) REmpty) (Missing_label x)))
    |}]
;;

let%expect_test "local polymorphic record operations" =
  check
    [ top
        "access"
        []
        (let_
           "get"
           [ "r" ]
           (dot (var "r") "x")
           (tuple
              [ app (var "get") [ poly_record [ "x", int 1 ] ]
              ; app (var "get") [ poly_record [ "x", bool true; "y", int 1 ] ]
              ]))
    ; top
        "restriction"
        []
        (let_
           "drop"
           [ "r" ]
           (restrict (var "r") "x")
           (tuple
              [ app (var "drop") [ poly_record [ "x", int 1 ] ]
              ; app (var "drop") [ poly_record [ "x", bool true; "y", int 1 ] ]
              ]))
    ; top
        "update"
        []
        (let_
           "set"
           [ "r" ]
           (update (var "r") [ "x", int 0 ])
           (tuple
              [ app (var "set") [ poly_record [ "x", bool true ] ]
              ; app (var "set") [ poly_record [ "x", unit; "y", int 1 ] ]
              ]))
    ; top
        "pattern"
        []
        (let_
           "rest"
           [ "r" ]
           (match_
              (var "r")
              [ Pat.poly_record ~rest:(Bind "rest") [ "x", Pat.any ], var "rest"
              ])
           (tuple
              [ app (var "rest") [ poly_record [ "x", int 1 ] ]
              ; app (var "rest") [ poly_record [ "x", int 1; "y", int 1 ] ]
              ]))
    ];
  [%expect
    {|
    val access : int * bool
    val restriction : {} * {y : int}
    val update : {x : int} * {x : int; y : int}
    val pattern : {} * {y : int}
    |}]
;;

let%expect_test "applied access and restriction are not values" =
  check
    [ top "r" [] (poly_record [ "f", var "id"; "g", var "id" ])
    ; top "field" [] (dot (app (var "id") [ var "r" ]) "f")
    ; top "restricted" [] (restrict (app (var "id") [ var "r" ]) "g")
    ];
  [%expect
    {|
    val r : {f : 'a -> 'a; g : 'b -> 'b}
    val field : '_a -> '_a
    val restricted : {f : '_a -> '_a}
    |}]
;;

let%expect_test "absent entries in records" =
  check
    [ Toplevel.Let
        ( "absent"
        , [ "c", None; typed "r" (Ty.record [] ~tail:"d") ]
        , None
        , seq
            (quote
               (quote
                  (let_
                     "z"
                     []
                     (int 1)
                     (splice
                        (splice
                           (var
                              "c"
                              ~annot:
                                (Ty.code
                                   (Ty.code (Ty.var "a") ~tail:"c")
                                   ~tail:"d")))))))
            (var "r") )
    ];
  [%expect
    {| val absent : <<'a>{z?'b : int; ..'c}>{-z; ..'d} -> {-z; ..'d} -> {-z; ..'d} where ..'c <= {..'e}, ..'d <= {..'f} |}]
;;

let%expect_test "structural types are not recursive" =
  check
    [ top
        "occurs"
        [ "r" ]
        (app (var "cond") [ bool true; dot (var "r") "x"; var "r" ])
    ];
  check
    [ top
        "cyclic"
        [ "r" ]
        (app
           (var "cond")
           [ bool true
           ; extend [ "x", int 1 ] (var "r")
           ; extend [ "y", int 1 ] (var "r")
           ])
    ];
  check [ Decl.alias "t" (Ty.record [ "next", Ty.con "t" ]) ];
  check
    [ Decl.alias "a" (Ty.record [ "b", Ty.con "b" ])
    ; Decl.alias "b" (Ty.variant [ "A", Ty.con "a" ])
    ];
  check
    [ Decl.variant "t" [ "T", [ Ty.record [ "next", Ty.con "t" ] ] ]
    ; top
        "deref"
        [ "t" ]
        (match_
           (var "t")
           [ Pat.construct "T" ~args:[ Pat.var "r" ], dot (var "r") "next" ])
    ];
  [%expect
    {|
    (Unification_error (Occurs_check_failed (ATy (TVar (Unbound 26 1)))))
    (Unification_error
     (Row_mismatch
      (RExtend x Present (TCon int ())
       (RVar
        (Link
         (RExtend x (PVar (Unbound 28 1)) (TVar (Unbound 29 1))
          (RVar (Unbound 30 1))))))
      (RExtend y Present (TCon int ())
       (RVar
        (Link
         (RVar
          (Link
           (RExtend x (PVar (Unbound 28 1)) (TVar (Unbound 29 1))
            (RVar (Unbound 30 1))))))))
      Cyclic_row))
    (Cyclic_type_alias t)
    (Cyclic_type_alias a)
    type t = T of {next : t}
    val deref : t -> t
    |}]
;;
