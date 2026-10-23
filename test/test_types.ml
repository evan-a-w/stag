open! Core
open Stag
open Syntax
open Helpers

let list_of ty = Ty.con "list" ~args:[ Ty ty ]
let cons x xs = construct "Cons" ~args:[ x; xs ]
let nil = construct "Nil"

(* Unifies the types of [a] and [b], unlike [cond]. *)
let either a b = match_ (bool true) [ Pat.any, a; Pat.any, b ]

let option =
  Decl.variant
    "option"
    ~params:[ Decl.ty "a" ]
    [ "None", []; "Some", [ Ty.var "a" ] ]
;;

let point = Decl.record "point" [ "x", Ty.int; "y", Ty.int ]
let box = Decl.record "box" ~params:[ Decl.ty "a" ] [ "contents", Ty.var "a" ]

let%expect_test "variants" =
  check
    [ Decl.list
    ; option
    ; top "nil" [] nil
    ; top "one" [] (cons (int 1) nil)
    ; top "some" [ "x" ] (construct "Some" ~args:[ var "x" ])
    ; top "none" [] (construct "None")
    ; top "nested" [] (construct "Some" ~args:[ cons (bool true) nil ])
    ];
  check [ Decl.list; top "bad" [] (cons (int 1) (cons (bool true) nil)) ];
  check [ top "unbound" [] (construct "Nil") ];
  check [ Decl.list; top "arity" [] (construct "Cons" ~args:[ int 1 ]) ];
  check [ Decl.list; top "arity" [] (construct "Nil" ~args:[ int 1 ]) ];
  [%expect
    {|
    nil : list['a]
    one : list[int]
    some : 'a -> option['a]
    none : option['a]
    nested : option[list[bool]]
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Unbound_constructor Nil)
    (Constructor_arity_mismatch (constructor Cons) (expected 2) (got 1))
    (Constructor_arity_mismatch (constructor Nil) (expected 0) (got 1))
    |}]
;;

let%expect_test "variant types are nominal" =
  check
    [ Decl.variant "a" [ "A", [] ]
    ; Decl.variant "b" [ "B", [] ]
    ; top "a" [] (construct "A")
    ; top "mixed" [] (either (construct "A") (construct "B"))
    ];
  [%expect
    {|
    (Unification_error
     (Type_mismatch (TCon a ()) (TCon b ()) (Base_types_not_equal a b)))
    |}]
;;

let%expect_test "declarations are order independent and may be recursive" =
  check
    [ top "tree" [] (construct "Node" ~args:[ cons (construct "Leaf") nil ])
    ; Decl.variant "tree" [ "Leaf", []; "Node", [ Ty.con "forest" ] ]
    ; Decl.alias "forest" (list_of (Ty.con "tree"))
    ; Decl.list
    ];
  check
    [ Decl.variant "even" [ "Zero", []; "Succ_even", [ Ty.con "odd" ] ]
    ; Decl.variant "odd" [ "Succ_odd", [ Ty.con "even" ] ]
    ; top
        "two"
        []
        (construct
           "Succ_even"
           ~args:[ construct "Succ_odd" ~args:[ construct "Zero" ] ])
    ; top "bad" [] (construct "Succ_even" ~args:[ construct "Zero" ])
    ];
  [%expect
    {|
    tree : tree
    (Unification_error
     (Type_mismatch (TCon odd ()) (TCon even ()) (Base_types_not_equal odd even)))
    |}]
;;

let%expect_test "records" =
  check
    [ point
    ; box
    ; top "origin" [] (record [ "y", int 0; "x", int 0 ])
    ; top "get_x" [ "p" ] (field (var "p") "x")
    ; top "boxed" [ "v" ] (record [ "contents", var "v" ])
    ; top "unbox" [ "b" ] (field (var "b") "contents")
    ; top "boxed_nil" [] (record [ "contents", nil ])
    ; top "unboxed" [] (field (var "boxed_nil") "contents")
    ; Decl.list
    ];
  [%expect
    {|
    origin : point
    get_x : point -> int
    boxed : 'a -> box['a]
    unbox : box['a] -> 'a
    boxed_nil : box[list['a]]
    unboxed : list['a]
    |}]
;;

let%expect_test "record errors" =
  let point3 = Decl.record "point3" [ "z", Ty.int ] in
  check [ point; top "missing" [] (record [ "x", int 0 ]) ];
  check
    [ point
    ; top "duplicate" [] (record [ "x", int 0; "y", int 0; "x", int 1 ])
    ];
  check
    [ point
    ; point3
    ; top "mixed" [] (record [ "x", int 0; "y", int 0; "z", int 0 ])
    ];
  check [ point; top "unbound" [] (record [ "x", int 0; "w", int 0 ]) ];
  check [ point; top "wrong_type" [] (record [ "x", int 0; "y", bool true ]) ];
  check [ point; top "empty" [] (record []) ];
  check [ point; top "unbound_field" [ "p" ] (field (var "p") "w") ];
  check [ point; top "not_a_record" [] (field (int 1) "x") ];
  check
    [ point
    ; point3
    ; top "wrong_record" [ "p" ] (field (field (var "p") "x") "z")
    ];
  [%expect
    {|
    (Missing_field y)
    (Duplicate_field x)
    (Fields_of_different_types (field z) (expected_type point)
     (actual_type point3))
    (Unbound_field w)
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    Empty_record
    (Unbound_field w)
    (Unification_error
     (Type_mismatch (TCon point ()) (TCon int ())
      (Base_types_not_equal point int)))
    (Unification_error
     (Type_mismatch (TCon point3 ()) (TCon int ())
      (Base_types_not_equal point3 int)))
    |}]
;;

let%expect_test "tuples" =
  check
    [ top "pair" [] (tuple [ int 1; bool true ])
    ; top "nested" [ "x" ] (tuple [ var "x"; tuple [ var "x"; unit ] ])
    ; top "fn" [] (tuple [ var "id"; app (var "add") [ int 1 ] ])
    ];
  [%expect
    {|
    pair : int * bool
    nested : 'a -> 'a * ('a * unit)
    fn : ('_a -> '_a) * (int -> int)
    |}]
;;

let%expect_test "match" =
  check
    [ Decl.list
    ; option
    ; point
    ; top
        "sum2"
        [ "xs" ]
        (match_
           (var "xs")
           [ ( Pat.construct
                 "Cons"
                 ~args:
                   [ Pat.var "x"
                   ; Pat.construct "Cons" ~args:[ Pat.var "y"; Pat.any ]
                   ]
             , app (var "add") [ var "x"; var "y" ] )
           ; ( Pat.construct "Cons" ~args:[ Pat.var "x"; Pat.construct "Nil" ]
             , var "x" )
           ; Pat.construct "Nil", int 0
           ])
    ; top
        "is_zero"
        [ "n" ]
        (match_ (var "n") [ Pat.int 0, bool true; Pat.any, bool false ])
    ; top
        "fst"
        [ "p" ]
        (match_ (var "p") [ Pat.tuple [ Pat.var "a"; Pat.any ], var "a" ])
    ; top
        "get_y"
        [ "p" ]
        (match_ (var "p") [ Pat.record [ "y", Pat.var "y" ], var "y" ])
    ; top
        "get_opt"
        [ "o"; "default" ]
        (match_
           (var "o")
           [ Pat.construct "Some" ~args:[ Pat.var "x" ], var "x"
           ; Pat.construct "None", var "default"
           ])
    ; top "shadows" [ "x" ] (match_ (int 1) [ Pat.var "x", var "x" ])
    ; top "no_cases" [ "x" ] (match_ (var "x") [])
    ; top
        "nested_record"
        [ "o" ]
        (match_
           (var "o")
           [ ( Pat.construct
                 "Some"
                 ~args:[ Pat.record [ "x", Pat.var "a"; "y", Pat.int 2 ] ]
             , var "a" )
           ])
    ];
  [%expect
    {|
    sum2 : list[int] -> int
    is_zero : int -> bool
    fst : 'a * 'b -> 'a
    get_y : point -> int
    get_opt : option['a] -> 'a -> 'a
    shadows : 'a -> int
    no_cases : 'a -> 'b
    nested_record : option[point] -> int
    |}]
;;

let%expect_test "match errors" =
  check
    [ option
    ; top
        "arm_types"
        [ "o" ]
        (match_
           (var "o")
           [ Pat.construct "Some" ~args:[ Pat.any ], int 1
           ; Pat.construct "None", bool true
           ])
    ];
  check
    [ option
    ; top
        "pattern_types"
        [ "o" ]
        (match_
           (var "o")
           [ Pat.construct "Some" ~args:[ Pat.any ], int 1; Pat.int 1, int 2 ])
    ];
  check
    [ top
        "duplicate"
        []
        (match_
           (tuple [ int 1; int 2 ])
           [ Pat.tuple [ Pat.var "x"; Pat.var "x" ], var "x" ])
    ];
  check
    [ option
    ; top
        "nested_duplicate"
        []
        (match_
           (tuple [ int 1; construct "None" ])
           [ ( Pat.tuple
                 [ Pat.var "x"; Pat.construct "Some" ~args:[ Pat.var "x" ] ]
             , var "x" )
           ])
    ];
  check
    [ top
        "same_name_other_arms"
        []
        (match_ (int 1) [ Pat.var "x", var "x"; Pat.var "x", var "x" ])
    ];
  check
    [ option; top "arity" [] (match_ (int 1) [ Pat.construct "Some", int 1 ]) ];
  check [ top "unbound" [] (match_ (int 1) [ Pat.construct "Some", int 1 ]) ];
  check
    [ point
    ; top
        "duplicate_field"
        [ "p" ]
        (match_ (var "p") [ Pat.record [ "x", Pat.any; "x", Pat.any ], int 1 ])
    ];
  check
    [ point
    ; box
    ; top
        "mixed_fields"
        [ "p" ]
        (match_
           (var "p")
           [ Pat.record [ "x", Pat.any; "contents", Pat.any ], int 1 ])
    ];
  check
    [ top "empty_record" [ "p" ] (match_ (var "p") [ Pat.record [], int 1 ]) ];
  check
    [ top
        "tuple_length"
        []
        (match_
           (tuple [ int 1; int 2 ])
           [ Pat.tuple [ Pat.any; Pat.any; Pat.any ], int 1 ])
    ];
  [%expect
    {|
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Unification_error
     (Type_mismatch (TCon option ((ATy (TVar (Link (TVar (Unbound 15 1)))))))
      (TCon int ()) (Base_types_not_equal option int)))
    (Duplicate_pattern_variable x)
    (Duplicate_pattern_variable x)
    same_name_other_arms : int
    (Constructor_arity_mismatch (constructor Some) (expected 1) (got 0))
    (Unbound_constructor Some)
    (Duplicate_field x)
    (Fields_of_different_types (field contents) (expected_type point)
     (actual_type box))
    Empty_record
    (Unification_error
     (Type_mismatch (TTuple ((TCon int ()) (TCon int ())))
      (TTuple
       ((TVar (Link (TVar (Unbound 15 1)))) (TVar (Link (TVar (Unbound 16 1))))
        (TVar (Link (TVar (Unbound 17 1))))))
      (Args_length_mismatch 2 3)))
    |}]
;;

let%expect_test "declaration errors" =
  check [ Decl.variant "t" [ "A", [] ]; Decl.record "t" [ "x", Ty.int ] ];
  check [ Decl.variant "t" ~params:[ Decl.ty "a"; Decl.context "a" ] [] ];
  check [ Decl.variant "t" [ "A", []; "A", [ Ty.int ] ] ];
  check [ Decl.variant "t" [ "A", [] ]; Decl.variant "u" [ "A", [] ] ];
  check [ Decl.record "t" [ "x", Ty.int; "x", Ty.bool ] ];
  check [ point; Decl.record "u" [ "x", Ty.int ] ];
  check [ Decl.variant "t" [ "A", [ Ty.var "a" ] ] ];
  check
    [ Decl.variant
        "t"
        ~params:[ Decl.ty "a" ]
        [ "A", [ Ty.code Ty.int ~tail:"a" ] ]
    ];
  check
    [ Decl.variant "t" ~params:[ Decl.context "r" ] [ "A", [ Ty.var "r" ] ] ];
  check [ Decl.variant "t" [ "A", [ Ty.con "nope" ] ] ];
  check [ Decl.alias "t" (Ty.con "nope") ];
  [%expect
    {|
    (Duplicate_type t)
    (Duplicate_type_parameter (type_name t) (param a))
    (Duplicate_constructor A)
    (Duplicate_constructor A)
    (Duplicate_field x)
    (Duplicate_field x)
    (Unbound_type_variable a)
    (Unbound_type_variable a)
    (Unbound_type_variable r)
    (Unbound_type nope)
    (Unbound_type nope)
    |}]
;;

let%expect_test "arity and kind of type arguments" =
  let stmt =
    Decl.variant
      "stmt"
      ~params:[ Decl.context "r" ]
      [ "Stmt", [ Ty.code Ty.unit ~tail:"r" ] ]
  in
  check [ Decl.list; Decl.variant "t" [ "A", [ Ty.con "list" ] ] ];
  check [ Decl.variant "t" [ "A", [ Ty.con "int" ~args:[ Ty Ty.int ] ] ] ];
  check [ top "f" [ "x" ] ~annot:(Ty.con "bool" ~args:[ Ty Ty.int ]) (var "x") ];
  check
    [ Decl.list
    ; Decl.variant "t" [ "A", [ Ty.con "list" ~args:[ Context Empty ] ] ]
    ];
  check
    [ stmt; Decl.variant "t" [ "A", [ Ty.con "stmt" ~args:[ Ty Ty.int ] ] ] ];
  check [ top "f" [ "x" ] ~annot:(Ty.con "list" ~args:[ Ty Ty.int ]) (var "x") ];
  check [ top "f" [ "x" ] ~annot:(Ty.con "M.t") (var "x") ];
  [%expect
    {|
    (Type_arity_mismatch (type_name list) (expected 1) (got 0))
    (Type_arity_mismatch (type_name int) (expected 0) (got 1))
    (Type_arity_mismatch (type_name bool) (expected 0) (got 1))
    (Type_arg_kind_mismatch (type_name list) (param (Ty a)))
    (Type_arg_kind_mismatch (type_name stmt) (param (Context r)))
    (Unbound_type list)
    (Unbound_type M.t)
    |}]
;;

let%expect_test "aliases" =
  let pair =
    Decl.alias
      "pair"
      ~params:[ Decl.ty "a" ]
      (Ty.tuple [ Ty.var "a"; Ty.var "a" ])
  in
  let code =
    Decl.alias "code" ~params:[ Decl.context "r" ] (Ty.code Ty.int ~tail:"r")
  in
  check
    [ pair
    ; code
    ; Decl.alias "int_pair" (Ty.con "pair" ~args:[ Ty Ty.int ])
    ; Decl.alias "unused" ~params:[ Decl.ty "a" ] Ty.bool
    ; Decl.variant "t" [ "A", [ Ty.con "int_pair" ] ]
    ; top "f" [ "p" ] ~annot:(Ty.con "int_pair") (var "p")
    ; top "a" [] (construct "A" ~args:[ tuple [ int 1; int 2 ] ])
    ; top
        "g"
        [ "c" ]
        ~annot:(Ty.con "code" ~args:[ Context (Extend ("x", Ty.int, Var "r")) ])
        (var "c")
    ; top "h" [] ~annot:(Ty.con "unused" ~args:[ Ty Ty.int ]) (bool true)
    ];
  check
    [ pair
    ; top
        "bad"
        []
        ~annot:(Ty.con "pair" ~args:[ Ty Ty.int ])
        (tuple [ int 1; bool true ])
    ];
  [%expect
    {|
    f : int * int -> int * int
    a : t
    g : <int>{x : int; ..'a} -> <int>{x : int; ..'a}
    h : bool
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "cyclic aliases" =
  check [ Decl.alias "t" (Ty.con "t") ];
  check
    [ Decl.alias "t" (Ty.con "u")
    ; Decl.alias "u" (Ty.arrow Ty.int (Ty.con "t"))
    ];
  check [ Decl.list; Decl.alias "t" (list_of (Ty.con "t")) ];
  check [ Decl.alias "t" (Ty.code Ty.int ~entries:[ "x", Ty.con "t" ]) ];
  check
    [ Decl.list
    ; Decl.alias "t" (list_of (Ty.con "u"))
    ; Decl.variant "u" [ "U", [ Ty.con "t" ] ]
    ; top "ok" [] (construct "U" ~args:[ nil ])
    ];
  [%expect
    {|
    (Cyclic_type_alias t)
    (Cyclic_type_alias t)
    (Cyclic_type_alias t)
    (Cyclic_type_alias t)
    ok : u
    |}]
;;

let%expect_test "builtins can be shadowed by user types" =
  check
    [ Decl.variant "unit" [ "Unit", [] ]
    ; top "u" [] ~annot:(Ty.con "unit") (construct "Unit")
    ];
  [%expect {| u : unit |}]
;;

let%expect_test "annotation variables are shared within a toplevel" =
  check
    [ top
        "annotated"
        [ "x"; "y" ]
        ~annot:(Ty.var "a")
        (let_
           ~annot:(Ty.var "a")
           "z"
           []
           (var "x" ~annot:(Ty.var "a"))
           (var "y"))
    ; top
        "other_toplevel"
        [ "x" ]
        (seq (var "x" ~annot:(Ty.var "a")) (app (var "add") [ var "x"; int 1 ]))
    ; top
        "independent"
        [ "x"; "y" ]
        (seq (var "x" ~annot:(Ty.var "a")) (var "y" ~annot:(Ty.var "b")))
    ; top
        "context"
        [ "c"; "d" ]
        (seq
           (var "c" ~annot:(Ty.code Ty.int ~entries:[ "x", Ty.int ] ~tail:"r"))
           (var "d" ~annot:(Ty.code Ty.int ~tail:"r")))
    ; top
        "ty_and_context_named_alike"
        [ "c" ]
        (var "c" ~annot:(Ty.code (Ty.var "r") ~tail:"r"))
    ];
  [%expect
    {|
    annotated : 'a -> 'a -> 'a
    other_toplevel : int -> int
    independent : 'a -> 'b -> 'b
    context : <int>{x : int; ..'a} -> <int>{..'a} -> <int>{..'a}
    ty_and_context_named_alike : <'a>{..'b} -> <'a>{..'b}
    |}]
;;

let%expect_test "annotation variables are not generalized by inner lets" =
  check
    [ top
        "f"
        []
        (let_
           "g"
           [ "y" ]
           (var "y" ~annot:(Ty.var "a"))
           (seq (app (var "g") [ int 1 ]) (app (var "g") [ bool true ])))
    ];
  [%expect
    {|
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "context parameters" =
  let stmt =
    Decl.variant
      "stmt"
      ~params:[ Decl.context "r" ]
      [ ( "Stmt"
        , [ Ty.code Ty.unit ~tail:"r"
          ; list_of (Ty.con "stmt" ~args:[ Context (Var "r") ])
          ] )
      ]
  in
  check
    [ Decl.list
    ; stmt
    ; top "single" [ "c" ] (construct "Stmt" ~args:[ var "c"; nil ])
    ; top
        "uses_x"
        []
        (construct "Stmt" ~args:[ quote (seq (var "x") unit); nil ])
    ; top
        "nested"
        []
        (construct
           "Stmt"
           ~args:
             [ quote (seq (app (var "add") [ var "x"; int 1 ]) unit)
             ; cons (var "uses_x") nil
             ])
    ; top
        "annotated"
        [ "s" ]
        ~annot:
          (Ty.con "stmt" ~args:[ Context (Extend ("y", Ty.bool, Var "r")) ])
        (var "s")
    ; top
        "body"
        [ "s" ]
        (match_
           (var "s")
           [ Pat.construct "Stmt" ~args:[ Pat.var "c"; Pat.any ], var "c" ])
    ];
  [%expect
    {|
    single : <unit>{..'a} -> stmt[{..'a}]
    uses_x : stmt[{x : 'a; ..'b}]
    nested : stmt[{x : int; ..'a}]
    annotated : stmt[{y : bool; ..'a}] -> stmt[{y : bool; ..'a}]
    body : stmt[{..'a}] -> <unit>{..'a}
    |}]
;;

let%expect_test "patterns in quotes are captured by splices" =
  check
    [ top
        "captures"
        [ "c" ]
        (quote (match_ (int 1) [ Pat.var "x", splice (var "c") ]))
    ; top
        "applied"
        []
        (app (var "captures") [ quote (app (var "add") [ var "x"; int 1 ]) ])
    ; top
        "local"
        []
        (quote
           (match_
              (tuple [ int 1; bool true ])
              [ ( Pat.tuple [ Pat.var "x"; Pat.var "y" ]
                , app (var "add") [ var "x"; int 1 ] )
              ]))
    ; top "bound_outside" [] (match_ (int 1) [ Pat.var "x", quote (var "x") ])
    ];
  check
    [ top
        "wrong_type"
        []
        (app
           (var "captures")
           [ quote (app (var "cond") [ var "x"; int 1; int 2 ]) ])
    ; top
        "captures"
        [ "c" ]
        (quote (match_ (int 1) [ Pat.var "x", splice (var "c") ]))
    ];
  check
    [ top "level" [] (quote (match_ (int 1) [ Pat.var "x", splice (var "x") ]))
    ];
  [%expect
    {|
    captures : <'a>{x?'b : int; ..'c} -> <'a>{..'d} where ..'c <= {..'d}
    applied : <int>{..'_a}
    local : <int>{..'a}
    bound_outside : <'a>{x : 'a; ..'b}
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Variable_used_at_wrong_level (name x) (var_level 1) (used_at 0))
    |}]
;;

let%expect_test "constructors and fields are not free variables of quotes" =
  check
    [ Decl.list
    ; point
    ; top "closed" [] (quote (cons (int 1) nil))
    ; top "open" [] (quote (cons (var "y") nil))
    ; top "record" [] (quote (field (record [ "x", int 1; "y", var "y" ]) "x"))
    ; top
        "pattern"
        []
        (quote
           (match_
              (var "l")
              [ Pat.construct "Cons" ~args:[ Pat.var "h"; Pat.any ], var "h"
              ; Pat.construct "Nil", int 0
              ]))
    ; top "in_splice" [] (quote (splice (quote nil)))
    ];
  [%expect
    {|
    closed : <list[int]>{..'a}
    open : <list['a]>{y : 'a; ..'b}
    record : <int>{y : int; ..'a}
    pattern : <int>{l : list[int]; ..'a}
    in_splice : <list['a]>{..'b}
    |}]
;;

let%expect_test "generalization of syntactic values" =
  check
    [ Decl.list
    ; box
    ; top "nil" [] nil
    ; top
        "uses"
        []
        (tuple [ cons (int 1) (var "nil"); cons (bool true) (var "nil") ])
    ; top "pair" [] (tuple [ nil; var "id" ])
    ; top "boxed" [] (record [ "contents", nil ])
    ; top "contents" [] (field (var "boxed") "contents")
    ; top
        "matched"
        []
        (match_ (var "nil") [ Pat.construct "Nil", nil; Pat.any, var "nil" ])
    ; top "local" [] (let_ "a" [] nil (cons (var "a") (var "a")))
    ; top "local_fn" [] (let_ "f" [ "x" ] (var "x") (var "f"))
    ; top "quoted" [] (quote nil)
    ; top "weak_app" [] (app (var "id") [ var "id" ])
    ; top "weak_cons" [] (cons (app (var "id") [ var "id" ]) nil)
    ; top "weak_let" [] (let_ "a" [] (app (var "id") [ nil ]) (var "a"))
    ; top "weak_match" [] (match_ (app (var "id") [ nil ]) [ Pat.any, nil ])
    ; top "weak_arm" [] (match_ nil [ Pat.any, app (var "id") [ nil ] ])
    ; top "weak_field" [] (field (app (var "id") [ var "boxed" ]) "contents")
    ; top "weak_splice" [] (splice (quote nil))
    ];
  [%expect
    {|
    nil : list['a]
    uses : list[int] * list[bool]
    pair : list['a] * ('b -> 'b)
    boxed : box[list['a]]
    contents : list['a]
    matched : list['a]
    local : list[list['a]]
    local_fn : 'a -> 'a
    quoted : <list['a]>{..'b}
    weak_app : '_a -> '_a
    weak_cons : list['_a -> '_a]
    weak_let : list['_a]
    weak_match : list['_a]
    weak_arm : list['_a]
    weak_field : list['_a]
    weak_splice : list['_a]
    |}]
;;

let%expect_test "local generalization of syntactic values" =
  check
    [ Decl.list
    ; top
        "poly"
        []
        (let_
           "n"
           []
           nil
           (tuple [ cons (int 1) (var "n"); cons (bool true) (var "n") ]))
    ];
  check
    [ Decl.list
    ; top
        "mono"
        []
        (let_
           "n"
           []
           (app (var "id") [ nil ])
           (tuple [ cons (int 1) (var "n"); cons (bool true) (var "n") ]))
    ];
  [%expect
    {|
    poly : list[int] * list[bool]
    (Unification_error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
    |}]
;;

let%expect_test "generalized values are not persistent" =
  check
    [ Decl.list
    ; top "nil" [] nil
    ; top "f" [ "x" ] (var "x")
    ; top "uses_nil" [] (quote (var "nil"))
    ; top "uses_f" [] (quote (var "f"))
    ];
  [%expect
    {|
    nil : list['a]
    f : 'a -> 'a
    uses_nil : <'a>{nil : 'a; ..'b}
    uses_f : <'a -> 'a>{..'b}
    |}]
;;

let%expect_test "SCCs are generalized iff all members are values" =
  let values =
    [ Decl.list
    ; top "a" [ "x" ] (var "b")
    ; top "b" [] (seq (var "a") nil)
    ; top
        "use"
        []
        (tuple [ cons (int 1) (var "b"); cons (bool true) (var "b") ])
    ; top "quoted" [] (quote (app (var "a") [ int 1 ]))
    ]
  in
  check values;
  check
    [ Decl.list
    ; top "a" [ "x" ] (var "b")
    ; top "b" [] (seq (var "a") (app (var "id") [ nil ]))
    ];
  check
    [ Decl.list
    ; top "xs" [] (cons (int 1) (var "ys"))
    ; top "ys" [] (cons (int 2) (var "xs"))
    ];
  [%expect
    {|
    a : 'a -> list['b]
    b : list['a]
    use : list[int] * list[bool]
    quoted : <'a>{a : int -> 'a; ..'b}
    a : '_a -> list['_b]
    b : list['_a]
    xs : list[int]
    ys : list[int]
    |}]
;;

let%expect_test "multi-segment paths are unbound" =
  check [ top "v" [] (var "M.x") ];
  check [ top "c" [] (construct "M.A") ];
  check [ point; top "f" [ "p" ] (field (var "p") "M.x") ];
  check [ point; top "f" [ "p" ] (quote (var "M.x")) ];
  [%expect
    {|
    (Not_in_scope M.x)
    (Unbound_constructor M.A)
    (Unbound_field M.x)
    (Not_in_scope M.x)
    |}]
;;
