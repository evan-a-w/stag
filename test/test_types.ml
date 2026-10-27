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
    type list['a] = Nil | Cons of 'a * list['a]
    type option['a] = None | Some of 'a
    val nil : list['a]
    val one : list[int]
    val some : 'a -> option['a]
    val none : option['a]
    val nested : option[list[bool]]
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

let%expect_test "abstract types" =
  let id ty = top_let ("id_" ^ ty, [ typed "x" (Ty.con ty) ], None, var "x") in
  check
    [ Decl.abstract "t"
    ; Decl.abstract "box" ~params:[ Decl.ty "a"; Decl.row "r" ]
    ; id "t"
    ; module_ "M" [ Decl.abstract "int"; id "int" ]
    ; id "int"
    ];
  check [ Decl.abstract "t"; id "t"; top "bad" [] (app (var "id_t") [ int 1 ]) ];
  check
    [ Decl.abstract "t"
    ; top_let ("absurd", [ typed "x" (Ty.con "t") ], None, match_ (var "x") [])
    ];
  check [ Decl.abstract "t"; top "bad" [] (construct "T") ];
  check
    [ module_ "M" [ Decl.abstract "int"; id "int" ]
    ; top "bad" [] (app (var "M.id_int") [ int 1 ])
    ];
  [%expect
    {|
    type t
    type box['a, {..'b}]
    val id_t : t -> t
    module M : sig
      type int
      val id_int : int -> int
    end
    val id_int : int -> int
    (Unification_error
     (Type_mismatch (TCon t ()) (TCon int ()) (Base_types_not_equal t int)))
    (Unification_error
     (Type_mismatch (TVariant REmpty) (TCon t ()) Structural_mismatch))
    (Unbound_constructor T)
    (Unification_error
     (Type_mismatch (TCon M.int ()) (TCon int ())
      (Base_types_not_equal M.int int)))
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
    val tree : tree
    type tree = Leaf | Node of list[tree]
    type forest = list[tree]
    type list['a] = Nil | Cons of 'a * list['a]
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
    type point = { x : int; y : int }
    type box['a] = { contents : 'a }
    val origin : point
    val get_x : point -> int
    val boxed : 'a -> box['a]
    val unbox : box['a] -> 'a
    val boxed_nil : box[list['a]]
    val unboxed : list['a]
    type list['a] = Nil | Cons of 'a * list['a]
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
    val pair : int * bool
    val nested : 'a -> 'a * ('a * unit)
    val fn : ('_a -> '_a) * (int -> int)
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
    type list['a] = Nil | Cons of 'a * list['a]
    type option['a] = None | Some of 'a
    type point = { x : int; y : int }
    val sum2 : list[int] -> int
    val is_zero : int -> bool
    val fst : 'a * 'b -> 'a
    val get_y : point -> int
    val get_opt : option['a] -> 'a -> 'a
    val shadows : 'a -> int
    val no_cases : [] -> 'a
    val nested_record : option[point] -> int
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
     (Type_mismatch (TCon option ((ATy (TVar (Link (TVar (Unbound 22 1)))))))
      (TCon int ()) (Base_types_not_equal option int)))
    (Duplicate_pattern_variable x)
    (Duplicate_pattern_variable x)
    val same_name_other_arms : int
    (Constructor_arity_mismatch (constructor Some) (expected 1) (got 0))
    (Unbound_constructor Some)
    (Duplicate_field x)
    (Fields_of_different_types (field contents) (expected_type point)
     (actual_type box))
    Empty_record
    (Unification_error
     (Type_mismatch (TTuple ((TCon int ()) (TCon int ())))
      (TTuple
       ((TVar (Link (TVar (Unbound 22 1)))) (TVar (Link (TVar (Unbound 23 1))))
        (TVar (Link (TVar (Unbound 24 1))))))
      (Args_length_mismatch 2 3)))
    |}]
;;

let%expect_test "declaration errors" =
  check [ Decl.variant "t" [ "A", [] ]; Decl.record "t" [ "x", Ty.int ] ];
  check [ Decl.variant "t" ~params:[ Decl.ty "a"; Decl.row "a" ] [] ];
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
  check [ Decl.variant "t" ~params:[ Decl.row "r" ] [ "A", [ Ty.var "r" ] ] ];
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
    (Type_variable_kind_mismatch a)
    (Type_variable_kind_mismatch r)
    (Unbound_type nope)
    (Unbound_type nope)
    |}]
;;

let%expect_test "duplicate context entries" =
  let twice = Ty.code Ty.int ~entries:[ "x", Ty.int; "x", Ty.bool ] in
  check [ top "f" [ "c" ] ~annot:twice (var "c") ];
  check [ top "f" [ "c" ] (var "c" ~annot:twice) ];
  check [ Decl.variant "t" [ "A", [ twice ] ] ];
  check [ Decl.alias "t" twice ];
  check
    [ Decl.alias "code" ~params:[ Decl.row "r" ] (Ty.code Ty.int ~tail:"r")
    ; top
        "f"
        [ "c" ]
        ~annot:
          (Ty.con
             "code"
             ~args:
               [ Row (Ty.row ~entries:[ "x", Ty.int; "x", Ty.int ] ~tail:"r" ())
               ])
        (var "c")
    ];
  check
    [ top
        "nested"
        [ "c" ]
        ~annot:
          (Ty.code
             (Ty.code Ty.int ~entries:[ "x", Ty.int ])
             ~entries:[ "x", Ty.bool ])
        (var "c")
    ];
  [%expect
    {|
    (Duplicate_row_entry x)
    (Duplicate_row_entry x)
    (Duplicate_row_entry x)
    (Duplicate_row_entry x)
    (Duplicate_row_entry x)
    val nested : <<int>{x : int}>{x : bool} -> <<int>{x : int}>{x : bool}
    |}]
;;

let%expect_test "arity and kind of type arguments" =
  let stmt =
    Decl.variant
      "stmt"
      ~params:[ Decl.row "r" ]
      [ "Stmt", [ Ty.code Ty.unit ~tail:"r" ] ]
  in
  check [ Decl.list; Decl.variant "t" [ "A", [ Ty.con "list" ] ] ];
  check [ Decl.variant "t" [ "A", [ Ty.con "int" ~args:[ Ty Ty.int ] ] ] ];
  check [ top "f" [ "x" ] ~annot:(Ty.con "bool" ~args:[ Ty Ty.int ]) (var "x") ];
  check
    [ Decl.list
    ; Decl.variant "t" [ "A", [ Ty.con "list" ~args:[ Row (Ty.row ()) ] ] ]
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
    (Type_arg_kind_mismatch (type_name stmt) (param (Row r)))
    (Unbound_type list)
    (Unbound_module M)
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
    Decl.alias "code" ~params:[ Decl.row "r" ] (Ty.code Ty.int ~tail:"r")
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
        ~annot:
          (Ty.con
             "code"
             ~args:[ Row (Ty.row ~entries:[ "x", Ty.int ] ~tail:"r" ()) ])
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
    type pair['a] = 'a * 'a
    type code[{..'a}] = <int>{..'a}
    type int_pair = int * int
    type unused['a] = bool
    type t = A of (int * int)
    val f : int * int -> int * int
    val a : t
    val g : <int>{x : int; ..'a} -> <int>{x : int; ..'a}
    val h : bool
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
    type list['a] = Nil | Cons of 'a * list['a]
    type t = list[u]
    type u = U of list[u]
    val ok : u
    |}]
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
    val annotated : 'a -> 'a -> 'a
    val other_toplevel : int -> int
    val independent : 'a -> 'b -> 'b
    val context : <int>{x : int; ..'a} -> <int>{..'a} -> <int>{..'a}
    val ty_and_context_named_alike : <'a>{..'b} -> <'a>{..'b}
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
      ~params:[ Decl.row "r" ]
      [ ( "Stmt"
        , [ Ty.code Ty.unit ~tail:"r"
          ; list_of (Ty.con "stmt" ~args:[ Row (Ty.row ~tail:"r" ()) ])
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
          (Ty.con
             "stmt"
             ~args:[ Row (Ty.row ~entries:[ "y", Ty.bool ] ~tail:"r" ()) ])
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
    type list['a] = Nil | Cons of 'a * list['a]
    type stmt[{..'a}] = Stmt of <unit>{..'a} * list[stmt[{..'a}]]
    val single : <unit>{..'a} -> stmt[{..'a}]
    val uses_x : stmt[{x : 'a; ..'b}]
    val nested : stmt[{x : int; ..'a}]
    val annotated : stmt[{y : bool; ..'a}] -> stmt[{y : bool; ..'a}]
    val body : stmt[{..'a}] -> <unit>{..'a}
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
    val captures : <'a>{x?'b : int; ..'c} -> <'a>{..'d} where ..'c <= {..'d}
    val applied : <int>{..'_a}
    val local : <int>{..'a}
    val bound_outside : <'a>{x : 'a; ..'b}
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
    type list['a] = Nil | Cons of 'a * list['a]
    type point = { x : int; y : int }
    val closed : <list[int]>{..'a}
    val open : <list['a]>{y : 'a; ..'b}
    val record : <int>{y : int; ..'a}
    val pattern : <int>{l : list[int]; ..'a}
    val in_splice : <list['a]>{..'b}
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
    ; top "spliced_value" [] (splice (quote nil))
    ];
  [%expect
    {|
    type list['a] = Nil | Cons of 'a * list['a]
    type box['a] = { contents : 'a }
    val nil : list['a]
    val uses : list[int] * list[bool]
    val pair : list['a] * ('b -> 'b)
    val boxed : box[list['a]]
    val contents : list['a]
    val matched : list['a]
    val local : list[list['a]]
    val local_fn : 'a -> 'a
    val quoted : <list['a]>{..'b}
    val weak_app : '_a -> '_a
    val weak_cons : list['_a -> '_a]
    val weak_let : list['_a]
    val weak_match : list['_a]
    val weak_arm : list['_a]
    val weak_field : list['_a]
    val spliced_value : list['a]
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
    type list['a] = Nil | Cons of 'a * list['a]
    val poly : list[int] * list[bool]
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
    type list['a] = Nil | Cons of 'a * list['a]
    val nil : list['a]
    val f : 'a -> 'a
    val uses_nil : <'a>{nil : 'a; ..'b}
    val uses_f : <'a -> 'a>{..'b}
    |}]
;;

let%expect_test "SCCs are generalized iff all members are values" =
  check
    [ Decl.list
    ; top "nil" [] nil
    ; top "f" [ "x" ] (app (var "g") [ var "x" ])
    ; top "g" [ "x" ] (seq (var "nil") (app (var "f") [ var "x" ]))
    ];
  check
    [ Decl.list
    ; top "a" [ "x" ] (var "b")
    ; top "b" [] (seq (var "a") (app (var "id") [ nil ]))
    ];
  [%expect
    {|
    type list['a] = Nil | Cons of 'a * list['a]
    val nil : list['a]
    val f : 'a -> 'b
    val g : 'a -> 'b
    type list['a] = Nil | Cons of 'a * list['a]
    val a : '_a -> list['_b]
    val b : list['_a]
    |}]
;;

let%expect_test "recursive SCCs with a non-function are not generalized" =
  check
    [ Decl.list
    ; top "a" [ "x" ] (var "b")
    ; top "b" [] (seq (var "a") nil)
    ; top "quoted" [] (quote (app (var "a") [ int 1 ]))
    ];
  check
    [ Decl.list
    ; top "xs" [] (cons (int 1) (var "ys"))
    ; top "ys" [] (cons (int 2) (var "xs"))
    ];
  check [ top "x" [] (var "x") ];
  check [ top "x" [] (var "y"); top "y" [] (var "x") ];
  check [ module_ "M" [ top "x" [] (var "M.x") ] ];
  check
    [ top "x" [] (var "x")
    ; top "use" [] (seq (app (var "x") [ int 1 ]) (app (var "x") [ bool true ]))
    ];
  [%expect
    {|
    type list['a] = Nil | Cons of 'a * list['a]
    val a : '_a -> list['_b]
    val b : list['_a]
    val quoted : <'a>{a : int -> 'a; ..'b}
    type list['a] = Nil | Cons of 'a * list['a]
    val xs : list[int]
    val ys : list[int]
    val x : '_a
    val x : '_a
    val y : '_a
    module M : sig
      val x : '_a
    end
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "recursive local non-functions are not generalized" =
  let print_local equal =
    let typecheck = typechecker () in
    match
      Typecheck.typecheck
        typecheck
        ~program:
          [ Decl.list
          ; top "local" [] (let_ "x" [] equal (tuple [ var "x"; var "x" ]))
          ]
    with
    | Ok { signature; _ } -> print_signature [ List.last_exn signature ]
    | Error error -> print_s [%sexp (error : Typecheck_error.t)]
  in
  print_local (var "x");
  print_local (seq (var "x") nil);
  print_local (match_ (var "x") [ Pat.any, nil ]);
  (* The splices' providers are of the quote's level, so are never the
     binder. *)
  print_local (quote (splice (quote nil)));
  print_local (quote (splice (quote (var "x"))));
  print_local nil;
  print_local (let_ "x" [] nil (var "x"));
  print_local (match_ nil [ Pat.var "x", var "x" ]);
  print_local (let_ "g" [ "x" ] (var "x") (seq (var "g") nil));
  print_local (quote (splice ~subs:[ "y", None ] (quote nil)));
  print_local (quote (splice ~subs:[ "x", None ] (quote nil)));
  [%expect
    {|
    val local : '_a * '_a
    val local : list['_a] * list['_a]
    val local : list['_a] * list['_a]
    val local : <list['a]>{..'b} * <list['c]>{..'d}
    val local : <'a>{x : 'a; ..'b} * <'c>{x : 'c; ..'d}
    val local : list['a] * list['b]
    val local : list['a] * list['b]
    val local : list['a] * list['b]
    val local : list['a] * list['b]
    val local : <list['a]>{y : 'b; ..'c} * <list['d]>{y : 'e; ..'f}
    val local : <list['a]>{x : 'b; ..'c} * <list['d]>{x : 'e; ..'f}
    |}]
;;

let%expect_test "quotes are values unless their level 0 splices are not" =
  check
    [ Decl.list
    ; top "no_splice" [] (quote nil)
    ; top "value_splice" [] (quote (splice (quote nil)))
    ; top "app_splice" [] (quote (splice (app (var "id") [ quote nil ])))
    ; top
        "nested_app_splice"
        []
        (quote (quote (splice (app (var "id") [ quote nil ]))))
    ; top
        "escaping_app_splice"
        []
        (quote (quote (splice (splice (app (var "id") [ quote (quote nil) ])))))
    ; top
        "substitution"
        []
        (quote
           (splice
              ~subs:[ "x", Some (app (var "id") [ nil ]) ]
              (quote (var "x"))))
    ];
  [%expect
    {|
    type list['a] = Nil | Cons of 'a * list['a]
    val no_splice : <list['a]>{..'b}
    val value_splice : <list['a]>{..'b}
    val app_splice : <list['_a]>{..'_b}
    val nested_app_splice : <<list['a]>{..'b}>{..'c}
    val escaping_app_splice : <<list['_a]>{..'_b}>{..'_c}
    val substitution : <list['a]>{..'b}
    |}]
;;
