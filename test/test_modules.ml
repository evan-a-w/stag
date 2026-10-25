open! Core
open Stag
open Syntax
open Helpers

let%expect_test "qualified and unqualified values" =
  check
    [ module_
        "M"
        [ top "x" [] (int 1)
        ; top "f" [ "y" ] (app (var "add") [ var "y"; var "x" ])
        ; top "g" [ "y" ] (app (var "M.f") [ var "y" ])
        ]
    ; top "a" [] (var "M.x")
    ; top "b" [] (app (var "M.f") [ int 2 ])
    ];
  [%expect
    {|
    module M : sig
      val x : int
      val f : int -> int
      val g : int -> int
    end
    val a : int
    val b : int
    |}]
;;

let%expect_test "nested modules" =
  check
    [ top "root" [] (Lit (String "s"))
    ; module_
        "A"
        [ top "v" [] (int 1)
        ; module_
            "B"
            [ top "x" [] (bool true)
            ; top "from_a" [] (var "v")
            ; top "from_root" [] (var "root")
            ; top "self" [] (var "B.x")
            ; top "absolute" [] (var "A.v")
            ]
        ; top "y" [] (var "B.x")
        ]
    ; top "z" [] (var "A.B.x")
    ; top "w" [] (var "A.y")
    ];
  [%expect
    {|
    val root : string
    module A : sig
      val v : int
      module B : sig
        val x : bool
        val from_a : int
        val from_root : string
        val self : bool
        val absolute : int
      end
      val y : bool
    end
    val z : bool
    val w : bool
    |}]
;;

let%expect_test "results are in program order, depth first" =
  check
    [ top "a" [] (int 1)
    ; module_
        "A"
        [ top "b" [] (int 2)
        ; module_ "B" [ top "c" [] (var "A.d") ]
        ; top "d" [] (int 3)
        ]
    ; top "e" [] (var "A.B.c")
    ];
  [%expect
    {|
    val a : int
    module A : sig
      val b : int
      module B : sig
        val c : int
      end
      val d : int
    end
    val e : int
    |}]
;;

let%expect_test "lexical shadowing" =
  check
    [ top "x" [] (int 1)
    ; module_
        "M"
        [ top "x" [] (bool true)
        ; top "y" [] (var "x")
        ; module_ "N" [ top "z" [] (var "x") ]
        ; top "local" [ "x" ] (var "x")
        ; top "local_let" [] (let_ "x" [] unit (var "x"))
        ; top "qualified_ignores_locals" [ "x" ] (var "M.x")
        ]
    ; top "root_x" [] (var "x")
    ];
  [%expect
    {|
    val x : int
    module M : sig
      val x : bool
      val y : bool
      module N : sig
        val z : bool
      end
      val local : 'a -> 'a
      val local_let : unit
      val qualified_ignores_locals : 'a -> bool
    end
    val root_x : int
    |}]
;;

let%expect_test "modules shadow lexically" =
  check
    [ module_ "N" [ top "x" [] (int 1) ]
    ; module_
        "M"
        [ module_ "N" [ top "x" [] (bool true) ]
        ; top "inner" [] (var "N.x")
        ; module_ "K" [ top "inner_too" [] (var "N.x") ]
        ]
    ; top "outer" [] (var "N.x")
    ; top "explicit" [] (var "M.N.x")
    ];
  [%expect
    {|
    module N : sig
      val x : int
    end
    module M : sig
      module N : sig
        val x : bool
      end
      val inner : bool
      module K : sig
        val inner_too : bool
      end
    end
    val outer : int
    val explicit : bool
    |}]
;;

let%expect_test "the rest of a path is looked up only in the module" =
  check [ top "x" [] (int 1); module_ "M" []; top "y" [] (var "M.x") ];
  check
    [ module_ "N" [ top "x" [] (int 1) ]
    ; module_ "M" []
    ; top "y" [] (var "M.N.x")
    ];
  check [ module_ "M" [ module_ "N" [] ]; top "y" [] (var "M.N.K.x") ];
  check [ module_ "M" [ top "x" [] (int 1) ]; top "y" [] (var "M.x.y") ];
  check
    [ Decl.variant "t" [ "A", [] ]
    ; module_ "M" []
    ; top "y" [] (construct "M.A")
    ];
  check
    [ Decl.variant "t" [ "A", [] ]
    ; module_ "M" []
    ; top "y" [] ~annot:(Ty.con "M.t") (construct "A")
    ];
  check
    [ module_ "N" [ Decl.record "r" [ "x", Ty.int ] ]
    ; module_ "M" []
    ; top "y" [ "r" ] (field (var "r") "M.N.x")
    ];
  [%expect
    {|
    (Not_in_scope M.x)
    (Unbound_module M.N)
    (Unbound_module M.N.K)
    (Unbound_module M.x)
    (Unbound_constructor M.A)
    (Unbound_type M.t)
    (Unbound_module M.N)
    |}]
;;

let%expect_test "modules and other members have separate namespaces" =
  check
    [ module_ "M" [ top "M" [] (int 1) ]
    ; Decl.variant "M" [ "M", [] ]
    ; top "a" [] (var "M.M")
    ; top "b" [] ~annot:(Ty.con "M") (construct "M")
    ];
  [%expect
    {|
    module M : sig
      val M : int
    end
    type M = M
    val a : int
    val b : M
    |}]
;;

let%expect_test "values are order independent and recursive across modules" =
  check
    [ top "main" [] (tuple [ app (var "A.even") [ int 4 ]; var "B.later" ])
    ; module_
        "A"
        [ top
            "even"
            [ "n" ]
            (app
               (var "cond")
               [ app (var "eq") [ var "n"; int 0 ]
               ; bool true
               ; app (var "B.odd") [ var "n" ]
               ])
        ]
    ; module_
        "B"
        [ top
            "odd"
            [ "n" ]
            (app
               (var "cond")
               [ app (var "eq") [ var "n"; int 0 ]
               ; bool false
               ; app (var "A.even") [ var "n" ]
               ])
        ; top "later" [] (var "C.value")
        ]
    ; module_ "C" [ top "value" [] (Lit (Char 'c')) ]
    ];
  [%expect
    {|
    val main : bool * char
    module A : sig
      val even : int -> bool
    end
    module B : sig
      val odd : int -> bool
      val later : char
    end
    module C : sig
      val value : char
    end
    |}]
;;

let%expect_test "SCCs across modules" =
  check
    [ module_ "A" [ top "f" [ "x" ] (app (var "B.g") [ var "x" ]) ]
    ; module_ "B" [ top "g" [ "x" ] (seq (var "A.f") (var "x")) ]
    ; top
        "use"
        []
        (tuple [ app (var "A.f") [ int 1 ]; app (var "B.g") [ bool true ] ])
    ];
  check
    [ module_
        "A"
        [ top "f" [ "x" ] (app (var "B.g") [ int 1 ])
        ; top "h" [] (seq (app (var "f") [ bool true ]) (var "B.g"))
        ]
    ; module_
        "B"
        [ top "g" [ "y" ] (seq (app (var "A.f") [ bool true ]) (var "y")) ]
    ];
  check
    [ module_
        "A"
        [ top "w" [] (app (var "B.g") [ app (var "id") [ var "id" ] ]) ]
    ; module_ "B" [ top "g" [ "y" ] (seq (var "A.w") (var "y")) ]
    ];
  [%expect
    {|
    module A : sig
      val f : 'a -> 'a
    end
    module B : sig
      val g : 'a -> 'a
    end
    val use : int * bool
    module A : sig
      val f : bool -> int
      val h : int -> int
    end
    module B : sig
      val g : int -> int
    end
    module A : sig
      val w : '_a -> '_a
    end
    module B : sig
      val g : ('_a -> '_a) -> '_a -> '_a
    end
    |}]
;;

let%expect_test "qualified and unqualified types, constructors and fields" =
  check
    [ module_
        "M"
        [ Decl.variant "t" [ "A", []; "B", [ Ty.int ] ]
        ; Decl.record "r" [ "x", Ty.int; "y", Ty.con "t" ]
        ; top "a" [] (construct "A")
        ; top
            "mk"
            [ "n" ]
            (record [ "x", var "n"; "y", construct "B" ~args:[ var "n" ] ])
        ; top "get" [ "r" ] (field (var "r") "x")
        ; top "annotated" [ "v" ] ~annot:(Ty.con "M.t") (var "v")
        ]
    ; top "b" [] (construct "M.B" ~args:[ int 1 ])
    ; top "r" [] (record [ "M.y", construct "M.A"; "M.x", int 2 ])
    ; top "get_x" [ "r" ] (field (var "r") "M.x")
    ; top "annotated" [ "v" ] ~annot:(Ty.con "M.r") (var "v")
    ; top
        "matched"
        [ "v" ]
        (match_
           (var "v")
           [ Pat.construct "M.B" ~args:[ Pat.var "n" ], var "n"
           ; Pat.construct "M.A", int 0
           ])
    ; top
        "record_pattern"
        [ "r" ]
        (match_ (var "r") [ Pat.record [ "M.y", Pat.var "y" ], var "y" ])
    ];
  [%expect
    {|
    module M : sig
      type t = A | B of int
      type r = { x : int; y : t }
      val a : t
      val mk : int -> r
      val get : r -> int
      val annotated : t -> t
    end
    val b : M.t
    val r : M.r
    val get_x : M.r -> int
    val annotated : M.r -> M.r
    val matched : M.t -> int
    val record_pattern : M.r -> M.t
    |}]
;;

let%expect_test "types in different modules are distinct" =
  check
    [ Decl.variant "t" [ "A", [] ]
    ; module_
        "M"
        [ Decl.variant "t" [ "A", [] ]
        ; top "inner" [] (construct "A")
        ; top "outer" [] ~annot:(Ty.con "t") (construct "M.A")
        ]
    ; top "root" [] (construct "A")
    ; top "qualified" [] (construct "M.A")
    ];
  check
    [ Decl.variant "t" [ "A", [] ]
    ; module_ "M" [ Decl.variant "t" [ "A", [] ] ]
    ; top "mixed" [] ~annot:(Ty.con "t") (construct "M.A")
    ];
  [%expect
    {|
    type t = A
    module M : sig
      type t = A
      val inner : t
      val outer : t
    end
    val root : t
    val qualified : M.t
    (Unification_error
     (Type_mismatch (TCon t ()) (TCon M.t ()) (Base_types_not_equal t M.t)))
    |}]
;;

let%expect_test "types resolve lexically" =
  check
    [ Decl.variant "t" [ "R", [] ]
    ; Decl.variant "u" [ "U", [] ]
    ; module_
        "M"
        [ Decl.variant "t" [ "T", [] ]
        ; module_
            "N"
            [ Decl.variant "s" [ "S", [ Ty.con "t"; Ty.con "u" ] ]
            ; top "s" [] (construct "S" ~args:[ construct "T"; construct "U" ])
            ]
        ; top "t" [] ~annot:(Ty.con "t") (construct "T")
        ; top "root_u" [] ~annot:(Ty.con "u") (construct "U")
        ]
    ; top "s" [] (construct "M.N.S" ~args:[ construct "M.T"; construct "U" ])
    ];
  [%expect
    {|
    type t = R
    type u = U
    module M : sig
      type t = T
      module N : sig
        type s = S of t * u
        val s : s
      end
      val t : t
      val root_u : u
    end
    val s : M.N.s
    |}]
;;

let%expect_test "types are order independent and recursive across modules" =
  check
    [ top
        "tree"
        []
        (construct
           "A.Node"
           ~args:
             [ construct
                 "B.Cons"
                 ~args:[ construct "A.Leaf"; construct "B.Nil" ]
             ])
    ; module_
        "A"
        [ Decl.variant "tree" [ "Leaf", []; "Node", [ Ty.con "B.forest" ] ] ]
    ; module_
        "B"
        [ Decl.variant
            "forest"
            [ "Nil", []; "Cons", [ Ty.con "A.tree"; Ty.con "forest" ] ]
        ; Decl.alias "trees" (Ty.tuple [ Ty.con "A.tree"; Ty.con "A.tree" ])
        ]
    ; top
        "pair"
        []
        ~annot:(Ty.con "B.trees")
        (tuple [ construct "A.Leaf"; var "tree" ])
    ];
  [%expect
    {|
    val tree : A.tree
    module A : sig
      type tree = Leaf | Node of B.forest
    end
    module B : sig
      type forest = Nil | Cons of A.tree * forest
      type trees = A.tree * A.tree
    end
    val pair : A.tree * A.tree
    |}]
;;

let%expect_test "cyclic aliases across modules" =
  check
    [ module_ "A" [ Decl.alias "t" (Ty.con "B.u") ]
    ; module_ "B" [ Decl.alias "u" (Ty.arrow Ty.int (Ty.con "A.t")) ]
    ];
  check
    [ module_ "A" [ Decl.alias "t" (Ty.con "u"); Decl.alias "u" Ty.int ]
    ; Decl.alias "u" (Ty.con "A.t")
    ; top "x" [] ~annot:(Ty.con "u") (int 1)
    ];
  [%expect
    {|
    (Cyclic_type_alias A.t)
    module A : sig
      type t = int
      type u = int
    end
    type u = int
    val x : int
    |}]
;;

let%expect_test "builtin types are declared in the root module" =
  check [ Decl.variant "int" [ "I", [] ] ];
  check [ Decl.alias "unit" Ty.bool ];
  check
    [ module_ "M" [ Decl.variant "bool" [ "B", [] ] ]
    ; Decl.alias "bool" Ty.int
    ];
  check
    [ module_
        "M"
        [ Decl.variant "unit" [ "U", [] ]
        ; Decl.alias "int" Ty.bool
        ; top "u" [] ~annot:(Ty.con "unit") (construct "U")
        ; top "i" [] ~annot:(Ty.con "int") (bool true)
        ; module_ "N" [ top "nested" [] ~annot:(Ty.con "unit") (construct "U") ]
        ]
    ; top "root" [] ~annot:(Ty.con "unit") unit
    ; top "qualified" [] ~annot:(Ty.con "M.unit") (construct "M.U")
    ];
  check
    [ module_
        "M"
        [ Decl.variant "unit" [ "U", [] ]
        ; top "lit" [] ~annot:(Ty.con "unit") unit
        ]
    ];
  [%expect
    {|
    (Duplicate_type int)
    (Duplicate_type unit)
    (Duplicate_type bool)
    module M : sig
      type unit = U
      type int = bool
      val u : unit
      val i : bool
      module N : sig
        val nested : unit
      end
    end
    val root : unit
    val qualified : M.unit
    (Unification_error
     (Type_mismatch (TCon M.unit ()) (TCon unit ())
      (Base_types_not_equal M.unit unit)))
    |}]
;;

let%expect_test "duplicates are per module" =
  check
    [ module_
        "M"
        [ top "x" [] (int 1)
        ; Decl.variant "t" [ "A", [] ]
        ; Decl.record "r" [ "f", Ty.int ]
        ; module_ "N" []
        ]
    ; module_
        "K"
        [ top "x" [] (bool true)
        ; Decl.variant "t" [ "A", [] ]
        ; Decl.record "r" [ "f", Ty.bool ]
        ; module_ "N" []
        ]
    ; top "x" [] unit
    ; Decl.variant "t" [ "A", [] ]
    ; Decl.record "r" [ "f", Ty.unit ]
    ; module_ "N" []
    ; top
        "fields"
        []
        (tuple
           [ field (record [ "M.f", int 1 ]) "M.f"
           ; field (record [ "K.f", bool true ]) "K.f"
           ])
    ];
  check [ module_ "M" []; module_ "M" [] ];
  check [ module_ "M" [ module_ "N" []; module_ "N" [] ] ];
  check [ module_ "M" [ top "x" [] (int 1); top "x" [] (int 2) ] ];
  check [ module_ "M" [ Decl.variant "t" []; Decl.alias "t" Ty.int ] ];
  check
    [ module_ "M" [ Decl.variant "t" [ "A", [] ]; Decl.variant "u" [ "A", [] ] ]
    ];
  check [ module_ "M" [ Decl.record "t" [ "x", Ty.int; "x", Ty.int ] ] ];
  check
    [ module_ "M" [ Decl.variant "t" ~params:[ Decl.ty "a"; Decl.ty "a" ] [] ] ];
  [%expect
    {|
    module M : sig
      val x : int
      type t = A
      type r = { f : int }
      module N : sig end
    end
    module K : sig
      val x : bool
      type t = A
      type r = { f : bool }
      module N : sig end
    end
    val x : unit
    type t = A
    type r = { f : unit }
    module N : sig end
    val fields : int * bool
    (Duplicate_module M)
    (Duplicate_module M.N)
    (Duplicate_toplevel M.x)
    (Duplicate_type M.t)
    (Duplicate_constructor M.A)
    (Duplicate_field M.x)
    (Duplicate_type_parameter (type_name M.t) (param a))
    |}]
;;

let%expect_test "unbound modules and members" =
  check [ top "v" [] (var "M.x") ];
  check [ top "v" [] ~annot:(Ty.con "M.t") (int 1) ];
  check [ top "c" [] (construct "M.A") ];
  check [ top "f" [ "p" ] (field (var "p") "M.x") ];
  check [ top "r" [] (record [ "M.x", int 1 ]) ];
  check [ top "p" [ "v" ] (match_ (var "v") [ Pat.construct "M.A", int 1 ]) ];
  check
    [ top
        "p"
        [ "v" ]
        (match_ (var "v") [ Pat.record [ "M.x", Pat.any ], int 1 ])
    ];
  check [ Decl.variant "t" [ "A", [ Ty.con "M.t" ] ] ];
  check [ module_ "M" []; top "v" [] (var "M.x") ];
  check [ module_ "M" []; top "v" [] ~annot:(Ty.con "M.t") (int 1) ];
  check [ module_ "M" []; top "c" [] (construct "M.A") ];
  check [ module_ "M" []; top "f" [ "p" ] (field (var "p") "M.x") ];
  check [ module_ "M" [ top "x" [] (var "y") ] ];
  [%expect
    {|
    (Unbound_module M)
    (Unbound_module M)
    (Unbound_module M)
    (Unbound_module M)
    (Unbound_module M)
    (Unbound_module M)
    (Unbound_module M)
    (Unbound_module M)
    (Not_in_scope M.x)
    (Unbound_type M.t)
    (Unbound_constructor M.A)
    (Unbound_field M.x)
    (Not_in_scope y)
    |}]
;;

let%expect_test "errors name canonical constructors, fields and types" =
  let m =
    module_
      "M"
      [ Decl.variant "t" [ "A", [ Ty.int ] ]
      ; Decl.record "r" [ "x", Ty.int; "y", Ty.int ]
      ; Decl.record "s" [ "z", Ty.int ]
      ; Decl.variant "p" ~params:[ Decl.ty "a" ] []
      ]
  in
  check [ m; top "a" [] (construct "M.A") ];
  check [ m; top "r" [] (record [ "M.x", int 1 ]) ];
  check [ m; top "r" [] (record [ "M.x", int 1; "M.y", int 1; "M.x", int 1 ]) ];
  check [ m; top "r" [] (record [ "M.x", int 1; "M.z", int 1 ]) ];
  check [ m; top "p" [] ~annot:(Ty.con "M.p") (int 1) ];
  check [ m; module_ "M2" [ top "x" [] (construct "M.A" ~args:[ bool true ]) ] ];
  [%expect
    {|
    (Constructor_arity_mismatch (constructor M.A) (expected 1) (got 0))
    (Missing_field M.y)
    (Duplicate_field M.x)
    (Fields_of_different_types (field M.z) (expected_type M.r) (actual_type M.s))
    (Type_arity_mismatch (type_name M.p) (expected 1) (got 0))
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "persistent toplevels of modules are usable in quotes" =
  check
    [ module_
        "M"
        [ top "f" [ "x" ] (app (var "add") [ var "x"; int 1 ])
        ; top "inner" [] (quote (app (var "f") [ int 1 ]))
        ]
    ; top "outer" [] (quote (app (var "M.f") [ int 1 ]))
    ];
  [%expect
    {|
    module M : sig
      val f : int -> int
      val inner : <int>{..'a}
    end
    val outer : <int>{..'a}
    |}]
;;

let%expect_test "non-persistent toplevels are free variables named canonically" =
  let q name = app (var name) [ unit ] in
  check
    [ top "n" [] (bool true)
    ; module_
        "M"
        [ top "n" [] (int 5)
        ; top "q" [ "u" ] (quote (var "n"))
        ; top "both" [] (quote (tuple [ var "n"; var "M.n" ]))
        ; top "local" [] (quote (let_ "n" [] unit (var "n")))
        ; top "root_code" [] (quote (splice (q "q_root")))
        ]
    ; top "q" [ "u" ] (quote (var "M.n"))
    ; top "q_root" [ "u" ] (quote (var "n"))
    ; top "spliced" [] (quote (splice (q "M.q")))
    ; top "not_captured" [] (quote (let_ "n" [] unit (splice (q "M.q"))))
    ; top "captured" [] (quote (let_ "n" [] unit (splice (q "q_root"))))
    ];
  [%expect
    {|
    val n : bool
    module M : sig
      val n : int
      val q : 'a -> <'b>{M.n : 'b; ..'c}
      val both : <'a * 'a>{M.n : 'a; ..'b}
      val local : <unit>{..'a}
      val root_code : <'_a>{n : '_a; ..'_b}
    end
    val q : 'a -> <'b>{M.n : 'b; ..'c}
    val q_root : 'a -> <'b>{n : 'b; ..'c}
    val spliced : <'_a>{M.n : '_a; ..'_b}
    val not_captured : <'_a>{M.n : '_a; ..'_b}
    val captured : <unit>{..'_a}
    |}]
;;

let%expect_test "splicing code with canonically named free variables" =
  check
    [ module_ "M" [ top "n" [] (int 5) ]
    ; top "implicit" [] (splice (quote (var "M.n")))
    ; top
        "local_does_not_provide"
        [ "n" ]
        (seq
           (app (var "cond") [ var "n"; unit; unit ])
           (splice (quote (var "M.n"))))
    ; top
        "explicit"
        []
        (splice ~subs:[ "M.n", Some (Lit (String "s")) ] (quote (var "M.n")))
    ; top "shorthand" [] (splice ~subs:[ "M.n", None ] (quote (var "M.n")))
    ];
  check
    [ module_
        "M"
        [ top "n" [] (int 5)
        ; top "as_written" [] (splice ~subs:[ "n", None ] (quote (var "n")))
        ]
    ];
  [%expect
    {|
    module M : sig
      val n : int
    end
    val implicit : int
    val local_does_not_provide : bool -> int
    val explicit : string
    val shorthand : int
    (Unification_error
     (Row_mismatch
      (RExtend M.n (PVar (Link Present)) (TVar (Unbound 25 1))
       (RVar (Unbound 26 1)))
      (RExtend n (PVar (Unbound 27 1)) (TCon int ()) REmpty) (Missing_label M.n)))
    |}]
;;

let%expect_test "non-persistent toplevels used at the wrong level" =
  check [ module_ "M" [ top "n" [] (int 1) ]; top "x" [] (splice (var "M.n")) ];
  [%expect
    {| (Variable_used_at_wrong_level (name M.n) (var_level 0) (used_at -1)) |}]
;;
