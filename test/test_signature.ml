open! Core
open Stag
open Syntax
open Helpers

let typecheck_exn program =
  let typecheck = Typecheck.create () in
  match Typecheck.typecheck typecheck ~program with
  | Ok signature -> signature
  | Error error -> raise_s [%sexp (error : Typecheck_error.t)]
;;

let print program = print_signature (typecheck_exn program)

let%expect_test "items mirror the program" =
  print
    [ top "first" [] (construct "M.N.Leaf")
    ; Decl.list
    ; Decl.variant
        "stmt"
        ~params:[ Decl.row "r"; Decl.ty "a" ]
        [ "Stmt", [ Ty.code (Ty.var "a") ~tail:"r" ] ]
    ; Decl.record
        "point"
        [ "x", Ty.int; "y", Ty.con "list" ~args:[ Ty Ty.int ] ]
    ; Decl.alias
        "pair"
        ~params:[ Decl.ty "b"; Decl.ty "a" ]
        (Ty.tuple [ Ty.var "a"; Ty.var "b" ])
    ; Decl.variant "void" []
    ; Decl.variant
        "args"
        [ "Arrow", [ Ty.arrow Ty.int Ty.int ]
        ; "Tuple", [ Ty.tuple [ Ty.int; Ty.bool ] ]
        ; "Two", [ Ty.int; Ty.arrow Ty.int Ty.int ]
        ]
    ; module_
        "M"
        [ module_ "Empty" []
        ; module_
            "N"
            [ Decl.variant "tree" [ "Leaf", []; "Node", [ Ty.con "tree" ] ] ]
        ; top "id" [ "x" ] (var "x")
        ; top "code" [ "c" ] (quote (splice (var "c")))
        ; Decl.alias "t" (Ty.con "pair" ~args:[ Ty Ty.int; Ty Ty.bool ])
        ]
    ; top "weak" [] (app (var "M.id") [ var "M.id" ])
    ; top "code" [ "c" ] (quote (splice (var "c")))
    ];
  [%expect
    {|
    val first : M.N.tree
    type list['a] = Nil | Cons of 'a * list['a]
    type stmt[{..'a}, 'b] = Stmt of <'b>{..'a}
    type point = { x : int; y : list[int] }
    type pair['a, 'b] = 'b * 'a
    type void = |
    type args = Arrow of (int -> int) | Tuple of (int * bool) | Two of int * (int -> int)
    module M : sig
      module Empty : sig end
      module N : sig
        type tree = Leaf | Node of tree
      end
      val id : 'a -> 'a
      val code : <'a>{..'b} -> <'a>{..'c} where ..'b <= {..'c}
      type t = bool * int
    end
    val weak : '_a -> '_a
    val code : <'a>{..'b} -> <'a>{..'c} where ..'b <= {..'c}
    |}]
;;

let%expect_test "variables are named per item" =
  print
    [ top "f" [ "x"; "y" ] (var "y")
    ; Decl.variant "t" ~params:[ Decl.ty "z" ] [ "T", [ Ty.var "z" ] ]
    ; top "weak" [] (app (var "f") [ int 1 ])
    ; top "weak_too" [] (app (var "f") [ bool true ])
    ];
  [%expect
    {|
    val f : 'a -> 'b -> 'b
    type t['a] = T of 'a
    val weak : '_a -> '_a
    val weak_too : '_a -> '_a
    |}]
;;

let%expect_test "builtin types are not items" =
  print [];
  [%expect {| |}]
;;

let%expect_test "types are named relative to the item's module" =
  print
    [ Decl.variant "r" [ "R", [] ]
    ; module_
        "M"
        [ Decl.variant "m" [ "Mk", [ Ty.con "r"; Ty.con "N.n" ] ]
        ; module_
            "N"
            [ Decl.variant
                "n"
                [ "Nn", [ Ty.con "n"; Ty.con "m"; Ty.con "r"; Ty.con "P.p" ] ]
            ]
        ; module_ "P" [ Decl.variant "p" [ "Pp", [] ] ]
        ; Decl.alias "a" (Ty.arrow (Ty.con "N.n") (Ty.con "P.p"))
        ]
    ; module_ "Sibling" [ Decl.alias "b" (Ty.con "M.N.n") ]
    ; Decl.alias "c" (Ty.tuple [ Ty.con "M.N.n"; Ty.con "M.m"; Ty.con "r" ])
    ];
  [%expect
    {|
    type r = R
    module M : sig
      type m = Mk of r * N.n
      module N : sig
        type n = Nn of n * m * r * P.p
      end
      module P : sig
        type p = Pp
      end
      type a = N.n -> P.p
    end
    module Sibling : sig
      type b = M.N.n
    end
    type c = M.N.n * M.m * r
    |}]
;;

let%expect_test "params, bounds and every part of an item are relative" =
  print
    [ module_
        "M"
        [ Decl.variant "t" ~params:[ Decl.ty "a" ] [ "T", [ Ty.var "a" ] ]
        ; Decl.variant
            "v"
            ~params:[ Decl.ty "a" ]
            [ "V", [ Ty.con "t" ~args:[ Ty (Ty.var "a") ] ] ]
        ; Decl.record "f" [ "x", Ty.con "t" ~args:[ Ty Ty.int ] ]
        ; top "wrap" [ "x" ] (construct "T" ~args:[ var "x" ])
        ; Toplevel.Let
            ( "use"
            , [ "x", Some (Ty.con "t" ~args:[ Ty Ty.int ]); "y", None ]
            , None
            , var "y" )
        ; top
            "code"
            [ "c" ]
            (quote (app (var "use") [ var "free"; splice (var "c") ]))
        ]
    ];
  [%expect
    {|
    module M : sig
      type t['a] = T of 'a
      type v['a] = V of t['a]
      type f = { x : t[int] }
      val wrap : 'a -> t['a]
      val use : t[int] -> 'a -> 'a
      val code : <'a>{..'b} -> <'a>{free : t[int]; ..'c} where ..'b <= {free : t[int]; ..'c}
    end
    |}]
;;

let%expect_test "a shadowed root type is written after a leading [.]" =
  print
    [ Decl.variant "t" [ "Root", [] ]
    ; module_ "N" [ Decl.variant "n" [ "Outer", [] ] ]
    ; top "root_outer" [] (construct "N.Outer")
    ; module_
        "M"
        [ Decl.variant "t" [ "Inner", [] ]
        ; module_ "N" [ Decl.variant "u" [ "U", [] ] ]
        ; top "root" [] (construct "Root")
        ; top "inner" [] (construct "Inner")
        ; top "outer" [] (var "root_outer")
        ; module_ "Deeper" [ top "root" [] (construct "Root") ]
        ]
    ; top "root" [] (construct "Root")
    ];
  [%expect
    {|
    type t = Root
    module N : sig
      type n = Outer
    end
    val root_outer : N.n
    module M : sig
      type t = Inner
      module N : sig
        type u = U
      end
      val root : .t
      val inner : t
      val outer : .N.n
      module Deeper : sig
        val root : .t
      end
    end
    val root : t
    |}]
;;

let%expect_test "a shadowed module makes a longer path or [.] needed" =
  print
    [ module_
        "A"
        [ module_ "N" [ Decl.variant "tree" [ "Leaf", [] ] ]
        ; top "leaf" [] (construct "N.Leaf")
        ; module_
            "P"
            [ module_ "N" [ Decl.variant "tree" [ "P_leaf", [] ] ]
            ; top "outer" [] (var "leaf")
            ; top "inner" [] (construct "N.P_leaf")
            ]
        ]
    ; module_
        "M"
        [ module_ "N" [ Decl.variant "tree" [ "Leaf", [] ] ]
        ; top "leaf" [] (construct "N.Leaf")
        ; module_
            "M"
            [ module_ "N" [ Decl.variant "tree" [ "M_leaf", [] ] ]
            ; top "outer" [] (var "leaf")
            ; top "inner" [] (construct "N.M_leaf")
            ]
        ]
    ];
  [%expect
    {|
    module A : sig
      module N : sig
        type tree = Leaf
      end
      val leaf : N.tree
      module P : sig
        module N : sig
          type tree = P_leaf
        end
        val outer : A.N.tree
        val inner : N.tree
      end
    end
    module M : sig
      module N : sig
        type tree = Leaf
      end
      val leaf : N.tree
      module M : sig
        module N : sig
          type tree = M_leaf
        end
        val outer : .M.N.tree
        val inner : N.tree
      end
    end
    |}]
;;

let%expect_test "a type shadowing a builtin" =
  print
    [ top "root" [] (int 1)
    ; module_
        "M"
        [ Decl.variant "int" [ "I", [] ]
        ; top "mine" [] (construct "I")
        ; top "builtin" [] (int 1)
        ; module_
            "N"
            [ top "mine" [] (construct "I"); top "builtin" [] (int 1) ]
        ]
    ; top "mine" [] (construct "M.I")
    ];
  [%expect
    {|
    val root : int
    module M : sig
      type int = I
      val mine : int
      val builtin : .int
      module N : sig
        val mine : int
        val builtin : .int
      end
    end
    val mine : M.int
    |}]
;;

let%expect_test "sexp" =
  let signature =
    typecheck_exn
      [ Decl.record "box" ~params:[ Decl.ty "a" ] [ "contents", Ty.var "a" ]
      ; module_
          "M"
          [ top "f" [ "x" ] (record [ "contents", var "x" ])
          ; top "n" [] (int 1)
          ]
      ]
  in
  print_s [%sexp (signature : Signature.t)];
  [%expect
    {|
    ((Type
      ((name box) (params ((ATy (TVar (Generic 0)))))
       (kind (Record (((name contents) (ty (TVar (Generic 0)))))))))
     (Module (name M)
      (items
       ((Value
         ((name f)
          (scheme
           (TArrow (TVar (Generic 2))
            (TVar (Link (TCon box ((ATy (TVar (Link (TVar (Generic 2)))))))))))
          (persistent true)))
        (Value
         ((name n) (scheme (TVar (Link (TCon int ())))) (persistent false)))))))
    |}]
;;
