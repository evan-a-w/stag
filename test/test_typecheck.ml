open! Core
open Stag
open Syntax

let int n = Expr.Lit (Int (Int.to_string n))
let bool b = Expr.Lit (Bool b)
let var name = Expr.Var (name, None)
let app f args = List.fold args ~init:f ~f:(fun f x -> Expr.App (f, x))
let args = List.map ~f:(fun name -> name, None)

let let_ ?annot name params equal in_ =
  Expr.Let (name, args params, annot, equal, in_)
;;

let seq first second = let_ "_" [] first second
let quote expr = Expr.Quote expr
let splice ?subs expr = Expr.Splice { expr; subs }
let top ?annot name params expr = Toplevel.Let (name, args params, annot, expr)
let arrow a b = Type.TArrow (a, b)

let prims =
  let int_arg name = name, Some Type.int in
  [ Toplevel.Let ("add", [ int_arg "x"; int_arg "y" ], Some Type.int, var "x")
  ; Let ("eq", [ int_arg "x"; int_arg "y" ], Some Type.bool, bool true)
  ; Let ("cond", [ "c", Some Type.bool; "x", None; "y", None ], None, var "x")
  ; Let ("id", [ "x", None ], None, var "x")
  ]
;;

let print_types types =
  List.iter types ~f:(fun (name, ty) ->
    printf "%s : %s\n" name (Type.to_string_hum ty))
;;

let check program =
  match
    Typecheck.typecheck (Typecheck.create ()) ~program:(prims @ program)
  with
  | Ok types -> print_types (List.drop types (List.length prims))
  | Error error -> print_s [%sexp (error : Typecheck.Typecheck_error.t)]
;;

let%expect_test "primitives" =
  Typecheck.typecheck (Typecheck.create ()) ~program:prims
  |> Result.ok
  |> Option.value_exn
  |> print_types;
  [%expect
    {|
    add : int -> int -> int
    eq : int -> int -> bool
    cond : bool -> 'a -> 'b -> 'a
    id : 'a -> 'a
    |}]
;;

let%expect_test "literals and application" =
  check
    [ top "i" [] (int 1)
    ; top "b" [] (bool false)
    ; top "s" [] (Lit (String "s"))
    ; top "c" [] (Lit (Char 'c'))
    ; top "u" [] (Lit Unit)
    ; top "sum" [] (app (var "add") [ int 1; int 2 ])
    ; top "partial" [] (app (var "add") [ int 1 ])
    ];
  [%expect
    {|
    i : int
    b : bool
    s : string
    c : char
    u : unit
    sum : int
    partial : int -> int
    |}]
;;

let%expect_test "application errors" =
  check [ top "bad" [] (app (var "add") [ bool true ]) ];
  check [ top "bad" [] (app (int 1) [ int 2 ]) ];
  [%expect
    {|
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Unification_error
     (Type_mismatch (TArrow (TVar (Unbound 11 1)) (TVar (Unbound 12 1)))
      (TCon int ()) Structural_mismatch))
    |}]
;;

let%expect_test "functions are generalized" =
  check
    [ top "k" [ "x"; "y" ] (var "x")
    ; top "flip" [ "f"; "x"; "y" ] (app (var "f") [ var "y"; var "x" ])
    ; top
        "both"
        []
        (seq
           (app (var "k") [ int 1; bool true ])
           (app (var "k") [ bool true; int 1 ]))
    ];
  [%expect
    {|
    k : 'a -> 'b -> 'a
    flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
    both : bool
    |}]
;;

let%expect_test "value restriction" =
  check [ top "weak" [] (app (var "id") [ var "id" ]) ];
  check
    [ top "weak" [] (app (var "id") [ var "id" ])
    ; top
        "use"
        []
        (seq (app (var "weak") [ int 1 ]) (app (var "weak") [ bool true ]))
    ];
  [%expect
    {|
    weak : '_a -> '_a
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "annotations" =
  check [ top "f" [ "x" ] ~annot:Type.int (var "x") ];
  check [ top "f" [] (Var ("id", Some (arrow Type.bool Type.bool))) ];
  check
    [ top "f" [ "x" ] ~annot:Type.bool (app (var "add") [ var "x"; int 1 ]) ];
  [%expect
    {|
    f : int -> int
    f : bool -> bool
    (Unification_error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
    |}]
;;

let%expect_test "scope errors" =
  check [ top "f" [] (var "nope") ];
  check [ top "f" [ "x" ] (var "x"); top "f" [] (int 1) ];
  check [ top "f" [ "x" ] (var "g"); top "g" [ "y" ] (var "x") ];
  [%expect
    {|
    (Not_in_scope nope)
    (Duplicate_toplevel f)
    (Not_in_scope x)
    |}]
;;

let%expect_test "toplevels are order independent and generalized per SCC" =
  check
    [ top
        "main"
        []
        (seq (app (var "f") [ int 1 ]) (app (var "f") [ bool true ]))
    ; top "f" [ "x" ] (app (var "g") [ var "x" ])
    ; top "g" [ "x" ] (var "x")
    ];
  [%expect
    {|
    main : bool
    f : 'a -> 'a
    g : 'a -> 'a
    |}]
;;

let%expect_test "mutual recursion is monomorphic within an SCC" =
  check
    [ top "a" [ "x" ] (app (var "b") [ int 1 ])
    ; top "b" [ "y" ] (seq (app (var "a") [ bool true ]) (var "y"))
    ; top "c" [] (seq (app (var "a") [ bool true ]) (app (var "b") [ int 2 ]))
    ];
  check
    [ top "a" [ "x" ] (app (var "b") [ int 1 ])
    ; top
        "b"
        [ "y" ]
        (seq (app (var "a") [ bool true ]) (app (var "a") [ int 1 ]))
    ];
  [%expect
    {|
    a : bool -> int
    b : int -> int
    c : int
    (Unification_error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
    |}]
;;

let%expect_test "SCC containing a non-function is not generalized" =
  check
    [ top "x" [] (app (var "f") [ int 1 ])
    ; top "f" [ "y" ] (seq (var "x") (var "y"))
    ];
  [%expect
    {|
    x : int
    f : int -> int
    |}]
;;

let%expect_test "an SCC below an in-progress toplevel is generalized" =
  check
    [ top
        "a"
        [ "x" ]
        (seq (app (var "b") [ var "a" ]) (app (var "c") [ int 1 ]))
    ; top "b" [ "x" ] (var "x")
    ; top "c" [ "x" ] (app (var "d") [ var "x" ])
    ; top
        "d"
        [ "x" ]
        (seq
           (app (var "c") [ var "x" ])
           (seq (app (var "a") [ var "x" ]) (var "x")))
    ; top "e" [] (seq (app (var "b") [ int 1 ]) (app (var "b") [ bool true ]))
    ];
  [%expect
    {|
    a : int -> int
    b : 'a -> 'a
    c : int -> int
    d : int -> int
    e : bool
    |}]
;;

let%expect_test "local lets" =
  check
    [ top
        "poly"
        []
        (let_
           "i"
           [ "x" ]
           (var "x")
           (seq (app (var "i") [ int 1 ]) (app (var "i") [ bool true ])))
    ; top
        "rec_"
        []
        (let_ "loop" [ "x" ] (app (var "loop") [ var "x" ]) (var "loop"))
    ; top "shadow" [ "x" ] (let_ "x" [] (int 1) (var "x"))
    ; top
        "arg_scope"
        [ "x" ]
        (seq (let_ "f" [ "x" ] (bool true) (var "f")) (var "x"))
    ];
  check
    [ top
        "mono"
        []
        (let_
           "i"
           []
           (var "id")
           (seq (app (var "i") [ int 1 ]) (app (var "i") [ bool true ])))
    ];
  check
    [ top
        "mono_rec"
        []
        (let_
           "f"
           [ "x" ]
           (seq (app (var "f") [ int 1 ]) (app (var "f") [ bool true ]))
           (var "f"))
    ];
  [%expect
    {|
    poly : bool
    rec_ : '_a -> '_b
    shadow : 'a -> int
    arg_scope : 'a -> 'a
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "quotes" =
  check
    [ top "closed" [] (quote (int 1))
    ; top
        "open"
        []
        (quote
           (app (var "add") [ var "y"; app (var "add") [ var "x"; var "y" ] ]))
    ; top
        "local"
        []
        (quote (let_ "y" [] (int 1) (app (var "add") [ var "y"; var "y" ])))
    ; top "persistent" [] (quote (app (var "id") [ int 1 ]))
    ; top "n" [] (int 5)
    ; top "not_persistent" [] (quote (var "n"))
    ; top "outer_arg" [ "x" ] (quote (var "x"))
    ; top "nested" [] (quote (quote (app (var "add") [ var "x"; int 1 ])))
    ];
  [%expect
    {|
    closed : <int>{..'_a}
    open : <int>{x : int; y : int; ..'_a}
    local : <int>{..'_a}
    persistent : <int>{..'_a}
    n : int
    not_persistent : <'_a>{n : '_a; ..'_b}
    outer_arg : 'a -> <'b>{x : 'b; ..'c}
    nested : <<int>{x : int; ..'_a}>{..'_b}
    |}]
;;

let%expect_test "free variables of a quote are not generalized by inner lets" =
  check
    [ top
        "f"
        [ "u" ]
        (quote
           (let_
              "g"
              [ "v" ]
              (var "x")
              (seq
                 (app (var "add") [ app (var "g") [ int 1 ]; int 1 ])
                 (app (var "g") [ bool true ]))))
    ];
  [%expect {| f : 'a -> <int>{x : int; ..'b} |}]
;;

let%expect_test "splice with explicit substitutions" =
  let code = quote (app (var "add") [ var "x"; var "y" ]) in
  check
    [ top
        "values"
        []
        (quote (splice ~subs:[ "x", Some (int 1); "y", Some (int 2) ] code))
    ; top
        "reorder"
        []
        (quote (splice ~subs:[ "y", Some (int 1); "x", Some (int 2) ] code))
    ; top
        "from_scope"
        []
        (quote
           (let_ "x" [] (int 1) (splice ~subs:[ "x", None; "y", None ] code)))
    ; top
        "infers_sub"
        []
        (quote (splice ~subs:[ "x", Some (var "z"); "y", Some (int 1) ] code))
    ; top "closed" [] (quote (splice ~subs:[] (quote (int 1))))
    ];
  [%expect
    {|
    values : <int>{..'_a}
    reorder : <int>{..'_a}
    from_scope : <int>{y : int; ..'_a}
    infers_sub : <int>{z : int; ..'_a}
    closed : <int>{..'_a}
    |}]
;;

let%expect_test "splice substitution errors" =
  let code = quote (app (var "add") [ var "x"; int 1 ]) in
  check
    [ top "wrong_type" [] (quote (splice ~subs:[ "x", Some (bool true) ] code))
    ];
  check [ top "missing" [] (quote (splice ~subs:[] code)) ];
  check
    [ top
        "extra"
        []
        (quote (splice ~subs:[ "x", Some (int 1); "y", Some (int 1) ] code))
    ];
  check
    [ top
        "dup"
        []
        (quote (splice ~subs:[ "x", Some (int 1); "x", Some (int 1) ] code))
    ];
  check [ top "not_code" [] (quote (splice ~subs:[] (int 1))) ];
  [%expect
    {|
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Unification_error
     (Context_mismatch
      (CExtend x (TVar (Link (TCon int ()))) (CVar (CUnbound 15 1))) CEmpty
      (Empty_context_has_variable x)))
    extra : <int>{..'_a}
    (Duplicate_substitution x)
    (Unification_error
     (Type_mismatch
      (TCode (inner (TVar (Unbound 13 1))) (context (CVar (CUnbound 14 1))))
      (TCon int ()) Structural_mismatch))
    |}]
;;

let%expect_test "splice with implicit substitutions" =
  let code = quote (app (var "add") [ var "x"; int 1 ]) in
  check
    [ top "bound" [] (quote (let_ "x" [] (int 1) (splice code)))
    ; top "propagates" [] (quote (splice code))
    ; top "unknown_context" [ "c" ] (quote (splice (var "c")))
    ; top
        "explicit_unknown"
        [ "c" ]
        (quote (splice ~subs:[ "x", Some (int 1) ] (var "c")))
    ];
  check
    [ top
        "two_unknown"
        [ "c1"; "c2" ]
        (quote (app (var "add") [ splice (var "c1"); splice (var "c2") ]))
    ; top
        "known_and_unknown"
        [ "c" ]
        (quote
           (let_
              "x"
              []
              (int 1)
              (splice
                 (Var
                    ( "c"
                    , Some
                        (TCode
                           { inner = Type.int
                           ; context =
                               CExtend
                                 ( "x"
                                 , Type.int
                                 , CVar
                                     (ref (Type.CUnbound (-1, Int.max_value)))
                                 )
                           }) )))))
    ; top
        "with_free_var"
        [ "c" ]
        (quote (app (var "add") [ var "y"; splice (var "c") ]))
    ; top "instantiated" [] (app (var "with_free_var") [ quote (var "z") ])
    ];
  check
    [ top
        "two_unknown"
        [ "c1"; "c2" ]
        (quote (app (var "add") [ splice (var "c1"); splice (var "c2") ]))
    ; top
        "conflict"
        []
        (app
           (var "two_unknown")
           [ quote (app (var "add") [ var "y"; int 1 ])
           ; quote (app (var "cond") [ var "y"; int 1; int 1 ])
           ])
    ];
  check
    [ top
        "toplevel_unknown"
        []
        (splice
           (let_
              "opaque"
              [ "u" ]
              (app (var "opaque") [ var "u" ])
              (app (var "opaque") [ int 1 ])))
    ];
  check [ top "wrong_type" [] (quote (let_ "x" [] (bool true) (splice code))) ];
  [%expect
    {|
    bound : <int>{..'_a}
    propagates : <int>{x : int; ..'_a}
    unknown_context : <'a>{..'b} -> <'a>{..'b}
    explicit_unknown : <'a>{x : int} -> <'a>{..'b}
    two_unknown : <int>{..'a} -> <int>{..'a} -> <int>{..'a}
    known_and_unknown : <int>{x : int; ..'a} -> <int>{..'a}
    with_free_var : <int>{..'a} -> <int>{y : int; ..'a}
    instantiated : <int>{y : int; z : int; ..'_a}
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    toplevel_unknown : '_a
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "levels" =
  check
    [ top "inner_in_splice" [] (quote (let_ "y" [] (int 1) (splice (var "y"))))
    ];
  check
    [ top
        "outer_in_splice"
        [ "c" ]
        (quote (splice (app (var "id") [ var "c" ])))
    ; top
        "rebind_inner"
        []
        (quote (let_ "y" [] (int 1) (splice (quote (var "y")))))
    ; top "toplevel_splice" [] (splice (quote (int 1)))
    ];
  check
    [ top "toplevel_splice_arg" [ "x" ] (splice (quote (var "x")))
    ; top
        "not_captured"
        []
        (quote
           (let_ "x" [] (int 1) (splice ~subs:[ "x", None ] (quote (var "x")))))
    ];
  check [ top "toplevel_splice_open" [] (splice (quote (var "x"))) ];
  check
    [ top
        "captured"
        []
        (quote (let_ "x" [] (bool true) (splice ~subs:[] (quote (var "x")))))
    ];
  check [ top "toplevel_splice_level" [ "x" ] (splice (var "x")) ];
  [%expect
    {|
    (Variable_used_at_wrong_level (name y) (var_level 1) (used_at 0))
    outer_in_splice : <'a>{..'b} -> <'a>{..'b}
    rebind_inner : <int>{..'_a}
    toplevel_splice : int
    toplevel_splice_arg : 'a -> 'a
    not_captured : <int>{..'_a}
    (Not_in_scope x)
    (Unification_error
     (Context_mismatch (CExtend x (TVar (Unbound 18 1)) (CVar (CUnbound 16 1)))
      CEmpty (Empty_context_has_variable x)))
    (Variable_used_at_wrong_level (name x) (var_level 0) (used_at -1))
    |}]
;;

let%expect_test "staged power" =
  let power =
    top
      "power"
      [ "n"; "x" ]
      (app
         (var "cond")
         [ app (var "eq") [ var "n"; int 0 ]
         ; quote (int 1)
         ; quote
             (app
                (var "add")
                [ splice (var "x")
                ; splice
                    (app
                       (var "power")
                       [ app (var "add") [ var "n"; int (-1) ]; var "x" ])
                ])
         ])
  in
  check [ power; top "cube" [] (app (var "power") [ int 3; quote (int 2) ]) ];
  check [ power; top "open" [] (app (var "power") [ int 3; quote (var "y") ]) ];
  check
    [ power
    ; top "wrong_inner" [] (app (var "power") [ int 3; quote (bool true) ])
    ];
  [%expect
    {|
    power : int -> <int>{..'a} -> <int>{..'a}
    cube : <int>{..'_a}
    power : int -> <int>{..'a} -> <int>{..'a}
    open : <int>{y : int; ..'_a}
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;
