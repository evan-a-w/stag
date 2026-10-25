open! Core
open Stag
open Syntax
open Helpers

let%expect_test "primitives" =
  let typecheck = Typecheck.create () in
  Typecheck.typecheck typecheck ~program:prims
  |> Result.ok
  |> Option.value_exn
  |> print_types typecheck;
  [%expect
    {|
    add : int -> int -> int
    eq : int -> int -> bool
    cond : bool -> 'a -> 'a -> 'a
    id : 'a -> 'a
    |}]
;;

let%expect_test "typechecking twice with the same state" =
  let typecheck = Typecheck.create () in
  let run program =
    match Typecheck.typecheck typecheck ~program with
    | Ok types -> print_types typecheck types
    | Error error -> print_s [%sexp (error : Typecheck.Typecheck_error.t)]
  in
  run prims;
  run [ top "id" [ "x" ] (var "x"); top "n" [] (app (var "id") [ int 1 ]) ];
  [%expect
    {|
    add : int -> int -> int
    eq : int -> int -> bool
    cond : bool -> 'a -> 'a -> 'a
    id : 'a -> 'a
    id : 'a -> 'a
    n : int
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
     (Type_mismatch (TArrow (TVar (Unbound 18 1)) (TVar (Unbound 19 1)))
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
  check [ top "f" [ "x" ] ~annot:Ty.int (var "x") ];
  check [ top "f" [] (var "id" ~annot:(Ty.arrow Ty.bool Ty.bool)) ];
  check [ top "f" [ "x" ] ~annot:Ty.bool (app (var "add") [ var "x"; int 1 ]) ];
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

let%expect_test "SCC containing a non-value is not generalized" =
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
           (app (var "id") [ var "id" ])
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
    rec_ : 'a -> 'b
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
    closed : <int>{..'a}
    open : <int>{x : int; y : int; ..'a}
    local : <int>{..'a}
    persistent : <int>{..'a}
    n : int
    not_persistent : <'a>{n : 'a; ..'b}
    outer_arg : 'a -> <'b>{x : 'b; ..'c}
    nested : <<int>{x : int; ..'a}>{..'b}
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
    values : <int>{..'a}
    reorder : <int>{..'a}
    from_scope : <int>{y : int; ..'a}
    infers_sub : <int>{z : int; ..'a}
    closed : <int>{..'a}
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
      (CExtend x (PVar (PLink Present)) (TVar (Link (TCon int ())))
       (CVar (CUnbound 30 1)))
      CEmpty (Empty_context_has_variable x)))
    extra : <int>{..'a}
    (Duplicate_substitution x)
    (Unification_error
     (Type_mismatch
      (TCode (inner (TVar (Unbound 20 1))) (context (CVar (CUnbound 21 1))))
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
                 (var
                    "c"
                    ~annot:(Ty.code Ty.int ~entries:[ "x", Ty.int ] ~tail:"r")))))
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
    bound : <int>{..'a}
    propagates : <int>{x : int; ..'a}
    unknown_context : <'a>{..'b} -> <'a>{..'c} where ..'b <= {..'c}
    explicit_unknown : <'a>{x?'b : int} -> <'a>{..'c}
    two_unknown : <int>{..'a} -> <int>{..'b} -> <int>{..'c} where ..'a <= {..'c}, ..'b <= {..'c}
    known_and_unknown : <int>{x : int; ..'a} -> <int>{..'b} where ..'a <= {..'b}
    with_free_var : <int>{..'a} -> <int>{y : int; ..'b} where ..'a <= {y : int; ..'b}
    instantiated : <int>{y : int; z : int; ..'_a}
    (Unification_error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
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
    outer_in_splice : <'a>{..'b} -> <'a>{..'c} where ..'b <= {..'c}
    rebind_inner : <int>{..'a}
    toplevel_splice : int
    toplevel_splice_arg : 'a -> 'a
    not_captured : <int>{..'a}
    (Not_in_scope x)
    (Unification_error
     (Context_mismatch
      (CExtend x (PVar (PLink Present)) (TVar (Unbound 26 1))
       (CVar (CUnbound 27 1)))
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
    ; top
        "open_as_closed"
        []
        ~annot:(Ty.code Ty.int)
        (app (var "power") [ int 3; quote (var "y") ])
    ];
  check
    [ power
    ; top "wrong_inner" [] (app (var "power") [ int 3; quote (bool true) ])
    ];
  [%expect
    {|
    power : int -> <int>{..'a} -> <int>{..'b} where ..'a <= {..'b}
    cube : <int>{..'_a}
    power : int -> <int>{..'a} -> <int>{..'b} where ..'a <= {..'b}
    open : <int>{y : int; ..'_a}
    (Unification_error
     (Context_mismatch CEmpty
      (CExtend y (PVar (PLink Present)) (TVar (Link (TCon int ())))
       (CVar (CUnbound 67 1)))
      (Empty_context_has_variable y)))
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test
    "splices capture binders of the quote even once the code is known"
  =
  let captures_x =
    top
      "captures_x"
      [ "c" ]
      (quote (let_ "x" [] (bool true) (splice (var "c"))))
  in
  let closed = top "closed" [] ~annot:(Ty.code Ty.int) (quote (int 1)) in
  check
    [ captures_x
    ; closed
    ; top
        "uses_x"
        []
        (app
           (var "captures_x")
           [ quote (app (var "cond") [ var "x"; int 1; int 2 ]) ])
    ; top
        "uses_y"
        []
        (app (var "captures_x") [ quote (app (var "add") [ var "y"; int 1 ]) ])
    ; top "uses_closed" [] (app (var "captures_x") [ var "closed" ])
    ];
  check
    [ captures_x
    ; top
        "wrong_type"
        []
        (app (var "captures_x") [ quote (app (var "add") [ var "x"; int 1 ]) ])
    ];
  [%expect
    {|
    captures_x : <'a>{x?'b : bool; ..'c} -> <'a>{..'d} where ..'c <= {..'d}
    closed : <int>{}
    uses_x : <int>{..'_a}
    uses_y : <int>{y : int; ..'_a}
    uses_closed : <int>{..'_a}
    (Unification_error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
    |}]
;;

let%expect_test "splices capture the same whether the context is known or not" =
  let code = quote (app (var "add") [ var "x"; var "y" ]) in
  check
    [ top "known" [] (quote (let_ "x" [] (int 1) (splice code)))
    ; top "unknown" [ "c" ] (quote (let_ "x" [] (int 1) (splice (var "c"))))
    ; top "unknown_applied" [] (app (var "unknown") [ code ])
    ];
  [%expect
    {|
    known : <int>{y : int; ..'a}
    unknown : <'a>{x?'b : int; ..'c} -> <'a>{..'d} where ..'c <= {..'d}
    unknown_applied : <int>{y : int; ..'_a}
    |}]
;;

let%expect_test "spliced code that may use a name propagates it as such" =
  check
    [ top
        "twice"
        [ "c" ]
        (seq
           (quote (let_ "x" [] (bool true) (splice (var "c"))))
           (quote (splice (var "c"))))
    ; top
        "propagates"
        []
        (app
           (var "twice")
           [ quote (app (var "cond") [ var "x"; int 1; int 2 ]) ])
    ; top "not_used" [] (app (var "twice") [ quote (int 1) ])
    ];
  check
    [ top
        "direct_use"
        [ "c" ]
        (quote
           (let_
              "y"
              []
              (splice (quote (let_ "x" [] (bool true) (splice (var "c")))))
              (var "x")))
    ];
  [%expect
    {|
    twice : <'a>{x?'b : bool; ..'c} -> <'a>{x?'b : bool; ..'d} where ..'c <= {x?'b : bool; ..'d}, ..'c <= {..'e}
    propagates : <int>{x : bool; ..'_a}
    not_used : <int>{x?'_a : bool; ..'_b}
    direct_use : <'a>{x?'b : bool; ..'c} -> <'d>{x : 'd; ..'e} where ..'c <= {y?'f : 'a; ..'g}, ..'g <= {x : 'd; ..'e}
    |}]
;;

let%expect_test "only binders of the enclosing quote capture" =
  check
    [ top
        "outer_persistent"
        [ "c" ]
        (let_ "g" [ "v" ] (var "v") (quote (splice (var "c"))))
    ; top "add_free" [ "add" ] (quote (var "add"))
    ; top "not_toplevel" [] (quote (splice (app (var "add_free") [ int 1 ])))
    ];
  [%expect
    {|
    outer_persistent : <'a>{..'b} -> <'a>{..'c} where ..'b <= {..'c}
    add_free : 'a -> <'b>{add : 'b; ..'c}
    not_toplevel : <'_a>{add : '_a; ..'_b}
    |}]
;;

let%expect_test "names bound at a later level are absent from spliced code" =
  let inner_splice =
    top
      "inner_splice"
      [ "c" ]
      (quote (quote (let_ "z" [] (int 1) (splice (splice (var "c"))))))
  in
  check
    [ inner_splice
    ; top "uses_z" [] (app (var "inner_splice") [ quote (quote (var "z")) ])
    ];
  check
    [ inner_splice
    ; top
        "uses_z_too_early"
        []
        (app (var "inner_splice") [ quote (seq (var "z") (quote (int 1))) ])
    ];
  let known_z =
    Ty.code (Ty.code Ty.int ~tail:"r1") ~entries:[ "z", Ty.int ] ~tail:"r2"
  in
  check
    [ top
        "known_z"
        [ "c" ]
        (quote
           (quote
              (let_ "z" [] (int 1) (splice (splice (var "c" ~annot:known_z))))))
    ];
  [%expect
    {|
    inner_splice : <<'a>{z?'b : int; ..'c}>{-z; ..'d} -> <<'a>{..'e}>{..'f} where ..'c <= {..'e}, ..'d <= {..'f}
    uses_z : <<int>{..'_a}>{..'_b}
    (Unification_error
     (Context_mismatch
      (CExtend z Absent (TVar (Unbound 40 1)) (CVar (CUnbound 39 1)))
      (CExtend z (PVar (PLink Present)) (TVar (Unbound 47 1))
       (CVar (CUnbound 48 1)))
      (Presence_mismatch z)))
    (Variable_used_at_wrong_level (name z) (var_level 2) (used_at 1))
    |}]
;;

let%expect_test "free variables of a quote are not duplicated by spliced code" =
  let f =
    top "f" [ "c" ] (quote (app (var "add") [ var "x"; splice (var "c") ]))
  in
  check
    [ f
    ; top
        "same"
        []
        (app (var "f") [ quote (app (var "add") [ var "x"; int 1 ]) ])
    ; top
        "other"
        []
        (app (var "f") [ quote (app (var "add") [ var "y"; int 1 ]) ])
    ];
  check
    [ f
    ; top
        "conflict"
        []
        (app (var "f") [ quote (app (var "cond") [ var "x"; int 1; int 1 ]) ])
    ];
  check
    [ top
        "later_use"
        [ "c" ]
        (quote (app (var "add") [ splice (var "c"); var "x" ]))
    ; top
        "conflict"
        []
        (app
           (var "later_use")
           [ quote (app (var "cond") [ var "x"; int 1; int 1 ]) ])
    ];
  [%expect
    {|
    f : <int>{..'a} -> <int>{x : int; ..'b} where ..'a <= {x : int; ..'b}
    same : <int>{x : int; ..'_a}
    other : <int>{x : int; y : int; ..'_a}
    (Unification_error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
    (Unification_error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
    |}]
;;

let%expect_test "spliced code does not take on the quote's free variables" =
  let closed = top "closed" [] ~annot:(Ty.code Ty.int) (quote (int 1)) in
  check
    [ closed
    ; top
        "before"
        []
        (quote (app (var "add") [ var "x"; splice (var "closed") ]))
    ; top
        "after"
        []
        (quote (app (var "add") [ splice (var "closed"); var "x" ]))
    ];
  [%expect
    {|
    closed : <int>{}
    before : <int>{x : int; ..'a}
    after : <int>{x : int; ..'a}
    |}]
;;

let%expect_test "mutually recursive staged functions" =
  let ping =
    top
      "ping"
      [ "n"; "c" ]
      (app
         (var "cond")
         [ app (var "eq") [ var "n"; int 0 ]
         ; var "c"
         ; quote
             (let_
                "x"
                []
                (int 1)
                (splice (app (var "pong") [ var "n"; var "c" ])))
         ])
  in
  let pong =
    top
      "pong"
      [ "n"; "c" ]
      (quote
         (app
            (var "add")
            [ splice
                (app
                   (var "ping")
                   [ app (var "add") [ var "n"; int (-1) ]; var "c" ])
            ; var "y"
            ]))
  in
  check [ ping; pong ];
  check
    [ ping
    ; pong
    ; top "uses_x" [] (app (var "ping") [ int 2; quote (var "x") ])
    ; top "uses_z" [] (app (var "ping") [ int 2; quote (var "z") ])
    ];
  check
    [ ping
    ; pong
    ; top
        "uses_z_as_closed"
        []
        ~annot:(Ty.code Ty.int)
        (app (var "ping") [ int 2; quote (var "z") ])
    ];
  [%expect
    {|
    ping : int -> <int>{y : int; ..'a} -> <int>{y : int; ..'a} where ..'a <= {x?'b : int; y : int; ..'c}, ..'c <= {y : int; ..'a}
    pong : int -> <int>{y : int; ..'a} -> <int>{x?'b : int; y : int; ..'c} where ..'a <= {x?'b : int; y : int; ..'c}, ..'c <= {y : int; ..'a}
    ping : int -> <int>{y : int; ..'a} -> <int>{y : int; ..'a} where ..'a <= {x?'b : int; y : int; ..'c}, ..'c <= {y : int; ..'a}
    pong : int -> <int>{y : int; ..'a} -> <int>{x?'b : int; y : int; ..'c} where ..'a <= {x?'b : int; y : int; ..'c}, ..'c <= {y : int; ..'a}
    uses_x : <int>{x : int; y : int; ..'_a} where ..'_a <= {x : int; y : int; ..'_b}, ..'_b <= {x : int; y : int; ..'_a}
    uses_z : <int>{y : int; z : int; ..'_a} where ..'_a <= {x?'_b : int; y : int; z : int; ..'_c}, ..'_c <= {y : int; z : int; ..'_a}
    (Unification_error
     (Context_mismatch CEmpty
      (CExtend y Present (TCon int ())
       (CVar
        (CLink
         (CExtend z (PVar (PLink Present)) (TVar (Link (TCon int ())))
          (CVar (CUnbound 84 1))))))
      (Empty_context_has_variable y)))
    |}]
;;

let%expect_test "toplevel splices drop names spliced code only may use" =
  let run ?(binder = "x") code =
    splice
      (let_
         "g"
         [ "c" ]
         (seq (quote (let_ binder [] (bool true) (splice (var "c")))) (var "c"))
         (app (var "g") [ code ]))
  in
  check [ top "unused" [] (run (quote (int 1))) ];
  check
    [ top "used" [] (run (quote (app (var "cond") [ var "x"; int 1; int 2 ]))) ];
  (* The generated code is at level 0, where the toplevel [x] is in scope. *)
  check
    [ top "x" [] (bool true); top "used_toplevel" [] (run (quote (var "x"))) ];
  (* The type of a name that may be used is unified with the name's, even if
     it turns out to be unused. *)
  check [ top "unused_but_typed" [] (run ~binder:"id" (quote (int 1))) ];
  [%expect
    {|
    unused : int
    (Not_in_scope x)
    x : bool
    used_toplevel : bool
    (Unification_error
     (Type_mismatch (TCon bool ())
      (TArrow (TVar (Unbound 41 1)) (TVar (Unbound 41 1))) Structural_mismatch))
    |}]
;;

let%expect_test "presence approximation" =
  (* [p <= q] on unknown presences is solved as [p = q], so using [x] in a
     quote after splicing code that may use [x] makes the code use [x], while
     using it before does not. *)
  let binds_x c = quote (let_ "x" [] (int 1) (splice c)) in
  let closed = top "closed" [] ~annot:(Ty.code Ty.int) (quote (int 1)) in
  let uses_x_then_splices =
    top
      "uses_x_then_splices"
      [ "c" ]
      (seq
         (binds_x (var "c"))
         (seq (quote (app (var "add") [ var "x"; splice (var "c") ])) (var "c")))
  in
  let splices_then_uses_x =
    top
      "splices_then_uses_x"
      [ "c" ]
      (seq
         (binds_x (var "c"))
         (seq (quote (app (var "add") [ splice (var "c"); var "x" ])) (var "c")))
  in
  check
    [ closed
    ; uses_x_then_splices
    ; splices_then_uses_x
    ; top
        "uses_x_then_splices_closed"
        []
        (app (var "uses_x_then_splices") [ var "closed" ])
    ];
  check
    [ closed
    ; splices_then_uses_x
    ; top
        "splices_then_uses_x_closed"
        []
        (app (var "splices_then_uses_x") [ var "closed" ])
    ];
  [%expect
    {|
    closed : <int>{}
    uses_x_then_splices : <int>{x?'a : int; ..'b} -> <int>{x?'a : int; ..'b} where ..'b <= {x : int; ..'c}, ..'b <= {..'d}
    splices_then_uses_x : <int>{x : int; ..'a} -> <int>{x : int; ..'a} where ..'a <= {x : int; ..'b}, ..'a <= {..'c}
    uses_x_then_splices_closed : <int>{-x}
    (Unification_error
     (Context_mismatch (CExtend x Present (TCon int ()) (CVar (CUnbound 47 1)))
      CEmpty (Empty_context_has_variable x)))
    |}]
;;
