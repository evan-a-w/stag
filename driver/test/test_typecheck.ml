open! Import
open Syntax
open Helpers

let%expect_test "primitives" =
  let typecheck = typechecker () in
  (Typecheck.typecheck typecheck ~program:prims |> Result.ok |> Option.value_exn)
    .signature
  |> print_signature;
  [%expect
    {|
    val add : int -> int -> int
    val eq : int -> int -> bool
    val cond : bool -> 'a -> 'a -> 'a
    val id : 'a -> 'a
    |}]
;;

let%expect_test "typechecking twice with the same state" =
  let typecheck = typechecker () in
  let run program =
    match Typecheck.typecheck typecheck ~program with
    | Ok { signature; _ } -> print_signature signature
    | Error error -> print_s [%sexp (error : Typecheck_error.t)]
  in
  run prims;
  run [ top "id" [ "x" ] (var "x"); top "n" [] (app (var "id") [ int 1 ]) ];
  [%expect
    {|
    val add : int -> int -> int
    val eq : int -> int -> bool
    val cond : bool -> 'a -> 'a -> 'a
    val id : 'a -> 'a
    val id : 'a -> 'a
    val n : int
    |}]
;;

let%expect_test "literals and application" =
  check
    [ top "i" [] (int 1)
    ; top "b" [] (bool false)
    ; top "s" [] (string "s")
    ; top "c" [] (char 'c')
    ; top "u" [] unit
    ; top "sum" [] (app (var "add") [ int 1; int 2 ])
    ; top "partial" [] (app (var "add") [ int 1 ])
    ];
  [%expect
    {|
    val i : int
    val b : bool
    val s : string
    val c : char
    val u : unit
    val sum : int
    val partial : int -> int
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
    val k : 'a -> 'b -> 'a
    val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
    val both : bool
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
    val weak : '_a -> '_a
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "later bindings don't generalize weak variables" =
  check
    [ top
        "f"
        [ "u" ]
        (let_
           "weak"
           []
           (app (var "id") [ var "id" ])
           (let_
              "through"
              [ "c" ]
              (app (var "weak") [ var "c" ])
              (tuple
                 [ app (var "through") [ int 1 ]
                 ; app (var "through") [ bool true ]
                 ])))
    ];
  [%expect
    {|
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "later toplevels don't generalize weak variables" =
  let weak = top "weak" [] (app (var "id") [ var "id" ]) in
  let through = top "through" [ "c" ] (app (var "weak") [ var "c" ]) in
  check [ weak; through ];
  check [ through; weak ];
  check
    [ weak
    ; through
    ; top
        "use"
        []
        (tuple
           [ app (var "through") [ int 1 ]; app (var "through") [ bool true ] ])
    ];
  check
    [ weak
    ; through
    ; top "use" [] (app (var "through") [ int 1 ])
    ; top "after" [] (var "weak")
    ];
  [%expect
    {|
    val weak : '_a -> '_a
    val through : '_a -> '_a
    val through : '_a -> '_a
    val weak : '_a -> '_a
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    val weak : int -> int
    val through : int -> int
    val use : int
    val after : int -> int
    |}]
;;

let%expect_test "annotations" =
  check [ top "f" [ "x" ] ~annot:Ty.int (var "x") ];
  check [ top "f" [] (var "id" ~annot:(Ty.arrow Ty.bool Ty.bool)) ];
  check [ top "f" [ "x" ] ~annot:Ty.bool (app (var "add") [ var "x"; int 1 ]) ];
  [%expect
    {|
    val f : int -> int
    val f : bool -> bool
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
    val main : bool
    val f : 'a -> 'a
    val g : 'a -> 'a
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
    val a : bool -> int
    val b : int -> int
    val c : int
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
    val x : int
    val f : int -> int
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
    val a : int -> int
    val b : 'a -> 'a
    val c : int -> int
    val d : int -> int
    val e : bool
    |}]
;;

let check_source source =
  match Parser.program ~file:"t.stag" source with
  | Ok program -> check program
  | Error error -> print_endline (Parser.Error.to_string error)
;;

let%expect_test "a recursive group is generalized at once, in any order" =
  (* The members share variables, which generalizing one at a time would
     simplify while the others still use them: generalizing [g] first gave its
     [y] type [<int>{x?'d : int; ..'e}]. The orders give the same schemes, up to which
     side of an equality is an entry type. *)
  let both_orders f g =
    check_source (f ^ "\n" ^ g);
    print_endline "---";
    check_source (g ^ "\n" ^ f)
  in
  both_orders
    "let f c d = let _ = .< let x = .~d in .~c >. in g c d"
    "let g c d = let _ = .< .~c >. in let _ = .< let x = 1 in .~c >. in f c d";
  [%expect
    {|
    val f : <'a>{x?'b : 'c; ..'d} -> <'c>{x?'e : 'c; ..'f} -> 'g where 'b => 'c = int, ..'d <= {..'h}, ..'f <= {..'h}
    val g : <'a>{x?'b : int; ..'c} -> <'d>{x?'e : 'd; ..'f} -> 'g where 'b => 'd = int, ..'c <= {..'h}, ..'f <= {..'h}
    ---
    val g : <'a>{x?'b : int; ..'c} -> <'d>{x?'e : 'd; ..'f} -> 'g where 'b => int = 'd, ..'c <= {..'h}, ..'f <= {..'h}
    val f : <'a>{x?'b : 'c; ..'d} -> <'c>{x?'e : 'c; ..'f} -> 'g where 'b => int = 'c, ..'d <= {..'h}, ..'f <= {..'h}
    |}];
  both_orders
    "let f c = let _ = g c in .< let x = 1 in .~c >."
    "let g c y = cond true (f c) .< let x = .~y in .~c >.";
  [%expect
    {|
    val f : <'a>{x?'b : 'c; ..'d} -> <'a>{..'e} where 'b => 'c = int, ..'d <= {..'e}
    val g : <'a>{x?'b : 'c; ..'d} -> <'c>{x?'e : 'c; ..'f} -> <'a>{..'g} where 'b => 'c = int, ..'d <= {..'g}, ..'f <= {..'g}
    ---
    val g : <'a>{x?'b : int; ..'c} -> <'d>{x?'e : 'd; ..'f} -> <'a>{..'g} where 'b => int = 'd, ..'c <= {..'g}, ..'f <= {..'g}
    val f : <'a>{x?'b : int; ..'c} -> <'a>{..'d} where 'b => int = 'e, ..'c <= {..'d}
    |}]
;;

let%expect_test "a kept bound is added by instances of every group member" =
  (* The scheme of the group keeps an equality on [w]'s weak presence of [x]
     with ['a] of [g] and ['a] of [f]. [f]'s instance must add it again, or
     the first [r] would be accepted with ill-typed generated code. In the
     signatures, [w] has it from the generalization, [r]'s instance of [g] and
     [q]'s of [f]. *)
  let prelude =
    {|let w = let k c = c in id k
let pre d e = .< let y = .~d in let x = .~e in .~(w .<2>.) >.|}
  in
  let f = "let f d c = .< let y = .~d in let x = (.~(g ()), y) in .~(w c) >." in
  let g = "let g u = let _ = f in .< let ident z = z in ident >." in
  let use = "let r = .~(f .< true >. .< match x with | (a, b) -> b >.)" in
  List.iter
    [ [ f; g ]; [ g; f ] ]
    ~f:(fun group ->
      check_source (String.concat ~sep:"\n" ((prelude :: group) @ [ use ]));
      print_endline "---");
  check_source
    (String.concat
       ~sep:"\n"
       [ prelude
       ; f
       ; g
       ; "let r = g ()"
       ; "let t = .~r true"
       ; "let q = f .< false >."
       ]);
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:5:24-56)
    ---
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:5:24-56)
    ---
    val w : <int>{x?'_a : '_b; y?'_c : '_d; ..'_e} -> <int>{x?'_a : '_b; y?'_c : '_d; ..'_e} where '_a => '_b = ('_f -> '_f) * bool, '_a => '_b = (bool -> bool) * '_g, '_a => '_b = ('_h -> '_h) * '_i, '_c => '_d = bool, '_c => '_d = '_g, '_c => '_d = '_i, ..'_e <= {..'_j}, ..'_e <= {..'_k}
    val pre : <'_a>{y?'b : '_a; ..'c} -> <'_d>{x?'e : '_d; y?'f : '_a; ..'g} -> <int>{..'h} where ..'c <= {..'h}, ..'g <= {..'h}, ..'_i <= {..'h}, ..'_i <= {..'_j}, ..'_i <= {..'_k}
    val f : <'a>{y?'b : 'a; ..'c} -> <int>{x?'_d : '_e; y?'_f : '_g; ..'_h} -> <int>{..'i} where '_f => '_g = 'a, '_d => '_e = ('j -> 'j) * 'a, ..'c <= {..'i}, '_d => '_e = ('_k -> '_k) * bool, '_d => '_e = (bool -> bool) * '_l, '_d => '_e = ('_m -> '_m) * '_n, '_f => '_g = bool, '_f => '_g = '_l, '_f => '_g = '_n, ..'_h <= {..'_o}, ..'_h <= {..'_p}, ..'_h <= {..'i}
    val g : unit -> <'a -> 'a>{x?'b : ('a -> 'a) * 'c; y?'d : 'c; ..'e} where '_f => '_g = ('a -> 'a) * 'c, '_h => '_i = 'c, ..'e <= {..'j}, '_f => '_g = ('_k -> '_k) * bool, '_f => '_g = (bool -> bool) * '_l, '_f => '_g = ('_m -> '_m) * '_n, '_h => '_i = bool, '_h => '_i = '_l, '_h => '_i = '_n, ..'_o <= {..'j}, ..'_o <= {..'_p}, ..'_o <= {..'_q}
    val r : <bool -> bool>{}
    val t : bool
    val q : <int>{x?'_a : '_b; y?'_c : '_d; ..'_e} -> <int>{..'_f} where '_a => '_b = ('_g -> '_g) * bool, '_a => '_b = (bool -> bool) * '_h, '_a => '_b = ('_i -> '_i) * '_j, '_c => '_d = bool, '_c => '_d = '_h, '_c => '_d = '_j, ..'_e <= {..'_f}, ..'_e <= {..'_k}
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
    val poly : bool
    val rec_ : 'a -> 'b
    val shadow : 'a -> int
    val arg_scope : 'a -> 'a
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
    val closed : <int>{..'a}
    val open : <int>{x : int; y : int; ..'a}
    val local : <int>{..'a}
    val persistent : <int>{..'a}
    val n : int
    val not_persistent : <'a>{n : 'a; ..'b}
    val outer_arg : 'a -> <'b>{x : 'b; ..'c}
    val nested : <<int>{x : int; ..'a}>{..'b}
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
  [%expect {| val f : 'a -> <int>{x : int; ..'b} |}]
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
    val values : <int>{..'a}
    val reorder : <int>{..'a}
    val from_scope : <int>{y : int; ..'a}
    val infers_sub : <int>{z : int; ..'a}
    val closed : <int>{..'a}
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
     (Row_mismatch
      (RExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
       (RVar (Unbound 30 1)))
      REmpty (Missing_label x)))
    val extra : <int>{..'a}
    (Duplicate_substitution x)
    (Unification_error
     (Type_mismatch
      (TCode (inner (TVar (Unbound 20 1))) (context (RVar (Unbound 21 1))))
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
              (match_ (int 1) [ Pat.int 2, app (var "opaque") [ int 1 ] ])))
    ];
  check [ top "wrong_type" [] (quote (let_ "x" [] (bool true) (splice code))) ];
  [%expect
    {|
    val bound : <int>{..'a}
    val propagates : <int>{x : int; ..'a}
    val unknown_context : <'a>{..'b} -> <'a>{..'c} where ..'b <= {..'c}
    val explicit_unknown : <'a>{x?'b : int} -> <'a>{..'c}
    val two_unknown : <int>{..'a} -> <int>{..'b} -> <int>{..'c} where ..'a <= {..'c}, ..'b <= {..'c}
    val known_and_unknown : <int>{x : int; ..'a} -> <int>{..'b} where ..'a <= {..'b}
    val with_free_var : <int>{..'a} -> <int>{y : int; ..'b} where ..'a <= {y : int; ..'b}
    val instantiated : <int>{y : int; z : int; ..'_a}
    (Unification_error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
    (Splice_evaluation_failed
     (code
      (Let (ident opaque/15) (args (u/14)) (generalized true)
       (body
        (App (Var (Bound (Local opaque/15)) : "'a -> 'b")
         (Var (Bound (Local u/14)) : 'a) : 'b))
       (in_
        (Match (Lit (Int 1) : int)
         (((Lit (Int 2) : int)
           (App (Var (Bound (Local opaque/15)) : "int -> <'_c>{}")
            (Lit (Int 1) : int) : <'_c>{})))
         : <'_c>{}))
       : <'_c>{}))
     (error ("Match failure: no case matches the value 1" @ _none_)))
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
    val outer_in_splice : <'a>{..'b} -> <'a>{..'c} where ..'b <= {..'c}
    val rebind_inner : <int>{..'a}
    val toplevel_splice : int
    val toplevel_splice_arg : 'a -> 'a
    val not_captured : <int>{..'a}
    (Not_in_scope x)
    (Unification_error
     (Row_mismatch
      (RExtend x (PVar (Link Present)) (TVar (Unbound 26 1))
       (RVar (Unbound 27 1)))
      REmpty (Missing_label x)))
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
    val power : int -> <int>{..'a} -> <int>{..'b} where ..'a <= {..'b}
    val cube : <int>{..'_a}
    val power : int -> <int>{..'a} -> <int>{..'b} where ..'a <= {..'b}
    val open : <int>{y : int; ..'_a}
    (Unification_error
     (Row_mismatch REmpty
      (RExtend y (PVar (Link Present)) (TVar (Link (TCon int ())))
       (RVar (Unbound 74 1)))
      (Missing_label y)))
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
    val captures_x : <'a>{x?'b : bool; ..'c} -> <'a>{..'d} where ..'c <= {..'d}
    val closed : <int>{}
    val uses_x : <int>{..'_a}
    val uses_y : <int>{y : int; ..'_a}
    val uses_closed : <int>{..'_a}
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
    val known : <int>{y : int; ..'a}
    val unknown : <'a>{x?'b : int; ..'c} -> <'a>{..'d} where ..'c <= {..'d}
    val unknown_applied : <int>{y : int; ..'_a}
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
    ; (* Instantiation keeps the order of bounds. *)
      top "instance" [ "c" ] (app (var "twice") [ var "c" ])
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
    val twice : <'a>{x?'b : bool; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => bool = 'e, ..'c <= {x?'d : 'e; ..'f}
    val propagates : <int>{x : bool; ..'_a}
    val not_used : <int>{x?'_a : '_b; ..'_c} where '_d <= '_a, '_d => bool = '_b
    val instance : <'a>{x?'b : bool; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => bool = 'e, ..'c <= {x?'d : 'e; ..'f}
    val direct_use : <'a>{x?'b : bool; ..'c} -> <'d>{x : 'd; ..'e} where ..'c <= {y?'f : 'a; ..'g}, ..'g <= {x : 'd; ..'e}
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
    val outer_persistent : <'a>{..'b} -> <'a>{..'c} where ..'b <= {..'c}
    val add_free : 'a -> <'b>{add : 'b; ..'c}
    val not_toplevel : <'_a>{add : '_a; ..'_b}
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
    val inner_splice : <<'a>{z?'b : int; ..'c}>{-z; ..'d} -> <<'a>{..'e}>{..'f} where ..'c <= {..'e}, ..'d <= {..'f}
    val uses_z : <<int>{..'_a}>{..'_b}
    (Unification_error
     (Row_mismatch (RExtend z Absent (TVar (Unbound 43 1)) (RVar (Unbound 44 1)))
      (RExtend z (PVar (Link Present)) (TVar (Unbound 52 1))
       (RVar (Unbound 53 1)))
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
    val f : <int>{..'a} -> <int>{x : int; ..'b} where ..'a <= {x : int; ..'b}
    val same : <int>{x : int; ..'_a}
    val other : <int>{x : int; y : int; ..'_a}
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
    val closed : <int>{}
    val before : <int>{x : int; ..'a}
    val after : <int>{x : int; ..'a}
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
    val ping : int -> <int>{y : int; ..'a} -> <int>{y : int; ..'a} where ..'a <= {x?'b : int; y : int; ..'c}, ..'c <= {y : int; ..'a}
    val pong : int -> <int>{y : int; ..'a} -> <int>{x?'b : int; y : int; ..'c} where ..'a <= {x?'b : int; y : int; ..'c}, ..'c <= {y : int; ..'a}
    val ping : int -> <int>{y : int; ..'a} -> <int>{y : int; ..'a} where ..'a <= {x?'b : int; y : int; ..'c}, ..'c <= {y : int; ..'a}
    val pong : int -> <int>{y : int; ..'a} -> <int>{x?'b : int; y : int; ..'c} where ..'a <= {x?'b : int; y : int; ..'c}, ..'c <= {y : int; ..'a}
    val uses_x : <int>{x : int; y : int; ..'_a} where ..'_a <= {x : int; y : int; ..'_b}, ..'_b <= {x : int; y : int; ..'_a}
    val uses_z : <int>{y : int; z : int; ..'_a} where ..'_a <= {x?'_b : int; y : int; z : int; ..'_c}, ..'_c <= {y : int; z : int; ..'_a}
    (Unification_error
     (Row_mismatch REmpty
      (RExtend y (PVar (Link Present)) (TVar (Link (TCon int ())))
       (RVar
        (Link
         (RExtend z (PVar (Link Present)) (TVar (Link (TCon int ())))
          (RVar (Unbound 93 1))))))
      (Missing_label y)))
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
  (* A name that may be used is not provided by a toplevel, so needn't have
     its type. *)
  check [ top "unused_of_another_type" [] (run ~binder:"id" (quote (int 1))) ];
  [%expect
    {|
    val unused : int
    (Not_in_scope x)
    val x : bool
    val used_toplevel : bool
    val unused_of_another_type : int
    |}]
;;

let%expect_test "using a name before or after splicing code that may use it" =
  (* Splicing [c] into a quote that uses [x] only bounds [c]'s [x] by a used
     one, which decides nothing, whichever comes first. *)
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
    val closed : <int>{}
    val uses_x_then_splices : <int>{x?'a : int; ..'b} -> <int>{x?'a : int; ..'b} where ..'b <= {x : int; ..'c}
    val splices_then_uses_x : <int>{x?'a : int; ..'b} -> <int>{x?'a : int; ..'b} where ..'b <= {x : int; ..'c}
    val uses_x_then_splices_closed : <int>{}
    val closed : <int>{}
    val splices_then_uses_x : <int>{x?'a : int; ..'b} -> <int>{x?'a : int; ..'b} where ..'b <= {x : int; ..'c}
    val splices_then_uses_x_closed : <int>{}
    |}]
;;

let%expect_test "presence constraints are solved in any order" =
  let closeit =
    top_let ("closeit", [ typed "c" (Ty.code Ty.int) ], None, unit)
  in
  let g =
    top
      "g"
      [ "d" ]
      (seq (quote (let_ "x" [] (int 1) (splice (var "d")))) (var "d"))
  in
  let use_x = quote (app (var "add") [ splice (var "c2"); var "x" ]) in
  let close = app (var "closeit") [ var "c" ] in
  let after_close = let_ "c2" [] (app (var "g") [ var "c" ]) use_x in
  let f body = top "f" [ "c" ] body in
  (* [g] bounds [c]'s [x] by its quote's, and [use_x] makes the quote's [x]
     used, while [closeit] makes [c]'s absent: all orders agree. *)
  List.iter
    [ "close first", seq close after_close
    ; ( "close last"
      , let_
          "c2"
          []
          (app (var "g") [ var "c" ])
          (let_ "r" [] use_x (seq close (var "r"))) )
    ; ( "close between"
      , let_ "c2" [] (app (var "g") [ var "c" ]) (seq close use_x) )
    ]
    ~f:(fun (name, body) ->
      print_endline name;
      check [ closeit; g; f body ]);
  [%expect
    {|
    close first
    val closeit : <int>{} -> unit
    val g : <'a>{x?'b : int; ..'c} -> <'a>{x?'b : int; ..'c}
    val f : <int>{} -> <int>{x : int; ..'a}
    close last
    val closeit : <int>{} -> unit
    val g : <'a>{x?'b : int; ..'c} -> <'a>{x?'b : int; ..'c}
    val f : <int>{} -> <int>{x : int; ..'a}
    close between
    val closeit : <int>{} -> unit
    val g : <'a>{x?'b : int; ..'c} -> <'a>{x?'b : int; ..'c}
    val f : <int>{} -> <int>{x : int; ..'a}
    |}]
;;

let twice =
  top
    "twice"
    [ "c" ]
    (seq
       (quote (let_ "x" [] (bool true) (splice (var "c"))))
       (quote (splice (var "c"))))
;;

let closeit = top_let ("closeit", [ typed "c" (Ty.code Ty.int) ], None, unit)
let uses_x = quote (app (var "cond") [ var "x"; int 1; int 2 ])

let needx =
  top_let
    ( "needx"
    , [ typed "e" (Ty.code ~entries:[ "x", Ty.int ] Ty.int) ]
    , None
    , unit )
;;

(* [h]'s argument may use [x] only if [e] does. *)
let local_bounded_by_outer ~e ~in_ =
  let_
    "h"
    [ "d" ]
    (seq
       (quote (let_ "x" [] (int 1) (splice (var "d"))))
       (app (var "cond") [ bool true; quote (splice (var "d")); var e ]))
    in_
;;

let%expect_test
    "generalization does not depend on when outer presences are known"
  =
  let uses =
    seq
      (app (var "h") [ quote (app (var "add") [ var "x"; int 1 ]) ])
      (seq (app (var "h") [ var "k" ]) (app (var "closeit") [ var "k" ]))
  in
  let needx_e = app (var "needx") [ var "e" ] in
  check
    [ closeit
    ; needx
    ; top
        "known_before"
        [ "e"; "k" ]
        (seq needx_e (local_bounded_by_outer ~e:"e" ~in_:uses))
    ; top
        "known_after"
        [ "e"; "k" ]
        (local_bounded_by_outer ~e:"e" ~in_:(seq uses needx_e))
    ; top "never_known" [ "e"; "k" ] (local_bounded_by_outer ~e:"e" ~in_:uses)
    ];
  [%expect
    {|
    val closeit : <int>{} -> unit
    val needx : <int>{x : int} -> unit
    val known_before : <int>{x : int} -> <int>{} -> unit
    val known_after : <int>{x : int} -> <int>{} -> unit
    val never_known : <int>{x : int; ..'a} -> <int>{} -> unit
    |}]
;;

let%expect_test "instances of a scheme bounded by an outer presence" =
  (* Each use of [h] constrains [e] and [h]'s scheme keeps the bound. *)
  let program in_ = top "f" [ "e" ] (local_bounded_by_outer ~e:"e" ~in_) in
  check
    [ closeit
    ; program
        (seq
           (app (var "h") [ quote (int 1) ])
           (app (var "h") [ quote (int 2) ]))
    ];
  check
    [ closeit
    ; program
        (seq
           (app (var "h") [ quote (int 1) ])
           (app (var "h") [ quote (app (var "add") [ var "x"; int 1 ]) ]))
    ];
  check
    [ closeit
    ; program
        (seq
           (app (var "closeit") [ var "e" ])
           (app (var "h") [ quote (app (var "add") [ var "x"; int 1 ]) ]))
    ];
  check
    [ closeit
    ; needx
    ; program
        (seq
           (app (var "needx") [ var "e" ])
           (seq
              (app (var "h") [ quote (int 1) ])
              (app (var "h") [ quote (app (var "add") [ var "x"; int 1 ]) ])))
    ];
  [%expect
    {|
    val closeit : <int>{} -> unit
    val f : <int>{x?'a : 'b; ..'c} -> <int>{x?'a : 'b; ..'c}
    val closeit : <int>{} -> unit
    val f : <int>{x : int; ..'a} -> <int>{x : int; ..'a}
    (Unification_error
     (Row_mismatch
      (RExtend x (PVar (Link Absent)) (TVar (Link (TCon int ())))
       (RVar (Link (RVar (Unbound 58 1)))))
      (RExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
       (RVar (Unbound 67 1)))
      (Presence_mismatch x)))
    val closeit : <int>{} -> unit
    val needx : <int>{x : int} -> unit
    val f : <int>{x : int} -> <int>{x : int}
    |}]
;;

let%expect_test "bounds and presences no instance can use are dropped" =
  (* [h]'s instance in [used] bounds [e]'s context by a row used nowhere else,
     and in [spliced] gives presences of [x] the spliced code doesn't use. *)
  check_source
    {|let closeit (c : <int>{}) = ()
let unused e = let h c = .< let _ = .~e in .~c >. in e
let used e = let h c = .< let _ = .~e in .~c >. in let _ = h .< 1 >. in e
let partial e = let h c = .< let _ = .~e in .~c >. in let _ = h .< 1 >. in h
let lh e = .< let h d = let _ = .< let x = 1 in .~d >. in cond true .< .~d >. e in h >.
let not_spliced e k =
  let r = .< let h d = let _ = .< let x = 1 in .~d >. in cond true .< .~d >. e in 0 >. in
  let _ = closeit k in r
let spliced e k =
  let r = .< let h d = let _ = .< let x = 1 in .~d >. in cond true .< .~d >. e in .~k >. in
  let _ = closeit k in r|};
  [%expect
    {|
    val closeit : <int>{} -> unit
    val unused : <'a>{_?'b : 'a; ..'c} -> <'a>{_?'b : 'a; ..'c}
    val used : <'a>{_?'b : 'a; ..'c} -> <'a>{_?'b : 'a; ..'c}
    val partial : <'a>{_?'b : 'a; ..'c} -> <'d>{_?'e : 'a; ..'f} -> <'d>{..'g} where ..'c <= {..'g}, ..'f <= {..'g}
    val lh : 'a -> <<'b>{x?'c : int; ..'d} -> <'b>{x?'e : 'f; ..'g}>{e : <'b>{x?'e : 'f; ..'g}; ..'h} where 'c <= 'e, 'c => int = 'f, ..'d <= {x?'e : 'f; ..'g}
    val not_spliced : 'a -> <int>{} -> <int>{e : <'b>{x?'c : 'd; ..'e}; ..'f}
    val spliced : 'a -> <int>{} -> <int>{e : <'b>{x?'c : 'd; ..'e}; ..'f}
    |}]
;;

let%expect_test "a witness left out still constrains through the scheme" =
  (* [h c]'s context has [e]'s entries through a row of [h]'s type, so the
     witness of [e]'s kept bound adds nothing. *)
  let f =
    {|let closeit (c : <int>{}) = ()
let f e = let h c = .< let _ = .~e in .~c >. in h .< 1 >.
let partial e = let h c = .< let _ = .~e in .~c >. in let _ = h .< 1 >. in h
|}
  in
  check_source f;
  check_source (f ^ "let ok = .< let y = 2 in .~(f .< y >.) >.");
  check_source (f ^ "let bad = closeit (f .< y >.)");
  check_source (f ^ "let bad = closeit (partial .< y >. .< 3 >.)");
  [%expect
    {|
    val closeit : <int>{} -> unit
    val f : <'a>{_?'b : 'a; ..'c} -> <int>{..'d} where ..'c <= {..'d}
    val partial : <'a>{_?'b : 'a; ..'c} -> <'d>{_?'e : 'a; ..'f} -> <'d>{..'g} where ..'c <= {..'g}, ..'f <= {..'g}
    val closeit : <int>{} -> unit
    val f : <'a>{_?'b : 'a; ..'c} -> <int>{..'d} where ..'c <= {..'d}
    val partial : <'a>{_?'b : 'a; ..'c} -> <'d>{_?'e : 'a; ..'f} -> <'d>{..'g} where ..'c <= {..'g}, ..'f <= {..'g}
    val ok : <int>{..'_a}
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend y (PVar (Link Present)) (TVar (Unbound 108 1))
        (RVar (Unbound 109 1)))
       (Missing_label y)))
     @ t.stag:4:18-29)
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend y (PVar (Link Present)) (TVar (Unbound 116 1))
        (RVar (Unbound 117 1)))
       (Missing_label y)))
     @ t.stag:4:18-43)
    |}]
;;

let%expect_test "presence bounds of polymorphic functions" =
  (* [twice]'s argument may use [x] only if its result does: each use decides
     differently. *)
  check
    [ twice
    ; closeit
    ; top "arg_uses_x" [] (app (var "twice") [ uses_x ])
    ; top
        "result_closed"
        [ "c" ]
        (seq (app (var "closeit") [ app (var "twice") [ var "c" ] ]) (var "c"))
    ; top_let
        ( "arg_closed"
        , [ typed "c" (Ty.code Ty.int) ]
        , None
        , app (var "twice") [ var "c" ] )
    ];
  [%expect
    {|
    val twice : <'a>{x?'b : bool; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => bool = 'e, ..'c <= {x?'d : 'e; ..'f}
    val closeit : <int>{} -> unit
    val arg_uses_x : <int>{x : bool; ..'_a}
    val result_closed : <int>{-x; ..'a} -> <int>{-x; ..'a} where ..'a <= {}
    val arg_closed : <int>{} -> <int>{x?'a : 'b; ..'c}
    |}]
;;

let%expect_test "conflicting presence constraints" =
  check
    [ twice
    ; closeit
    ; top "conflict" [] (app (var "closeit") [ app (var "twice") [ uses_x ] ])
    ];
  check
    [ twice
    ; closeit
    ; top
        "conflict_later"
        [ "c" ]
        (let_
           "r"
           []
           (app (var "twice") [ var "c" ])
           (seq (app (var "closeit") [ var "r" ]) (quote (splice (var "c")))))
    ; top "use" [] (app (var "conflict_later") [ uses_x ])
    ];
  [%expect
    {|
    (Unification_error
     (Row_mismatch REmpty
      (RExtend x (PVar (Link Present)) (TVar (Link (TCon bool ())))
       (RVar (Unbound 55 1)))
      (Missing_label x)))
    (Unification_error
     (Row_mismatch
      (RExtend x (PVar (Link Absent)) (TVar (Link (TCon bool ())))
       (RVar (Link (RVar (Link (RVar (Unbound 73 1)))))))
      (RExtend x (PVar (Link Present)) (TVar (Link (TCon bool ())))
       (RVar (Unbound 91 1)))
      (Presence_mismatch x)))
    |}]
;;

let%expect_test "references to toplevels in quotes don't join their components" =
  let a = top "a" [] (quote (var "b")) in
  let b = top "b" [] (tuple [ var "a"; int 1 ]) in
  check [ a; b ];
  check [ b; a ];
  [%expect
    {|
    val a : <'a>{b : 'a; ..'b}
    val b : <'a>{b : 'a; ..'b} * int
    val b : <'a>{b : 'a; ..'b} * int
    val a : <'a>{b : 'a; ..'b}
    |}]
;;

let tw =
  top
    "tw"
    [ "c" ]
    (seq
       (quote (let_ "x" [] (int 1) (splice (var "c"))))
       (quote (splice (var "c"))))
;;

let needy =
  top_let
    ( "needy"
    , [ typed "e" (Ty.code ~entries:[ "y", Ty.int ] ~tail:"r" Ty.int) ]
    , None
    , unit )
;;

(* [h]'s argument is bounded by [e]'s row through [tw]'s bound. *)
let local_bounded_by_outer_row ~in_ =
  let_
    "h"
    [ "d" ]
    (app (var "cond") [ bool true; app (var "tw") [ var "e" ]; var "d" ])
    in_
;;

let%expect_test "row bounds are solved in any order" =
  let uses_x_y = quote (app (var "add") [ var "x"; var "y" ]) in
  let h arg = app (var "h") [ arg ] in
  let close name = app (var "closeit") [ var name ] in
  let program body = [ closeit; needx; tw; top "f" [ "e"; "k" ] body ] in
  List.iter
    [ ( "closed before"
      , seq
          (close "e")
          (local_bounded_by_outer_row
             ~in_:(seq (h uses_x_y) (seq (h (var "k")) (close "k")))) )
    ; ( "closed between"
      , local_bounded_by_outer_row
          ~in_:
            (seq (h uses_x_y) (seq (close "e") (seq (h (var "k")) (close "k"))))
      )
    ; ( "closed after"
      , local_bounded_by_outer_row
          ~in_:
            (seq (h uses_x_y) (seq (h (var "k")) (seq (close "k") (close "e"))))
      )
    ; ( "never closed"
      , local_bounded_by_outer_row
          ~in_:(seq (h uses_x_y) (seq (h (var "k")) (close "k"))) )
    ]
    ~f:(fun (name, body) ->
      print_endline name;
      check (program body));
  [%expect
    {|
    closed before
    val closeit : <int>{} -> unit
    val needx : <int>{x : int} -> unit
    val tw : <'a>{x?'b : int; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => int = 'e, ..'c <= {x?'d : 'e; ..'f}
    val f : <int>{} -> <int>{} -> unit
    closed between
    val closeit : <int>{} -> unit
    val needx : <int>{x : int} -> unit
    val tw : <'a>{x?'b : int; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => int = 'e, ..'c <= {x?'d : 'e; ..'f}
    val f : <int>{} -> <int>{} -> unit
    closed after
    val closeit : <int>{} -> unit
    val needx : <int>{x : int} -> unit
    val tw : <'a>{x?'b : int; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => int = 'e, ..'c <= {x?'d : 'e; ..'f}
    val f : <int>{} -> <int>{} -> unit
    never closed
    val closeit : <int>{} -> unit
    val needx : <int>{x : int} -> unit
    val tw : <'a>{x?'b : int; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => int = 'e, ..'c <= {x?'d : 'e; ..'f}
    val f : <int>{-x; ..'a} -> <int>{} -> unit where ..'a <= {}
    |}]
;;

let%expect_test "instances of a scheme bounded by an outer row" =
  (* Each use of [h] bounds [e]'s row by its argument's, so what [e] uses
     every argument must allow, whether [e] uses it before or after. *)
  let h arg = app (var "h") [ arg ] in
  let uses_y = app (var "needy") [ var "e" ] in
  let uses = seq (h (quote (int 1))) (h (var "k")) in
  let program body = [ closeit; needy; tw; top "f" [ "e"; "k" ] body ] in
  List.iter
    [ "unused", local_bounded_by_outer_row ~in_:uses
    ; ( "unused, k closed"
      , local_bounded_by_outer_row
          ~in_:(seq uses (app (var "closeit") [ var "k" ])) )
    ; "used before", seq uses_y (local_bounded_by_outer_row ~in_:uses)
    ; "used after", local_bounded_by_outer_row ~in_:(seq uses uses_y)
    ; ( "used before, k closed"
      , seq
          uses_y
          (local_bounded_by_outer_row
             ~in_:(seq uses (app (var "closeit") [ var "k" ]))) )
    ; ( "used after, k closed"
      , local_bounded_by_outer_row
          ~in_:(seq uses (seq (app (var "closeit") [ var "k" ]) uses_y)) )
    ]
    ~f:(fun (name, body) ->
      print_endline name;
      check (program body));
  [%expect
    {|
    unused
    val closeit : <int>{} -> unit
    val needy : <int>{y : int; ..'a} -> unit
    val tw : <'a>{x?'b : int; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => int = 'e, ..'c <= {x?'d : 'e; ..'f}
    val f : <int>{x?'a : int; ..'b} -> <int>{x?'c : 'd; ..'e} -> <int>{x?'c : 'd; ..'e} where 'a <= 'c, 'a => int = 'd, ..'b <= {x?'c : 'd; ..'e}
    unused, k closed
    val closeit : <int>{} -> unit
    val needy : <int>{y : int; ..'a} -> unit
    val tw : <'a>{x?'b : int; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => int = 'e, ..'c <= {x?'d : 'e; ..'f}
    val f : <int>{-x; ..'a} -> <int>{} -> unit where ..'a <= {}
    used before
    val closeit : <int>{} -> unit
    val needy : <int>{y : int; ..'a} -> unit
    val tw : <'a>{x?'b : int; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => int = 'e, ..'c <= {x?'d : 'e; ..'f}
    val f : <int>{x?'a : int; y : int; ..'b} -> <int>{x?'c : 'd; y : int; ..'e} -> <int>{x?'c : 'd; y : int; ..'e} where 'a <= 'c, 'a => int = 'd, ..'b <= {x?'c : 'd; y : int; ..'e}
    used after
    val closeit : <int>{} -> unit
    val needy : <int>{y : int; ..'a} -> unit
    val tw : <'a>{x?'b : int; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => int = 'e, ..'c <= {x?'d : 'e; ..'f}
    val f : <int>{x?'a : int; y : int; ..'b} -> <int>{x?'c : 'd; y : int; ..'e} -> unit where 'a <= 'c, 'a => int = 'd, ..'b <= {x?'c : 'd; y : int; ..'e}
    used before, k closed
    (Unification_error
     (Row_mismatch REmpty
      (RExtend y (PVar (Link Present)) (TVar (Link (TCon int ())))
       (RVar (Unbound 115 1)))
      (Missing_label y)))
    used after, k closed
    (Unification_error
     (Row_mismatch (RExtend y Present (TCon int ()) (RVar (Unbound 107 1)))
      (RExtend x (PVar (Link Absent)) (TVar (Link (TCon int ())))
       (RVar
        (Link
         (RVar
          (Link
           (RExtend y (PVar (Link Absent)) (TCon int ()) (RVar (Unbound 109 1))))))))
      (Presence_mismatch y)))
    |}]
;;

let%expect_test "weak toplevels in a recursive group or a module" =
  let weak = app (var "id") [ var "id" ] in
  let at_two_types f =
    top "use" [] (tuple [ app f [ int 1 ]; app f [ bool true ] ])
  in
  let scc =
    [ top "weak" [] (seq (var "other") weak)
    ; top "other" [ "u" ] (app (var "weak") [ var "u" ])
    ]
  in
  check scc;
  check (scc @ [ at_two_types (var "other") ]);
  check (scc @ [ at_two_types (var "weak") ]);
  check [ module_ "M" [ top "weak" [] weak ]; at_two_types (var "M.weak") ];
  let annotated =
    top "weak" [] ~annot:(Ty.arrow (Ty.var "a") (Ty.var "a")) weak
  in
  check [ annotated ];
  check [ annotated; at_two_types (var "weak") ];
  [%expect
    {|
    val weak : '_a -> '_a
    val other : '_a -> '_a
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    val weak : '_a -> '_a
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let%expect_test "an entry code may use only gets a type if it is present" =
  (* [tw c] and the quote give [c]'s [x] different types, which only
     conflict if [c] has [x]: closing [c] makes it absent, whenever it is. *)
  let binds_bool = quote (let_ "x" [] (bool true) (splice (var "c"))) in
  let close = app (var "closeit") [ var "c" ] in
  let tw_c = app (var "tw") [ var "c" ] in
  let orders =
    [ "closed_first", seq close (seq tw_c binds_bool)
    ; "closed_between", seq tw_c (seq close binds_bool)
    ; "closed_last", seq tw_c (seq binds_bool close)
    ; "never_closed", seq tw_c binds_bool
    ]
  in
  List.iter orders ~f:(fun (name, body) ->
    check [ closeit; tw; top name [ "c" ] body ]);
  [%expect
    {|
    val closeit : <int>{} -> unit
    val tw : <'a>{x?'b : int; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => int = 'e, ..'c <= {x?'d : 'e; ..'f}
    val closed_first : <int>{} -> <int>{..'a}
    val closeit : <int>{} -> unit
    val tw : <'a>{x?'b : int; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => int = 'e, ..'c <= {x?'d : 'e; ..'f}
    val closed_between : <int>{} -> <int>{..'a}
    val closeit : <int>{} -> unit
    val tw : <'a>{x?'b : int; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => int = 'e, ..'c <= {x?'d : 'e; ..'f}
    val closed_last : <int>{} -> unit
    val closeit : <int>{} -> unit
    val tw : <'a>{x?'b : int; ..'c} -> <'a>{x?'d : 'e; ..'f} where 'b <= 'd, 'b => int = 'e, ..'c <= {x?'d : 'e; ..'f}
    val never_closed : <'a>{-x; ..'b} -> <'a>{..'c} where ..'b <= {..'c}, ..'b <= {x?'d : 'e; ..'f}
    |}]
;;

let%expect_test "a chain of lets each instantiating the previous one" =
  let tw =
    top
      "tw"
      [ "c" ]
      (seq
         (quote (let_ "x" [] (int 1) (splice (var "c"))))
         (quote (splice (var "c"))))
  in
  let chain n =
    let h i = var (sprintf "h%d" i) in
    let rec go i =
      let earlier =
        if i = 0
        then app (var "tw") [ var "e" ]
        else app (h (i - 1)) [ app (var "tw") [ var "e" ] ]
      in
      let_
        (sprintf "h%d" i)
        [ "d" ]
        (app (var "cond") [ bool true; earlier; var "d" ])
        (seq
           (app (h i) [ quote (int 1) ])
           (if i = n - 1 then app (h i) [ var "k" ] else go (i + 1)))
    in
    top "f" [ "e"; "k" ] (go 0)
  in
  let typecheck n =
    match
      Typecheck.typecheck (typechecker ()) ~program:(prims @ [ tw; chain n ])
    with
    | Ok { signature; _ } -> List.last_exn signature
    | Error error -> raise_s [%sexp (error : Typecheck_error.t)]
  in
  print_signature [ typecheck 2 ];
  let f = Signature.to_string_hum [ typecheck 200 ] in
  let constraints =
    match String.substr_index f ~pattern:" where " with
    | None -> 0
    | Some at -> List.length (String.split (String.drop_prefix f at) ~on:',')
  in
  print_s [%message (constraints : int)];
  [%expect
    {|
    val f : <int>{x?'a : int; ..'b} -> <int>{x?'c : 'd; ..'e} -> <int>{x?'c : 'd; ..'e} where 'a <= 'c, 'a => int = 'd, ..'b <= {x?'c : 'd; ..'e}
    (constraints 3)
    |}]
;;
