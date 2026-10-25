open! Core
open Stag
open Syntax
open Helpers

let option =
  Decl.variant
    "option"
    ~params:[ Decl.ty "a" ]
    [ "None", []; "Some", [ Ty.var "a" ] ]
;;

(* [fun x -> body] *)
let fn x body = let_ "fn" [ x ] body (var "fn")

let%expect_test "injection" =
  check
    [ top "a" [] (tag "A" ~arg:(int 1))
    ; top "b" [] (tag "B")
    ; top "unit_arg" [] (tag "B" ~arg:unit)
    ; top "c" [ "x" ] (tag "C" ~arg:(var "x"))
    ; top "nested" [] (tag "A" ~arg:(tag "B" ~arg:(int 1)))
    ; top "pair" [] (tuple [ tag "A"; tag "B" ])
    ; top "same_type" [] (app (var "cond") [ bool true; tag "A"; tag "B" ])
    ];
  [%expect
    {|
    val a : [`A of int; ..'a]
    val b : [`B; ..'a]
    val unit_arg : [`B; ..'a]
    val c : 'a -> [`C of 'a; ..'b]
    val nested : [`A of [`B of int; ..'a]; ..'b]
    val pair : [`A; ..'a] * [`B; ..'b]
    val same_type : [`A | `B; ..'_a]
    |}]
;;

let%expect_test "matches without a catch-all are closed" =
  check
    [ top
        "f"
        [ "v" ]
        (match_
           (var "v")
           [ Pat.tag "A" ~arg:(Pat.var "n"), var "n"; Pat.tag "B", int 0 ])
    ; top "applied" [] (app (var "f") [ tag "B" ])
    ; top "absurd" [ "v" ] (match_ (var "v") [])
    ; top
        "with_any"
        [ "v" ]
        (match_ (var "v") [ Pat.tag "A", int 1; Pat.any, int 0 ])
    ; top
        "with_var"
        [ "v" ]
        (match_ (var "v") [ Pat.tag "A", int 1; Pat.var "x", int 0 ])
    ; top
        "var_first"
        [ "v" ]
        (match_ (var "v") [ Pat.var "x", var "x"; Pat.tag "A", tag "B" ])
    ; top
        "repeated"
        [ "v" ]
        (match_
           (var "v")
           [ Pat.tag "A" ~arg:(Pat.int 1), int 1
           ; Pat.tag "A" ~arg:Pat.any, int 2
           ])
    ];
  check
    [ top
        "f"
        [ "v" ]
        (match_
           (var "v")
           [ Pat.tag "A" ~arg:(Pat.var "n"), var "n"; Pat.tag "B", int 0 ])
    ; top "unhandled" [] (app (var "f") [ tag "C" ])
    ];
  check [ top "absurd_applied" [] (match_ (tag "A") []) ];
  check
    [ top
        "argument_mismatch"
        [ "v" ]
        (match_
           (var "v")
           [ Pat.tag "A" ~arg:(Pat.int 1), int 1; Pat.tag "A", int 2 ])
    ];
  [%expect
    {|
    val f : [`A of int | `B] -> int
    val applied : int
    val absurd : [] -> 'a
    val with_any : [`A; ..'a] -> int
    val with_var : [`A; ..'a] -> int
    val var_first : [`A | `B; ..'a] -> [`A | `B; ..'a]
    val repeated : [`A of int] -> int
    (Unification_error
     (Row_mismatch REmpty
      (RExtend C Present (TCon unit ()) (RVar (Unbound 38 1))) (Missing_label C)))
    (Unification_error
     (Row_mismatch REmpty
      (RExtend A Present (TCon unit ()) (RVar (Unbound 18 1))) (Missing_label A)))
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon unit ()) (Base_types_not_equal int unit)))
    |}]
;;

let%expect_test "residuals of a catch-all" =
  let handle name =
    top
      ("handle_" ^ String.lowercase name)
      [ "e"; "k" ]
      (match_
         (var "e")
         [ Pat.tag name ~arg:(Pat.var "n"), var "n"
         ; Pat.var "other", app (var "k") [ var "other" ]
         ])
  in
  check
    [ handle "A"
    ; handle "B"
    ; top
        "handle_both"
        [ "e" ]
        (app
           (var "handle_a")
           [ var "e"
           ; fn
               "e"
               (app (var "handle_b") [ var "e"; fn "e" (match_ (var "e") []) ])
           ])
    ; top "both_a" [] (app (var "handle_both") [ tag "A" ~arg:(int 1) ])
    ; top
        "open_ended"
        [ "e"; "k" ]
        (app
           (var "handle_a")
           [ var "e"; fn "e" (app (var "handle_b") [ var "e"; var "k" ]) ])
    ];
  check
    [ handle "A"
    ; handle "B"
    ; top
        "handle_both"
        [ "e" ]
        (app
           (var "handle_a")
           [ var "e"
           ; fn
               "e"
               (app (var "handle_b") [ var "e"; fn "e" (match_ (var "e") []) ])
           ])
    ; top "unhandled" [] (app (var "handle_both") [ tag "C" ])
    ];
  [%expect
    {|
    val handle_a : [`A of 'a; ..'b] -> ([..'b] -> 'a) -> 'a
    val handle_b : [`B of 'a; ..'b] -> ([..'b] -> 'a) -> 'a
    val handle_both : [`A of 'a | `B of 'a] -> 'a
    val both_a : int
    val open_ended : [`A of 'a | `B of 'a; ..'b] -> ([..'b] -> 'a) -> 'a
    (Unification_error
     (Row_mismatch REmpty
      (RExtend C Present (TCon unit ()) (RVar (Unbound 71 1))) (Missing_label C)))
    |}]
;;

let%expect_test "which cases are removed from residuals" =
  let residual first_cases =
    match_
      (var "v")
      (first_cases @ [ Pat.var "other", app (var "k") [ var "other" ] ])
  in
  check
    [ option
    ; top
        "two_tags"
        [ "v"; "k" ]
        (residual [ Pat.tag "A", int 1; Pat.tag "B", int 2 ])
    ; top
        "refutable"
        [ "v"; "k" ]
        (residual [ Pat.tag "A" ~arg:(Pat.int 0), int 0 ])
    ; top
        "refutable_then_irrefutable"
        [ "v"; "k" ]
        (residual
           [ Pat.tag "A" ~arg:(Pat.int 0), int 0
           ; Pat.tag "A" ~arg:Pat.any, int 1
           ])
    ; top
        "tuple"
        [ "v"; "k" ]
        (residual
           [ Pat.tag "A" ~arg:(Pat.tuple [ Pat.var "x"; Pat.any ]), var "x" ])
    ; top
        "refutable_tuple"
        [ "v"; "k" ]
        (residual
           [ Pat.tag "A" ~arg:(Pat.tuple [ Pat.var "x"; Pat.int 1 ]), var "x" ])
    ; top
        "record"
        [ "v"; "k" ]
        (residual
           [ ( Pat.tag
                 "A"
                 ~arg:(Pat.poly_record ~rest:(Bind "r") [ "x", Pat.var "x" ])
             , var "x" )
           ])
    ; top
        "nominal"
        [ "v"; "k" ]
        (residual
           [ ( Pat.tag "A" ~arg:(Pat.construct "Some" ~args:[ Pat.var "x" ])
             , var "x" )
           ])
    ; top
        "nested_variant"
        [ "v"; "k" ]
        (residual [ Pat.tag "A" ~arg:(Pat.tag "B"), int 1 ])
    ; top
        "removed_once"
        [ "v"; "k" ]
        (residual
           [ Pat.tag "A" ~arg:(Pat.var "x"), var "x"
           ; Pat.tag "A" ~arg:(Pat.var "y"), var "y"
           ])
    ; top
        "shadowed_tag"
        [ "v" ]
        (match_
           (var "v")
           [ Pat.tag "A" ~arg:(Pat.var "n"), var "n"
           ; ( Pat.var "other"
             , match_
                 (var "other")
                 [ ( Pat.tag "A" ~arg:(Pat.var "b")
                   , app (var "cond") [ var "b"; int 1; int 2 ] )
                 ; Pat.any, int 0
                 ] )
           ])
    ; top
        "later_cases"
        [ "v"; "k" ]
        (match_
           (var "v")
           [ Pat.tag "A", int 1
           ; Pat.var "other", app (var "k") [ var "other" ]
           ; Pat.tag "B", int 2
           ])
    ];
  [%expect
    {|
    type option['a] = None | Some of 'a
    val two_tags : [`A | `B; ..'a] -> ([..'a] -> int) -> int
    val refutable : [`A of int; ..'a] -> ([`A of int; ..'a] -> int) -> int
    val refutable_then_irrefutable : [`A of int; ..'a] -> ([..'a] -> int) -> int
    val tuple : [`A of 'a * 'b; ..'c] -> ([..'c] -> 'a) -> 'a
    val refutable_tuple : [`A of 'a * int; ..'b] -> ([`A of 'a * int; ..'b] -> 'a) -> 'a
    val record : [`A of {x : 'a; ..'b}; ..'c] -> ([..'c] -> 'a) -> 'a
    val nominal : [`A of option['a]; ..'b] -> ([`A of option['a]; ..'b] -> 'a) -> 'a
    val nested_variant : [`A of [`B; ..'a]; ..'b] -> ([`A of [`B; ..'a]; ..'b] -> int) -> int
    val removed_once : [`A of 'a; ..'b] -> ([..'b] -> 'a) -> 'a
    val shadowed_tag : [`A of int | `A of bool; ..'a] -> int
    val later_cases : [`A | `B; ..'a] -> ([`B; ..'a] -> int) -> int
    |}]
;;

let%expect_test "nested variant patterns stay open" =
  check
    [ option
    ; top
        "pair"
        [ "p" ]
        (match_ (var "p") [ Pat.tuple [ Pat.tag "A"; Pat.tag "B" ], int 1 ])
    ; top
        "inner"
        [ "v" ]
        (match_ (var "v") [ Pat.tag "A" ~arg:(Pat.tag "B"), int 1 ])
    ; top
        "in_option"
        [ "o" ]
        (match_
           (var "o")
           [ Pat.construct "Some" ~args:[ Pat.tag "A" ], int 1
           ; Pat.construct "None", int 0
           ])
    ; top
        "option_in_tag"
        [ "v" ]
        (match_
           (var "v")
           [ ( Pat.tag "Some" ~arg:(Pat.construct "Some" ~args:[ Pat.var "x" ])
             , var "x" )
           ; Pat.tag "None", int 0
           ])
    ; top
        "in_record"
        [ "r" ]
        (match_ (var "r") [ Pat.poly_record [ "x", Pat.tag "A" ], int 1 ])
    ; top
        "tuple_of_tags"
        []
        (tuple
           [ tag "A" ~arg:(construct "Some" ~args:[ int 1 ])
           ; tag "B" ~arg:(tuple [ int 1; bool true ])
           ])
    ];
  [%expect
    {|
    type option['a] = None | Some of 'a
    val pair : [`A; ..'a] * [`B; ..'b] -> int
    val inner : [`A of [`B; ..'a]] -> int
    val in_option : option[[`A; ..'a]] -> int
    val option_in_tag : [`None | `Some of option[int]] -> int
    val in_record : {x : [`A; ..'a]} -> int
    val tuple_of_tags : [`A of option[int]; ..'a] * [`B of int * bool; ..'b]
    |}]
;;

let%expect_test "aliases" =
  check
    [ Decl.alias "ab" (Ty.variant [ "A", Ty.int; "B", Ty.unit ])
    ; Decl.alias
        "with_c"
        ~params:[ Decl.row "r" ]
        (Ty.variant [ "C", Ty.bool ] ~tail:"r")
    ; Decl.variant "wrapper" [ "W", [ Ty.con "ab" ] ]
    ; Toplevel.Let ("a", [], Some (Ty.con "ab"), tag "A" ~arg:(int 1))
    ; Toplevel.Let
        ( "c"
        , []
        , Some (Ty.con "with_c" ~args:[ Row (Extend ("D", Ty.unit, Empty)) ])
        , tag "D" )
    ; top "w" [] (construct "W" ~args:[ tag "B" ])
    ; Toplevel.Let
        ( "open_param"
        , [ typed "v" (Ty.con "with_c" ~args:[ Row (Var "r") ]) ]
        , None
        , var "v" )
    ];
  check
    [ Decl.alias "ab" (Ty.variant [ "A", Ty.int; "B", Ty.unit ])
    ; Toplevel.Let ("bad", [], Some (Ty.con "ab"), tag "C")
    ];
  check [ Decl.alias "dup" (Ty.variant [ "A", Ty.int; "A", Ty.bool ]) ];
  [%expect
    {|
    type ab = [`A of int | `B]
    type with_c[{..'a}] = [`C of bool; ..'a]
    type wrapper = W of [`A of int | `B]
    val a : [`A of int | `B]
    val c : [`C of bool | `D]
    val w : wrapper
    val open_param : [`C of bool; ..'a] -> [`C of bool; ..'a]
    (Unification_error
     (Row_mismatch REmpty
      (RExtend C Present (TCon unit ()) (RVar (Unbound 23 1))) (Missing_label C)))
    (Duplicate_row_entry A)
    |}]
;;

let%expect_test "generalization and the value restriction" =
  check
    [ top "v" [] (tag "A" ~arg:(var "id"))
    ; top "weak" [] (tag "A" ~arg:(app (var "id") [ var "id" ]))
    ; top "weak_row" [] (app (var "id") [ tag "A" ])
    ; top "matched" [] (match_ (tag "A") [ Pat.tag "A", var "id" ])
    ; top
        "use"
        []
        (tuple
           [ app (var "cond") [ bool true; var "v"; tag "B" ]
           ; app (var "cond") [ bool true; var "v"; tag "C" ]
           ])
    ];
  [%expect
    {|
    val v : [`A of 'a -> 'a; ..'b]
    val weak : [`A of '_a -> '_a; ..'_b]
    val weak_row : [`A; ..'_a]
    val matched : 'a -> 'a
    val use : [`A of '_a -> '_a | `B; ..'_b] * [`A of '_c -> '_c | `C; ..'_d]
    |}]
;;

let%expect_test "tags in quotes are not free variables" =
  check
    [ top "inject" [] (quote (tag "A" ~arg:(var "x")))
    ; top
        "match_inside"
        []
        (quote
           (match_
              (tag "A" ~arg:(int 1))
              [ Pat.tag "A" ~arg:(Pat.var "x"), var "x"
              ; Pat.var "other", int 0
              ]))
    ; top
        "residual_spliced"
        [ "k" ]
        (quote
           (match_
              (var "v")
              [ Pat.tag "A", int 0
              ; Pat.var "other", splice (app (var "k") [ quote (var "other") ])
              ]))
    ];
  [%expect
    {|
    val inject : <[`A of 'a; ..'b]>{x : 'a; ..'c}
    val match_inside : <int>{..'a}
    val residual_spliced : (<'a>{other : 'a; ..'b} -> <int>{other?'c : [..'d]; ..'e}) -> <int>{v : [`A; ..'d]; ..'f} where ..'e <= {v : [`A; ..'d]; ..'f}
    |}]
;;

let%expect_test "presences reach variant rows through contexts" =
  check
    [ Toplevel.Let
        ( "unknown"
        , [ "c", None; typed "v" (Ty.variant [] ~tail:"r") ]
        , None
        , seq
            (quote
               (let_
                  "y"
                  []
                  (int 1)
                  (splice (var "c" ~annot:(Ty.code Ty.int ~tail:"r")))))
            (var "v") )
    ; Toplevel.Let
        ( "absent"
        , [ "c", None; typed "v" (Ty.variant [] ~tail:"d") ]
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
            (var "v") )
    ];
  [%expect
    {|
    val unknown : <int>{y?'a : int; ..'b} -> [`y?'a of int; ..'b] -> [`y?'a of int; ..'b] where ..'b <= {..'c}
    val absent : <<'a>{z?'b : int; ..'c}>{-z; ..'d} -> [-`z; ..'d] -> [-`z; ..'d] where ..'c <= {..'e}, ..'d <= {..'f}
    |}]
;;

let%expect_test "unification errors" =
  check
    [ top
        "variant_vs_record"
        []
        (match_ (poly_record []) [ Pat.tag "A", int 1 ])
    ];
  check [ top "variant_vs_tuple" [] (app (var "add") [ tag "A"; int 1 ]) ];
  check
    [ top
        "argument"
        []
        (app
           (var "cond")
           [ bool true; tag "A" ~arg:(int 1); tag "A" ~arg:(bool true) ])
    ];
  check
    [ top
        "closed"
        []
        (match_ (tag "C") [ Pat.tag "A", int 1; Pat.tag "B", int 2 ])
    ];
  check
    [ top
        "maybe_c"
        [ "v" ]
        (seq
           (app (var "cond") [ bool true; var "v"; tag "C" ])
           (match_ (var "v") [ Pat.tag "A", int 1 ]))
    ];
  [%expect
    {|
    (Unification_error
     (Type_mismatch (TRecord REmpty)
      (TVariant (RExtend A Present (TCon unit ()) (RVar (Unbound 19 1))))
      Structural_mismatch))
    (Unification_error
     (Type_mismatch (TCon int ())
      (TVariant (RExtend A Present (TCon unit ()) (RVar (Unbound 22 1))))
      Structural_mismatch))
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Unification_error
     (Row_mismatch REmpty
      (RExtend C Present (TCon unit ()) (RVar (Unbound 30 1))) (Missing_label C)))
    (Unification_error
     (Row_mismatch REmpty
      (RExtend C Present (TCon unit ()) (RVar (Unbound 32 1))) (Missing_label C)))
    |}]
;;

let%expect_test "empty tuples print as ()" =
  check
    [ top "empty" [] (tuple [])
    ; top "arg" [] (tag "A" ~arg:(tuple []))
    ; top "unit_arg" [] (tag "A" ~arg:unit)
    ; Decl.alias "t" (Ty.variant [ "A", Ty.tuple [] ])
    ; Decl.variant "c" [ "C", [ Ty.tuple [] ] ]
    ];
  [%expect
    {|
    val empty : ()
    val arg : [`A of (); ..'a]
    val unit_arg : [`A; ..'a]
    type t = [`A of ()]
    type c = C of ()
    |}]
;;

let%expect_test "literal unit and closed records as tag arguments" =
  let residual first_cases =
    match_
      (var "v")
      (first_cases @ [ Pat.var "other", app (var "k") [ var "other" ] ])
  in
  check
    [ top
        "unit_literal"
        [ "v"; "k" ]
        (residual [ Pat.tag "A" ~arg:(Pattern.Lit Unit), int 0 ])
    ; top
        "closed_record"
        [ "v"; "k" ]
        (residual
           [ Pat.tag "A" ~arg:(Pat.poly_record [ "x", Pat.var "n" ]), var "n" ])
    ];
  [%expect
    {|
    val unit_literal : [`A; ..'a] -> ([`A; ..'a] -> int) -> int
    val closed_record : [`A of {x : 'a}; ..'b] -> ([..'b] -> 'a) -> 'a
    |}]
;;

let%expect_test "local polymorphic matches" =
  check
    [ top
        "closed"
        []
        (let_
           "f"
           [ "v" ]
           (match_ (var "v") [ Pat.tag "A" ~arg:(Pat.var "x"), var "x" ])
           (tuple
              [ app (var "f") [ tag "A" ~arg:(int 1) ]
              ; app (var "f") [ tag "A" ~arg:(bool true) ]
              ]))
    ; top
        "residual"
        []
        (let_
           "f"
           [ "v"; "k" ]
           (match_
              (var "v")
              [ Pat.tag "A", int 1
              ; Pat.var "other", app (var "k") [ var "other" ]
              ])
           (tuple
              [ app
                  (var "f")
                  [ tag "B" ~arg:(int 1)
                  ; fn
                      "o"
                      (match_
                         (var "o")
                         [ Pat.tag "B" ~arg:(Pat.var "n"), var "n" ])
                  ]
              ; app
                  (var "f")
                  [ tag "C" ~arg:(bool true)
                  ; fn
                      "o"
                      (match_
                         (var "o")
                         [ ( Pat.tag "C" ~arg:(Pat.var "b")
                           , app (var "cond") [ var "b"; int 1; int 2 ] )
                         ])
                  ]
              ]))
    ];
  [%expect
    {|
    val closed : int * bool
    val residual : int * int
    |}]
;;

let%expect_test "closing scrutinees with shadowed tags" =
  let shadowed =
    Decl.alias
      "t"
      ~params:[ Decl.row "r" ]
      (Ty.variant [ "A", Ty.int ] ~tail:"r")
  in
  let v_type = Ty.con "t" ~args:[ Row (Extend ("A", Ty.bool, Empty)) ] in
  check
    [ shadowed
    ; Toplevel.Let
        ( "residual"
        , [ typed "v" v_type ]
        , None
        , match_
            (var "v")
            [ Pat.tag "A" ~arg:(Pat.var "n"), var "n"
            ; ( Pat.var "other"
              , match_
                  (var "other")
                  [ ( Pat.tag "A" ~arg:(Pat.var "b")
                    , app (var "cond") [ var "b"; int 1; int 2 ] )
                  ] )
            ] )
    ];
  (* Closing gives each tag one entry, so the shadowed [`A of bool] is
     extra. *)
  check
    [ shadowed
    ; Toplevel.Let
        ( "closed"
        , [ typed "v" v_type ]
        , None
        , match_ (var "v") [ Pat.tag "A" ~arg:(Pat.var "n"), int 1 ] )
    ];
  check
    [ shadowed
    ; Toplevel.Let
        ( "repeated_tag"
        , [ typed "v" v_type ]
        , None
        , match_
            (var "v")
            [ Pat.tag "A" ~arg:(Pat.var "n"), int 1
            ; Pat.tag "A" ~arg:(Pat.var "b"), int 2
            ] )
    ];
  [%expect
    {|
    type t[{..'a}] = [`A of int; ..'a]
    val residual : [`A of int | `A of bool] -> int
    (Unification_error
     (Row_mismatch REmpty (RExtend A Present (TCon bool ()) REmpty)
      (Missing_label A)))
    (Unification_error
     (Row_mismatch REmpty (RExtend A Present (TCon bool ()) REmpty)
      (Missing_label A)))
    |}]
;;

let%expect_test "rows shared between records and variants" =
  check
    [ Toplevel.Let
        ( "same"
        , [ typed "r" (Ty.record [] ~tail:"r")
          ; typed "v" (Ty.variant [] ~tail:"r")
          ]
        , None
        , tuple [ var "r"; var "v" ] )
    ; top
        "ok"
        []
        (app (var "same") [ poly_record [ "A", int 1 ]; tag "A" ~arg:(int 2) ])
    ; top
        "matched"
        [ "r"; "v" ]
        (seq
           (app (var "same") [ var "r"; var "v" ])
           (app
              (var "cond")
              [ bool true
              ; dot (var "r") "A"
              ; match_ (var "v") [ Pat.tag "A" ~arg:(Pat.var "n"), var "n" ]
              ]))
    ];
  check
    [ Toplevel.Let
        ( "same"
        , [ typed "r" (Ty.record [] ~tail:"r")
          ; typed "v" (Ty.variant [] ~tail:"r")
          ]
        , None
        , tuple [ var "r"; var "v" ] )
    ; top
        "missing"
        []
        (app (var "same") [ poly_record [ "A", int 1 ]; tag "B" ])
    ];
  [%expect
    {|
    val same : {..'a} -> [..'a] -> {..'a} * [..'a]
    val ok : {A : int} * [`A of int]
    val matched : {A : 'a} -> [`A of 'a] -> 'a
    (Unification_error
     (Row_mismatch REmpty
      (RExtend B Present (TCon unit ()) (RVar (Unbound 29 1))) (Missing_label B)))
    |}]
;;

let%expect_test "structural types are not recursive" =
  check
    [ top
        "occurs"
        [ "v" ]
        (app (var "cond") [ bool true; var "v"; tag "A" ~arg:(var "v") ])
    ];
  check
    [ Toplevel.Let
        ( "cyclic"
        , [ typed "a" (Ty.variant [ "A", Ty.unit ] ~tail:"r")
          ; typed "b" (Ty.variant [ "B", Ty.unit ] ~tail:"r")
          ]
        , None
        , app (var "cond") [ bool true; var "a"; var "b" ] )
    ];
  check
    [ Decl.alias
        "t"
        (Ty.variant [ "Nil", Ty.unit; "Cons", Ty.tuple [ Ty.int; Ty.con "t" ] ])
    ];
  check
    [ Decl.variant
        "t"
        [ ( "T"
          , [ Ty.variant
                [ "Nil", Ty.unit; "Cons", Ty.tuple [ Ty.int; Ty.con "t" ] ]
            ] )
        ]
    ; top
        "two"
        []
        (construct
           "T"
           ~args:
             [ tag
                 "Cons"
                 ~arg:(tuple [ int 1; construct "T" ~args:[ tag "Nil" ] ])
             ])
    ];
  [%expect
    {|
    (Unification_error (Occurs_check_failed (ATy (TVar (Unbound 17 1)))))
    (Unification_error
     (Row_mismatch
      (RExtend A Present (TCon unit ())
       (RVar
        (Link
         (RExtend A (PVar (Unbound 26 1)) (TVar (Unbound 27 1))
          (RVar (Unbound 28 1))))))
      (RExtend B Present (TCon unit ())
       (RVar
        (Link
         (RExtend A (PVar (Unbound 26 1)) (TVar (Unbound 27 1))
          (RVar (Unbound 28 1))))))
      Cyclic_row))
    (Cyclic_type_alias t)
    type t = T of [`Cons of int * t | `Nil]
    val two : t
    |}]
;;

let%expect_test "empty matches on empty nominal types" =
  let void = Decl.variant "void" [] in
  let absurd ?(name = "absurd") ty =
    Toplevel.Let (name, [ typed "x" ty ], None, match_ (var "x") [])
  in
  check
    [ void
    ; absurd (Ty.con "void")
    ; top "poly" [ "x" ] (match_ (var "x") [])
    ; Toplevel.Let
        ( "through_let"
        , [ typed "v" (Ty.con "void") ]
        , None
        , let_ "y" [] (var "v") (match_ (var "y") []) )
    ];
  check
    [ module_ "M" [ void; absurd (Ty.con "void") ]; absurd (Ty.con "M.void") ];
  check [ void; Decl.alias "empty" (Ty.con "void"); absurd (Ty.con "empty") ];
  [%expect
    {|
    type void = |
    val absurd : void -> 'a
    val poly : [] -> 'a
    val through_let : void -> 'a
    module M : sig
      type void = |
      val absurd : void -> 'a
    end
    val absurd : M.void -> 'a
    type void = |
    type empty = void
    val absurd : void -> 'a
    |}];
  check [ Decl.variant "t" [ "A", [] ]; absurd (Ty.con "t") ];
  check [ absurd Ty.int ];
  [%expect
    {|
    (Unification_error
     (Type_mismatch (TVariant REmpty) (TCon t ()) Structural_mismatch))
    (Unification_error
     (Type_mismatch (TVariant REmpty) (TCon int ()) Structural_mismatch))
    |}];
  (* Order dependent: [f]'s scrutinee is not yet known to be [void]. *)
  check
    [ void
    ; top "f" [ "x" ] (match_ (var "x") [])
    ; Toplevel.Let
        ("g", [ typed "v" (Ty.con "void") ], None, app (var "f") [ var "v" ])
    ];
  check
    [ void
    ; Toplevel.Let
        ( "local"
        , [ typed "v" (Ty.con "void") ]
        , None
        , let_ "f" [ "x" ] (match_ (var "x") []) (app (var "f") [ var "v" ]) )
    ];
  [%expect
    {|
    (Unification_error
     (Type_mismatch (TVariant REmpty) (TCon void ()) Structural_mismatch))
    (Unification_error
     (Type_mismatch (TVariant REmpty) (TCon void ()) Structural_mismatch))
    |}]
;;
