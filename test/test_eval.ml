open! Core
open Stag
open Syntax
open Helpers

let prim_names =
  List.filter_map prims ~f:(function
    | Toplevel.Let (name, _, _, _) -> Some name
    | Type _ | Module _ -> None)
;;

(* Evaluates [program], then prints its toplevels in evaluation order, or the
   error evaluation stopped at and the toplevels evaluated before it. *)
let run program =
  let stamps = Stamp_counter.create () in
  match
    Typecheck.typecheck (Typecheck.create ~stamps ()) ~program:(prims @ program)
  with
  | Error error -> print_s [%sexp (error : Typecheck_error.t)]
  | Ok { program; _ } ->
    let t = Treewalk.create ~stamps () in
    let result =
      let%bind.Or_error program = Treewalk.expand_program t ~program in
      Treewalk.add_program t ~program;
      Treewalk.eval t
    in
    List.iter program.sccs ~f:(fun scc ->
      List.iter scc.members ~f:(fun { ident; _ } ->
        if not (List.mem prim_names ident.name ~equal:String.equal)
        then (
          match Treewalk.find t ~ident with
          | Ok value ->
            print_s [%sexp (ident.name : string), (value : Treewalk.Value.t)]
          | Error _ -> ())));
    (match result with
     | Ok () -> ()
     | Error error -> print_s [%sexp (error : Error.t)])
;;

let nat n =
  List.fold (List.range 0 n) ~init:(tag "Z") ~f:(fun acc _ -> tag "S" ~arg:acc)
;;

let%expect_test "values" =
  run
    [ top
        "lits"
        []
        (tuple
           [ int 1
           ; bool true
           ; Expr.Lit (String "s")
           ; Expr.Lit (Char 'c')
           ; unit
           ])
    ; top "tags" [] (tuple [ tag "A"; tag "B" ~arg:(int 1) ])
    ; top "poly_record" [] (poly_record [ "a", int 1; "b", bool false ])
    ; top "fn" [ "x" ] (var "x")
    ];
  [%expect
    {|
    (lits (Tuple 1 true (String s) (Char c) "()"))
    (tags (Tuple `A (`B 1)))
    (poly_record ((a 1) (b false)))
    (fn <fun>)
    |}]
;;

let%expect_test "application, partial application and closures" =
  run
    [ top "pair" [ "a"; "b" ] (tuple [ var "a"; var "b" ])
    ; top "applied" [] (app (var "pair") [ int 1; int 2 ])
    ; top "partial" [] (app (var "pair") [ int 1 ])
    ; top
        "partial_twice"
        []
        (tuple [ app (var "partial") [ int 2 ]; app (var "partial") [ int 3 ] ])
    ; top "make" [ "x" ] (let_ "get" [ "u" ] (var "x") (var "get"))
    ; top
        "closures"
        []
        (let_
           "one"
           []
           (app (var "make") [ int 1 ])
           (let_
              "two"
              []
              (app (var "make") [ int 2 ])
              (tuple [ app (var "one") [ unit ]; app (var "two") [ unit ] ])))
    ; top
        "shadowing"
        []
        (let_ "x" [] (int 1) (tuple [ let_ "x" [] (int 2) (var "x"); var "x" ]))
    ];
  [%expect
    {|
    (pair <fun>)
    (applied (Tuple 1 2))
    (partial <fun>)
    (partial_twice (Tuple (Tuple 1 2) (Tuple 1 3)))
    (make <fun>)
    (closures (Tuple 1 2))
    (shadowing (Tuple 2 1))
    |}]
;;

let%expect_test "recursion" =
  let double =
    match_
      (var "n")
      [ Pat.tag "Z", tag "Z"
      ; ( Pat.tag "S" ~arg:(Pat.var "m")
        , tag "S" ~arg:(tag "S" ~arg:(app (var "double") [ var "m" ])) )
      ]
  in
  let parity name other =
    top
      name
      [ "n" ]
      (match_
         (var "n")
         [ Pat.tag "Z", bool (String.equal name "even")
         ; Pat.tag "S" ~arg:(Pat.var "m"), app (var other) [ var "m" ]
         ])
  in
  run
    [ top "double" [ "n" ] double
    ; top "doubled" [] (app (var "double") [ nat 2 ])
    ; parity "even" "odd"
    ; parity "odd" "even"
    ; top
        "parities"
        []
        (tuple [ app (var "even") [ nat 3 ]; app (var "odd") [ nat 3 ] ])
    ; top
        "local"
        []
        (let_
           "count"
           [ "n" ]
           (match_
              (var "n")
              [ Pat.tag "Z", tag "Done"
              ; Pat.tag "S" ~arg:(Pat.var "m"), app (var "count") [ var "m" ]
              ])
           (app (var "count") [ nat 4 ]))
    ];
  [%expect
    {|
    (double <fun>)
    (doubled (`S (`S (`S (`S `Z)))))
    (even <fun>)
    (odd <fun>)
    (parities (Tuple false true))
    (local `Done)
    |}]
;;

let%expect_test "recursive values" =
  (* [y] is evaluated after [f], whatever the order of the component. *)
  run
    [ top "y" [] (app (var "f") [ tag "A" ])
    ; top
        "f"
        [ "x" ]
        (match_ (var "x") [ Pat.tag "A", int 1; Pat.tag "B", var "y" ])
    ];
  [%expect
    {|
    (y 1)
    (f <fun>)
    |}];
  (* A value used by its own definition is an error. *)
  run [ top "x" [] (app (var "add") [ var "x"; int 1 ]) ];
  [%expect
    {|
    ("Used value before it was defined (eg. because of recursion on values)"
     (ident x/4))
    |}];
  run
    [ top "z" [] (app (var "g") [ tag "B" ])
    ; top
        "g"
        [ "x" ]
        (match_ (var "x") [ Pat.tag "A", int 1; Pat.tag "B", var "z" ])
    ];
  [%expect
    {|
    (g <fun>)
    ("Used value before it was defined (eg. because of recursion on values)"
     (ident z/4))
    |}];
  (* A closure may refer to the value it is part of. *)
  run
    [ top "h" [] (let_ "k" [ "u" ] (seq (var "h") (var "u")) (var "k"))
    ; top "called" [] (app (var "h") [ int 7 ])
    ];
  [%expect
    {|
    (h <fun>)
    (called 7)
    |}]
;;

let%expect_test "nominal types" =
  run
    [ Decl.variant "t" [ "A", []; "B", [ Ty.int; Ty.bool ] ]
    ; Decl.record "r" [ "x", Ty.int; "y", Ty.bool ]
    ; top
        "of_t"
        [ "t" ]
        (match_
           (var "t")
           [ Pat.construct "A", int 0
           ; Pat.construct "B" ~args:[ Pat.var "n"; Pat.any ], var "n"
           ])
    ; top
        "constructed"
        []
        (tuple
           [ construct "A"
           ; construct "B" ~args:[ int 1; bool true ]
           ; app (var "of_t") [ construct "B" ~args:[ int 2; bool true ] ]
           ])
    ; top "r" [] (record [ "y", bool true; "x", int 3 ])
    ; top "fields" [] (tuple [ field (var "r") "x"; field (var "r") "y" ])
    ; top
        "matched"
        []
        (match_
           (var "r")
           [ ( Pat.record [ "y", Pat.var "y"; "x", Pat.var "x" ]
             , tuple [ var "x"; var "y" ] )
           ])
    ; top "only_a" [ "t" ] (match_ (var "t") [ Pat.construct "A", int 0 ])
    ; top
        "failure"
        []
        (app (var "only_a") [ construct "B" ~args:[ int 1; bool true ] ])
    ];
  [%expect
    {|
    (of_t <fun>)
    (constructed (Tuple (A) (B 1 true) 2))
    (r (Record 3 true))
    (fields (Tuple 3 true))
    (matched (Tuple 3 true))
    (only_a <fun>)
    ("Match failure" (value (B 1 true)))
    |}]
;;

let%expect_test "polymorphic records and variants" =
  let r = poly_record [ "a", int 1; "b", bool true ] in
  run
    [ top "field" [] (dot r "b")
    ; top "extended" [] (extend [ "c", int 2 ] r)
    ; top "restricted" [] (restrict r "a")
    ; top "updated" [] (update r [ "a", int 5 ])
    ; top
        "rest"
        []
        (match_
           r
           [ ( Pat.poly_record ~rest:(Bind "rest") [ "a", Pat.var "a" ]
             , tuple [ var "a"; var "rest" ] )
           ])
    ; top
        "lits"
        []
        (match_
           (int 2)
           [ Pat.int 1, tag "One"; Pat.int 2, tag "Two"; Pat.any, tag "Other" ])
    ; top
        "variant"
        []
        (match_
           (tag "B" ~arg:(int 3))
           [ Pat.tag "A", int 0; Pat.tag "B" ~arg:(Pat.var "n"), var "n" ])
    ];
  [%expect
    {|
    (field true)
    (extended ((a 1) (b true) (c 2)))
    (restricted ((b true)))
    (updated ((a 5) (b true)))
    (rest (Tuple 1 ((b true))))
    (lits `Two)
    (variant 3)
    |}]
;;

let%expect_test "toplevel splices" =
  run
    [ top "five" [] (splice (quote (int 5)))
    ; (* Toplevel splices are expanded once, before evaluation. *)
      top
        "generated"
        []
        (splice
           (let_
              "gen"
              [ "n" ]
              (match_
                 (var "n")
                 [ Pat.tag "Z", quote (tag "Z")
                 ; ( Pat.tag "S" ~arg:(Pat.var "m")
                   , quote (tag "S" ~arg:(splice (app (var "gen") [ var "m" ])))
                   )
                 ])
              (app (var "gen") [ nat 3 ])))
    ; (* The level 0 binders at the splice provide the code's context. *)
      top "captured" [ "x" ] (splice (quote (tuple [ var "x"; var "x" ])))
    ; top "applied" [] (app (var "captured") [ int 3 ])
    ; top "nested" [] (splice (splice (quote (quote (int 6)))))
    ];
  [%expect
    {|
    (five 5)
    (generated (`S (`S (`S `Z))))
    (captured <fun>)
    (applied (Tuple 3 3))
    (nested 6)
    |}]
;;

let%expect_test "persistent values in code" =
  (* [h] is a level -1 closure, used by the level 0 code. *)
  run
    [ top
        "embedded"
        []
        (splice
           (let_
              "h"
              [ "y" ]
              (tuple [ var "y"; var "y" ])
              (quote (app (var "h") [ int 1 ]))))
    ; (* Each call of [mk] embeds its own [h]. *)
      top
        "per_build"
        []
        (splice
           (let_
              "mk"
              [ "n" ]
              (let_ "h" [ "u" ] (var "n") (quote (app (var "h") [ unit ])))
              (quote
                 (tuple
                    [ splice (app (var "mk") [ int 1 ])
                    ; splice (app (var "mk") [ int 2 ])
                    ]))))
    ];
  [%expect
    {|
    (embedded (Tuple 1 1))
    (per_build (Tuple 1 2))
    |}]
;;

let%expect_test "hygiene" =
  run
    [ (* An implicit splice is provided by the innermost binder. *)
      top
        "implicit"
        []
        (splice
           (let_
              "c"
              []
              (quote (var "x"))
              (quote
                 (let_
                    "x"
                    []
                    (int 1)
                    (tuple
                       [ splice (var "c")
                       ; let_ "x" [] (int 2) (splice (var "c"))
                       ])))))
    ; (* From proofs.md: [x] is the outer [y], not the code's. *)
      top
        "explicit"
        []
        (splice
           (quote
              (let_
                 "y"
                 []
                 (int 1)
                 (splice
                    ~subs:[ "x", Some (var "y") ]
                    (quote (let_ "y" [] (int 2) (tuple [ var "x"; var "y" ])))))))
    ; (* Each copy of the closure [g] captures its own [x]. *)
      top
        "copies"
        []
        (splice
           (let_
              "c"
              []
              (quote (let_ "g" [ "u" ] (var "x") (app (var "g") [ unit ])))
              (quote
                 (tuple
                    [ let_ "x" [] (int 1) (splice (var "c"))
                    ; let_ "x" [] (int 2) (splice (var "c"))
                    ]))))
    ];
  [%expect
    {|
    (implicit (Tuple 1 2))
    (explicit (Tuple 1 2))
    (copies (Tuple 1 2))
    |}]
;;

let%expect_test "code values" =
  run
    [ top "code" [] (quote (tuple [ int 1; var "x" ]))
    ; top "wrap" [ "c" ] (quote (tuple [ splice (var "c"); splice (var "c") ]))
    ; top "wrapped" [] (app (var "wrap") [ quote (int 2) ])
    ];
  [%expect
    {|
    (code (Code (Tuple ((Lit (Int 1) : int) (Var (Free x) : 'a)) : "int * 'a")))
    (wrap <fun>)
    (wrapped
     (Code (Tuple ((Lit (Int 2) : int) (Lit (Int 2) : int)) : "'a * 'a")))
    |}]
;;

let%expect_test "explicit substitutions at toplevel splices" =
  run
    [ top
        "f"
        [ "y" ]
        (splice
           ~subs:[ "x", Some (var "y") ]
           (quote (let_ "g" [ "u" ] (tuple [ var "x"; var "u" ]) (var "g"))))
    ; top "applied" [] (app (var "f") [ int 1; int 2 ])
    ];
  [%expect
    {|
    (f <fun>)
    (applied (Tuple 1 2))
    |}]
;;

let%expect_test "evaluation is left to right" =
  let abc = Decl.variant "abc" [ "A", []; "B", []; "C", [] ] in
  let only name = match_ (construct name) [ Pat.construct "A", int 0 ] in
  run
    [ abc
    ; top
        "subs"
        []
        (splice
           (quote
              (splice
                 ~subs:[ "b", Some (only "B"); "a", Some (only "C") ]
                 (quote (tuple [ var "a"; var "b" ])))))
    ];
  [%expect {| ("Match failure" (value (B))) |}];
  run [ abc; top "tuple" [] (tuple [ only "B"; only "C" ]) ];
  [%expect {| ("Match failure" (value (B))) |}]
;;

let%expect_test "toplevel splices provided by toplevels and persistent locals" =
  run
    [ top "n" [] (int 3)
    ; top "f" [ "u" ] (splice (quote (tuple [ var "n"; var "u" ])))
    ; top "applied" [] (app (var "f") [ int 4 ])
    ; top
        "local"
        [ "x" ]
        (let_
           "h"
           [ "y" ]
           (tuple [ var "x"; var "y" ])
           (let_
              "g"
              [ "u" ]
              (splice (quote (app (var "h") [ var "u" ])))
              (var "g")))
    ; top "local_applied" [] (app (var "local") [ int 5; int 6 ])
    ];
  [%expect
    {|
    (n 3)
    (f <fun>)
    (applied (Tuple 3 4))
    (local <fun>)
    (local_applied (Tuple 5 6))
    |}]
;;

let%expect_test "spliced code binding patterns and recursive closures" =
  let c =
    quote
      (let_
         "count"
         [ "m" ]
         (match_
            (var "m")
            [ Pat.tag "Z", var "x"
            ; Pat.tag "S" ~arg:(Pat.var "k"), app (var "count") [ var "k" ]
            ])
         (app (var "count") [ nat 2 ]))
  in
  let under x =
    match_
      (poly_record [ "x", int x; "y", int 0 ])
      [ ( Pat.poly_record ~rest:(Bind "rest") [ "x", Pat.var "x" ]
        , tuple [ splice (var "c"); var "rest" ] )
      ]
  in
  run
    [ top
        "copies"
        []
        (splice (let_ "c" [] c (quote (tuple [ under 1; under 2 ]))))
    ; top
        "open_rest"
        []
        (match_
           (poly_record [ "a", int 1; "b", int 2 ])
           [ Pat.poly_record ~rest:Open [ "a", Pat.var "a" ], var "a" ])
    ];
  [%expect
    {|
    (copies (Tuple (Tuple 1 ((y 0))) (Tuple 2 ((y 0)))))
    (open_rest 1)
    |}]
;;

let%expect_test "nested quotes at runtime" =
  run
    [ top "twice" [ "c" ] (quote (quote (splice (splice (var "c")))))
    ; top "built" [] (app (var "twice") [ quote (quote (int 1)) ])
    ; top
        "embedding"
        [ "x" ]
        (let_ "h" [ "u" ] (var "x") (quote (app (var "h") [ unit ])))
    ; top "embedded" [] (app (var "embedding") [ int 7 ])
    ];
  [%expect
    {|
    (twice <fun>)
    (built
     (Code
      (Quote
       (Splice (code (Quote (Lit (Int 1) : int) : <int>{..'_a}))
        (subs (Implicit ())) : 'b)
       : <'b>{..'c} (where "..'_a <= {..'_d}"))))
    (embedding <fun>)
    (embedded
     (Code
      (App (Var (Persistent (Local h/21)) : "unit -> 'a") (Lit Unit : unit) : 'a)
      ((h/21 <fun>))))
    |}]
;;

(* The bodies of [program]'s toplevels after expansion. *)
let expand program =
  let stamps = Stamp_counter.create () in
  match
    Typecheck.typecheck (Typecheck.create ~stamps ()) ~program:(prims @ program)
  with
  | Error error -> print_s [%sexp (error : Typecheck_error.t)]
  | Ok { program; _ } ->
    (match Treewalk.expand_program (Treewalk.create ~stamps ()) ~program with
     | Error error -> print_s [%sexp (error : Error.t)]
     | Ok program ->
       List.iter program.sccs ~f:(fun scc ->
         List.iter scc.members ~f:(fun { ident; body; _ } ->
           if not (List.mem prim_names ident.name ~equal:String.equal)
           then print_s [%sexp (ident : Typed.Ident.t), (body : Typed.Expr.t)])))
;;

let%expect_test "expanded programs" =
  expand
    [ top "five" [] (splice (quote (int 5)))
    ; top "captured" [ "x" ] (splice (quote (tuple [ var "x"; var "x" ])))
    ; top
        "explicit"
        [ "y" ]
        (splice
           ~subs:[ "x", Some (var "y") ]
           (quote (let_ "z" [] (var "x") (var "z"))))
    ; top "in_quote" [] (quote (splice (splice (quote (quote (int 1))))))
    ];
  [%expect
    {|
    (five/4 (Lit (Int 5) : int))
    (captured/5
     (Tuple ((Var (Bound (Local x/17)) : 'a) (Var (Bound (Local x/17)) : 'a)) :
      "'a * 'a"))
    (explicit/6
     (Let (ident x/20) (args ()) (generalized false)
      (body (Var (Bound (Local y/18)) : 'a))
      (in_
       (Let (ident z/21) (args ()) (generalized true)
        (body (Var (Bound (Local x/20)) : 'a))
        (in_ (Var (Bound (Local z/21)) : 'a)) : 'a))
      : 'a))
    (in_quote/7
     (Quote
      (Splice (code (Quote (Lit (Int 1) : int) : <int>{..'_a}))
       (subs (Implicit ())) : int)
      : <int>{..'_b} (where "..'_a <= {..'_b}")))
    |}]
;;

let%expect_test "expansion fails before anything is evaluated" =
  let abc = Decl.variant "abc" [ "A", []; "B", []; "C", [] ] in
  run
    [ abc
    ; top
        "fails_at_runtime"
        []
        (match_ (construct "B") [ Pat.construct "A", int 0 ])
    ; top
        "fails_at_expansion"
        []
        (splice (match_ (construct "C") [ Pat.construct "A", quote (int 0) ]))
    ];
  [%expect {| ("Match failure" (value (C))) |}]
;;
