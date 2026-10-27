open! Core
open Stag
open Syntax
open Helpers

module T = struct
  open Type

  let int = TCon ("int", [])
  let tvar fresh = TVar (Fresh.var fresh ~rank:1)
  let rvar fresh = RVar (Fresh.var fresh ~rank:1)

  let row ?(tail = REmpty) entries =
    List.fold_right entries ~init:tail ~f:(fun (name, ty) rest ->
      RExtend (name, Present, ty, rest))
  ;;

  let variant fresh ?tail entries = Fresh.variant fresh (row ?tail entries)
  let record fresh ?tail entries = Fresh.record fresh (row ?tail entries)

  (* [(f 'a as 'a)] *)
  let fix fresh f =
    let a = tvar fresh in
    (match Unify.unify_ty fresh ~ty1:a ~ty2:(f a) with
     | Ok () -> ()
     | Error error -> raise_s [%sexp (error : Unification_error.t)]);
    a
  ;;

  let unify fresh ty1 ty2 =
    print_s
      [%sexp
        (Unify.unify_ty fresh ~ty1 ~ty2 : (unit, Unification_error.t) Result.t)]
  ;;

  let print ty = print_endline (Type.to_string_hum ty)
end

let fn x body = let_ "fn" [ x ] body (var "fn")

(* [match l with `Nil -> 0 | `Cons (_, t) -> add 1 (len t)] *)
let len =
  top
    "len"
    [ "l" ]
    (match_
       (var "l")
       [ Pat.tag "Nil", int 0
       ; ( Pat.tag "Cons" ~arg:(Pat.tuple [ Pat.any; Pat.var "t" ])
         , app (var "add") [ int 1; app (var "len") [ var "t" ] ] )
       ])
;;

let cons x l = tag "Cons" ~arg:(tuple [ x; l ])

let%expect_test "recursive functions over recursive variants" =
  check
    [ len
    ; top
        "map"
        [ "f"; "l" ]
        (match_
           (var "l")
           [ Pat.tag "Nil", tag "Nil"
           ; ( Pat.tag "Cons" ~arg:(Pat.tuple [ Pat.var "x"; Pat.var "t" ])
             , cons
                 (app (var "f") [ var "x" ])
                 (app (var "map") [ var "f"; var "t" ]) )
           ])
    ; top "two" [] (app (var "len") [ cons (int 1) (cons (int 2) (tag "Nil")) ])
    ; top
        "lengths"
        []
        (tuple
           [ app (var "len") [ cons (int 1) (tag "Nil") ]
           ; app (var "len") [ cons (bool true) (tag "Nil") ]
           ])
    ; top "mapped" [] (app (var "map") [ var "id"; cons (int 1) (tag "Nil") ])
    ];
  [%expect
    {|
    val len : ([`Cons of 'a * 'b | `Nil] as 'b) -> int
    val map : ('a -> 'b) -> ([`Cons of 'a * 'c | `Nil] as 'c) -> ([`Cons of 'b * 'd | `Nil; ..'e] as 'd)
    val two : int
    val lengths : int * int
    val mapped : ([`Cons of int * '_a | `Nil; ..'_b] as '_a)
    |}]
;;

let%expect_test "recursive functions over recursive records" =
  check
    [ top
        "sum"
        [ "r" ]
        (app
           (var "add")
           [ dot (var "r") "v"; app (var "sum") [ dot (var "r") "next" ] ])
    ; top "next" [ "r" ] (dot (var "r") "next")
    ; top
        "both"
        [ "r" ]
        (app
           (var "cond")
           [ bool true; var "r"; dot (dot (var "r") "next") "next" ])
    ; top
        "self"
        [ "r" ]
        (app (var "cond") [ bool true; var "r"; dot (var "r") "self" ])
    ; top "pair" [ "r" ] (tuple [ app (var "self") [ var "r" ]; var "r" ])
    ];
  [%expect
    {|
    val sum : ({next : 'a; v : int; ..'b} as 'a) -> int
    val next : {next : 'a; ..'b} -> 'a
    val both : ({next : {next : 'a; ..'b}; ..'c} as 'a) -> 'a
    val self : ({self : 'a; ..'b} as 'a) -> 'a
    val pair : ({self : 'a; ..'b} as 'a) -> 'a * 'a
    |}]
;;

let%expect_test "recursive values are weak" =
  check
    [ top "ones" [] (cons (int 1) (var "ones"))
    ; top "loop" [] (poly_record [ "next", var "loop"; "v", var "id" ])
    ];
  [%expect
    {|
    val ones : ([`Cons of int * '_a; ..'_b] as '_a)
    val loop : ({next : '_a; v : '_b -> '_b} as '_a)
    |}]
;;

let%expect_test "open rows and residuals" =
  check
    [ top
        "strip"
        [ "e" ]
        (match_
           (var "e")
           [ Pat.tag "Wrap" ~arg:(Pat.var "x"), app (var "strip") [ var "x" ]
           ; Pat.var "other", var "other"
           ])
    ; top
        "count"
        [ "e" ]
        (match_
           (var "e")
           [ ( Pat.tag "S" ~arg:(Pat.var "x")
             , app (var "add") [ int 1; app (var "count") [ var "x" ] ] )
           ; Pat.any, int 0
           ])
    ];
  [%expect
    {|
    val strip : ([`Wrap of 'a; ..'b] as 'a) -> [..'b]
    val count : ([`S of 'a; ..'b] as 'a) -> int
    |}]
;;

(* Handlers for each case of an open recursive AST, composed by [eval]. *)
let%expect_test "extensible interpreter" =
  let handler name tag ~arg body =
    top_let
      ( name
      , args [ "self"; "k"; "e" ]
      , None
      , match_
          (var "e")
          [ Pat.tag tag ~arg, body
          ; Pat.var "other", app (var "k") [ var "other" ]
          ] )
  in
  check
    [ handler "eval_num" "Num" ~arg:(Pat.var "n") (var "n")
    ; handler
        "eval_add"
        "Add"
        ~arg:(Pat.tuple [ Pat.var "a"; Pat.var "b" ])
        (app
           (var "add")
           [ app (var "self") [ var "a" ]; app (var "self") [ var "b" ] ])
    ; handler
        "eval_neg"
        "Neg"
        ~arg:(Pat.var "a")
        (app (var "add") [ int 0; app (var "self") [ var "a" ] ])
    ; top
        "eval"
        [ "e" ]
        (app
           (var "eval_add")
           [ var "eval"
           ; app
               (var "eval_num")
               [ var "eval"
               ; app
                   (var "eval_neg")
                   [ var "eval"; fn "x" (match_ (var "x") []) ]
               ]
           ; var "e"
           ])
    ; top
        "example"
        []
        (app
           (var "eval")
           [ tag
               "Add"
               ~arg:
                 (tuple
                    [ tag "Num" ~arg:(int 1)
                    ; tag "Neg" ~arg:(tag "Num" ~arg:(int 2))
                    ])
           ])
    ];
  check
    [ handler "eval_num" "Num" ~arg:(Pat.var "n") (var "n")
    ; top
        "eval"
        [ "e" ]
        (app
           (var "eval_num")
           [ var "eval"; fn "x" (match_ (var "x") []); var "e" ])
    ; top
        "bad"
        []
        (app
           (var "eval")
           [ tag
               "Add"
               ~arg:(tuple [ tag "Num" ~arg:(int 1); tag "Num" ~arg:(int 2) ])
           ])
    ];
  [%expect
    {|
    val eval_num : 'a -> ([..'b] -> 'c) -> [`Num of 'c; ..'b] -> 'c
    val eval_add : ('a -> int) -> ([..'b] -> int) -> [`Add of 'a * 'a; ..'b] -> int
    val eval_neg : ('a -> int) -> ([..'b] -> int) -> [`Neg of 'a; ..'b] -> int
    val eval : ([`Add of 'a * 'a | `Neg of 'a | `Num of int] as 'a) -> int
    val example : int
    (Unification_error
     (Row_mismatch REmpty
      (RExtend Add Present
       (TTuple
        ((TVariant (RExtend Num Present (TCon int ()) (RVar (Unbound 69 1))))
         (TVariant (RExtend Num Present (TCon int ()) (RVar (Unbound 71 1))))))
       (RVar (Unbound 77 1)))
      (Missing_label Add)))
    |}]
;;

let%expect_test "cyclic types in quotes and contexts" =
  check
    [ top
        "compile"
        [ "e" ]
        (match_
           (var "e")
           [ Pat.tag "Code" ~arg:(Pat.var "c"), var "c"
           ; ( Pat.tag "Add" ~arg:(Pat.tuple [ Pat.var "a"; Pat.var "b" ])
             , quote
                 (app
                    (var "add")
                    [ splice (app (var "compile") [ var "a" ])
                    ; splice (app (var "compile") [ var "b" ])
                    ]) )
           ])
    ; len
    ; top "free" [] (quote (app (var "len") [ var "l" ]))
    ; top
        "provided"
        []
        (quote
           (splice ~subs:[ "l", Some (cons (int 1) (tag "Nil")) ] (var "free")))
    ; top
        "spliced_in"
        [ "l" ]
        (quote (splice ~subs:[ "l", Some (var "l") ] (var "free")))
    ; top
        "quoted_ones"
        []
        (quote (let_ "ones" [] (cons (int 1) (var "ones")) (var "ones")))
    ];
  [%expect
    {|
    val compile : ([`Add of 'a * 'a | `Code of <int>{..'b}] as 'a) -> <int>{..'b}
    val len : ([`Cons of 'a * 'b | `Nil] as 'b) -> int
    val free : <int>{l : ([`Cons of 'a * 'b | `Nil] as 'b); ..'c}
    val provided : <int>{..'a}
    val spliced_in : 'a -> <int>{l : ([`Cons of 'b * 'c | `Nil] as 'c); ..'d}
    val quoted_ones : <([`Cons of int * 'a; ..'b] as 'a)>{..'c}
    |}]
;;

let%expect_test "occurs check fails for unguarded cycles" =
  let fresh = Fresh.create () in
  let open T in
  let check f =
    let a = tvar fresh in
    unify fresh a (f a)
  in
  check (fun a -> TArrow (a, int));
  check (fun a -> TTuple [ a; int ]);
  check (fun a -> TCon ("list", [ ATy a ]));
  check (fun a -> TCode { inner = int; context = row [ "x", a ] });
  check (fun a -> TCode { inner = a; context = REmpty });
  check (fun a -> TArrow (record fresh [ "x", a ], a));
  let check_row f =
    let r = rvar fresh in
    print_s
      [%sexp
        (Unify.unify_row fresh ~row1:r ~row2:(f r)
         : (unit, Unification_error.t) Result.t)]
  in
  check_row (fun r -> row [ "x", int ] ~tail:r);
  check_row (fun r -> row [ "x", TCode { inner = int; context = r } ]);
  check_row (fun r -> row [ "x", TCon ("t", [ ARow r ]) ]);
  (* Through a variable already bound to an unguarded cycle's part. *)
  let a = tvar fresh in
  let b = tvar fresh in
  unify fresh a (TTuple [ b; int ]);
  unify fresh b (TArrow (int, a));
  [%expect
    {|
    (Error (Occurs_check_failed (ATy (TVar (Unbound 0 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 1 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 2 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 3 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 4 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 5 1)))))
    (Error (Occurs_check_failed (ARow (RVar (Unbound 7 1)))))
    (Error (Occurs_check_failed (ARow (RVar (Unbound 8 1)))))
    (Error (Occurs_check_failed (ARow (RVar (Unbound 9 1)))))
    (Ok ())
    (Error (Occurs_check_failed (ATy (TVar (Unbound 11 1)))))
    |}]
;;

let%expect_test "guarded cycles" =
  let fresh = Fresh.create () in
  let open T in
  let check f =
    let a = tvar fresh in
    unify fresh a (f a);
    print a
  in
  check (fun a -> variant fresh [ "A", a ]);
  check (fun a -> record fresh [ "x", TCon ("list", [ ATy a ]) ]);
  check (fun a -> variant fresh [ "A", TArrow (a, int) ]);
  check (fun a ->
    TCode { inner = int; context = row [ "x", record fresh [ "y", a ] ] });
  check (fun a -> TArrow (variant fresh [ "A", a ], int));
  (* Through another variable. *)
  let b = tvar fresh in
  let a = tvar fresh in
  unify fresh b (variant fresh [ "A", a ]);
  unify fresh a (TArrow (b, int));
  print a;
  print b;
  (* A row variable within its own row, under a record. *)
  let r = rvar fresh in
  let record_r = Fresh.record fresh r in
  print_s
    [%sexp
      (Unify.unify_row fresh ~row1:r ~row2:(row [ "x", record_r ])
       : (unit, Unification_error.t) Result.t)];
  print record_r;
  [%expect
    {|
    (Ok ())
    ([`A of 'a] as 'a)
    (Ok ())
    ({x : list['a]} as 'a)
    (Ok ())
    ([`A of 'a -> int] as 'a)
    (Ok ())
    <int>{x : ({y : <int>{x : 'a}} as 'a)}
    (Ok ())
    ([`A of 'a -> int] as 'a) -> int
    (Ok ())
    (Ok ())
    ([`A of 'a -> int] as 'a) -> int
    ([`A of 'a -> int] as 'a)
    (Ok ())
    ({x : 'a} as 'a)
    |}]
;;

let%expect_test "sexps of cyclic types" =
  let fresh = Fresh.create () in
  let open T in
  let list =
    fix fresh (fun l ->
      variant fresh [ "Nil", Type.unit; "Cons", TTuple [ int; l ] ])
  in
  print_s [%sexp (list : Type.ty)];
  let r = rvar fresh in
  let record_r = Fresh.record fresh r in
  ignore
    (Unify.unify_row fresh ~row1:r ~row2:(row [ "x", record_r ]) : _ Result.t);
  print_s [%sexp (record_r : Type.ty)];
  print_s [%sexp (Type.ATy list : Type.arg)];
  [%expect
    {|
    (TVar
     (Link
      (Cycle 0
       (TVariant
        (RExtend Nil Present (TCon unit ())
         (RExtend Cons Present
          (TTuple ((TCon int ()) (TVar (Link (Cycle_ref 0))))) REmpty))))))
    (Cycle 0 (TRecord (RVar (Link (RExtend x Present (Cycle_ref 0) REmpty)))))
    (ATy
     (TVar
      (Link
       (Cycle 0
        (TVariant
         (RExtend Nil Present (TCon unit ())
          (RExtend Cons Present
           (TTuple ((TCon int ()) (TVar (Link (Cycle_ref 0))))) REmpty)))))))
    |}]
;;

let%expect_test "unifying cyclic types of different unrollings" =
  let fresh = Fresh.create () in
  let open T in
  let a = fix fresh (fun a -> variant fresh [ "A", a ]) in
  let b = tvar fresh in
  let unrolled = variant fresh [ "A", variant fresh [ "A", b ] ] in
  unify fresh a unrolled;
  print b;
  print unrolled;
  let two =
    fix fresh (fun a -> variant fresh [ "A", variant fresh [ "A", a ] ])
  in
  let three =
    fix fresh (fun a ->
      variant fresh [ "A", variant fresh [ "A", variant fresh [ "A", a ] ] ])
  in
  unify fresh two three;
  print (TTuple [ two; three ]);
  let open_ =
    fix fresh (fun a -> variant fresh [ "A", a ] ~tail:(rvar fresh))
  in
  unify fresh open_ a;
  print open_;
  [%expect
    {|
    (Ok ())
    ([`A of 'a] as 'a)
    [`A of [`A of ([`A of 'a] as 'a)]]
    (Ok ())
    ([`A of [`A of 'a]] as 'a) * ([`A of [`A of [`A of 'b]]] as 'b)
    (Ok ())
    ([`A of 'a] as 'a)
    |}]
;;

let%expect_test "mismatching cyclic types" =
  let fresh = Fresh.create () in
  let open T in
  let a = fix fresh (fun a -> variant fresh [ "A", a ]) in
  let ab =
    fix fresh (fun a -> variant fresh [ "A", variant fresh [ "B", a ] ])
  in
  unify fresh a ab;
  let r = fix fresh (fun r -> record fresh [ "x", r; "y", int ]) in
  let s =
    fix fresh (fun s ->
      record
        fresh
        [ "x", record fresh [ "x", s; "y", TCon ("bool", []) ]; "y", int ])
  in
  unify fresh r s;
  print r;
  print s;
  [%expect
    {|
    (Error
     (Row_mismatch
      (RExtend A Present
       (TVar
        (Link
         (Cycle 0
          (TVariant (RExtend A Present (TVar (Link (Cycle_ref 0))) REmpty)))))
       REmpty)
      (RExtend B Present
       (TVar
        (Link
         (Cycle 0
          (TVariant
           (RExtend A Present
            (TVariant (RExtend B Present (TVar (Link (Cycle_ref 0))) REmpty))
            REmpty)))))
       REmpty)
      (Missing_label A)))
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    ({x : 'a; y : int} as 'a)
    ({x : {x : 'a; y : bool}; y : int} as 'a)
    |}]
;;

let%expect_test "generalization and instantiation of cyclic types" =
  let fresh = Fresh.create () in
  let open T in
  let elt = Type.TVar (Fresh.var fresh ~rank:2) in
  let list = Type.TVar (Fresh.var fresh ~rank:2) in
  unify
    fresh
    list
    (variant fresh [ "Nil", Type.unit; "Cons", TTuple [ elt; list ] ]);
  let ty = Type.TTuple [ list; list ] in
  print ty;
  Scheme.generalize_ty ~rank:1 ty;
  print ty;
  let inst () = Scheme.instantiate fresh ~rank:1 ty in
  let ints = inst () in
  let bools = inst () in
  print ints;
  let cons elt =
    variant fresh [ "Cons", TTuple [ elt; tvar fresh ] ] ~tail:(rvar fresh)
  in
  (match ints, bools with
   | TTuple [ i; _ ], TTuple [ b; _ ] ->
     unify fresh i (cons int);
     unify fresh b (cons (TCon ("bool", [])))
   | _ -> assert false);
  print (TTuple [ ints; bools; ty ]);
  [%expect
    {|
    (Ok ())
    ([`Cons of '_a * '_b | `Nil] as '_b) * '_b
    ([`Cons of 'a * 'b | `Nil] as 'b) * 'b
    ([`Cons of '_a * '_b | `Nil] as '_b) * '_b
    (Ok ())
    (Ok ())
    (([`Cons of int * 'a | `Nil] as 'a) * 'a) * (([`Cons of bool * 'b | `Nil] as 'b) * 'b) * (([`Cons of 'c * 'd | `Nil] as 'd) * 'd)
    |}]
;;

let list_of ty = Ty.con "list" ~args:[ Ty ty ]

(* [type 'a l = [`Nil | `Cons of 'a * 'a l]] *)
let l =
  Decl.alias
    "l"
    ~params:[ Decl.ty "a" ]
    (Ty.variant
       [ "Nil", Ty.unit
       ; "Cons", Ty.tuple [ Ty.var "a"; Ty.con "l" ~args:[ Ty (Ty.var "a") ] ]
       ])
;;

let l_of ty = Ty.con "l" ~args:[ Ty ty ]

let%expect_test "recursive aliases" =
  check
    [ l
    ; len
    ; top_let
        ( "len_ints"
        , [ typed "l" (l_of Ty.int) ]
        , None
        , app (var "len") [ var "l" ] )
    ; top_let
        ( "len_any"
        , [ typed "l" (l_of (Ty.var "a")) ]
        , None
        , app (var "len") [ var "l" ] )
    ; top "nil" [] ~annot:(l_of Ty.bool) (tag "Nil")
    ; top "one" [] ~annot:(l_of Ty.int) (cons (int 1) (tag "Nil"))
    ; Decl.alias "ints" (l_of Ty.int)
    ; Decl.variant "n" [ "N", [ l_of Ty.int ] ]
    ; top "wrapped" [] (construct "N" ~args:[ var "one" ])
    ];
  check [ l; top "bad" [] ~annot:(l_of Ty.int) (cons (bool true) (tag "Nil")) ];
  check [ l; top "open" [] ~annot:(l_of Ty.int) (tag "Other") ];
  [%expect
    {|
    type l['a] = [`Cons of 'a * l['a] | `Nil]
    val len : ([`Cons of 'a * 'b | `Nil] as 'b) -> int
    val len_ints : ([`Cons of int * 'a | `Nil] as 'a) -> int
    val len_any : ([`Cons of 'a * 'b | `Nil] as 'b) -> int
    val nil : ([`Cons of bool * 'a | `Nil] as 'a)
    val one : ([`Cons of int * 'a | `Nil] as 'a)
    type ints = [`Cons of int * ints | `Nil]
    type n = N of ([`Cons of int * 'a | `Nil] as 'a)
    val wrapped : n
    (Unification_error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Unification_error
     (Row_mismatch REmpty
      (RExtend Other Present (TCon unit ()) (RVar (Unbound 29 1)))
      (Missing_label Other)))
    |}]
;;

let%expect_test "recursive aliases through records, tuples and type arguments" =
  check
    [ Decl.alias
        "stream"
        ~params:[ Decl.ty "a" ]
        (Ty.record
           [ "head", Ty.var "a"
           ; ( "tail"
             , Ty.arrow Ty.unit (Ty.con "stream" ~args:[ Ty (Ty.var "a") ]) )
           ])
    ; Decl.list
    ; Decl.alias
        "tree"
        (Ty.variant [ "Leaf", Ty.int; "Node", list_of (Ty.con "tree") ])
    ; Decl.alias "pair" (Ty.tuple [ Ty.variant [ "A", Ty.con "pair" ]; Ty.int ])
    ; Decl.alias "id" ~params:[ Decl.ty "a" ] (Ty.var "a")
    ; Decl.alias
        "through_id"
        (Ty.con "id" ~args:[ Ty (Ty.variant [ "A", Ty.con "through_id" ]) ])
    ; Decl.alias
        "rows"
        ~params:[ Decl.row "r" ]
        (Ty.record
           [ "x", Ty.con "rows" ~args:[ Row (Ty.row ~tail:"r" ()) ] ]
           ~tail:"r")
    ; top_let
        ( "heads"
        , [ typed "s" (Ty.con "stream" ~args:[ Ty Ty.int ]) ]
        , None
        , tuple
            [ dot (var "s") "head"
            ; dot (app (dot (var "s") "tail") [ unit ]) "head"
            ] )
    ; top_let
        ( "leaf"
        , [ typed "t" (Ty.con "tree") ]
        , None
        , match_
            (var "t")
            [ Pat.tag "Leaf" ~arg:(Pat.var "n"), var "n"; Pat.any, int 0 ] )
    ];
  [%expect
    {|
    type stream['a] = {head : 'a; tail : unit -> stream['a]}
    type list['a] = Nil | Cons of 'a * list['a]
    type tree = [`Leaf of int | `Node of list[tree]]
    type pair = [`A of pair] * int
    type id['a] = 'a
    type through_id = [`A of through_id]
    type rows[{..'a}] = {x : rows[{..'a}]; ..'a}
    val heads : ({head : int; tail : unit -> 'a} as 'a) -> int * int
    val leaf : ([`Leaf of int | `Node of list['a]] as 'a) -> int
    |}]
;;

let%expect_test "mutually recursive aliases" =
  check
    [ Decl.alias
        "a"
        ~params:[ Decl.ty "x" ]
        (Ty.record
           [ "b", Ty.con "b" ~args:[ Ty (Ty.var "x") ]; "v", Ty.var "x" ])
    ; Decl.alias
        "b"
        ~params:[ Decl.ty "y" ]
        (Ty.variant
           [ "End", Ty.unit; "More", Ty.con "a" ~args:[ Ty (Ty.var "y") ] ])
    ; Decl.alias "c" (Ty.con "a" ~args:[ Ty Ty.int ])
    ; top
        "a"
        [ "x" ]
        ~annot:(Ty.con "a" ~args:[ Ty (Ty.var "t") ])
        (poly_record [ "b", tag "End"; "v", var "x" ])
    ; top
        "more"
        []
        (poly_record
           [ "b", tag "More" ~arg:(app (var "a") [ int 1 ]); "v", int 2 ])
    ; top "c" [] ~annot:(Ty.con "c") (var "more")
    ];
  check
    [ module_ "M" [ l ]
    ; Decl.alias
        "u"
        (Ty.variant [ "U", Ty.con "M.l" ~args:[ Ty (Ty.con "u") ] ])
    ];
  [%expect
    {|
    type a['a] = {b : [`End | `More of a['a]]; v : 'a}
    type b['a] = [`End | `More of {b : b['a]; v : 'a}]
    type c = {b : [`End | `More of c]; v : int}
    val a : 'a -> ({b : [`End | `More of 'b]; v : 'a} as 'b)
    val more : {b : [`End | `More of ({b : [`End | `More of 'a]; v : int} as 'a)]; v : int}
    val c : ({b : [`End | `More of 'a]; v : int} as 'a)
    module M : sig
      type l['a] = [`Cons of 'a * l['a] | `Nil]
    end
    type u = [`U of ([`Cons of u * 'a | `Nil] as 'a)]
    |}]
;;

let%expect_test "non-regular and unguarded recursive aliases" =
  let t_of ty = Ty.con "t" ~args:[ Ty ty ] in
  check
    [ Decl.alias
        "t"
        ~params:[ Decl.ty "a" ]
        (Ty.variant [ "A", t_of (Ty.tuple [ Ty.var "a"; Ty.var "a" ]) ])
    ];
  check
    [ Decl.alias "t" ~params:[ Decl.ty "a" ] (Ty.variant [ "A", t_of Ty.int ]) ];
  check
    [ Decl.alias
        "t"
        ~params:[ Decl.ty "a"; Decl.ty "b" ]
        (Ty.variant
           [ "A", Ty.con "t" ~args:[ Ty (Ty.var "b"); Ty (Ty.var "a") ] ])
    ];
  check
    [ Decl.alias
        "a"
        ~params:[ Decl.ty "x" ]
        (Ty.record [ "b", Ty.con "b" ~args:[ Ty (Ty.var "x") ] ])
    ; Decl.alias
        "b"
        ~params:[ Decl.ty "y" ]
        (Ty.variant [ "A", Ty.con "a" ~args:[ Ty Ty.int ] ])
    ];
  check
    [ Decl.alias
        "t"
        ~params:[ Decl.row "r" ]
        (Ty.record
           [ ( "x"
             , Ty.con
                 "t"
                 ~args:[ Row (Ty.row ~entries:[ "y", Ty.int ] ~tail:"r" ()) ] )
           ])
    ];
  check [ Decl.alias "t" (Ty.tuple [ Ty.con "t"; Ty.int ]) ];
  check [ Decl.list; Decl.alias "t" (list_of (Ty.con "t")) ];
  check
    [ Decl.alias "id" ~params:[ Decl.ty "a" ] (Ty.var "a")
    ; Decl.alias "t" (Ty.con "id" ~args:[ Ty (Ty.con "t") ])
    ];
  check [ Decl.alias "t" (Ty.code (Ty.con "t")) ];
  check [ Decl.alias "t" (Ty.code Ty.int ~entries:[ "x", Ty.con "t" ]) ];
  check [ Decl.alias "t" (Ty.arrow (Ty.variant [ "A", Ty.int ]) (Ty.con "t")) ];
  [%expect
    {|
    (Non_regular_recursive_alias t)
    (Non_regular_recursive_alias t)
    (Non_regular_recursive_alias t)
    (Non_regular_recursive_alias a)
    (Non_regular_recursive_alias t)
    (Cyclic_type_alias t)
    (Cyclic_type_alias t)
    (Cyclic_type_alias t)
    (Cyclic_type_alias t)
    (Cyclic_type_alias t)
    (Cyclic_type_alias t)
    |}]
;;

let%expect_test "sexps of recursive aliases" =
  let typecheck = typechecker () in
  (match Typecheck.typecheck typecheck ~program:[ l ] with
   | Ok { signature; _ } -> print_s [%sexp (signature : Signature.t)]
   | Error error -> print_s [%sexp (error : Typecheck_error.t)]);
  [%expect
    {|
    ((Type
      ((name l) (params ((ATy (TVar (Generic 0)))))
       (kind
        (Alias
         (TVar
          (Link
           (Cycle 0
            (TVariant
             (RExtend Nil Present (TCon unit ())
              (RExtend Cons Present
               (TTuple ((TVar (Generic 0)) (TVar (Link (Cycle_ref 0))))) REmpty)))))))))))
    |}]
;;

let%expect_test "bounds with cyclic types" =
  let fresh = Fresh.create () in
  let open T in
  let list () =
    fix fresh (fun l ->
      variant fresh [ "Nil", Type.unit; "Cons", TTuple [ int; l ] ])
  in
  let r = rvar fresh in
  print_s
    [%sexp
      (Unify.constrain_row
         fresh
         ~row:r
         ~bound:(row [ "x", list () ] ~tail:(rvar fresh))
         ~shadowed:String.Set.empty
       : (unit, Unification_error.t) Result.t)];
  print (Fresh.record fresh r);
  let x = tvar fresh in
  unify fresh (Fresh.record fresh r) (record fresh [ "x", x ]);
  unify
    fresh
    x
    (variant fresh [ "Cons", TTuple [ int; list () ] ] ~tail:(rvar fresh));
  print (Fresh.record fresh r);
  let s = rvar fresh in
  ignore
    (Unify.constrain_row
       fresh
       ~row:s
       ~bound:(row [ "x", list () ])
       ~shadowed:String.Set.empty
     : _ Result.t);
  unify
    fresh
    (Fresh.record fresh s)
    (record
       fresh
       [ ( "x"
         , variant
             fresh
             [ "Cons", TTuple [ Type.bool; tvar fresh ] ]
             ~tail:(rvar fresh) )
       ]);
  [%expect
    {|
    (Ok ())
    {..'_a} where ..'_a <= {x : ([`Cons of int * 'b | `Nil] as 'b); ..'_c}
    (Ok ())
    (Ok ())
    {x : ([`Cons of int * 'a | `Nil] as 'a)}
    (Error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
    |}]
;;

(* [type ai = [`T0 of a0 | ... | `T(n-1) of a(n-1)]] for each [i < n]. *)
let clique n =
  let name i = sprintf "a%d" i in
  List.init n ~f:(fun i ->
    Decl.alias
      (name i)
      (Ty.variant (List.init n ~f:(fun j -> sprintf "T%d" j, Ty.con (name j)))))
;;

let%expect_test "mutually recursive alias cliques expand each alias once" =
  check (clique 3);
  [%expect
    {|
    type a0 = [`T0 of a0 | `T1 of ([`T0 of a0 | `T1 of 'a | `T2 of ([`T0 of a0 | `T1 of 'a | `T2 of 'b] as 'b)] as 'a) | `T2 of 'b]
    type a1 = [`T0 of ([`T0 of 'a | `T1 of a1 | `T2 of ([`T0 of 'a | `T1 of a1 | `T2 of 'b] as 'b)] as 'a) | `T1 of a1 | `T2 of 'b]
    type a2 = [`T0 of ([`T0 of 'a | `T1 of ([`T0 of 'a | `T1 of 'b | `T2 of a2] as 'b) | `T2 of a2] as 'a) | `T1 of 'b | `T2 of a2]
    |}];
  (* Each declaration shows each of the 10 aliases' variants once. *)
  let typecheck = typechecker () in
  (match Typecheck.typecheck typecheck ~program:(clique 10) with
   | Error error -> print_s [%sexp (error : Typecheck_error.t)]
   | Ok { signature; _ } ->
     Signature.to_string_hum signature
     |> String.split_lines
     |> List.iter ~f:(fun line ->
       let variants = String.count line ~f:(Char.equal '[') in
       printf
         "%s: %d variants\n"
         (List.nth_exn (String.split line ~on:' ') 1)
         variants));
  [%expect
    {|
    a0: 10 variants
    a1: 10 variants
    a2: 10 variants
    a3: 10 variants
    a4: 10 variants
    a5: 10 variants
    a6: 10 variants
    a7: 10 variants
    a8: 10 variants
    a9: 10 variants
    |}]
;;

let%expect_test "guardedness is judged on the expansion" =
  let f_of ty = Ty.con "f" ~args:[ Ty ty ] in
  let f body = Decl.alias "f" ~params:[ Decl.ty "a" ] body in
  let variant_f = f (Ty.variant [ "A", Ty.var "a" ]) in
  let record_f = f (Ty.record [ "x", Ty.var "a" ]) in
  let g = Decl.alias "g" (f_of (Ty.con "g")) in
  check [ variant_f; g ];
  check [ g; variant_f ];
  check [ record_f; g ];
  check [ g; record_f ];
  check
    [ variant_f
    ; g
    ; top_let ("v", [ typed "x" (Ty.con "g") ], None, var "x")
    ; top "a" [] ~annot:(Ty.con "g") (tag "A" ~arg:(tag "A" ~arg:(var "a")))
    ];
  check
    [ Decl.alias "a" (Ty.tuple [ Ty.con "b"; Ty.int ])
    ; Decl.alias "b" (Ty.variant [ "A", Ty.con "a" ])
    ];
  [%expect
    {|
    type f['a] = [`A of 'a]
    type g = [`A of g]
    type g = [`A of g]
    type f['a] = [`A of 'a]
    type f['a] = {x : 'a}
    type g = {x : g}
    type g = {x : g}
    type f['a] = {x : 'a}
    type f['a] = [`A of 'a]
    type g = [`A of g]
    val v : ([`A of 'a] as 'a) -> 'a
    val a : ([`A of 'a] as 'a)
    type a = [`A of a] * int
    type b = [`A of b * int]
    |}];
  check [ Decl.alias "t" (Ty.con "t") ];
  check [ f (Ty.tuple [ Ty.var "a"; Ty.int ]); g ];
  check [ Decl.list; f (list_of (Ty.var "a")); g ];
  check [ f (Ty.code (Ty.var "a")); g ];
  check [ Decl.alias "a" (Ty.con "b"); Decl.alias "b" (Ty.con "a") ];
  check
    [ Decl.alias "id" ~params:[ Decl.ty "a" ] (Ty.var "a")
    ; Decl.alias
        "t"
        (Ty.tuple
           [ Ty.variant [ "A", Ty.con "t" ]
           ; Ty.con "id" ~args:[ Ty (Ty.con "t") ]
           ])
    ];
  [%expect
    {|
    (Cyclic_type_alias t)
    (Cyclic_type_alias g)
    (Cyclic_type_alias g)
    (Cyclic_type_alias g)
    (Cyclic_type_alias a)
    (Cyclic_type_alias t)
    |}]
;;

let%expect_test "instantiation and unification of large types" =
  let fresh = Fresh.create () in
  let n = 4000 in
  let generic () = Type.TVar (Fresh.var fresh ~rank:2) in
  let chain =
    Fn.apply_n_times
      ~n
      (fun ty ->
         let var = Fresh.var fresh ~rank:2 in
         var.state <- Link (Type.TTuple [ ty; Type.int ]);
         Type.TVar var)
      (generic ())
  in
  let records =
    Type.TTuple (List.init n ~f:(fun _ -> T.record fresh [ "x", generic () ]))
  in
  let ty = Type.TTuple [ chain; records ] in
  Scheme.generalize_ty ~rank:1 ty;
  (* The distinct variables that are not [Link]s, by state. *)
  let vars ty =
    let vars = Int.Table.create () in
    let add ~guarded:_ (var : _ Type.Var.t) =
      let state =
        match var.state with
        | Unbound rank -> sprintf "unbound at %d" rank
        | Generic -> "generic"
        | Link _ -> assert false
      in
      Hashtbl.set vars ~key:var.id ~data:state;
      Ok ()
    in
    (match
       (Type.iter_ty { ty = add; row = add; presence = add } ty
        : (unit, Nothing.t) Result.t)
     with
     | Ok () -> ()
     | Error _ -> .);
    Hashtbl.data vars
    |> List.sort_and_group ~compare:String.compare
    |> List.map ~f:(fun group -> List.hd_exn group, List.length group)
    |> [%sexp_of: (string * int) list]
    |> print_s
  in
  vars ty;
  let instance () = Scheme.instantiate fresh ~rank:1 ty in
  let instance1 = instance () in
  let instance2 = instance () in
  vars (TTuple [ ty; instance1; instance2 ]);
  T.unify fresh instance1 instance2;
  vars (TTuple [ instance1; instance2 ]);
  [%expect
    {|
    ((generic 4001))
    ((generic 4001) ("unbound at 1" 8002))
    (Ok ())
    (("unbound at 1" 4001))
    |}]
;;

(* [t0['a] = 'a] and [t(k+1)['a] = step tk['a]], so [tn] has [2^n] paths
   when [step] uses its argument twice, but [n] expansions. *)
let alias_dag n ~step =
  let name k = sprintf "t%d" k in
  let a = Decl.ty "a" in
  Decl.alias (name 0) ~params:[ a ] (Ty.var "a")
  :: List.init n ~f:(fun k ->
    Decl.alias
      (name (k + 1))
      ~params:[ a ]
      (step (Ty.con (name k) ~args:[ Ty (Ty.var "a") ])))
;;

(* The distinct records and variants, [Link]s and other variables reachable. *)
let print_parts ty =
  let nodes = Int.Hash_set.create () in
  let links = Int.Hash_set.create () in
  let vars = Int.Hash_set.create () in
  let first set id =
    match Hash_set.strict_add set id with
    | Ok () -> true
    | Error _ -> false
  in
  let rec go_ty (ty : Type.ty) =
    match ty with
    | TVar ({ state = Link target; _ } as var) ->
      if first links var.id then go_ty target
    | TVar var -> ignore (first vars var.id : bool)
    | TCon (_, args) ->
      List.iter args ~f:(function
        | ATy ty -> go_ty ty
        | ARow row -> go_row row)
    | TArrow (ty1, ty2) ->
      go_ty ty1;
      go_ty ty2
    | TTuple tys -> List.iter tys ~f:go_ty
    | TCode { inner; context } ->
      go_ty inner;
      go_row context
    | TRecord { id; row } | TVariant { id; row } ->
      if first nodes id then go_row row
  and go_row (row : Type.row) =
    match row with
    | REmpty -> ()
    | RExtend (_, _, ty, rest) ->
      go_ty ty;
      go_row rest
    | RVar ({ state = Link target; _ } as var) ->
      if first links var.id then go_row target
    | RVar var -> ignore (first vars var.id : bool)
  in
  go_ty ty;
  print_s
    [%message
      ""
        ~records_and_variants:(Hash_set.length nodes : int)
        ~links:(Hash_set.length links : int)
        ~vars:(Hash_set.length vars : int)]
;;

let%expect_test "traversals of shared aliases take linear time" =
  let n = 40 in
  let t_n param = Ty.con (sprintf "t%d" n) ~args:[ Ty param ] in
  let check ~step =
    let program =
      alias_dag n ~step
      @ [ top_let ("f", [ typed "x" (t_n (Ty.var "a")) ], None, int 1)
        ; top "g" [ "y" ] (app (var "f") [ var "y" ])
        ; top_let
            ("h", [ typed "z" (t_n Ty.int) ], None, app (var "g") [ var "z" ])
        ]
    in
    match Typecheck.typecheck (typechecker ()) ~program with
    | Error error -> print_s [%sexp (error : Typecheck_error.t)]
    | Ok { signature; _ } ->
      List.iter signature ~f:(function
        | Value { name; scheme; _ } ->
          (match Type.canonical_ty scheme with
           | TArrow (arg, result) ->
             ignore (Type.sexp_of_ty arg : Sexp.t);
             printf "%s : _ -> %s " name (Type.to_string_hum result);
             print_parts arg
           | _ -> assert false)
        | Type _ | Module _ -> ())
  in
  check ~step:(fun t -> Ty.variant [ "L", t; "R", t ]);
  check ~step:(fun t -> Ty.tuple [ t; t ]);
  [%expect
    {|
    f : _ -> int ((records_and_variants 40) (links 41) (vars 1))
    g : _ -> int ((records_and_variants 40) (links 41) (vars 1))
    h : _ -> int ((records_and_variants 40) (links 41) (vars 0))
    f : _ -> int ((records_and_variants 0) (links 41) (vars 1))
    g : _ -> int ((records_and_variants 0) (links 41) (vars 1))
    h : _ -> int ((records_and_variants 0) (links 41) (vars 0))
    |}]
;;

let%expect_test "cycle names are introduced in each type printed" =
  let l_of a = Ty.con "l" ~args:[ Ty a ] in
  let l =
    Decl.alias
      "l"
      ~params:[ Decl.ty "a" ]
      (Ty.variant
         [ "Cons", Ty.tuple [ Ty.var "a"; l_of (Ty.var "a") ]; "Nil", Ty.unit ])
  in
  let a = Ty.var "a" in
  check
    [ l
    ; Decl.record "s" ~params:[ Decl.ty "a" ] [ "c", l_of a; "d", l_of a ]
    ; Decl.variant "v" ~params:[ Decl.ty "a" ] [ "V", [ l_of a; l_of a ] ]
    ; top_let
        ("pair", [ typed "x" (Ty.tuple [ l_of a; l_of a ]) ], None, var "x")
    ];
  [%expect
    {|
    type l['a] = [`Cons of 'a * l['a] | `Nil]
    type s['a] = { c : ([`Cons of 'a * 'b | `Nil] as 'b); d : ([`Cons of 'a * 'b | `Nil] as 'b) }
    type v['a] = V of ([`Cons of 'a * 'b | `Nil] as 'b) * 'b
    val pair : ([`Cons of 'a * 'b | `Nil] as 'b) * 'b -> 'b * 'b
    |}]
;;

let%expect_test "elaboration stops at a failed expansion" =
  let c = Ty.con "c" in
  check
    [ Decl.variant "v" [ "A", [ c ]; "B", [ c ] ]
    ; Decl.alias "u" (Ty.tuple [ c; c ])
    ; Decl.alias "c" (Ty.tuple [ c; Ty.int ])
    ];
  check
    [ Decl.alias "u" (Ty.tuple [ c; c ])
    ; Decl.alias "c" (Ty.tuple [ c; Ty.int ])
    ];
  [%expect
    {|
    (Cyclic_type_alias c)
    (Cyclic_type_alias c)
    |}]
;;

let%expect_test "instantiation copies each part of a cycle once" =
  let fresh = Fresh.create () in
  let open T in
  let elt = Type.TVar (Fresh.var fresh ~rank:2) in
  let list = Type.TVar (Fresh.var fresh ~rank:2) in
  unify
    fresh
    list
    (variant
       fresh
       [ "Nil", Type.unit; "Cons", Type.TTuple [ elt; TArrow (int, list) ] ]);
  (* Entered through a [Link], as through the variant. *)
  let ty = Type.TTuple [ list; Type.canonical_ty list ] in
  Scheme.generalize_ty ~rank:1 ty;
  print_parts ty;
  let instance = Scheme.instantiate fresh ~rank:1 ty in
  print_parts instance;
  print instance;
  [%expect
    {|
    (Ok ())
    ((records_and_variants 1) (links 1) (vars 1))
    ((records_and_variants 1) (links 1) (vars 1))
    ([`Cons of '_a * (int -> '_b) | `Nil] as '_b) * '_b
    |}]
;;

let%expect_test "occurs check through a link visited guarded, then unguarded" =
  let fresh = Fresh.create () in
  let open T in
  (* [l = v -> int], then [v = f l]. *)
  let check f =
    let v = tvar fresh in
    let l = tvar fresh in
    unify fresh l (TArrow (v, int));
    unify fresh v (f l)
  in
  check (fun l -> TTuple [ record fresh [ "x", l ]; l ]);
  check (fun l -> TTuple [ l; record fresh [ "x", l ] ]);
  check (fun l -> TTuple [ record fresh [ "x", l ]; variant fresh [ "A", l ] ]);
  (* [r = {y : v}], then [v = f <int>{..r}]. *)
  let check_row f =
    let v = tvar fresh in
    let r = rvar fresh in
    print_s
      [%sexp
        (Unify.unify_row fresh ~row1:r ~row2:(row [ "y", v ])
         : (unit, Unification_error.t) Result.t)];
    unify fresh v (f (Type.TCode { inner = int; context = r }))
  in
  check_row (fun code -> TTuple [ record fresh [ "x", code ]; code ]);
  check_row (fun code -> TTuple [ code; record fresh [ "x", code ] ]);
  check_row (fun code ->
    TTuple [ record fresh [ "x", code ]; record fresh [ "z", code ] ]);
  [%expect
    {|
    (Ok ())
    (Error (Occurs_check_failed (ATy (TVar (Unbound 0 1)))))
    (Ok ())
    (Error (Occurs_check_failed (ATy (TVar (Unbound 3 1)))))
    (Ok ())
    (Ok ())
    (Ok ())
    (Error (Occurs_check_failed (ATy (TVar (Unbound 10 1)))))
    (Ok ())
    (Error (Occurs_check_failed (ATy (TVar (Unbound 13 1)))))
    (Ok ())
    (Ok ())
    |}]
;;

let%expect_test "unified link pairs are memoised as pairs" =
  let fresh = Fresh.create () in
  let open T in
  let linked ty =
    let l = tvar fresh in
    unify fresh l ty;
    l
  in
  let l1 = linked int in
  let l2 = linked int in
  let l3 = linked Type.bool in
  unify fresh (TTuple [ l1; l1 ]) (TTuple [ l2; l3 ]);
  (* The pair of the cycles' [Link]s is reached again within the cycles, and
     [a] then meets [c], whose [y] differs. *)
  let cycle y = fix fresh (fun a -> record fresh [ "x", a; "y", y ]) in
  let a = cycle int in
  let b = cycle int in
  let c = cycle Type.bool in
  unify fresh (TTuple [ a; a ]) (TTuple [ b; c ]);
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Ok ())
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    |}]
;;

let field ty name =
  match Type.canonical_ty ty with
  | TRecord { row; _ } | TVariant { row; _ } ->
    let rec find row =
      match Type.canonical_row row with
      | Type.RExtend (name', _, ty, rest) ->
        if String.equal name name' then ty else find rest
      | REmpty | RVar _ -> raise_s [%message "no field" name]
    in
    find row
  | _ -> assert false
;;

let%expect_test "instantiation shares cycles without generic variables" =
  let fresh = Fresh.create () in
  let open T in
  let fix_at ~rank f =
    let a = Type.TVar (Fresh.var fresh ~rank) in
    unify fresh a (f a);
    a
  in
  let weak = tvar fresh in
  let inner = fix_at ~rank:1 (fun a -> variant fresh [ "A", a; "W", weak ]) in
  let generic = Type.TVar (Fresh.var fresh ~rank:2) in
  let outer =
    fix_at ~rank:2 (fun a ->
      record fresh [ "next", a; "g", generic; "inner", inner ])
  in
  Scheme.generalize_ty ~rank:1 outer;
  print outer;
  let instance1 = Scheme.instantiate fresh ~rank:1 outer in
  let instance2 = Scheme.instantiate fresh ~rank:1 outer in
  let same name ty1 ty2 =
    printf
      "%s shared: %b\n"
      name
      (phys_equal (Type.canonical_ty ty1) (Type.canonical_ty ty2))
  in
  same "inner" (field instance1 "inner") (field instance2 "inner");
  same "inner (original)" (field instance1 "inner") inner;
  same "outer" instance1 instance2;
  same "outer next" (field instance1 "next") (field instance2 "next");
  same "instance1 next" (field instance1 "next") instance1;
  unify fresh (field instance1 "g") int;
  unify fresh (field instance2 "g") Type.bool;
  unify fresh weak Type.string;
  print (TTuple [ instance1; instance2; outer ]);
  [%expect
    {|
    (Ok ())
    (Ok ())
    ({g : 'a; inner : ([`A of '_b | `W of '_c] as '_b); next : '_d} as '_d)
    inner shared: true
    inner (original) shared: true
    outer shared: false
    outer next shared: false
    instance1 next shared: true
    (Ok ())
    (Ok ())
    (Ok ())
    ({g : int; inner : ([`A of 'a | `W of string] as 'a); next : 'b} as 'b) * ({g : bool; inner : 'a; next : 'c} as 'c) * ({g : 'd; inner : 'a; next : 'e} as 'e)
    |}]
;;

let%expect_test "instantiation copies generic presences and bounds" =
  let fresh = Fresh.create () in
  let open T in
  let generic () = Fresh.var fresh ~rank:2 in
  (* [({self : 'a; x?'p : int} as 'a)] *)
  let knot = Type.TVar (generic ()) in
  unify
    fresh
    knot
    (Fresh.record
       fresh
       (RExtend
          ("x", PVar (generic ()), int, RExtend ("self", Present, knot, REmpty))));
  (* [{..'r} where ..'r <= {x : 'g}] *)
  let r = generic () in
  print_s
    [%sexp
      (Unify.constrain_row
         fresh
         ~row:(RVar r)
         ~bound:(row [ "x", TVar (generic ()) ])
         ~shadowed:String.Set.empty
       : (unit, Unification_error.t) Result.t)];
  let ty = Type.TTuple [ knot; Fresh.record fresh (RVar r) ] in
  Scheme.generalize_ty ~rank:1 ty;
  print ty;
  let instance1 = Scheme.instantiate fresh ~rank:1 ty in
  let instance2 = Scheme.instantiate fresh ~rank:1 ty in
  print (TTuple [ instance1; instance2 ]);
  (match instance1, instance2 with
   | TTuple [ knot1; _ ], TTuple [ knot2; _ ] ->
     unify fresh knot1 (record fresh [ "self", tvar fresh ]);
     unify fresh knot2 (record fresh [ "self", tvar fresh; "x", int ])
   | _ -> assert false);
  print (TTuple [ instance1; instance2; ty ]);
  [%expect
    {|
    (Ok ())
    (Ok ())
    ({self : 'a; x?'b : int} as 'a) * {..'c} where ..'c <= {x : 'd}
    (({self : '_a; x?'_b : int} as '_a) * {..'_c}) * (({self : '_d; x?'_e : int} as '_d) * {..'_f}) where ..'_c <= {x : '_g}, ..'_f <= {x : '_h}
    (Ok ())
    (Ok ())
    (({self : 'a; -x} as 'a) * {..'_b}) * (({self : 'c; x : int} as 'c) * {..'_d}) * (({self : 'e; x?'f : int} as 'e) * {..'g}) where ..'_b <= {x : '_h}, ..'_d <= {x : '_i}, ..'g <= {x : 'j}
    |}]
;;
