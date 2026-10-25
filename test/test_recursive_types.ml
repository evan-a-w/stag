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

  let variant ?tail entries = TVariant (row ?tail entries)
  let record ?tail entries = TRecord (row ?tail entries)

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
    Toplevel.Let
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
        ((TVariant (RExtend Num Present (TCon int ()) (RVar (Unbound 49 1))))
         (TVariant (RExtend Num Present (TCon int ()) (RVar (Unbound 50 1))))))
       (RVar (Unbound 54 1)))
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
  check (fun a -> TArrow (record [ "x", a ], a));
  let r = rvar fresh in
  print_s
    [%sexp
      (Unify.unify_row fresh ~row1:r ~row2:(row [ "x", int ] ~tail:r)
       : (unit, Unification_error.t) Result.t)];
  [%expect
    {|
    (Error (Occurs_check_failed (ATy (TVar (Unbound 0 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 1 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 2 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 3 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 4 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 5 1)))))
    (Error (Occurs_check_failed (ARow (RVar (Unbound 6 1)))))
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
  check (fun a -> variant [ "A", a ]);
  check (fun a -> record [ "x", TCon ("list", [ ATy a ]) ]);
  check (fun a -> variant [ "A", TArrow (a, int) ]);
  check (fun a ->
    TCode { inner = int; context = row [ "x", record [ "y", a ] ] });
  check (fun a -> TArrow (variant [ "A", a ], int));
  (* Through another variable. *)
  let b = tvar fresh in
  let a = tvar fresh in
  unify fresh b (variant [ "A", a ]);
  unify fresh a (TArrow (b, int));
  print a;
  print b;
  (* A row variable within its own row, under a record. *)
  let r = rvar fresh in
  print_s
    [%sexp
      (Unify.unify_row fresh ~row1:r ~row2:(row [ "x", TRecord r ])
       : (unit, Unification_error.t) Result.t)];
  print (TRecord r);
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
    fix fresh (fun l -> variant [ "Nil", Type.unit; "Cons", TTuple [ int; l ] ])
  in
  print_s [%sexp (list : Type.ty)];
  let r = rvar fresh in
  ignore
    (Unify.unify_row fresh ~row1:r ~row2:(row [ "x", TRecord r ]) : _ Result.t);
  print_s [%sexp (TRecord r : Type.ty)];
  print_s [%sexp (Type.ATy list : Type.arg)];
  [%expect
    {|
    (TVar
     (Cycle_link 0
      (TVariant
       (RExtend Nil Present (TCon unit ())
        (RExtend Cons Present (TTuple ((TCon int ()) (TVar (Back_link 0))))
         REmpty)))))
    (TRecord
     (RVar
      (Cycle_link 1 (RExtend x Present (TRecord (RVar (Back_link 1))) REmpty))))
    (ATy
     (TVar
      (Cycle_link 0
       (TVariant
        (RExtend Nil Present (TCon unit ())
         (RExtend Cons Present (TTuple ((TCon int ()) (TVar (Back_link 0))))
          REmpty))))))
    |}]
;;

let%expect_test "unifying cyclic types of different unrollings" =
  let fresh = Fresh.create () in
  let open T in
  let a = fix fresh (fun a -> variant [ "A", a ]) in
  let b = tvar fresh in
  let unrolled = variant [ "A", variant [ "A", b ] ] in
  unify fresh a unrolled;
  print b;
  print unrolled;
  let two = fix fresh (fun a -> variant [ "A", variant [ "A", a ] ]) in
  let three =
    fix fresh (fun a -> variant [ "A", variant [ "A", variant [ "A", a ] ] ])
  in
  unify fresh two three;
  print (TTuple [ two; three ]);
  let open_ = fix fresh (fun a -> variant [ "A", a ] ~tail:(rvar fresh)) in
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
  let a = fix fresh (fun a -> variant [ "A", a ]) in
  let ab = fix fresh (fun a -> variant [ "A", variant [ "B", a ] ]) in
  unify fresh a ab;
  let r = fix fresh (fun r -> record [ "x", r; "y", int ]) in
  let s =
    fix fresh (fun s ->
      record [ "x", record [ "x", s; "y", TCon ("bool", []) ]; "y", int ])
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
        (Cycle_link 0 (TVariant (RExtend A Present (TVar (Back_link 0)) REmpty))))
       REmpty)
      (RExtend B Present
       (TVar
        (Cycle_link 1
         (TVariant
          (RExtend A Present
           (TVariant (RExtend B Present (TVar (Back_link 1)) REmpty)) REmpty))))
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
  unify fresh list (variant [ "Nil", Type.unit; "Cons", TTuple [ elt; list ] ]);
  let ty = Type.TTuple [ list; list ] in
  print ty;
  Scheme.generalize_ty ~rank:1 ty;
  print ty;
  let inst () = Scheme.instantiate fresh ~rank:1 ty in
  let ints = inst () in
  let bools = inst () in
  print ints;
  let cons elt =
    variant [ "Cons", TTuple [ elt; tvar fresh ] ] ~tail:(rvar fresh)
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
