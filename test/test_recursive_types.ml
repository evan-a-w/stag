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
    (Error (Occurs_check_failed (ARow (RVar (Unbound 6 1)))))
    (Error (Occurs_check_failed (ARow (RVar (Unbound 7 1)))))
    (Error (Occurs_check_failed (ARow (RVar (Unbound 8 1)))))
    (Ok ())
    (Error (Occurs_check_failed (ATy (TVar (Unbound 10 1)))))
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
    ; Toplevel.Let
        ( "len_ints"
        , [ typed "l" (l_of Ty.int) ]
        , None
        , app (var "len") [ var "l" ] )
    ; Toplevel.Let
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
      (RExtend Other Present (TCon unit ()) (RVar (Unbound 26 1)))
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
        (Ty.record [ "x", Ty.con "rows" ~args:[ Row (Var "r") ] ] ~tail:"r")
    ; Toplevel.Let
        ( "heads"
        , [ typed "s" (Ty.con "stream" ~args:[ Ty Ty.int ]) ]
        , None
        , tuple
            [ dot (var "s") "head"
            ; dot (app (dot (var "s") "tail") [ unit ]) "head"
            ] )
    ; Toplevel.Let
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
           [ "x", Ty.con "t" ~args:[ Row (Extend ("y", Ty.int, Var "r")) ] ])
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
  let typecheck = Typecheck.create () in
  (match Typecheck.typecheck typecheck ~program:[ l ] with
   | Ok signature -> print_s [%sexp (signature : Signature.t)]
   | Error error -> print_s [%sexp (error : Typecheck_error.t)]);
  [%expect
    {|
    ((Type
      ((name l) (params ((ATy (TVar (Generic 0)))))
       (kind
        (Alias
         (Cycle 0
          (TVariant
           (RExtend Nil Present (TCon unit ())
            (RExtend Cons Present
             (TTuple ((TVar (Generic 0)) (TVar (Link (Cycle_ref 0))))) REmpty)))))))))
    |}]
;;

let%expect_test "bounds with cyclic types" =
  let fresh = Fresh.create () in
  let open T in
  let list () =
    fix fresh (fun l -> variant [ "Nil", Type.unit; "Cons", TTuple [ int; l ] ])
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
  print (TRecord r);
  let x = tvar fresh in
  unify fresh (TRecord r) (record [ "x", x ]);
  unify fresh x (variant [ "Cons", TTuple [ int; list () ] ] ~tail:(rvar fresh));
  print (TRecord r);
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
    (TRecord s)
    (record
       [ ( "x"
         , variant
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
  let typecheck = Typecheck.create () in
  (match Typecheck.typecheck typecheck ~program:(clique 10) with
   | Error error -> print_s [%sexp (error : Typecheck_error.t)]
   | Ok signature ->
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
    ; Toplevel.Let ("v", [ typed "x" (Ty.con "g") ], None, var "x")
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
    Type.TTuple (List.init n ~f:(fun _ -> T.record [ "x", generic () ]))
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
