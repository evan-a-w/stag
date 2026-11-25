open! Import
open Helpers
open Source_helpers

module T = struct
  open Type

  let int = TCon ("int", [])
  let tvar fresh = TVar (Fresh.var fresh ~rank:1)
  let rvar fresh = RVar (Fresh.var fresh ~rank:1)

  let row ?(tail = REmpty) entries =
    List.fold_right entries ~init:tail ~f:(fun (name, ty) rest ->
      RExtend (name, Present, Absent, ty, rest))
  ;;

  let variant fresh ?tail entries = Fresh.variant fresh (row ?tail entries)
  let record fresh ?tail entries = Fresh.record fresh (row ?tail entries)
  let ptr ty = TCon ("ptr", [ ATy ty ])

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

let%expect_test "recursive functions over recursive variants" =
  check
    {|
let len l = match l with `Nil -> 0 | `Cons (_, *t) -> add 1 (len t)
let map f l = match l with `Nil -> `Nil | `Cons (x, *t) -> `Cons (f x, Ptr.make (map f t))
let two = len (`Cons (1, Ptr.make (`Cons (2, Ptr.make (`Nil)))))
let lengths = (len (`Cons (1, Ptr.make (`Nil))), len (`Cons (true, Ptr.make (`Nil))))
let mapped = map id (`Cons (1, Ptr.make (`Nil)))
|};
  [%expect
    {|
    val len : ([`Cons of 'a * ptr['b] | `Nil] as 'b) -> int
    val map : ('a -> 'b) -> ([`Cons of 'a * ptr['c] | `Nil] as 'c) -> ([`Cons of 'b * ptr['d] | `Nil; ..'e] as 'd)
    val two : int
    val lengths : int * int
    val mapped : ([`Cons of int * ptr['_a] | `Nil; ..'_b] as '_a)
    |}]
;;

let%expect_test "recursive functions over recursive records" =
  check
    {|
let sum r = add r.`v (sum r.`next.*)
let next r = r.`next
let both r = cond true r r.`next.*.`next.*
let self r = cond true r r.`self.*
let pair r = (self r, r)
|};
  [%expect
    {|
    val sum : ({`next : ptr['a]; `v : int; ..'b} as 'a) -> int
    val next : {`next : 'a; ..'b} -> 'a
    val both : ({`next : ptr[{`next : ptr['a]; ..'b}]; ..'c} as 'a) -> 'a
    val self : ({`self : ptr['a]; ..'b} as 'a) -> 'a
    val pair : ({`self : ptr['a]; ..'b} as 'a) -> 'a * 'a
    |}]
;;

let%expect_test "recursive values are weak" =
  check
    {|
let ones = `Cons (1, fun _ -> Ptr.make ones)
let loop = {`next = fun _ -> Ptr.make loop; `v = id}
|};
  [%expect
    {|
    val ones : ([`Cons of int * ('_a -> ptr['_b]); ..'_c] as '_b)
    val loop : ({`next : '_a -> ptr['_b]; `v : '_c -> '_c} as '_b)
    |}]
;;

let%expect_test "open rows and residuals" =
  check
    {|
let strip e = match e with `Wrap x -> strip x.* | other -> other
let count e = match e with `S x -> add 1 (count x.*) | _ -> 0
|};
  [%expect
    {|
    val strip : ([`Wrap of ptr['a]; ..'b] as 'a) -> [..'b]
    val count : ([`S of ptr['a]; ..'b] as 'a) -> int
    |}]
;;

(* Handlers for each case of an open recursive AST, composed by [eval]. [eval]
   is on the DSL: source can't write an empty match. *)
let%expect_test "extensible interpreter" =
  let absurd = let_ "fn" [ "x" ] (match_ (var "x") []) (var "fn") in
  let eval body = top "eval" [ "e" ] body in
  check_program
    (parse_exn
       {|
let eval_num self k e = match e with `Num n -> n | other -> k other
let eval_add self k e = match e with `Add *(a, b) -> add (self a) (self b) | other -> k other
let eval_neg self k e = match e with `Neg *a -> add 0 (self a) | other -> k other
|}
     @ [ eval
           (app
              (var "eval_add")
              [ var "eval"
              ; app
                  (var "eval_num")
                  [ var "eval"; app (var "eval_neg") [ var "eval"; absurd ] ]
              ; var "e"
              ])
       ]
     @ parse_exn
         ~file:"after_eval.stag"
         {|
let example = eval (`Add (Ptr.make (`Num 1, `Neg (Ptr.make (`Num 2)))))
|}
    );
  check_program
    (parse_exn
       {|
let eval_num self k e = match e with `Num n -> n | other -> k other
|}
     @ [ eval (app (var "eval_num") [ var "eval"; absurd; var "e" ]) ]
     @ parse_exn
         ~file:"after_eval.stag"
         {|
let bad = eval (`Add (Ptr.make (`Num 1, `Num 2)))
|});
  [%expect
    {|
    val eval_num : 'a -> ([..'b] -> 'c) -> [`Num of 'c; ..'b] -> 'c
    val eval_add : ('a -> int) -> ([..'b] -> int) -> [`Add of ptr['a * 'a]; ..'b] -> int
    val eval_neg : ('a -> int) -> ([..'b] -> int) -> [`Neg of ptr['a]; ..'b] -> int
    val eval : ([`Add of ptr['a * 'a] | `Neg of ptr['a] | `Num of int] as 'a) -> int
    val example : int
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend Add Present
        (TVar
         (Link
          (TCon ptr
           ((ATy
             (TVar
              (Link
               (TTuple
                ((TVariant
                  (RExtend Num Present (TCon int ()) (RVar (Unbound 81 1))))
                 (TVariant
                  (RExtend Num Present (TCon int ()) (RVar (Unbound 83 1)))))))))
            (AMode (Var 149))))))
        (RVar (Unbound 88 1)))
       (Missing_label Add)))
     @ after_eval.stag:2:15-49)
    |}]
;;

let%expect_test "cyclic types in quotes and contexts" =
  check
    {|
let compile e =
  match e with
  | `Code c -> c
  | `Add *(a, b) -> .<add .~(compile a) .~(compile b)>.
let len l = match l with `Nil -> 0 | `Cons (_, *t) -> add 1 (len t)
let free = .<len l>.
let provided = .<.~free.{l = `Cons (1, Ptr.make `Nil)}>.
let spliced_in l = forget l in .<.~free.{l = l}>.
let quoted_ones = .<let ones = `Cons (1, Ptr.make ones) in ones>.
|};
  [%expect
    {|
    val compile : ([`Add of ptr['a * 'a] | `Code of <int>{..'b}] as 'a) -> <int>{..'b}
    val len : ([`Cons of 'a * ptr['b] | `Nil] as 'b) -> int
    val free : <int>{l : ([`Cons of 'a * ptr['b] | `Nil] as 'b); ..'c}
    val provided : <int>{..'a}
    val spliced_in : 'a -> <int>{l : ([`Cons of 'b * ptr['c] | `Nil] as 'c); ..'d}
    val quoted_ones : <([`Cons of int * ptr['a]; ..'b] as 'a)>{..'c}
    |}]
;;

let%expect_test "occurs check fails for unguarded cycles" =
  let fresh = Fresh.create () in
  let open T in
  let check f =
    let a = tvar fresh in
    unify fresh a (f a)
  in
  check (fun a -> Type.arrow a int);
  check (fun a -> TTuple [ a; int ]);
  check (fun a -> TCon ("list", [ ATy a ]));
  check (fun a -> TCode { inner = int; context = row [ "x", a ] });
  check (fun a -> TCode { inner = a; context = REmpty });
  check (fun a -> Type.arrow (record fresh [ "x", a ]) a);
  (* Records, variants and functions don't guard without a pointer. *)
  check (fun a -> variant fresh [ "A", Type.arrow a int ]);
  check (fun a -> Type.arrow (variant fresh [ "A", a ]) int);
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
  unify fresh b (TTuple [ a; int ]);
  [%expect
    {|
    (Error (Occurs_check_failed (ATy (TVar (Unbound 0 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 1 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 2 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 3 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 4 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 5 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 7 1)))))
    (Error (Occurs_check_failed (ATy (TVar (Unbound 9 1)))))
    (Error (Occurs_check_failed (ARow (RVar (Unbound 11 1)))))
    (Error (Occurs_check_failed (ARow (RVar (Unbound 12 1)))))
    (Error (Occurs_check_failed (ARow (RVar (Unbound 13 1)))))
    (Ok ())
    (Error (Occurs_check_failed (ATy (TVar (Unbound 15 1)))))
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
  check (fun a -> variant fresh [ "A", ptr a ]);
  check (fun a -> record fresh [ "x", TCon ("list", [ ATy (ptr a) ]) ]);
  check (fun a -> variant fresh [ "A", Type.arrow (ptr a) int ]);
  check (fun a ->
    TCode { inner = int; context = row [ "x", record fresh [ "y", ptr a ] ] });
  check (fun a -> Type.arrow (variant fresh [ "A", ptr a ]) int);
  check (fun a -> Type.arrow (ptr a) int);
  (* Through another variable. *)
  let b = tvar fresh in
  let a = tvar fresh in
  unify fresh b (variant fresh [ "A", a ]);
  unify fresh a (Type.arrow (ptr b) int);
  print a;
  print b;
  (* Through a variable already bound to a guarded cycle's part. *)
  let c = tvar fresh in
  let d = tvar fresh in
  unify fresh c (TTuple [ d; int ]);
  unify fresh d (Type.arrow int (ptr c));
  print (TTuple [ c; d ]);
  (* A row variable within its own row, under a pointer. *)
  let r = rvar fresh in
  let record_r = Fresh.record fresh r in
  print_s
    [%sexp
      (Unify.unify_row fresh ~row1:r ~row2:(row [ "x", ptr record_r ])
       : (unit, Unification_error.t) Result.t)];
  print record_r;
  [%expect
    {|
    (Ok ())
    ([`A of ptr['a]] as 'a)
    (Ok ())
    ({`x : list[ptr['a]]} as 'a)
    (Ok ())
    ([`A of ptr['a] -> int] as 'a)
    (Ok ())
    (<int>{x : immut {`y : ptr['a]}} as 'a)
    (Ok ())
    ([`A of ptr['a]] -> int as 'a)
    (Ok ())
    (ptr['a] -> int as 'a)
    (Ok ())
    (Ok ())
    (ptr[[`A of 'a]] -> int as 'a)
    ([`A of ptr['a] -> int] as 'a)
    (Ok ())
    (Ok ())
    (((int -> ptr['a]) * int) as 'a) * (int -> ptr['a])
    (Ok ())
    ({`x : ptr['a]} as 'a)
    |}]
;;

let%expect_test "sexps of cyclic types" =
  let fresh = Fresh.create () in
  let open T in
  let list =
    fix fresh (fun l ->
      variant fresh [ "Nil", Type.unit; "Cons", TTuple [ int; ptr l ] ])
  in
  print_s [%sexp (list : Type.ty)];
  let r = rvar fresh in
  let record_r = Fresh.record fresh r in
  ignore
    (Unify.unify_row fresh ~row1:r ~row2:(row [ "x", ptr record_r ])
     : _ Result.t);
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
          (TTuple ((TCon int ()) (TCon ptr ((ATy (TVar (Link (Cycle_ref 0))))))))
          REmpty))))))
    (Cycle 0
     (TRecord
      (RVar (Link (RExtend x Present (TCon ptr ((ATy (Cycle_ref 0)))) REmpty)))))
    (ATy
     (TVar
      (Link
       (Cycle 0
        (TVariant
         (RExtend Nil Present (TCon unit ())
          (RExtend Cons Present
           (TTuple
            ((TCon int ()) (TCon ptr ((ATy (TVar (Link (Cycle_ref 0))))))))
           REmpty)))))))
    |}]
;;

let%expect_test "unifying cyclic types of different unrollings" =
  let fresh = Fresh.create () in
  let open T in
  let a = fix fresh (fun a -> variant fresh [ "A", ptr a ]) in
  let b = tvar fresh in
  let unrolled = variant fresh [ "A", ptr (variant fresh [ "A", ptr b ]) ] in
  unify fresh a unrolled;
  print b;
  print unrolled;
  let two =
    fix fresh (fun a ->
      variant fresh [ "A", ptr (variant fresh [ "A", ptr a ]) ])
  in
  let three =
    fix fresh (fun a ->
      variant
        fresh
        [ "A", ptr (variant fresh [ "A", ptr (variant fresh [ "A", ptr a ]) ]) ])
  in
  unify fresh two three;
  print (TTuple [ two; three ]);
  let open_ =
    fix fresh (fun a -> variant fresh [ "A", ptr a ] ~tail:(rvar fresh))
  in
  unify fresh open_ a;
  print open_;
  [%expect
    {|
    (Ok ())
    ([`A of ptr['a]] as 'a)
    [`A of ptr[[`A of ptr[([`A of ptr['a]] as 'a)]]]]
    (Ok ())
    ([`A of ptr[[`A of ptr['a]]]] as 'a) * ([`A of ptr[[`A of ptr[[`A of ptr['b]]]]]] as 'b)
    (Ok ())
    ([`A of ptr['a]] as 'a)
    |}]
;;

let%expect_test "mismatching cyclic types" =
  let fresh = Fresh.create () in
  let open T in
  let a = fix fresh (fun a -> variant fresh [ "A", ptr a ]) in
  let ab =
    fix fresh (fun a ->
      variant fresh [ "A", ptr (variant fresh [ "B", ptr a ]) ])
  in
  unify fresh a ab;
  let r = fix fresh (fun r -> record fresh [ "x", ptr r; "y", int ]) in
  let s =
    fix fresh (fun s ->
      record
        fresh
        [ "x", ptr (record fresh [ "x", ptr s; "y", TCon ("bool", []) ])
        ; "y", int
        ])
  in
  unify fresh r s;
  print r;
  print s;
  [%expect
    {|
    (Error
     (Row_mismatch
      (RExtend A Present
       (TCon ptr
        ((ATy
          (TVar
           (Link
            (Cycle 0
             (TVariant
              (RExtend A Present (TCon ptr ((ATy (TVar (Link (Cycle_ref 0))))))
               REmpty))))))))
       REmpty)
      (RExtend B Present
       (TCon ptr
        ((ATy
          (TVar
           (Link
            (Cycle 0
             (TVariant
              (RExtend A Present
               (TCon ptr
                ((ATy
                  (TVariant
                   (RExtend B Present
                    (TCon ptr ((ATy (TVar (Link (Cycle_ref 0)))))) REmpty)))))
               REmpty))))))))
       REmpty)
      (Missing_label A)))
    (Error
     (Type_mismatch (TCon int ()) (TCon bool ()) (Base_types_not_equal int bool)))
    ({`x : ptr['a]; `y : int} as 'a)
    ({`x : ptr[{`x : ptr['a]; `y : bool}]; `y : int} as 'a)
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
    (variant fresh [ "Nil", Type.unit; "Cons", TTuple [ elt; ptr list ] ]);
  let ty = Type.TTuple [ list; list ] in
  print ty;
  Scheme.generalize fresh ~rank:1 [ ty ];
  print ty;
  let inst () = Scheme.instantiate fresh ~rank:1 ty in
  let ints = inst () in
  let bools = inst () in
  print ints;
  let cons elt =
    variant
      fresh
      [ "Cons", TTuple [ elt; ptr (tvar fresh) ] ]
      ~tail:(rvar fresh)
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
    ([`Cons of '_a * ptr['_b] | `Nil] as '_b) * '_b
    ([`Cons of 'a * ptr['b] | `Nil] as 'b) * 'b
    ([`Cons of '_a * ptr['_b] | `Nil] as '_b) * '_b
    (Ok ())
    (Ok ())
    (([`Cons of int * ptr['a] | `Nil] as 'a) * 'a) * (([`Cons of bool * ptr['b] | `Nil] as 'b) * 'b) * (([`Cons of 'c * ptr['d] | `Nil] as 'd) * 'd)
    |}]
;;

let%expect_test "functions don't guard cycles, nor pointers alone in aliases" =
  check "let self x = x x";
  check "let self x = x (Ptr.make x)";
  check "type al = ptr[al]";
  check "type f = int -> f";
  check "type f = int -> ptr[f]";
  check "type f = [`F of int -> ptr[f]]";
  check "type s = {`head : int; `tail : unit -> ptr[s]}";
  [%expect
    {|
    ((Unification_error (Occurs_check_failed (ATy (TVar (Unbound 25 1))))) @
     t.stag:1:15-16)
    val self : ((ptr['a -> 'b] as 'a) -> 'b) -> 'b
    ((Cyclic_type_alias al) @ t.stag:1:5-7)
    ((Infinite_type_layout f) @ t.stag:1:5-6)
    type f = int -> ptr[f]
    type f = [`F of int -> ptr[f]]
    type s = {`head : int; `tail : unit -> ptr[s]}
    |}]
;;

let%expect_test "recursive aliases" =
  check
    {|
type l['a] = [`Nil | `Cons of 'a * ptr[l['a]]]
let len l = match l with `Nil -> 0 | `Cons (_, *t) -> add 1 (len t)
let len_ints (l : l[int]) = len l
let len_any (l : l['a]) = len l
let nil : l[bool] = `Nil
let one : l[int] = `Cons (1, Ptr.make (`Nil))
type ints = l[int]
type n = N of l[int]
let wrapped = N one
|};
  check
    {|
type l['a] = [`Nil | `Cons of 'a * ptr[l['a]]]
let bad : l[int] = `Cons (true, Ptr.make (`Nil))
|};
  check
    {|
type l['a] = [`Nil | `Cons of 'a * ptr[l['a]]]
let open : l[int] = `Other
|};
  [%expect
    {|
    type l['a] = [`Cons of 'a * ptr[l['a]] | `Nil]
    val len : ([`Cons of 'a * ptr['b] | `Nil] as 'b) -> int
    val len_ints : ([`Cons of int * ptr['a] | `Nil] as 'a) -> int
    val len_any : ([`Cons of 'a * ptr['b] | `Nil] as 'b) -> int
    val nil : ([`Cons of bool * ptr['a] | `Nil] as 'a)
    val one : ([`Cons of int * ptr['a] | `Nil] as 'a)
    type ints = [`Cons of int * ptr[ints] | `Nil]
    type n = N of ([`Cons of int * ptr['a] | `Nil] as 'a)
    val wrapped : n
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:19-48)
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend Other Present (TCon unit ()) (RVar (Unbound 32 1)))
       (Missing_label Other)))
     @ t.stag:3:20-26)
    |}]
;;

let%expect_test "recursive aliases through records, tuples and type arguments" =
  check
    {|
type stream['a] = {`head : 'a; `tail : unit -> ptr[stream['a]]}
type list['a] = Nil | Cons of box['a * list['a]]
type tree = [`Leaf of int | `Node of list[tree]]
type pair = [`A of ptr[pair]] * int
type id['a] = 'a
type through_id = id[[`A of ptr[through_id]]]
type rows[{..'r}] = {`x : ptr[rows[{..'r}]]; ..'r}
let heads (s : stream[int]) = (s.`head, (s.`tail ()).*.`head)
let leaf (t : tree) = match t with `Leaf n -> n | _ -> 0
|};
  [%expect
    {|
    type stream['a] = {`head : 'a; `tail : unit -> ptr[stream['a]]}
    type list['a] = Nil | Cons of box['a * list['a]]
    type tree = [`Leaf of int | `Node of list[tree]]
    type pair = [`A of ptr[pair]] * int
    type id['a] = 'a
    type through_id = [`A of ptr[through_id]]
    type rows[{..'a}] = {`x : ptr[rows[{..'a}]]; ..'a}
    val heads : ({`head : int; `tail : unit -> ptr['a]} as 'a) -> int * int
    val leaf : ([`Leaf of int | `Node of list['a]] as 'a) -> int
    |}]
;;

let%expect_test "mutually recursive aliases" =
  check
    {|
type a['x] = {`b : b['x]; `v : 'x}
type b['y] = [`End | `More of ptr[a['y]]]
type c = a[int]
let a x : a['t] = {`b = `End; `v = x}
let more = {`b = `More (Ptr.make (a 1)); `v = 2}
let c : c = more
|};
  check
    {|
module M = struct
type l['a] = [`Nil | `Cons of 'a * ptr[l['a]]]
end
type u = [`U of ptr[M.l[u]]]
|};
  [%expect
    {|
    type a['a] = {`b : [`End | `More of ptr[a['a]]]; `v : 'a}
    type b['a] = [`End | `More of ptr[{`b : b['a]; `v : 'a}]]
    type c = {`b : [`End | `More of ptr[c]]; `v : int}
    val a : 'a -> ({`b : [`End | `More of ptr['b]]; `v : 'a} as 'b)
    val more : {`b : [`End | `More of ptr[({`b : [`End | `More of ptr['a]]; `v : int} as 'a)]]; `v : int}
    val c : ({`b : [`End | `More of ptr['a]]; `v : int} as 'a)
    module M : sig
      type l['a] = [`Cons of 'a * ptr[l['a]] | `Nil]
    end
    type u = [`U of ptr[([`Cons of u * ptr['a] | `Nil] as 'a)]]
    |}]
;;

let%expect_test "non-regular and unguarded recursive aliases" =
  check {|
type t['a] = [`A of t['a * 'a]]
|};
  check {|
type t['a] = [`A of t[int]]
|};
  check {|
type t['a, 'b] = [`A of t['b, 'a]]
|};
  check {|
type a['x] = {`b : b['x]}
type b['y] = [`A of a[int]]
|};
  check {|
type t[{..'r}] = {`x : t[{`y : int; ..'r}]}
|};
  check {|
type t = t * int
|};
  check {|
type list['a] = Nil | Cons of box['a * list['a]]
type t = list[t]
|};
  check {|
type id['a] = 'a
type t = id[t]
|};
  check {|
type t = <t>{}
|};
  check {|
type t = <int>{x : t}
|};
  check {|
type t = [`A of int] -> t
|};
  [%expect
    {|
    ((Non_regular_recursive_alias t) @ t.stag:2:20-30)
    ((Non_regular_recursive_alias t) @ t.stag:2:20-26)
    ((Non_regular_recursive_alias t) @ t.stag:2:24-33)
    ((Non_regular_recursive_alias a) @ t.stag:3:20-26)
    ((Non_regular_recursive_alias t) @ t.stag:2:23-42)
    ((Infinite_type_layout t) @ t.stag:2:5-6)
    type list['a] = Nil | Cons of box['a * list['a]]
    type t = list[t]
    ((Cyclic_type_alias t) @ t.stag:3:5-6)
    ((Infinite_type_layout t) @ t.stag:2:5-6)
    ((Infinite_type_layout t) @ t.stag:2:5-6)
    ((Infinite_type_layout t) @ t.stag:2:5-6)
    |}]
;;

let%expect_test "sexps of recursive aliases" =
  Option.iter
    (typecheck
       ~prelude:false
       {|
type l['a] = [`Nil | `Cons of 'a * ptr[l['a]]]
|})
    ~f:(fun t -> print_s [%sexp ((checked t).signature : Signature.t)]);
  [%expect
    {|
    ((Type
      ((name l) (params ((ATy (TVar (Generic 1)))))
       (kind
        (Alias
         (TVar
          (Link
           (Cycle 0
            (TVariant
             (RExtend Nil Present (TCon unit ())
              (RExtend Cons Present
               (TTuple
                ((TVar (Generic 1))
                 (TCon ptr
                  ((ATy (TVar (Link (Cycle_ref 0))))
                   (AMode
                    ((stage Nonportable) (provenance Gc) (locality Global)))))))
               REmpty)))))))))))
    |}]
;;

let%expect_test "bounds with cyclic types" =
  let fresh = Fresh.create () in
  let open T in
  let list () =
    fix fresh (fun l ->
      variant fresh [ "Nil", Type.unit; "Cons", TTuple [ int; ptr l ] ])
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
    (variant fresh [ "Cons", TTuple [ int; ptr (list ()) ] ] ~tail:(rvar fresh));
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
    {..'_a} where ..'_a <= {x : immut ([`Cons of int * ptr['b] | `Nil] as 'b); ..'_c}
    (Ok ())
    (Ok ())
    {`x : ([`Cons of int * ptr['a] | `Nil] as 'a)}
    (Error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
    |}]
;;

(* [type ai = [`T0 of a0 | ... | `T(n-1) of a(n-1)]] for each [i < n]. *)
let clique n =
  let variant =
    List.init n ~f:(fun j -> sprintf "`T%d of ptr[a%d]" j j)
    |> String.concat ~sep:" | "
  in
  List.init n ~f:(fun i -> sprintf "type a%d = [%s]\n" i variant)
  |> String.concat
;;

let%expect_test "mutually recursive alias cliques expand each alias once" =
  check (clique 3);
  [%expect
    {|
    type a0 = [`T0 of ptr[a0] | `T1 of ptr[([`T0 of ptr[a0] | `T1 of ptr['a] | `T2 of ptr[([`T0 of ptr[a0] | `T1 of ptr['a] | `T2 of ptr['b]] as 'b)]] as 'a)] | `T2 of ptr['b]]
    type a1 = [`T0 of ptr[([`T0 of ptr['a] | `T1 of ptr[a1] | `T2 of ptr[([`T0 of ptr['a] | `T1 of ptr[a1] | `T2 of ptr['b]] as 'b)]] as 'a)] | `T1 of ptr[a1] | `T2 of ptr['b]]
    type a2 = [`T0 of ptr[([`T0 of ptr['a] | `T1 of ptr[([`T0 of ptr['a] | `T1 of ptr['b] | `T2 of ptr[a2]] as 'b)] | `T2 of ptr[a2]] as 'a)] | `T1 of ptr['b] | `T2 of ptr[a2]]
    |}];
  (* Each declaration shows each of the 10 aliases' variants once. *)
  Option.iter
    (typecheck ~prelude:false (clique 10))
    ~f:(fun t ->
      Signature.to_string_hum (checked t).signature
      |> String.split_lines
      |> List.iter ~f:(fun line ->
        let variants = String.count line ~f:(Char.equal '[') in
        printf
          "%s: %d variants\n"
          (List.nth_exn (String.split line ~on:' ') 1)
          variants));
  [%expect
    {|
    a0: 110 variants
    a1: 110 variants
    a2: 110 variants
    a3: 110 variants
    a4: 110 variants
    a5: 110 variants
    a6: 110 variants
    a7: 110 variants
    a8: 110 variants
    a9: 110 variants
    |}]
;;

let%expect_test "guardedness is judged on the expansion" =
  check {|
type f['a] = [`A of ptr['a]]
type g = f[g]
|};
  check {|
type g = f[g]
type f['a] = [`A of ptr['a]]
|};
  check {|
type f['a] = {`x : ptr['a]}
type g = f[g]
|};
  check {|
type g = f[g]
type f['a] = {`x : ptr['a]}
|};
  check
    {|
type f['a] = [`A of ptr['a]]
type g = f[g]
let v (x : g) = x
let a (x : g) : g = `A (Ptr.make (`A (Ptr.make x)))
|};
  check {|
type a = b * int
type b = [`A of ptr[a]]
|};
  check {|
type f['a] = [`A of 'a]
type g = f[g]
|};
  check {|
type a = b * int
type b = [`A of a]
|};
  [%expect
    {|
    type f['a] = [`A of ptr['a]]
    type g = [`A of ptr[g]]
    type g = [`A of ptr[g]]
    type f['a] = [`A of ptr['a]]
    type f['a] = {`x : ptr['a]}
    type g = {`x : ptr[g]}
    type g = {`x : ptr[g]}
    type f['a] = {`x : ptr['a]}
    type f['a] = [`A of ptr['a]]
    type g = [`A of ptr[g]]
    val v : ([`A of ptr['a]] as 'a) -> 'a
    val a : ([`A of ptr['a]] as 'a) -> ([`A of ptr['b]] as 'b)
    type a = [`A of ptr[a]] * int
    type b = [`A of ptr[b * int]]
    ((Infinite_type_layout g) @ t.stag:3:5-6)
    ((Infinite_type_layout a) @ t.stag:2:5-6)
    |}];
  check {|
type t = t
|};
  check {|
type f['a] = 'a * int
type g = f[g]
|};
  check
    {|
type list['a] = Nil | Cons of box['a * list['a]]
type f['a] = list['a]
type g = f[g]
|};
  check {|
type f['a] = <'a>{}
type g = f[g]
|};
  check {|
type a = b
type b = a
|};
  check {|
type id['a] = 'a
type t = [`A of t] * id[t]
|};
  [%expect
    {|
    ((Cyclic_type_alias t) @ t.stag:2:5-6)
    ((Infinite_type_layout g) @ t.stag:3:5-6)
    type list['a] = Nil | Cons of box['a * list['a]]
    type f['a] = list['a]
    type g = list[g]
    ((Infinite_type_layout g) @ t.stag:3:5-6)
    ((Cyclic_type_alias a) @ t.stag:2:5-6)
    ((Infinite_type_layout t) @ t.stag:3:5-6)
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
  Scheme.generalize fresh ~rank:1 [ ty ];
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
       (Type.iter_ty
          { ty = add; row = add; presence = add; mode = (fun _ -> Ok ()) }
          ty
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

(* [t0['a] = 'a] and [t(k+1)['a] = step tk['a]], so [tn] has [2^n] paths when
   [step] uses its argument twice, but [n] expansions. *)
let alias_dag n ~step =
  "type t0['a] = 'a\n"
  ^ String.concat
      (List.init n ~f:(fun k ->
         sprintf "type t%d['a] = %s\n" (k + 1) (step (sprintf "t%d['a]" k))))
;;

(* The distinct records and variants, [Link]s and other variables reachable. *)
let print_parts ty =
  let nodes = Int.Hash_set.create () in
  let links = Int.Hash_set.create () in
  let vars = Int.Hash_set.create () in
  let first set id =
    match Hash_set.strict_add set id with
    | Ok -> true
    | Duplicate -> false
  in
  let rec go_ty (ty : Type.ty) =
    match ty with
    | TVar ({ state = Link target; _ } as var) ->
      if first links var.id then go_ty target
    | TVar var -> ignore (first vars var.id : bool)
    | TCon (_, args) ->
      List.iter args ~f:(function
        | ATy ty -> go_ty ty
        | ARow row -> go_row row
        | AMode _ -> ())
    | TArrow (ty1, ty2, _) ->
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
    | RExtend (_, _, _, ty, rest) ->
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
  let check ~step =
    let source =
      alias_dag n ~step
      ^ sprintf
          {|let f (x : t%d['a]) = 1
let g y = f y
let h (z : t%d[int]) = g z
|}
          n
          n
    in
    Option.iter (typecheck ~prelude:false source) ~f:(fun t ->
      List.iter (checked t).signature ~f:(function
        | Value { name; scheme; _ } ->
          (match Type.canonical_ty scheme with
           | TArrow (arg, result, _) ->
             ignore (Type.sexp_of_ty arg : Sexp.t);
             printf "%s : _ -> %s " name (Type.to_string_hum result);
             print_parts arg
           | _ -> assert false)
        | Type _ | Exception _ | Module _ -> ()))
  in
  check ~step:(fun t -> sprintf "[`L of %s | `R of %s]" t t);
  check ~step:(fun t -> sprintf "%s * %s" t t);
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
  check
    {|
type l['a] = [`Cons of 'a * ptr[l['a]] | `Nil]
type s['a] = {c : l['a]; d : l['a]}
type v['a] = V of l['a] * l['a]
let pair (x : l['a] * l['a]) = x
|};
  [%expect
    {|
    type l['a] = [`Cons of 'a * ptr[l['a]] | `Nil]
    type s['a] = { c : ([`Cons of 'a * ptr['b] | `Nil] as 'b); d : ([`Cons of 'a * ptr['b] | `Nil] as 'b) }
    type v['a] = V of ([`Cons of 'a * ptr['b] | `Nil] as 'b) * 'b
    val pair : ([`Cons of 'a * ptr['b] | `Nil] as 'b) * 'b -> 'b * 'b
    |}]
;;

let%expect_test "elaboration stops at a failed expansion" =
  check {|
type v = A of c | B of c
type u = c * c
type c = c * int
|};
  check {|
type u = c * c
type c = c * int
|};
  [%expect
    {|
    ((Infinite_type_layout c) @ t.stag:4:5-6)
    ((Infinite_type_layout c) @ t.stag:3:5-6)
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
       [ "Nil", Type.unit
       ; "Cons", Type.TTuple [ elt; Type.arrow int (ptr list) ]
       ]);
  (* Entered through a [Link], as through the variant. *)
  let ty = Type.TTuple [ list; Type.canonical_ty list ] in
  Scheme.generalize fresh ~rank:1 [ ty ];
  print_parts ty;
  let instance = Scheme.instantiate fresh ~rank:1 ty in
  print_parts instance;
  print instance;
  [%expect
    {|
    (Ok ())
    ((records_and_variants 1) (links 1) (vars 1))
    ((records_and_variants 1) (links 1) (vars 1))
    ([`Cons of '_a * (int -> ptr['_b]) | `Nil] as '_b) * '_b
    |}]
;;

let%expect_test "occurs check through a link visited guarded, then unguarded" =
  let fresh = Fresh.create () in
  let open T in
  (* [l = v -> int], then [v = f l]. *)
  let check f =
    let v = tvar fresh in
    let l = tvar fresh in
    unify fresh l (Type.arrow v int);
    unify fresh v (f l)
  in
  check (fun l -> TTuple [ ptr l; l ]);
  check (fun l -> TTuple [ l; ptr l ]);
  check (fun l -> TTuple [ ptr l; variant fresh [ "A", ptr l ] ]);
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
  check_row (fun code -> TTuple [ ptr code; code ]);
  check_row (fun code -> TTuple [ code; ptr code ]);
  check_row (fun code -> TTuple [ ptr code; record fresh [ "z", ptr code ] ]);
  [%expect
    {|
    (Ok ())
    (Error (Occurs_check_failed (ATy (TVar (Unbound 0 1)))))
    (Ok ())
    (Error (Occurs_check_failed (ATy (TVar (Unbound 2 1)))))
    (Ok ())
    (Ok ())
    (Ok ())
    (Error (Occurs_check_failed (ATy (TVar (Unbound 7 1)))))
    (Ok ())
    (Error (Occurs_check_failed (ATy (TVar (Unbound 9 1)))))
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
  (* The pair of the cycles' [Link]s is reached again within the cycles, and [a]
     then meets [c], whose [y] differs. *)
  let cycle y = fix fresh (fun a -> record fresh [ "x", ptr a; "y", y ]) in
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
      | Type.RExtend (name', _, _, ty, rest) ->
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
  let inner =
    fix_at ~rank:1 (fun a -> variant fresh [ "A", ptr a; "W", weak ])
  in
  let generic = Type.TVar (Fresh.var fresh ~rank:2) in
  let outer =
    fix_at ~rank:2 (fun a ->
      record fresh [ "next", ptr a; "g", generic; "inner", inner ])
  in
  Scheme.generalize fresh ~rank:1 [ outer ];
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
  let pointee ty =
    match Type.canonical_ty ty with
    | TCon ("ptr", [ ATy ty ]) -> ty
    | _ -> assert false
  in
  same "instance1 next" (pointee (field instance1 "next")) instance1;
  unify fresh (field instance1 "g") int;
  unify fresh (field instance2 "g") Type.bool;
  unify fresh weak Type.string;
  print (TTuple [ instance1; instance2; outer ]);
  [%expect
    {|
    (Ok ())
    (Ok ())
    ({`g : 'a; `inner : ([`A of ptr['_b] | `W of '_c] as '_b); `next : ptr['_d]} as '_d)
    inner shared: true
    inner (original) shared: true
    outer shared: false
    outer next shared: false
    instance1 next shared: true
    (Ok ())
    (Ok ())
    (Ok ())
    ({`g : int; `inner : ([`A of ptr['a] | `W of string] as 'a); `next : ptr['b]} as 'b) * ({`g : bool; `inner : 'a; `next : ptr['c]} as 'c) * ({`g : 'd; `inner : 'a; `next : ptr['e]} as 'e)
    |}]
;;

let%expect_test "instantiation copies generic presences and bounds" =
  let fresh = Fresh.create () in
  let open T in
  let generic () = Fresh.var fresh ~rank:2 in
  (* [({self : ptr['a]; x?'p : int} as 'a)] *)
  let knot = Type.TVar (generic ()) in
  unify
    fresh
    knot
    (Fresh.record
       fresh
       (RExtend
          ( "x"
          , PVar (generic ())
          , Absent
          , int
          , RExtend ("self", Present, Absent, ptr knot, REmpty) )));
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
  Scheme.generalize fresh ~rank:1 [ ty ];
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
    ({`self : ptr['a]; `x?'b : int} as 'a) * {..'c} where ..'c <= {x : immut 'd}
    (({`self : ptr['_a]; `x?'_b : int} as '_a) * {..'_c}) * (({`self : ptr['_d]; `x?'_e : int} as '_d) * {..'_f}) where ..'_c <= {x : immut '_g}, ..'_f <= {x : immut '_h}
    (Ok ())
    (Ok ())
    (({`self : ptr['a]} as 'a) * {..'_b}) * (({`self : ptr['c]; `x : int} as 'c) * {..'_d}) * (({`self : ptr['e]; `x?'f : int} as 'e) * {..'g}) where ..'_b <= {x : immut '_h}, ..'_d <= {x : immut '_i}, ..'g <= {x : immut 'j}
    |}]
;;
