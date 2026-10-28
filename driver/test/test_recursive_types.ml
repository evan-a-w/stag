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

let%expect_test "recursive functions over recursive variants" =
  check
    {|
let len l = match l with `Nil -> 0 | `Cons (_, t) -> add 1 (len t)
let map f l = match l with `Nil -> `Nil | `Cons (x, t) -> `Cons (f x, map f t)
let two = len (`Cons (1, `Cons (2, `Nil)))
let lengths = (len (`Cons (1, `Nil)), len (`Cons (true, `Nil)))
let mapped = map id (`Cons (1, `Nil))
|};
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
    {|
let sum r = add r.`v (sum r.`next)
let next r = r.`next
let both r = cond true r r.`next.`next
let self r = cond true r r.`self
let pair r = (self r, r)
|};
  [%expect
    {|
    val sum : ({`next : 'a; `v : int; ..'b} as 'a) -> int
    val next : {`next : 'a; ..'b} -> 'a
    val both : ({`next : {`next : 'a; ..'b}; ..'c} as 'a) -> 'a
    val self : ({`self : 'a; ..'b} as 'a) -> 'a
    val pair : ({`self : 'a; ..'b} as 'a) -> 'a * 'a
    |}]
;;

let%expect_test "recursive values are weak" =
  check
    {|
let ones = `Cons (1, ones)
let loop = {`next = loop; `v = id}
|};
  [%expect
    {|
    val ones : ([`Cons of int * '_a; ..'_b] as '_a)
    val loop : ({`next : '_a; `v : '_b -> '_b} as '_a)
    |}]
;;

let%expect_test "open rows and residuals" =
  check
    {|
let strip e = match e with `Wrap x -> strip x | other -> other
let count e = match e with `S x -> add 1 (count x) | _ -> 0
|};
  [%expect
    {|
    val strip : ([`Wrap of 'a; ..'b] as 'a) -> [..'b]
    val count : ([`S of 'a; ..'b] as 'a) -> int
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
let eval_add self k e = match e with `Add (a, b) -> add (self a) (self b) | other -> k other
let eval_neg self k e = match e with `Neg a -> add 0 (self a) | other -> k other
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
let example = eval (`Add (`Num 1, `Neg (`Num 2)))
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
let bad = eval (`Add (`Num 1, `Num 2))
|}
    );
  [%expect
    {|
    val eval_num : 'a -> ([..'b] -> 'c) -> [`Num of 'c; ..'b] -> 'c
    val eval_add : ('a -> int) -> ([..'b] -> int) -> [`Add of 'a * 'a; ..'b] -> int
    val eval_neg : ('a -> int) -> ([..'b] -> int) -> [`Neg of 'a; ..'b] -> int
    val eval : ([`Add of 'a * 'a | `Neg of 'a | `Num of int] as 'a) -> int
    val example : int
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend Add Present
        (TTuple
         ((TVariant (RExtend Num Present (TCon int ()) (RVar (Unbound 66 1))))
          (TVariant (RExtend Num Present (TCon int ()) (RVar (Unbound 68 1))))))
        (RVar (Unbound 73 1)))
       (Missing_label Add)))
     @ after_eval.stag:2:15-38)
    |}]
;;

let%expect_test "cyclic types in quotes and contexts" =
  check
    {|
let compile e =
  match e with
  | `Code c -> c
  | `Add (a, b) -> .<add .~(compile a) .~(compile b)>.
let len l = match l with `Nil -> 0 | `Cons (_, t) -> add 1 (len t)
let free = .<len l>.
let provided = .<.~free.{l = `Cons (1, `Nil)}>.
let spliced_in l = .<.~free.{l = l}>.
let quoted_ones = .<let ones = `Cons (1, ones) in ones>.
|};
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
    ({`x : list['a]} as 'a)
    (Ok ())
    ([`A of 'a -> int] as 'a)
    (Ok ())
    <int>{x : ({`y : <int>{x : 'a}} as 'a)}
    (Ok ())
    ([`A of 'a -> int] as 'a) -> int
    (Ok ())
    (Ok ())
    ([`A of 'a -> int] as 'a) -> int
    ([`A of 'a -> int] as 'a)
    (Ok ())
    ({`x : 'a} as 'a)
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
    ({`x : 'a; `y : int} as 'a)
    ({`x : {`x : 'a; `y : bool}; `y : int} as 'a)
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
  Scheme.generalize fresh ~rank:1 [ ty ];
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

let%expect_test "recursive aliases" =
  check
    {|
type l['a] = [`Nil | `Cons of 'a * l['a]]
let len l = match l with `Nil -> 0 | `Cons (_, t) -> add 1 (len t)
let len_ints (l : l[int]) = len l
let len_any (l : l['a]) = len l
let nil : l[bool] = `Nil
let one : l[int] = `Cons (1, `Nil)
type ints = l[int]
type n = N of l[int]
let wrapped = N one
|};
  check
    {|
type l['a] = [`Nil | `Cons of 'a * l['a]]
let bad : l[int] = `Cons (true, `Nil)
|};
  check
    {|
type l['a] = [`Nil | `Cons of 'a * l['a]]
let open : l[int] = `Other
|};
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
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:19-37)
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend Other Present (TCon unit ()) (RVar (Unbound 27 1)))
       (Missing_label Other)))
     @ t.stag:3:20-26)
    |}]
;;

let%expect_test "recursive aliases through records, tuples and type arguments" =
  check
    {|
type stream['a] = {`head : 'a; `tail : unit -> stream['a]}
type list['a] = Nil | Cons of 'a * list['a]
type tree = [`Leaf of int | `Node of list[tree]]
type pair = [`A of pair] * int
type id['a] = 'a
type through_id = id[[`A of through_id]]
type rows[{..'r}] = {`x : rows[{..'r}]; ..'r}
let heads (s : stream[int]) = (s.`head, (s.`tail ()).`head)
let leaf (t : tree) = match t with `Leaf n -> n | _ -> 0
|};
  [%expect
    {|
    type stream['a] = {`head : 'a; `tail : unit -> stream['a]}
    type list['a] = Nil | Cons of 'a * list['a]
    type tree = [`Leaf of int | `Node of list[tree]]
    type pair = [`A of pair] * int
    type id['a] = 'a
    type through_id = [`A of through_id]
    type rows[{..'a}] = {`x : rows[{..'a}]; ..'a}
    val heads : ({`head : int; `tail : unit -> 'a} as 'a) -> int * int
    val leaf : ([`Leaf of int | `Node of list['a]] as 'a) -> int
    |}]
;;

let%expect_test "mutually recursive aliases" =
  check
    {|
type a['x] = {`b : b['x]; `v : 'x}
type b['y] = [`End | `More of a['y]]
type c = a[int]
let a x : a['t] = {`b = `End; `v = x}
let more = {`b = `More (a 1); `v = 2}
let c : c = more
|};
  check
    {|
module M = struct
type l['a] = [`Nil | `Cons of 'a * l['a]]
end
type u = [`U of M.l[u]]
|};
  [%expect
    {|
    type a['a] = {`b : [`End | `More of a['a]]; `v : 'a}
    type b['a] = [`End | `More of {`b : b['a]; `v : 'a}]
    type c = {`b : [`End | `More of c]; `v : int}
    val a : 'a -> ({`b : [`End | `More of 'b]; `v : 'a} as 'b)
    val more : {`b : [`End | `More of ({`b : [`End | `More of 'a]; `v : int} as 'a)]; `v : int}
    val c : ({`b : [`End | `More of 'a]; `v : int} as 'a)
    module M : sig
      type l['a] = [`Cons of 'a * l['a] | `Nil]
    end
    type u = [`U of ([`Cons of u * 'a | `Nil] as 'a)]
    |}]
;;

let%expect_test "non-regular and unguarded recursive aliases" =
  check
    {|
type t['a] = [`A of t['a * 'a]]
|};
  check
    {|
type t['a] = [`A of t[int]]
|};
  check
    {|
type t['a, 'b] = [`A of t['b, 'a]]
|};
  check
    {|
type a['x] = {`b : b['x]}
type b['y] = [`A of a[int]]
|};
  check
    {|
type t[{..'r}] = {`x : t[{`y : int; ..'r}]}
|};
  check
    {|
type t = t * int
|};
  check
    {|
type list['a] = Nil | Cons of 'a * list['a]
type t = list[t]
|};
  check
    {|
type id['a] = 'a
type t = id[t]
|};
  check
    {|
type t = <t>{}
|};
  check
    {|
type t = <int>{x : t}
|};
  check
    {|
type t = [`A of int] -> t
|};
  [%expect
    {|
    ((Non_regular_recursive_alias t) @ t.stag:2:20-30)
    ((Non_regular_recursive_alias t) @ t.stag:2:20-26)
    ((Non_regular_recursive_alias t) @ t.stag:2:24-33)
    ((Non_regular_recursive_alias a) @ t.stag:3:20-26)
    ((Non_regular_recursive_alias t) @ t.stag:2:23-42)
    ((Cyclic_type_alias t) @ t.stag:2:5-6)
    ((Cyclic_type_alias t) @ t.stag:3:5-6)
    ((Cyclic_type_alias t) @ t.stag:3:5-6)
    ((Cyclic_type_alias t) @ t.stag:2:5-6)
    ((Cyclic_type_alias t) @ t.stag:2:5-6)
    ((Cyclic_type_alias t) @ t.stag:2:5-6)
    |}]
;;

let%expect_test "sexps of recursive aliases" =
  Option.iter
    (typecheck
       ~prelude:false
       {|
type l['a] = [`Nil | `Cons of 'a * l['a]]
|})
    ~f:(fun t -> print_s [%sexp ((checked t).signature : Signature.t)]);
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
    {`x : ([`Cons of int * 'a | `Nil] as 'a)}
    (Error
     (Type_mismatch (TCon bool ()) (TCon int ()) (Base_types_not_equal bool int)))
    |}]
;;

(* [type ai = [`T0 of a0 | ... | `T(n-1) of a(n-1)]] for each [i < n]. *)
let clique n =
  let variant =
    List.init n ~f:(fun j -> sprintf "`T%d of a%d" j j)
    |> String.concat ~sep:" | "
  in
  List.init n ~f:(fun i -> sprintf "type a%d = [%s]\n" i variant)
  |> String.concat
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
  check
    {|
type f['a] = [`A of 'a]
type g = f[g]
|};
  check
    {|
type g = f[g]
type f['a] = [`A of 'a]
|};
  check
    {|
type f['a] = {`x : 'a}
type g = f[g]
|};
  check
    {|
type g = f[g]
type f['a] = {`x : 'a}
|};
  check
    {|
type f['a] = [`A of 'a]
type g = f[g]
let v (x : g) = x
let a : g = `A (`A a)
|};
  check
    {|
type a = b * int
type b = [`A of a]
|};
  [%expect
    {|
    type f['a] = [`A of 'a]
    type g = [`A of g]
    type g = [`A of g]
    type f['a] = [`A of 'a]
    type f['a] = {`x : 'a}
    type g = {`x : g}
    type g = {`x : g}
    type f['a] = {`x : 'a}
    type f['a] = [`A of 'a]
    type g = [`A of g]
    val v : ([`A of 'a] as 'a) -> 'a
    val a : ([`A of 'a] as 'a)
    type a = [`A of a] * int
    type b = [`A of b * int]
    |}];
  check
    {|
type t = t
|};
  check
    {|
type f['a] = 'a * int
type g = f[g]
|};
  check
    {|
type list['a] = Nil | Cons of 'a * list['a]
type f['a] = list['a]
type g = f[g]
|};
  check
    {|
type f['a] = <'a>{}
type g = f[g]
|};
  check
    {|
type a = b
type b = a
|};
  check
    {|
type id['a] = 'a
type t = [`A of t] * id[t]
|};
  [%expect
    {|
    ((Cyclic_type_alias t) @ t.stag:2:5-6)
    ((Cyclic_type_alias g) @ t.stag:3:5-6)
    ((Cyclic_type_alias g) @ t.stag:4:5-6)
    ((Cyclic_type_alias g) @ t.stag:3:5-6)
    ((Cyclic_type_alias a) @ t.stag:2:5-6)
    ((Cyclic_type_alias t) @ t.stag:3:5-6)
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
           | TArrow (arg, result) ->
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
type l['a] = [`Cons of 'a * l['a] | `Nil]
type s['a] = {c : l['a]; d : l['a]}
type v['a] = V of l['a] * l['a]
let pair (x : l['a] * l['a]) = x
|};
  [%expect
    {|
    type l['a] = [`Cons of 'a * l['a] | `Nil]
    type s['a] = { c : ([`Cons of 'a * 'b | `Nil] as 'b); d : ([`Cons of 'a * 'b | `Nil] as 'b) }
    type v['a] = V of ([`Cons of 'a * 'b | `Nil] as 'b) * 'b
    val pair : ([`Cons of 'a * 'b | `Nil] as 'b) * 'b -> 'b * 'b
    |}]
;;

let%expect_test "elaboration stops at a failed expansion" =
  check
    {|
type v = A of c | B of c
type u = c * c
type c = c * int
|};
  check
    {|
type u = c * c
type c = c * int
|};
  [%expect
    {|
    ((Cyclic_type_alias c) @ t.stag:4:5-6)
    ((Cyclic_type_alias c) @ t.stag:3:5-6)
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
  same "instance1 next" (field instance1 "next") instance1;
  unify fresh (field instance1 "g") int;
  unify fresh (field instance2 "g") Type.bool;
  unify fresh weak Type.string;
  print (TTuple [ instance1; instance2; outer ]);
  [%expect
    {|
    (Ok ())
    (Ok ())
    ({`g : 'a; `inner : ([`A of '_b | `W of '_c] as '_b); `next : '_d} as '_d)
    inner shared: true
    inner (original) shared: true
    outer shared: false
    outer next shared: false
    instance1 next shared: true
    (Ok ())
    (Ok ())
    (Ok ())
    ({`g : int; `inner : ([`A of 'a | `W of string] as 'a); `next : 'b} as 'b) * ({`g : bool; `inner : 'a; `next : 'c} as 'c) * ({`g : 'd; `inner : 'a; `next : 'e} as 'e)
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
    ({`self : 'a; `x?'b : int} as 'a) * {..'c} where ..'c <= {x : 'd}
    (({`self : '_a; `x?'_b : int} as '_a) * {..'_c}) * (({`self : '_d; `x?'_e : int} as '_d) * {..'_f}) where ..'_c <= {x : '_g}, ..'_f <= {x : '_h}
    (Ok ())
    (Ok ())
    (({`self : 'a} as 'a) * {..'_b}) * (({`self : 'c; `x : int} as 'c) * {..'_d}) * (({`self : 'e; `x?'f : int} as 'e) * {..'g}) where ..'_b <= {x : '_h}, ..'_d <= {x : '_i}, ..'g <= {x : 'j}
    |}]
;;
