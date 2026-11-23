open! Import
open Source_helpers

(* Modes are not checked yet: annotations are ignored, and [local_], [exclave_]
   and [[@nontail]] evaluate their body. *)
let%expect_test "mode syntax typechecks and evaluates as its body" =
  run
    {|
type r = { x : global_ int; y : int }
type t = A of global_ int * int
let f (x @ local) (y : int @ 'm) : int @ global = Int.add x y
let pair (x @ local) = exclave_ (x, local_ (x, x))
let apply (h : int @ local -> int @ local) = h 1 [@nontail]
let annotated = (f : int @ local -> int -> int)
let mut counter : int @ static = 0
let record = local_ { x = 1; y = 2 }
let constructed = A (1, 2)
let applied = apply (fun x -> Int.add x 1)
let summed = f 1 2
let paired = pair 3
let counted = counter <- 1; counter
let var = (summed @ local)
|};
  [%expect
    {|
    type r = { x : int; y : int }
    type t = A of int * int
    val f : int -> int -> int
    val pair : 'a -> 'a * ('a * 'a)
    val apply : (int -> int) -> int
    val annotated : int -> int -> int
    val counter : int
    val record : r
    val constructed : t
    val applied : int
    val summed : int
    val paired : int * (int * int)
    val counted : int
    val var : int
    -- evaluated at compile time
    -- evaluated
    (f <fun>)
    (pair <fun>)
    (apply <fun>)
    (annotated <fun>)
    (counter 1)
    (record (r (x 1) (y 2)))
    (constructed (A 1 2))
    (applied 2)
    (summed 3)
    (paired (Tuple 3 (Tuple 3 3)))
    (counted 1)
    (var 3)
    |}]
;;

(* The typed tree has no modes yet, so erased code loses them. *)
let%expect_test "mode syntax in quotes" =
  run
    {|
let code = .< fun (x @ local) -> exclave_ local_ (x, Int.add x 1 [@nontail]) >.
let erased = Stag.Code.erase code
let spliced = .~code 1
|};
  [%expect
    {|
    val code : <int -> int * int>{..'a}
    val erased : Stag.Expr.t
    val spliced : int * int
    -- evaluated at compile time
    (code
     (Code
      (Fun (args (x/16))
       (body
        (Tuple
         ((Var (Bound (Local x/16)) : int)
          (App
           (App (Var (Prim Int.add) : "int -> int -> int")
            (Var (Bound (Local x/16)) : int) : "int -> int")
           (Lit (Int 1) : int) : int))
         : "int * int"))
       : "int -> int * int")))
    -- evaluated
    (code
     (Code
      (Fun (args (x/16))
       (body
        (Tuple
         ((Var (Bound (Local x/16)) : int)
          (App
           (App (Var (Prim Int.add) : "int -> int -> int")
            (Var (Bound (Local x/16)) : int) : "int -> int")
           (Lit (Int 1) : int) : int))
         : "int * int"))
       : "int -> int * int")))
    (erased
     (Stag.Expr.Fun
      (Ptr
       (Tuple (Cons (Tuple (String x) ((mode (None)) (ty (None)))) (Nil))
        (Stag.Expr.Tuple
         (Cons
          (Stag.Expr.Var (Stag.Path.Name (String x)) ((mode (None)) (ty (None))))
          (Cons
           (Stag.Expr.App
            (Ptr
             (Tuple
              (Stag.Expr.App
               (Ptr
                (Tuple
                 (Stag.Expr.Var (Stag.Path.Ref (Ref (value Prim.Int.add)))
                  ((mode (None)) (ty (None))))
                 (Stag.Expr.Var (Stag.Path.Name (String x))
                  ((mode (None)) (ty (None)))))))
              (Stag.Expr.Lit (Stag.Lit.Int 1)))))
           (Nil))))))))
    (spliced (Tuple 1 2))
    |}]
;;
