open! Import
open Source_helpers

let%expect_test "anonymous functions are typed as lets of their parameters" =
  check
    {|
let ident = fun x -> x
let both = (ident 1, ident true)
let k = fun x _ -> x
let annotated = fun (x : int) (y : bool) -> cond y x x
let curried = fun x -> fun y -> (x, y)
let applied = (fun x -> add x 1) 2
let weak = ident (fun x -> x)
let local = let f = fun x -> x in (f 1, f true)
let higher f = f 1
let passed = higher (fun x -> eq x 2)
|};
  [%expect
    {|
    val ident : 'a -> 'a
    val both : int * bool
    val k : 'a -> 'b -> 'a
    val annotated : int -> bool -> int
    val curried : 'a -> 'b -> 'a * 'b
    val applied : int
    val weak : '_a -> '_a
    val local : int * bool
    val higher : (int -> 'a) -> 'a
    val passed : bool
    |}]
;;

let%expect_test "type errors in anonymous functions" =
  check
    {|
let bad = fun (x : int) -> cond x 1 2
|};
  check
    {|
let bad = (fun x -> add x 1) true
|};
  check
    {|
let unbound = fun x -> y
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:2:32-33)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:29-33)
    ((Not_in_scope y) @ t.stag:2:23-24)
    |}]
;;

let%expect_test "anonymous functions evaluate to closures" =
  run
    {|
let adder = fun n -> fun x -> Int.add x n
let add2 = adder 2
let sums = (add2 1, add2 5, adder 10 1)
let shadow = let x = 1 in (fun x -> x) 2
let captured = let x = 1 in let f = fun y -> Int.add x y in let x = 100 in f x
let twice f x = f (f x)
let four = twice (fun x -> Int.add x x) 1
|};
  [%expect
    {|
    val adder : int -> int -> int
    val add2 : int -> int
    val sums : int * int * int
    val shadow : int
    val captured : int
    val twice : ('a -> 'a) -> 'a -> 'a
    val four : int
    -- evaluated at compile time
    -- evaluated
    (adder <fun>)
    (add2 <fun>)
    (sums (Tuple 3 7 11))
    (shadow 2)
    (captured 101)
    (twice <fun>)
    (four 4)
    |}]
;;

let%expect_test "anonymous functions in quotes and splices" =
  run
    ~bodies:[ "made" ]
    {|
let make c = .<fun x -> Int.add x .~c>.
let made = .~(make .<1>.)
let inc = .~(.<fun x -> .~((fun y -> .<Int.add .~y 1>.) .<x>.)>.)
let applied = (made 2, inc 5)
let hygiene = .~(let wrap c = .<fun x -> Int.add x .~c>. in wrap .<x>.)
let x = 10
let hygienic = hygiene 1
|};
  [%expect
    {|
    val make : <int>{x?'a : int; ..'b} -> <int -> int>{..'c} where ..'b <= {..'c}
    val made : int -> int
    val inc : int -> int
    val applied : int * int
    val hygiene : int -> int
    val x : int
    val hygienic : int
    (made/5
     (Toplevel_splice
      (code
       (App
        (Var (Bound (Toplevel make/4)) :
         "<int>{x?'_a : int; ..'_b} -> <int -> int>{}")
        (Quote (Lit (Int 1) : int) : "<int>{x?'_a : int; ..'_b}") :
        "<int -> int>{}"))
      (expansion
       (Fun (args (x/22))
        (body
         (App
          (App (Var (Prim Int.add) : "int -> int -> int")
           (Var (Bound (Local x/22)) : int) : "int -> int")
          (Lit (Int 1) : int) : int))
        : "int -> int"))
      : "int -> int" (where "..'_b <= {}")))
    -- evaluated at compile time
    (make <fun>)
    -- evaluated
    (make <fun>)
    (made <fun>)
    (inc <fun>)
    (applied (Tuple 3 6))
    (hygiene <fun>)
    (x 10)
    (hygienic 2)
    |}]
;;
