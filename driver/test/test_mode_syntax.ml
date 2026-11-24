open! Import
open Source_helpers

(* [stack_], [exclave_] and [[@nontail]] evaluate their body: the interpreter
   allocates everything on the heap. *)
let%expect_test "mode syntax typechecks and evaluates as its body" =
  run
    {|
type r = { x : global_ int; y : int }
type t = A of global_ int * int
let f (x @ local) (y : int @ 'm) : int @ global = Int.add x y
let pair (x @ local) = exclave_ (x, stack_ (fun u -> x))
let apply (h : int @ local -> int @ local) = h 1 [@nontail]
let annotated = (f : int @ local -> int -> int)
let mut counter : int @ static = 0
let record = let r = stack_ (Ptr.make { x = 1; y = 2 }) in match r.* with { x = x; y = _ } -> x
let constructed = A (1, 2)
let applied = apply (fun x -> Int.add x 1)
let summed = f 1 2
let paired = match pair 3 with (a, _) -> a
let counted = counter <- 1; counter
let var = (summed @ local)
|};
  [%expect
    {|
    type r = { x : global_ int; y : int }
    type t = A of global_ int * int
    val f : int @ local -> (int -> int) @ local
    val pair : 'a @ local -> 'a * ('b -> 'a @ local) @ local
    val apply : (int @ local -> int @ local) -> int
    val annotated : int @ local -> (int -> int) @ local
    val counter : int
    val record : int
    val constructed : t
    val applied : int
    val summed : int
    val paired : int
    val counted : int
    val var : int
    -- evaluated at compile time
    -- evaluated
    (f <fun>)
    (pair <fun>)
    (apply <fun>)
    (annotated <fun>)
    (counter 1)
    (record 1)
    (constructed (A 1 2))
    (applied 2)
    (summed 3)
    (paired 3)
    (counted 1)
    (var 3)
    |}]
;;

(* The typed tree keeps mode annotations, [stack_], [exclave_] and [[@nontail]],
   which erased code keeps. *)
let%expect_test "mode syntax in quotes" =
  run
    {|
let code = .< fun (x @ local) -> exclave_ stack_ (x, Ptr.make (Int.add x 1 [@nontail])) >.
let erased = Stag.Code.erase code
let spliced = match .~code 1 with (a, b) -> Int.add a (b.*)
|};
  [%expect
    {|
    val code : <int @ local -> int * ptr[int] @ local>{..'a}
    val erased : Stag.Expr.t
    val spliced : int
    -- evaluated at compile time
    (code
     (Code
      (Fun (args (x/16))
       (annots ((x/16 ((mode ((Atoms (Local)) @ t.stag:2:23-28))))))
       (body
        (Marked Exclave
         (Marked Stack
          (Tuple
           ((Var (Bound (Local x/16)) : int)
            (App
             (Var (Persistent (Toplevel prelude.Ptr.make/17)) :
              "int -> ptr[int]")
             (Marked Nontail
              (App
               (App (Var (Prim Int.add) : "int -> int -> int")
                (Var (Bound (Local x/16)) : int) : "int -> int")
               (Lit (Int 1) : int) : int)
              : int)
             : ptr[int]))
           : "int * ptr[int]")
          : "int * ptr[int]")
         : "int * ptr[int]"))
       : "int @ local -> int * ptr[int] @ local")))
    -- evaluated
    (code
     (Code
      (Fun (args (x/16))
       (annots ((x/16 ((mode ((Atoms (Local)) @ t.stag:2:23-28))))))
       (body
        (Marked Exclave
         (Marked Stack
          (Tuple
           ((Var (Bound (Local x/16)) : int)
            (App
             (Var (Persistent (Toplevel prelude.Ptr.make/17)) :
              "int -> ptr[int]")
             (Marked Nontail
              (App
               (App (Var (Prim Int.add) : "int -> int -> int")
                (Var (Bound (Local x/16)) : int) : "int -> int")
               (Lit (Int 1) : int) : int)
              : int)
             : ptr[int]))
           : "int * ptr[int]")
          : "int * ptr[int]")
         : "int * ptr[int]"))
       : "int @ local -> int * ptr[int] @ local")))
    (erased
     (Stag.Expr.Fun
      (Ptr
       (Tuple
        (Cons
         (Tuple (String x)
          ((mode
            (Some
             (Stag.Mode_expr.Atoms (Cons (Stag.Mode_expr.Atom.Local) (Nil)))))
           (ty (None))))
         (Nil))
        (Stag.Expr.Exclave
         (Ptr
          (Stag.Expr.Stack
           (Ptr
            (Stag.Expr.Tuple
             (Cons
              (Stag.Expr.Var (Stag.Path.Name (String x))
               ((mode (None)) (ty (None))))
              (Cons
               (Stag.Expr.App
                (Ptr
                 (Tuple
                  (Stag.Expr.Var (Stag.Path.Ref (Ref (value Ptr.make)))
                   ((mode (None)) (ty (None))))
                  (Stag.Expr.Nontail
                   (Ptr
                    (Stag.Expr.App
                     (Ptr
                      (Tuple
                       (Stag.Expr.App
                        (Ptr
                         (Tuple
                          (Stag.Expr.Var
                           (Stag.Path.Ref (Ref (value Prim.Int.add)))
                           ((mode (None)) (ty (None))))
                          (Stag.Expr.Var (Stag.Path.Name (String x))
                           ((mode (None)) (ty (None)))))))
                       (Stag.Expr.Lit (Stag.Lit.Int 1))))))))))
               (Nil))))))))))))
    (spliced 3)
    |}]
;;
