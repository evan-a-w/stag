open! Import
open Source_helpers

let%expect_test "read through a pointer and match its pointee" =
  run
    {|
type tree = Leaf of int | Branch of ptr[tree * tree]
let pair : ptr[int * bool] = Ptr.make (42, true)
let prefix = *pair
let postfix = pair.*
let tree = Branch (Ptr.make (Leaf 7, Leaf 8))
let first t = match t with Branch *(Leaf x, _) -> x | _ -> 0
let found = first tree
let quoted = .< (Ptr.make 3).* >.
let quoted_value = .~quoted
|};
  [%expect
    {|
    type tree = Leaf of int | Branch of ptr[tree * tree]
    val pair : ptr[int * bool]
    val prefix : int * bool
    val postfix : int * bool
    val tree : tree
    val first : tree -> int
    val found : int
    val quoted : <'a>{prelude.Ptr.make : int -> ptr['a]; ..'b}
    val quoted_value : int
    -- evaluated at compile time
    (quoted
     (Code
      (Deref
       (App (Var (Free_toplevel prelude.Ptr.make) : "int -> ptr['a]")
        (Lit (Int 3) : int) : ptr['a])
       : 'a)))
    -- evaluated
    (pair <ptr>)
    (prefix (Tuple 42 true))
    (postfix (Tuple 42 true))
    (tree (Branch <ptr>))
    (first <fun>)
    (found 7)
    (quoted
     (Code
      (Deref
       (App (Var (Free_toplevel prelude.Ptr.make) : "int -> ptr['a]")
        (Lit (Int 3) : int) : ptr['a])
       : 'a)))
    (quoted_value 3)
    |}]
;;

let%expect_test "allocating a cell restricts generalisation" =
  run
    {|
let cell = Ptr.make (fun x -> x)
let i = cell.* 42
let b = cell.* true
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:4:15-19)
    |}]
;;

let%expect_test "an annotated allocator alias still checks its annotation" =
  run {|
let bad : int -> int = Ptr.make
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon ptr ((ATy (TVar (Link (TCon int ()))))))
       (Base_types_not_equal int ptr)))
     @ t.stag:2:23-31)
    |}]
;;

let%expect_test "a shadowed allocator remains an ordinary function" =
  run
    {|
module Ptr = struct
  let make x = Int.add x 10
end
let value = Ptr.make 1
let generated = .~(.< Ptr.make 2 >.)
let canonical_list = .~(.< [3] >.)
|};
  [%expect
    {|
    module Ptr : sig
      val make : int -> int
    end
    val value : int
    val generated : int
    val canonical_list : list[int]
    -- evaluated at compile time
    -- evaluated
    (Ptr.make <fun>)
    (value 11)
    (generated 12)
    (canonical_list (Cons 3 (Nil)))
    |}]
;;
