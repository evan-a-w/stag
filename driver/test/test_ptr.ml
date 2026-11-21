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
