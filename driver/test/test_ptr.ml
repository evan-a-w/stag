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
    val quoted : <int>{..'a}
    val quoted_value : int
    -- evaluated at compile time
    (quoted
     (Code
      (Deref
       (App (Var (Persistent (Toplevel prelude.Ptr.make/21)) : "int -> ptr[int]")
        (Lit (Int 3) : int) : ptr[int])
       : int)))
    -- evaluated
    (pair (Ptr (Tuple 42 true)))
    (prefix (Tuple 42 true))
    (postfix (Tuple 42 true))
    (tree (Branch (Ptr (Tuple (Leaf 7) (Leaf 8)))))
    (first <fun>)
    (found 7)
    (quoted
     (Code
      (Deref
       (App (Var (Persistent (Toplevel prelude.Ptr.make/21)) : "int -> ptr[int]")
        (Lit (Int 3) : int) : ptr[int])
       : int)))
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

let%expect_test "allocation is independently instantiated inside quotes" =
  run
    {|
let pair = .~(.< ((Ptr.make 1).*, (Ptr.make "a").*) >.)
let lists = .~(.< [[1]] >.)
include .~([Stag.Toplevel.let_ "erased" [] (Stag.Code.erase .< [[1]] >.)])
|};
  [%expect
    {|
    val pair : int * string
    val lists : list[list[int]]
    val erased : list[list[int]]
    -- evaluated at compile time
    -- evaluated
    (pair (Tuple 1 (String a)))
    (lists (Cons (Cons 1 (Nil)) (Nil)))
    (erased (Cons (Cons 1 (Nil)) (Nil)))
    |}]
;;

let%expect_test "an annotated allocator alias still checks its annotation" =
  run {|
let bad : int -> int = Ptr.make
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ())
       (TCon ptr ((ATy (TVar (Link (TCon int ())))) (AMode (Var 74))))
       (Base_types_not_equal int ptr)))
     @ t.stag:2:23-31)
    |}]
;;

let%expect_test "pointer allocation propagates the expected pointee type" =
  run
    {|
module M = struct
  type t = A | B of int
end
let cell : ptr[M.t] = Ptr.make (B 7)
let cells : list[M.t] = [A; B 8]
let values = (cell.*, cells)
|};
  [%expect
    {|
    module M : sig
      type t = A | B of int
    end
    val cell : ptr[M.t]
    val cells : list[M.t]
    val values : M.t * list[M.t]
    -- evaluated at compile time
    -- evaluated
    (cell (Ptr (M.B 7)))
    (cells (Cons (M.A) (Cons (M.B 8) (Nil))))
    (values (Tuple (M.B 7) (Cons (M.A) (Cons (M.B 8) (Nil)))))
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

let%expect_test "constructors resolve through a wrapper of the allocator" =
  run
    {|
module M = struct
  type t = A | B of int
end
let wrap x = Ptr.make x
let cell : ptr[M.t] = wrap (B 7)
let quoted : <ptr[M.t]>{} = .< wrap A >.
let values = (cell.*, (.~quoted).*)
|};
  [%expect
    {|
    module M : sig
      type t = A | B of int
    end
    val wrap : 'a -> ptr['a]
    val cell : ptr[M.t]
    val quoted : <ptr[M.t]>{}
    val values : M.t * M.t
    -- evaluated at compile time
    (quoted
     (Code
      (App (Var (Persistent (Toplevel wrap/4)) : "M.t -> ptr[M.t]")
       (Construct ((type_name M.t) (name M.A) (index 0)) () : M.t) : ptr[M.t])))
    -- evaluated
    (wrap <fun>)
    (cell (Ptr (M.B 7)))
    (quoted
     (Code
      (App (Var (Persistent (Toplevel wrap/4)) : "M.t -> ptr[M.t]")
       (Construct ((type_name M.t) (name M.A) (index 0)) () : M.t) : ptr[M.t])))
    (values (Tuple (M.B 7) (M.A)))
    |}]
;;

let%expect_test "an argument's errors are reported with its result's" =
  run
    {|
let clash : string = Ptr.make (add 1 true)
let pointee : ptr[string] = Ptr.make (add 1 true)
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon string ())
       (TCon ptr ((ATy (TVar (Link (TCon int ())))) (AMode (Var 75))))
       (Base_types_not_equal string ptr)))
     @ t.stag:2:21-42)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:37-41)
    ((Unification_error
      (Type_mismatch (TCon string ()) (TCon int ())
       (Base_types_not_equal string int)))
     @ t.stag:3:37-49)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:44-48)
    |}]
;;

let%expect_test "aliases of the allocator are polymorphic in quotes" =
  run
    {|
let mk = Ptr.make
let mk2 = mk
module M = struct let mk = Ptr.make end
let quoted = .~(.< ((mk 1).*, (mk "a").*, (mk2 true).*, (M.mk 2).*) >.)
include .~([Stag.Toplevel.let_ "erased" [] (Stag.Code.erase .< ((mk2 3).*, (M.mk "c").*) >.)])
let r = raise
exception E
let raised = .~(.< fun (b : bool) -> match b with true -> r E | false -> 1 >.)
|};
  [%expect
    {|
    val mk : 'a -> ptr['a]
    val mk2 : 'a -> ptr['a]
    module M : sig
      val mk : 'a -> ptr['a]
    end
    val quoted : int * string * bool * int
    val erased : int * string
    val r : exn -> 'a
    exception E
    val raised : bool -> int
    -- evaluated at compile time
    -- evaluated
    (mk <fun>)
    (mk2 <fun>)
    (M.mk <fun>)
    (quoted (Tuple 1 (String a) true 2))
    (erased (Tuple 3 (String c)))
    (r <fun>)
    (raised <fun>)
    |}]
;;

let%expect_test "aliases through an annotation or a shadowing module are not \
                 the allocator"
  =
  run
    {|
let annotated : int -> ptr[int] = Ptr.make
let mk = annotated
let ok = .< (mk 1, mk 2) >.
let bad = .< (mk 1, mk "a") >.
|};
  run
    {|
module Ptr = struct let make x = x end
let mk = Ptr.make
let bad = .< (mk 1, mk "a") >.
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon string ())
       (Base_types_not_equal int string)))
     @ t.stag:5:23-26)
    module Ptr : sig
      val make : 'a -> 'a
    end
    val mk : 'a -> 'a
    val bad : <int * string>{..'a}
    -- evaluated at compile time
    -- evaluated
    (Ptr.make <fun>)
    (mk <fun>)
    (bad
     (Code
      (Tuple
       ((App (Var (Persistent (Toplevel mk/5)) : "int -> int")
         (Lit (Int 1) : int) : int)
        (App (Var (Persistent (Toplevel mk/5)) : "string -> string")
         (Lit (String a) : string) : string))
       : "int * string")))
    |}]
;;

let%expect_test "cyclic aliases are not the allocator" =
  run {|
let a = b
let b = a
let q = .< (a, b) >.
|};
  [%expect {| ((Toplevel_initialization_cycle (b a b)) @ t.stag:3:8-9) |}]
;;

let%expect_test "a cycle in an argument prints without parentheses" =
  run
    {|
type chain = Link of ptr[int * chain] | End
exception E of chain
let cyc = Link (Ptr.make (1, End))
let main =
  match cyc with
  | Link q -> q.* <- (1, Link q); raise (E cyc)
  | End -> ()
|};
  [%expect
    {|
    type chain = Link of ptr[int * chain] | End
    exception E of chain
    val cyc : chain
    val main : unit
    -- evaluated at compile time
    -- evaluated
    (cyc (Link (Ptr (Tuple 1 (Link <cycle>)))))
    ("Uncaught exception E (Link (Ptr.make (1, Link <cycle>)))" @ t.stag:7:34-47)
    |}]
;;
