open! Import
open Source_helpers

let%expect_test "items mirror the program" =
  check
    ~prelude:false
    {|
let first = M.N.Leaf
type list['a] = Nil | Cons of ptr['a * list['a]]
type stmt[{..'r}, 'a] = Stmt of <'a>{..'r}
type point = {x : int; y : list[int]}
type pair['b, 'a] = 'a * 'b
type void = |
type args = Arrow of (int -> int) | Tuple of (int * bool) | Two of int * (int -> int)
module M = struct
  module Empty = struct end
  module N = struct
    type tree = Leaf | Node of ptr[tree]
  end
  let id x = x
  let code c = .<.~c>.
  type t = pair[int, bool]
end
let weak = M.id M.id
let code c = .<.~c>.
|};
  [%expect
    {|
    val first : M.N.tree
    type list['a] = Nil | Cons of ptr['a * list['a]]
    type stmt[{..'a}, 'b] = Stmt of <'b>{..'a}
    type point = { x : int; y : list[int] }
    type pair['a, 'b] = 'b * 'a
    type void = |
    type args = Arrow of (int -> int) | Tuple of (int * bool) | Two of int * (int -> int)
    module M : sig
      module Empty : sig end
      module N : sig
        type tree = Leaf | Node of ptr[tree]
      end
      val id : 'a -> 'a
      val code : <'a>{..'b} -> <'a>{..'c} where ..'b <= {..'c}
      type t = bool * int
    end
    val weak : '_a -> '_a
    val code : <'a>{..'b} -> <'a>{..'c} where ..'b <= {..'c}
    |}]
;;

let%expect_test "variables are named per item" =
  check
    ~prelude:false
    {|
let f x y = y
type t['z] = T of 'z
let weak = f 1
let weak_too = f true
|};
  [%expect
    {|
    val f : 'a -> 'b -> 'b
    type t['a] = T of 'a
    val weak : '_a -> '_a
    val weak_too : '_a -> '_a
    |}]
;;

let%expect_test "builtin types are not items" =
  check ~prelude:false "";
  [%expect {| |}]
;;

let%expect_test "types are named relative to the item's module" =
  check
    ~prelude:false
    {|
type r = R
module M = struct
  type m = Mk of r * N.n
  module N = struct
    type n = Nn of ptr[n * m] * r * P.p
  end
  module P = struct
    type p = Pp
  end
  type a = N.n -> P.p
end
module Sibling = struct
  type b = M.N.n
end
type c = M.N.n * M.m * r
|};
  [%expect
    {|
    type r = R
    module M : sig
      type m = Mk of r * N.n
      module N : sig
        type n = Nn of ptr[n * m] * r * P.p
      end
      module P : sig
        type p = Pp
      end
      type a = N.n -> P.p
    end
    module Sibling : sig
      type b = M.N.n
    end
    type c = M.N.n * M.m * r
    |}]
;;

let%expect_test "params, bounds and every part of an item are relative" =
  check
    ~prelude:false
    {|
module M = struct
  type t['a] = T of 'a
  type v['a] = V of t['a]
  type f = {x : t[int]}
  let wrap x = T x
  let use (x : t[int]) y = y
  let code c = .<use free .~c>.
end
|};
  [%expect
    {|
    module M : sig
      type t['a] = T of 'a
      type v['a] = V of t['a]
      type f = { x : t[int] }
      val wrap : 'a -> t['a]
      val use : t[int] -> 'a -> 'a
      val code : <'a>{..'b} -> <'a>{free : mut?'c t[int]; ..'d} where ..'b <= {free : mut?'c t[int]; ..'d}
    end
    |}]
;;

let%expect_test "a shadowed root type is written after a leading [.]" =
  check
    ~prelude:false
    {|
type t = Root
module N = struct
  type n = Outer
end
let root_outer = N.Outer
module M = struct
  type t = Inner
  module N = struct
    type u = U
  end
  let root = Root
  let inner = Inner
  let outer = root_outer
  module Deeper = struct
    let root = Root
  end
end
let root = Root
|};
  [%expect
    {|
    type t = Root
    module N : sig
      type n = Outer
    end
    val root_outer : N.n
    module M : sig
      type t = Inner
      module N : sig
        type u = U
      end
      val root : .t
      val inner : t
      val outer : .N.n
      module Deeper : sig
        val root : .t
      end
    end
    val root : t
    |}]
;;

let%expect_test "a shadowed module makes a longer path or [.] needed" =
  check
    ~prelude:false
    {|
module A = struct
  module N = struct
    type tree = Leaf
  end
  let leaf = N.Leaf
  module P = struct
    module N = struct
      type tree = P_leaf
    end
    let outer = leaf
    let inner = N.P_leaf
  end
end
module M = struct
  module N = struct
    type tree = Leaf
  end
  let leaf = N.Leaf
  module M = struct
    module N = struct
      type tree = M_leaf
    end
    let outer = leaf
    let inner = N.M_leaf
  end
end
|};
  [%expect
    {|
    module A : sig
      module N : sig
        type tree = Leaf
      end
      val leaf : N.tree
      module P : sig
        module N : sig
          type tree = P_leaf
        end
        val outer : A.N.tree
        val inner : N.tree
      end
    end
    module M : sig
      module N : sig
        type tree = Leaf
      end
      val leaf : N.tree
      module M : sig
        module N : sig
          type tree = M_leaf
        end
        val outer : .M.N.tree
        val inner : N.tree
      end
    end
    |}]
;;

let%expect_test "a type shadowing a builtin" =
  check
    ~prelude:false
    {|
let root = 1
module M = struct
  type int = I
  let mine = I
  let builtin = 1
  module N = struct
    let mine = I
    let builtin = 1
  end
end
let mine = M.I
|};
  [%expect
    {|
    val root : int
    module M : sig
      type int = I
      val mine : int
      val builtin : .int
      module N : sig
        val mine : int
        val builtin : .int
      end
    end
    val mine : M.int
    |}]
;;

let%expect_test "sexp" =
  Option.iter
    (typecheck
       ~prelude:false
       {|
type box['a] = {contents : 'a}
module M = struct
  let f x = {contents = x}
  let n = 1
end
|})
    ~f:(fun t -> print_s [%sexp ((checked t).signature : Signature.t)]);
  [%expect
    {|
    ((Type
      ((name box) (params ((ATy (TVar (Generic 1)))))
       (kind (Record (((name contents) (ty (TVar (Generic 1)))))))))
     (Module (name M)
      (items
       ((Value
         ((name f)
          (scheme
           (TArrow (TVar (Generic 3))
            (TVar (Link (TCon box ((ATy (TVar (Link (TVar (Generic 3)))))))))))
          (stage Const)))
        (Value ((name n) (scheme (TVar (Link (TCon int ())))) (stage Const)))))))
    |}]
;;
