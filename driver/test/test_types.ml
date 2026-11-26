open! Import
open Helpers
open Source_helpers

let list_decl = "type list['a] = Nil | Cons of ptr['a * list['a]]\n"

let%expect_test "variants" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
type option['a] = None | Some of 'a
let nil = Nil
let one = Cons (Ptr.make (1, Nil))
let some x = Some x
let none = None
let nested = Some (Cons (Ptr.make (true, Nil)))
|};
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let bad = Cons (Ptr.make (1, Cons (Ptr.make (true, Nil))))
|};
  check {|
let unbound = Nil
|};
  check {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let arity = Cons
|};
  check {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let arity = Nil 1
|};
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    type option['a] = None | Some of 'a
    val nil : list['a]
    val one : list[int]
    val some : 'a -> option['a]
    val none : option['a]
    val nested : option[list[bool]]
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:3:25-57)
    val unbound : list['a]
    ((Constructor_arity_mismatch (constructor Cons) (expected 1) (got 0)) @
     t.stag:3:12-16)
    ((Constructor_arity_mismatch (constructor Nil) (expected 0) (got 1)) @
     t.stag:3:12-17)
    |}]
;;

let%expect_test "variant types are nominal" =
  check
    {|
type a = A
type b = B
let a = A
let mixed = match true with _ -> A | _ -> B
|};
  [%expect
    {| ((No_such_constructor (type_name a) (constructor B)) @ t.stag:5:42-43) |}]
;;

let%expect_test "abstract types" =
  check
    {|
type t
type box['a, {..'r}]
let id_t (x : t) = x
module M = struct
  type int
  let id_int (x : int) = x
end
let id_int (x : int) = x
|};
  check {|
type t
let id_t (x : t) = x
let bad = id_t 1
|};
  (* On the DSL: source can't write an empty match. *)
  check_program
    [ Decl.abstract "t"
    ; top_let ("absurd", [ typed "x" (Ty.con "t") ], None, match_ (var "x") [])
    ];
  check {|
type t
let bad = T
|};
  check
    {|
module M = struct
  type int
  let id_int (x : int) = x
end
let bad = M.id_int 1
|};
  [%expect
    {|
    type t
    type box['a, {..'b}]
    val id_t : t -> t
    module M : sig
      type int
      val id_int : int -> int
    end
    val id_int : int -> int
    ((Unification_error
      (Type_mismatch (TCon t ()) (TCon int ()) (Base_types_not_equal t int)))
     @ t.stag:4:15-16)
    (Unification_error
     (Type_mismatch (TVariant REmpty) (TCon t ()) Structural_mismatch))
    ((Unbound_constructor T) @ t.stag:3:10-11)
    ((Unification_error
      (Type_mismatch (TCon M.int ()) (TCon int ())
       (Base_types_not_equal M.int int)))
     @ t.stag:6:19-20)
    |}]
;;

let%expect_test "declarations are order independent and may be recursive" =
  check
    {|
let tree = Node (Cons (Ptr.make (Leaf, Nil)))
type tree = Leaf | Node of forest
type forest = list[tree]
type list['a] = Nil | Cons of ptr['a * list['a]]
|};
  check
    {|
type even = Zero | Succ_even of odd
type odd = Succ_odd of ptr[even]
let two = Succ_even (Succ_odd (Ptr.make Zero))
let bad = Succ_even Zero
|};
  [%expect
    {|
    val tree : tree
    type tree = Leaf | Node of list[tree]
    type forest = list[tree]
    type list['a] = Nil | Cons of ptr['a * list['a]]
    ((No_such_constructor (type_name odd) (constructor Zero)) @ t.stag:5:20-24)
    |}]
;;

let%expect_test "records" =
  check
    {|
type point = {x : int; y : int}
type box['a] = {contents : 'a}
let origin = {y = 0; x = 0}
let get_x p = p.x
let boxed v = {contents = v}
let unbox b = b.contents
let boxed_nil = {contents = Nil}
let unboxed = boxed_nil.contents
type list['a] = Nil | Cons of ptr['a * list['a]]
|};
  [%expect
    {|
    type point = { x : int; y : int }
    type box['a] = { contents : 'a }
    val origin : point
    val get_x : point -> int
    val boxed : 'a -> box['a]
    val unbox : box['a] -> 'a
    val boxed_nil : box[list['a]]
    val unboxed : list['a]
    type list['a] = Nil | Cons of ptr['a * list['a]]
    |}]
;;

let%expect_test "record errors" =
  check {|
type point = {x : int; y : int}
let missing = {x = 0}
|};
  check
    {|
type point = {x : int; y : int}
let duplicate = {x = 0; y = 0; x = 1}
|};
  check
    {|
type point = {x : int; y : int}
type point3 = {z : int}
let mixed = {x = 0; y = 0; z = 0}
|};
  check {|
type point = {x : int; y : int}
let unbound = {x = 0; w = 0}
|};
  check {|
type point = {x : int; y : int}
let wrong_type = {x = 0; y = true}
|};
  (* On the DSL: in source, [{}] is the empty polymorphic record. *)
  check_program
    [ Decl.record "point" [ "x", Ty.int; "y", Ty.int ]
    ; top "empty" [] (record [])
    ];
  check {|
type point = {x : int; y : int}
let unbound_field p = p.w
|};
  check {|
type point = {x : int; y : int}
let not_a_record = (1).x
|};
  check
    {|
type point = {x : int; y : int}
type point3 = {z : int}
let wrong_record p = p.x.z
|};
  [%expect
    {|
    ((Missing_field y) @ t.stag:3:14-21)
    ((Repeated_field x) @ t.stag:3:31-32)
    ((No_such_field (type_name point) (field z)) @ t.stag:4:27-28)
    ((No_such_field (type_name point) (field w)) @ t.stag:3:22-23)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:29-33)
    Empty_record
    ((Unbound_field w) @ t.stag:3:24-25)
    ((Unification_error
      (Type_mismatch (TCon point ()) (TCon int ())
       (Base_types_not_equal point int)))
     @ t.stag:3:19-22)
    ((Unification_error
      (Type_mismatch (TCon point3 ()) (TCon int ())
       (Base_types_not_equal point3 int)))
     @ t.stag:4:21-24)
    |}]
;;

let%expect_test "tuples" =
  check
    {|
let pair = (1, true)
let nested x = (x, (x, ()))
let fn = (id, add 1)
|};
  [%expect
    {|
    val pair : int * bool
    val nested : 'a -> 'a * ('a * unit)
    val fn : ('_a -> '_a) * (int -> int)
    |}]
;;

let%expect_test "match" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
type option['a] = None | Some of 'a
type point = {x : int; y : int}
let sum2 xs =
  match xs with
  | Cons * (x, Cons * (y, _)) -> add x y
  | Cons * (x, Nil) -> x
  | Nil -> 0
let is_zero n = match n with 0 -> true | _ -> false
let fst p = match p with (a, _) -> a
let get_y p = match p with {y = y} -> y
let get_opt o default = match o with Some x -> x | None -> default
let shadows x = match 1 with x -> x
let nested_record o = match o with Some {x = a; y = 2} -> a
|};
  (* On the DSL: source can't write an empty match. *)
  check_program [ top "no_cases" [ "x" ] (match_ (var "x") []) ];
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    type option['a] = None | Some of 'a
    type point = { x : int; y : int }
    val sum2 : list[int] -> int
    val is_zero : int -> bool
    val fst : 'a * 'b -> 'a
    val get_y : point -> int
    val get_opt : option['a] -> 'a -> 'a
    val shadows : 'a -> int
    val nested_record : option[point] -> int
    val no_cases : [] -> 'a
    |}]
;;

let%expect_test "match errors" =
  check
    {|
type option['a] = None | Some of 'a
let arm_types o = match o with Some _ -> 1 | None -> true
|};
  check
    {|
type option['a] = None | Some of 'a
let pattern_types o = match o with Some _ -> 1 | 1 -> 2
|};
  check {|
let duplicate = match (1, 2) with (x, x) -> x
|};
  check
    {|
type option['a] = None | Some of 'a
let nested_duplicate = match (1, None) with (x, Some x) -> x
|};
  check {|
let same_name_other_arms = match 1 with x -> x | x -> x
|};
  check
    {|
type option['a] = None | Some of 'a
let arity = match 1 with Some -> 1
|};
  check {|
let unbound = match 1 with Some -> 1
|};
  check
    {|
type point = {x : int; y : int}
let duplicate_field p = match p with {x = _; x = _} -> 1
|};
  check
    {|
type point = {x : int; y : int}
type box['a] = {contents : 'a}
let mixed_fields p = match p with {x = _; contents = _} -> 1
|};
  (* On the DSL: in source, [{}] is the empty polymorphic record pattern. *)
  check_program
    [ top "empty_record" [ "p" ] (match_ (var "p") [ Pat.record [], int 1 ]) ];
  check {|
let tuple_length = match (1, 2) with (_, _, _) -> 1
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:53-57)
    ((Unification_error
      (Type_mismatch (TCon option ((ATy (TVar (Link (TVar (Unbound 28 1)))))))
       (TCon int ()) (Base_types_not_equal option int)))
     @ t.stag:3:49-50)
    ((Duplicate_pattern_variable x) @ t.stag:2:38-39)
    ((Duplicate_pattern_variable x) @ t.stag:3:53-54)
    val same_name_other_arms : int
    ((Constructor_arity_mismatch (constructor Some) (expected 1) (got 0)) @
     t.stag:3:25-29)
    ((Constructor_arity_mismatch (constructor prelude.Some) (expected 1) (got 0))
     @ t.stag:2:27-31)
    ((Repeated_field x) @ t.stag:3:45-46)
    ((No_such_field (type_name point) (field contents)) @ t.stag:4:42-50)
    Empty_record
    ((Unification_error
      (Type_mismatch (TTuple ((TCon int ()) (TCon int ())))
       (TTuple
        ((TVar (Link (TVar (Unbound 27 1)))) (TVar (Link (TVar (Unbound 28 1))))
         (TVar (Link (TVar (Unbound 29 1))))))
       (Args_length_mismatch 2 3)))
     @ t.stag:2:37-46)
    |}]
;;

let%expect_test "declaration errors" =
  check {|
type t = A
type t = {x : int}
|};
  check {|
type t['a, {..'a}] = |
|};
  check {|
type t = A | A of int
|};
  check {|
type t = A
type u = A
|};
  check {|
type t = {x : int; x : bool}
|};
  check {|
type point = {x : int; y : int}
type u = {x : int}
|};
  check {|
type t = A of 'a
|};
  check {|
type t['a] = A of <int>{..'a}
|};
  check {|
type t[{..'r}] = A of 'r
|};
  check {|
type t = A of nope
|};
  check {|
type t = nope
|};
  [%expect
    {|
    ((Duplicate_type t) @ t.stag:3:5-6)
    ((Duplicate_type_parameter (type_name t) (param a)) @ t.stag:2:11-17)
    ((Duplicate_constructor A) @ t.stag:2:13-14)
    ((Duplicate_constructor A) @ t.stag:3:9-10)
    ((Duplicate_field x) @ t.stag:2:19-20)
    ((Duplicate_field x) @ t.stag:3:10-11)
    ((Unbound_type_variable a) @ t.stag:2:14-16)
    ((Type_variable_kind_mismatch a) @ t.stag:2:26-28)
    ((Type_variable_kind_mismatch r) @ t.stag:2:22-24)
    ((Unbound_type nope) @ t.stag:2:14-18)
    ((Unbound_type nope) @ t.stag:2:9-13)
    |}]
;;

let%expect_test "duplicate context entries" =
  check {|
let f c : <int>{x : int; x : bool} = c
|};
  check {|
let f c = (c : <int>{x : int; x : bool})
|};
  check {|
type t = A of <int>{x : int; x : bool}
|};
  check {|
type t = <int>{x : int; x : bool}
|};
  check
    {|
type code[{..'r}] = <int>{..'r}
let f c : code[{`x : int; `x : int; ..'r}] = c
|};
  check {|
let nested c : <<int>{x : int}>{x : bool} = c
|};
  [%expect
    {|
    ((Duplicate_row_entry x) @ t.stag:2:25-26)
    ((Duplicate_row_entry x) @ t.stag:2:30-31)
    ((Duplicate_row_entry x) @ t.stag:2:29-30)
    ((Duplicate_row_entry x) @ t.stag:2:24-25)
    ((Duplicate_row_entry x) @ t.stag:3:26-28)
    val nested : <<int>{x : mut?'a int}>{x : mut?'b bool} -> <<int>{x : mut?'a int}>{x : mut?'b bool}
    |}]
;;

let%expect_test "arity and kind of type arguments" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
type t = A of list
|};
  check {|
type t = A of int[int]
|};
  check {|
let f x : bool[int] = x
|};
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
type t = A of list[{}]
|};
  check {|
type stmt[{..'r}] = Stmt of <unit>{..'r}
type t = A of stmt[int]
|};
  check {|
let f x : list[int] = x
|};
  check {|
let f x : M.t = x
|};
  [%expect
    {|
    ((Type_arity_mismatch (type_name list) (expected 1) (got 0)) @
     t.stag:3:14-18)
    ((Type_arity_mismatch (type_name int) (expected 0) (got 1)) @ t.stag:2:14-22)
    ((Type_arity_mismatch (type_name bool) (expected 0) (got 1)) @
     t.stag:2:10-19)
    type list['a] = Nil | Cons of ptr['a * list['a]]
    type t = A of list[{}]
    ((Type_arg_kind_mismatch (type_name stmt) (param (Row r))) @ t.stag:3:19-22)
    val f : list[int] -> list[int]
    ((Unbound_module M) @ t.stag:2:10-11)
    |}]
;;

let%expect_test "aliases" =
  (* [A (1, 2)] would give [A] two arguments, so the tuple is bound first. *)
  check
    {|
type pair['a] = 'a * 'a
type code[{..'r}] = <int>{..'r}
type int_pair = pair[int]
type unused['a] = bool
type t = A of int_pair
let f p : int_pair = p
let a = let p = (1, 2) in A p
let g c : code[{`x : int; ..'r}] = c
let h : unused[int] = true
|};
  check {|
type pair['a] = 'a * 'a
let bad : pair[int] = (1, true)
|};
  [%expect
    {|
    type pair['a] = 'a * 'a
    type code[{..'a}] = <int>{..'a}
    type int_pair = int * int
    type unused['a] = bool
    type t = A of (int * int)
    val f : int * int -> int * int
    val a : t
    val g : <int>{x : mut?'a int; ..'b} -> <int>{x : mut?'a int; ..'b}
    val h : bool
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:22-31)
    |}]
;;

let%expect_test "cyclic aliases" =
  check {|
type t = t
|};
  check {|
type t = u
type u = int -> t
|};
  check {|
type list['a] = Nil | Cons of ptr['a * list['a]]
type t = list[t]
|};
  check {|
type t = <int>{x : t}
|};
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
type t = list[u]
type u = U of t
let ok = U Nil
|};
  [%expect
    {|
    ((Cyclic_type_alias t) @ t.stag:2:5-6)
    ((Infinite_type_layout t) @ t.stag:2:5-6)
    type list['a] = Nil | Cons of ptr['a * list['a]]
    type t = list[t]
    ((Infinite_type_layout t) @ t.stag:2:5-6)
    type list['a] = Nil | Cons of ptr['a * list['a]]
    type t = list[u]
    type u = U of list[u]
    val ok : u
    |}]
;;

let%expect_test "annotation variables are shared within a toplevel" =
  check
    {|
let annotated x y : 'a = let z : 'a = (x : 'a) in y
let other_toplevel x = (x : 'a); add x 1
let independent x y = (x : 'a); (y : 'b)
let context c d = (c : <int>{x : int; ..'r}); (d : <int>{..'r})
let ty_and_context_named_alike c = (c : <'r>{..'r})
|};
  [%expect
    {|
    val annotated : 'a -> 'a -> 'a
    val other_toplevel : int -> int
    val independent : 'a -> 'b -> 'b
    val context : <int>{x : int; ..'a} -> <int>{..'a} -> <int>{..'a}
    val ty_and_context_named_alike : <'a>{..'b} -> <'a>{..'b}
    |}]
;;

let%expect_test "annotation variables are not generalized by inner lets" =
  check {|
let f = let g y = (y : 'a) in g 1; g true
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:37-41)
    |}]
;;

let%expect_test "context parameters" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
type stmt[{..'r}] = Stmt of <unit>{..'r} * list[stmt[{..'r}]]
let single c = Stmt (c, Nil)
let uses_x = Stmt (.<x; ()>., Nil)
let nested = Stmt (.<add x 1; ()>., Cons (Ptr.make (uses_x, Nil)))
let annotated s : stmt[{`y : bool; ..'r}] = s
let body s = match s with Stmt (c, _) -> c
|};
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    type stmt[{..'a}] = Stmt of <unit>{..'a} * list[stmt[{..'a}]]
    val single : <unit>{..'a} -> stmt[{..'a}]
    val uses_x : stmt[{`x : 'a; ..'b}]
    val nested : stmt[{`x : int; ..'_a}]
    val annotated : stmt[{`y : bool; ..'a}] -> stmt[{`y : bool; ..'a}]
    val body : stmt[{..'a}] -> <unit>{..'a}
    |}]
;;

let%expect_test "patterns in quotes are captured by splices" =
  check
    {|
let captures c = .<match 1 with x -> .~c>.
let applied = captures .<add x 1>.
let local = .<match (1, true) with (x, y) -> add x 1>.
let bound_outside = match 1 with x -> .<x>.
|};
  check
    {|
let wrong_type = captures .<cond x 1 2>.
let captures c = .<match 1 with x -> .~c>.
|};
  check {|
let level = .<match 1 with x -> .~x>.
|};
  [%expect
    {|
    val captures : <'a>{x?'b : immut int; ..'c} -> <'a>{..'d} where ..'c <= {..'d}
    val applied : <int>{..'_a}
    val local : <int>{..'a}
    val bound_outside : <int>{..'a}
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:26-40)
    ((Variable_used_at_wrong_level (name x) (var_level 1) (used_at 0)) @
     t.stag:2:34-35)
    |}]
;;

let%expect_test "constructors and fields are not free variables of quotes" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
type point = {x : int; y : int}
let closed = .<Cons (Ptr.make (1, Nil))>.
let open = .<Cons (Ptr.make (y, Nil))>.
let record = .<{x = 1; y = y}.x>.
let pattern = .<match l with Cons * (h, _) -> h | Nil -> 0>.
let in_splice = .<.~(.<Nil>.)>.
|};
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    type point = { x : int; y : int }
    val closed : <list[int]>{..'a}
    val open : <list['a]>{y : 'a; ..'b}
    val record : <int>{y : int; ..'a}
    val pattern : <int>{l : list[int]; ..'a}
    val in_splice : <list['a]>{..'b}
    |}]
;;

let%expect_test "generalization of syntactic values" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
type box['a] = {contents : 'a}
let nil = Nil
let uses = (Cons (Ptr.make (1, nil)), Cons (Ptr.make (true, nil)))
let pair = (Nil, id)
let boxed = {contents = Nil}
let contents = boxed.contents
let matched = match nil with Nil -> Nil | _ -> nil
let local = let a = Nil in Cons (Ptr.make (a, a))
let local_fn = let f x = x in f
let quoted = .<Nil>.
let weak_app = id id
let weak_cons = Cons (Ptr.make (id id, Nil))
let weak_let = let a = id Nil in a
let weak_match = match id Nil with _ -> Nil
let weak_arm = match Nil with _ -> id Nil
let weak_field = (id boxed).contents
let spliced_value = .~(.<Nil>.)
|};
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    type box['a] = { contents : 'a }
    val nil : list['a]
    val uses : list[int] * list[bool]
    val pair : list['a] * ('b -> 'b)
    val boxed : box[list['a]]
    val contents : list['a]
    val matched : list['a]
    val local : list[list['_a]]
    val local_fn : 'a -> 'a
    val quoted : <list['a]>{..'b}
    val weak_app : '_a -> '_a
    val weak_cons : list['_a -> '_a]
    val weak_let : list['_a]
    val weak_match : list['_a]
    val weak_arm : list['_a]
    val weak_field : list['_a]
    val spliced_value : list['a]
    |}]
;;

let%expect_test "local generalization of syntactic values" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let poly = let n = Nil in (Cons (Ptr.make (1, n)), Cons (Ptr.make (true, n)))
|};
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let mono = let n = id Nil in (Cons (Ptr.make (1, n)), Cons (Ptr.make (true, n)))
|};
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    val poly : list[int] * list[bool]
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:3:69-78)
    |}]
;;

let%expect_test "generalized values are not persistent" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let nil = Nil
let f x = x
let uses_nil = .<nil>.
let uses_f = .<f>.
|};
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    val nil : list['a]
    val f : 'a -> 'a
    val uses_nil : <list['a]>{..'b}
    val uses_f : <'a -> 'a>{..'b}
    |}]
;;

let%expect_test "SCCs are generalized iff all members are values" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let nil = Nil
let f x = g x
let g x = nil; f x
|};
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let a x = b
let b = a; id Nil
|};
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    val nil : list['a]
    val f : 'a -> 'b
    val g : 'a -> 'b
    type list['a] = Nil | Cons of ptr['a * list['a]]
    val a : '_a -> list['_b]
    val b : list['_a]
    |}]
;;

let%expect_test "recursive SCCs with a non-function are not generalized" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let a x = b
let b = a; Nil
let quoted = .<a 1>.
|};
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let xs = Cons (Ptr.make (1, ys))
let ys = Cons (Ptr.make (2, xs))
|};
  check {|
let x = x
|};
  check {|
let x = y
let y = x
|};
  check {|
module M = struct let x = M.x end
|};
  check {|
let x = x
let use = x 1; x true
|};
  (* Without initialization cycles. *)
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let xs = fun u -> Cons (Ptr.make (1, ys u))
let ys = fun u -> Cons (Ptr.make (2, xs u))
|};
  check
    {|
let x = fun u -> y u
let y = fun u -> x u
module M = struct let x = fun u -> M.x u end
|};
  check {|
let x = fun u -> x u
let use = x 1; x true
|};
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    val a : int -> list['_a]
    val b : list['_a]
    val quoted : <list['_a]>{..'b}
    ((Toplevel_initialization_cycle (ys xs ys)) @ t.stag:4:28-30)
    ((Toplevel_initialization_cycle (x x)) @ t.stag:2:8-9)
    ((Toplevel_initialization_cycle (y x y)) @ t.stag:3:8-9)
    ((Toplevel_initialization_cycle (M.x M.x)) @ t.stag:2:26-29)
    ((Toplevel_initialization_cycle (x x)) @ t.stag:2:8-9)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:17-21)
    type list['a] = Nil | Cons of ptr['a * list['a]]
    val xs : '_a -> list[int]
    val ys : '_a -> list[int]
    val x : '_a -> '_b
    val y : '_a -> '_b
    module M : sig
      val x : '_a -> '_b
    end
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:17-21)
    |}]
;;

let%expect_test "recursive local non-functions are not generalized" =
  let print_local equal =
    Option.iter
      (typecheck (list_decl ^ "let local = let x = " ^ equal ^ " in (x, x)\n"))
      ~f:(fun t ->
        print_string
          (Signature.to_string_hum [ List.last_exn (checked t).signature ]))
  in
  (* Using [x]'s value in computing it is an error. *)
  print_local "x";
  print_local "x; Nil";
  print_local "match x with _ -> Nil";
  (* The splices' providers are of the quote's level, so are never the binder. *)
  print_local ".<.~(.<Nil>.)>.";
  print_local ".<.~(forget x in .<x>.)>.";
  print_local "Nil";
  print_local "let x = Nil in x";
  print_local "match Nil with x -> x";
  print_local "let g x = x in g; Nil";
  print_local ".<.~(.<Nil>.).{y}>.";
  print_local ".<forget x in .~(.<Nil>.).{x}>.";
  [%expect
    {|
    ((Used_before_defined (name x) (shadows false)) @ t.stag:2:20-21)
    ((Used_before_defined (name x) (shadows false)) @ t.stag:2:20-21)
    ((Used_before_defined (name x) (shadows false)) @ t.stag:2:26-27)
    val local : <list['a]>{..'b} * <list['c]>{..'d}
    val local : <'a>{x : 'a; ..'b} * <'c>{x : 'c; ..'d}
    val local : list['a] * list['b]
    val local : list['a] * list['b]
    val local : list['a] * list['b]
    val local : list['a] * list['b]
    val local : <list['a]>{y : 'b; ..'c} * <list['d]>{y : 'e; ..'f}
    val local : <list['a]>{x : 'b; ..'c} * <list['d]>{x : 'e; ..'f}
    |}]
;;

let%expect_test "quotes are values unless their level 0 splices are not" =
  check
    {|
type list['a] = Nil | Cons of ptr['a * list['a]]
let no_splice = .<Nil>.
let value_splice = .<.~(.<Nil>.)>.
let app_splice = .<.~(id .<Nil>.)>.
let nested_app_splice = .<.<.~(id .<Nil>.)>.>.
let escaping_app_splice = .<.<.~(.~(id .<.<Nil>.>.))>.>.
let substitution = .<.~(.<x>.).{x = id Nil}>.
|};
  [%expect
    {|
    type list['a] = Nil | Cons of ptr['a * list['a]]
    val no_splice : <list['a]>{..'b}
    val value_splice : <list['a]>{..'b}
    val app_splice : <list['_a]>{..'_b}
    val nested_app_splice : <<list['a]>{..'b}>{..'c}
    val escaping_app_splice : <<list['_a]>{..'_b}>{..'_c}
    val substitution : <list['a]>{..'b}
    |}]
;;

let%expect_test "a record pattern may end with [_]" =
  check
    {|
type r = {a : int; b : string; c : bool}
let f x = match x with {a = n; _} -> n
let g (x : r) = match x with {b = s} -> s
let h x = match x with {a = 1; c = true; _} -> 1 | {a = n; _} -> n
|};
  [%expect
    {|
    type r = { a : int; b : string; c : bool }
    val f : r -> int
    val g : r -> string
    val h : r -> int
    |}]
;;
