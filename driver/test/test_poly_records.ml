open! Import
open Source_helpers

let%expect_test "record operations" =
  check
    {|
let lit = {`x = 1; `y = true}
let empty = {}
let get_x r = r.`x
let extend r = {`z = 1 | r}
let extend_many r a = {`y = a; `z = 1 | r}
let restrict r = r \ `x
let update r = {r with `x = true}
let nested r = r.`a.`b
let lit_x = {`x = 1; `y = 2}.`x
let extend_lit = {`z = 'c' | {`x = 1}}
let restrict_lit = {`x = 1; `y = 2} \ `x
let two_fields r = add r.`x r.`y
|};
  [%expect
    {|
    val lit : {`x : int; `y : bool}
    val empty : {}
    val get_x : {`x : 'a; ..'b} -> 'a
    val extend : {..'a} -> {`z : int; ..'a}
    val extend_many : {..'a} -> 'b -> {`y : 'b; `z : int; ..'a}
    val restrict : {`x : 'a; ..'b} -> {..'b}
    val update : {`x : 'a; ..'b} -> {`x : bool; ..'b}
    val nested : {`a : {`b : 'a; ..'b}; ..'c} -> 'a
    val lit_x : int
    val extend_lit : {`x : int; `z : char}
    val restrict_lit : {`y : int}
    val two_fields : {`x : int; `y : int; ..'a} -> int
    |}]
;;

let%expect_test "extension shadows and restriction reveals" =
  check
    {|
let shadow = {`x = true | {`x = 1}}
let shadow_x = {`x = true | {`x = 1}}.`x
let revealed = ({`x = true | {`x = 1}} \ `x).`x
let update_shadow = {{`x = true | {`x = 1}} with `x = 'c'}
let update_shadow_inner = ({{`x = true | {`x = 1}} with `x = 'c'} \ `x).`x
let restrict_twice = {`x = true | {`x = 1}} \ `x \ `x
let extend_restrict r = {`x = 1 | r} \ `x
let extend_get r = {`x = 1 | r}.`x
let reveal r = ({`x = 1 | r} \ `x).`x
|};
  [%expect
    {|
    val shadow : {`x : bool; `x : int}
    val shadow_x : bool
    val revealed : int
    val update_shadow : {`x : char; `x : int}
    val update_shadow_inner : int
    val restrict_twice : {}
    val extend_restrict : {..'a} -> {..'a}
    val extend_get : {..'a} -> int
    val reveal : {`x : 'a; ..'b} -> 'a
    |}]
;;

let%expect_test "update, restriction and access require the field" =
  check
    {|
let update_missing = {{`y = 1} with `x = 1}
|};
  check
    {|
let restrict_missing = {`y = 1} \ `x
|};
  check
    {|
let access_missing = {}.`x
|};
  check
    {|
let get_y r = r.`y
let closed = get_y {`x = 1}
|};
  check
    {|
let restrict_all r = ({`x = 1 | {}} \ `x).`x
|};
  [%expect
    {|
    ((Unification_error
      (Row_mismatch
       (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1)))
       (RExtend y Present (TCon int ()) REmpty) (Missing_label x)))
     @ t.stag:2:22-30)
    ((Unification_error
      (Row_mismatch
       (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1)))
       (RExtend y Present (TCon int ()) REmpty) (Missing_label x)))
     @ t.stag:2:23-31)
    ((Unification_error
      (Row_mismatch
       (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))) REmpty
       (Missing_label x)))
     @ t.stag:2:21-23)
    ((Unification_error
      (Row_mismatch
       (RExtend y Present (TVar (Unbound 27 1)) (RVar (Unbound 28 1)))
       (RExtend x Present (TCon int ()) REmpty) (Missing_label y)))
     @ t.stag:3:19-27)
    ((Unification_error
      (Row_mismatch
       (RExtend x Present (TVar (Unbound 19 1)) (RVar (Unbound 20 1))) REmpty
       (Missing_label x)))
     @ t.stag:2:21-41)
    |}]
;;

let%expect_test "update may change field types" =
  check
    {|
let changed = {{`x = 1; `y = 2; `z = 3} with `x = true; `y = "s"}
let update_fn r v = {r with `x = v}
|};
  [%expect
    {|
    val changed : {`x : bool; `y : string; `z : int}
    val update_fn : {`x : 'a; ..'b} -> 'c -> {`x : 'c; ..'b}
    |}]
;;

let%expect_test "duplicate labels" =
  check
    {|
let literal = {`x = 1; `y = 2; `x = 2}
|};
  check
    {|
let extension r = {`x = 1; `x = 2 | r}
|};
  check
    {|
let update r = {r with `x = 1; `x = 2}
|};
  check
    {|
let pattern r = match r with {`x = a; `x = b} -> a
|};
  check
    {|
let annotation (r : {`x : int; `x : bool}) = r
|};
  check
    {|
type t = {`x : int; `x : int; ..'r}
|};
  [%expect
    {|
    ((Duplicate_row_entry x) @ t.stag:2:31-33)
    ((Duplicate_row_entry x) @ t.stag:2:27-29)
    ((Duplicate_row_entry x) @ t.stag:2:31-33)
    ((Duplicate_row_entry x) @ t.stag:2:38-40)
    ((Duplicate_row_entry x) @ t.stag:2:31-33)
    ((Duplicate_row_entry x) @ t.stag:2:20-22)
    |}]
;;

let%expect_test "record patterns" =
  check
    {|
let closed r = match r with {`x = a; `y = b} -> add a b
let open r = match r with {`x = a; ..} -> a
let rest r = match r with {`x = a; ..rest} -> (a, rest)
let rest_reveals_shadowed =
  match {`x = true | {`x = 1}} with {`x = a; ..rest} -> (a, rest.`x)
let nested r = match r with {`p = {`x = a; ..}; ..} -> a
let empty r = match r with {} -> 1
let with_literals r = match r with {`x = 1; ..} -> true | _ -> false
|};
  check
    {|
let closed_mismatch = match {`x = 1; `y = 2} with {`x = a} -> a
|};
  check
    {|
let duplicate_rest r = match r with {`x = a; ..a} -> a
|};
  [%expect
    {|
    val closed : {`x : int; `y : int} -> int
    val open : {`x : 'a; ..'b} -> 'a
    val rest : {`x : 'a; ..'b} -> 'a * {..'b}
    val rest_reveals_shadowed : bool * int
    val nested : {`p : {`x : 'a; ..'b}; ..'c} -> 'a
    val empty : {} -> int
    val with_literals : {`x : int; ..'a} -> bool
    ((Unification_error
      (Row_mismatch (RExtend y Present (TCon int ()) REmpty) REmpty
       (Missing_label y)))
     @ t.stag:2:50-58)
    ((Duplicate_pattern_variable a) @ t.stag:2:47-48)
    |}]
;;

let%expect_test "aliases" =
  check
    {|
type point = {`x : int; `y : int}
type with_x[{..'r}] = {`x : int; ..'r}
type box['a] = {`contents : 'a}
type holder = {p : point}
let origin : point = {`x = 0; `y = 0}
let get (b : box[int]) = b.`contents
let x_of (r : with_x[{..'r}]) = r.`x
let x_of_origin = x_of origin
let hold = {p = origin}
module M = struct
  type t = A
  type r = {`f : t}
  type v[{..'r}] = [`A of t | `B of unit; ..'r]
end
let m r = (r : M.r)
|};
  check
    {|
let not_a_point : point = {`x = 0}
type point = {`x : int; `y : int}
|};
  [%expect
    {|
    type point = {`x : int; `y : int}
    type with_x[{..'a}] = {`x : int; ..'a}
    type box['a] = {`contents : 'a}
    type holder = { p : {`x : int; `y : int} }
    val origin : {`x : int; `y : int}
    val get : {`contents : int} -> int
    val x_of : {`x : int; ..'a} -> int
    val x_of_origin : int
    val hold : holder
    module M : sig
      type t = A
      type r = {`f : t}
      type v[{..'a}] = [`A of t | `B; ..'a]
    end
    val m : {`f : M.t} -> {`f : M.t}
    ((Unification_error
      (Row_mismatch (RExtend y Present (TCon int ()) REmpty) REmpty
       (Missing_label y)))
     @ t.stag:2:26-34)
    |}]
;;

let%expect_test "generalization and the value restriction" =
  check
    {|
let r = {`f = id}
let weak = {`f = id id}
let ext = {`g = id | r}
let ext_weak = {`g = id | id r}
let field = r.`f
let restricted = ext \ `g
let updated = {r with `f = {}}
let used_twice = (r.`f 1, r.`f true)
let local = let p = {`f = id} in (p.`f 1, p.`f true)
|};
  [%expect
    {|
    val r : {`f : 'a -> 'a}
    val weak : {`f : '_a -> '_a}
    val ext : {`f : 'a -> 'a; `g : 'b -> 'b}
    val ext_weak : {`f : '_a -> '_a; `g : '_b -> '_b}
    val field : 'a -> 'a
    val restricted : {`f : 'a -> 'a}
    val updated : {`f : {}}
    val used_twice : int * bool
    val local : int * bool
    |}]
;;

let%expect_test "values through the new forms" =
  check
    {|
let recursive = let p = {`a = id; `b = p.`a} in p
let rest_binds = let y = match {`x = id} with {`x = _; ..y} -> (y, id) in y
let tag_binds = let y = match `A id with `A y -> y in y
let splice_value = .<{`f = .~.<id>.}>.
let splice_not_value = .<{`f = .~(id .<id>.) | {}}>.
|};
  [%expect
    {|
    val recursive : {`a : '_a -> '_a; `b : '_a -> '_a}
    val rest_binds : {} * ('a -> 'a)
    val tag_binds : 'a -> 'a
    val splice_value : <{`f : 'a -> 'a}>{..'b}
    val splice_not_value : <{`f : '_a -> '_a}>{..'_b}
    |}]
;;

let%expect_test "labels in quotes are not free variables" =
  check
    {|
let label = .<{`x = 1}.`x>.
let var_and_label = .<{`y = y}>.
let outside = let r = {`x = 1} in .<r.`x>.
let extend_inside c = .<{`z = 1 | .~c} \ `z>.
let pattern_rest = .<match r with {`x = x; ..rest} -> (x, rest)>.
|};
  [%expect
    {|
    val label : <int>{..'a}
    val var_and_label : <{`y : 'a}>{y : 'a; ..'b}
    val outside : <'a>{r : {`x : 'a; ..'b}; ..'c}
    val extend_inside : <{..'a}>{..'b} -> <{..'a}>{..'c} where ..'b <= {..'c}
    val pattern_rest : <'a * {..'b}>{r : {`x : 'a; ..'b}; ..'c}
    |}]
;;

let%expect_test "rows shared between contexts and records" =
  check
    {|
let same_row (c : <int>{..'r}) (r : {..'r}) = c
let ok = same_row .<x>. {`x = 1}
let captured c (r : {..'r}) = .<let y = 1 in .~(c : <int>{..'r})>.; r
|};
  check
    {|
let same_row (c : <int>{..'r}) (r : {..'r}) = c
let missing = same_row .<x>. {}
|};
  [%expect
    {|
    val same_row : <int>{..'a} -> {..'a} -> <int>{..'a}
    val ok : <int>{x : immut int}
    val captured : <int>{y?'a : immut int; ..'b} -> {`y?'a : int; ..'b} -> {`y?'a : int; ..'b}
    ((Unification_error
      (Row_mismatch
       (RExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
        (RVar (Unbound 34 1)))
       REmpty (Missing_label x)))
     @ t.stag:3:29-31)
    |}]
;;

let%expect_test "unification errors" =
  check
    {|
let record_vs_variant = (`A).`x
|};
  check
    {|
let record_vs_int = 1.`x
|};
  check
    {|
let record_vs_code = .<1>. \ `x
|};
  check
    {|
let field_type = add {`x = true}.`x 1
|};
  check
    {|
let different_rows r =
  ((match r with {`x = _} -> 1), (match r with {`y = _} -> 1))
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch
       (TRecord (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))))
       (TVariant (RExtend A Present (TCon unit ()) (RVar (Unbound 21 1))))
       Structural_mismatch))
     @ t.stag:2:24-28)
    ((Unification_error
      (Type_mismatch
       (TRecord (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))))
       (TCon int ()) Structural_mismatch))
     @ t.stag:2:20-21)
    ((Unification_error
      (Type_mismatch
       (TRecord (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))))
       (TCode (inner (TCon int ())) (context (RVar (Unbound 21 1))))
       Structural_mismatch))
     @ t.stag:2:21-26)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:21-35)
    ((Unification_error
      (Row_mismatch (RExtend x Present (TVar (Unbound 20 1)) REmpty)
       (RExtend y Present (TVar (Unbound 23 1)) REmpty) (Missing_label x)))
     @ t.stag:3:47-55)
    |}]
;;

let%expect_test "local polymorphic record operations" =
  check
    {|
let access = let get r = r.`x in (get {`x = 1}, get {`x = true; `y = 1})
let restriction =
  let drop r = r \ `x in (drop {`x = 1}, drop {`x = true; `y = 1})
let update =
  let set r = {r with `x = 0} in (set {`x = true}, set {`x = (); `y = 1})
let pattern =
  let rest r = match r with {`x = _; ..rest} -> rest in
  (rest {`x = 1}, rest {`x = 1; `y = 1})
|};
  [%expect
    {|
    val access : int * bool
    val restriction : {} * {`y : int}
    val update : {`x : int} * {`x : int; `y : int}
    val pattern : {} * {`y : int}
    |}]
;;

let%expect_test "applied access and restriction are not values" =
  check
    {|
let r = {`f = id; `g = id}
let field = (id r).`f
let restricted = id r \ `g
|};
  [%expect
    {|
    val r : {`f : 'a -> 'a; `g : 'b -> 'b}
    val field : '_a -> '_a
    val restricted : {`f : '_a -> '_a}
    |}]
;;

let%expect_test "absent entries in records" =
  check
    {|
let absent c (r : {..'d}) =
  .<.<let z = 1 in .~.~(c : <<'a>{..'c}>{..'d})>.>.; r
|};
  [%expect
    {| val absent : <<'a>{z?'b : immut int; ..'c}>{-z; ..'d} -> {-`z; ..'d} -> {-`z; ..'d} |}]
;;

let%expect_test "structural types may be recursive" =
  check
    {|
let occurs r = cond true r.`x r
|};
  check
    {|
let cyclic r = cond true {`x = 1 | r} {`y = 1 | r}
|};
  check
    {|
type t = {`next : t}
|};
  check
    {|
type a = {`b : b}
type b = [`A of a]
|};
  check
    {|
type t = T of {`next : t}
let deref t = match t with T r -> r.`next
|};
  [%expect
    {|
    val occurs : ({`x : 'a; ..'b} as 'a) -> 'a
    ((Unification_error
      (Row_mismatch
       (RExtend x Present (TCon int ())
        (RVar
         (Link
          (RExtend x (PVar (Unbound 34 1)) (TCon int ()) (RVar (Unbound 35 1))))))
       (RExtend y Present (TCon int ())
        (RVar
         (Link
          (RExtend x (PVar (Unbound 34 1)) (TCon int ()) (RVar (Unbound 35 1))))))
       Cyclic_row))
     @ t.stag:2:38-50)
    type t = {`next : t}
    type a = {`b : [`A of a]}
    type b = [`A of {`b : b}]
    type t = T of {`next : t}
    val deref : t -> t
    |}]
;;
