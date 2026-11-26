open! Import
open Source_helpers

(* [N.r]'s and [M.t]'s fields are out of scope, and [s]'s [x] is in scope. *)
let types =
  {|
module N = struct
  type r = {x : int; y : bool}
end
module M = struct
  type t = {a : int; b : N.r}
  type v = A | B of int
  let t = {a = 1; b = {x = 2; y = true}}
end
type s = {x : string; z : int}
|}
;;

let%expect_test "field access resolves by the type of the record" =
  run
    (types
     ^ {|
let a = M.t.a
let bx = M.t.b.x
let annotated (t : M.t) = t.b.y
let shadowed (v : N.r) = v.x
let lexical v = v.x
|}
    );
  [%expect
    {|
    module N : sig
      type r = { x : int; y : bool }
    end
    module M : sig
      type t = { a : int; b : N.r }
      type v = A | B of int
      val t : t
    end
    type s = { x : string; z : int }
    val a : int
    val bx : int
    val annotated : M.t -> bool
    val shadowed : N.r -> int
    val lexical : s -> string
    -- evaluated at compile time
    -- evaluated
    (M.t (M.t (a 1) (b (N.r (x 2) (y true)))))
    (a 1)
    (bx 2)
    (annotated <fun>)
    (shadowed <fun>)
    (lexical <fun>)
    |}]
;;

let%expect_test "construction resolves by the expected type" =
  run
    (types
     ^ {|
let annotated : M.t = {a = 3; b = {x = 4; y = false}}
let argument = M.B 1
let in_constructor : option[N.r] = Some {x = 7; y = false}
let in_tuple : N.r * M.t = ({x = 5; y = true}, {a = 6; b = M.t.b})
let returned n : N.r = {x = n; y = true}
let lexical = {x = "s"; z = 7}
let qualified : N.r = {M.a = 1; b = M.t.b}.b
let first_qualified = {b = M.t.b; M.a = 8}
let get_a (t : M.t) = t.a
let argument_of_known = get_a {a = 9; b = M.t.b}
let in_poly_record : {`p : N.r} = {`p = {x = 10; y = true}}
let in_poly_variant : [`T of N.r] = `T {x = 11; y = true}
let extended : {`q : N.r; `p : N.r} = {`q = {x = 12; y = true} | in_poly_record}
let in_poly_update : {`p : N.r} = {in_poly_record with `p = {x = 13; y = true}}
|}
    );
  [%expect
    {|
    module N : sig
      type r = { x : int; y : bool }
    end
    module M : sig
      type t = { a : int; b : N.r }
      type v = A | B of int
      val t : t
    end
    type s = { x : string; z : int }
    val annotated : M.t
    val argument : M.v
    val in_constructor : option[N.r]
    val in_tuple : N.r * M.t
    val returned : int -> N.r
    val lexical : s
    val qualified : N.r
    val first_qualified : M.t
    val get_a : M.t -> int
    val argument_of_known : int
    val in_poly_record : {`p : N.r}
    val in_poly_variant : [`T of N.r]
    val extended : {`p : N.r; `q : N.r}
    val in_poly_update : {`p : N.r}
    -- evaluated at compile time
    -- evaluated
    (M.t (M.t (a 1) (b (N.r (x 2) (y true)))))
    (annotated (M.t (a 3) (b (N.r (x 4) (y false)))))
    (argument (M.B 1))
    (in_constructor (Some (N.r (x 7) (y false))))
    (in_tuple (Tuple (N.r (x 5) (y true)) (M.t (a 6) (b (N.r (x 2) (y true))))))
    (returned <fun>)
    (lexical (s (x (String s)) (z 7)))
    (qualified (N.r (x 2) (y true)))
    (first_qualified (M.t (a 8) (b (N.r (x 2) (y true)))))
    (get_a <fun>)
    (argument_of_known 9)
    (in_poly_record ((p (N.r (x 10) (y true)))))
    (in_poly_variant (`T (N.r (x 11) (y true))))
    (extended ((p (N.r (x 10) (y true))) (q (N.r (x 12) (y true)))))
    (in_poly_update ((p (N.r (x 13) (y true)))))
    |}]
;;

let%expect_test "update resolves by the type of the record or the expected type"
  =
  run
    (types
     ^ {|
let updated = {M.t with a = 10}
let nested = {M.t with b = {M.t.b with x = 11}}
let by_expected (v : N.r) = {v with x = 12}
let by_annotation v : N.r = {v with x = 13}
let lexical v = {v with x = "t"}
|}
    );
  [%expect
    {|
    module N : sig
      type r = { x : int; y : bool }
    end
    module M : sig
      type t = { a : int; b : N.r }
      type v = A | B of int
      val t : t
    end
    type s = { x : string; z : int }
    val updated : M.t
    val nested : M.t
    val by_expected : N.r -> N.r
    val by_annotation : N.r -> N.r
    val lexical : s -> s
    -- evaluated at compile time
    -- evaluated
    (M.t (M.t (a 1) (b (N.r (x 2) (y true)))))
    (updated (M.t (a 10) (b (N.r (x 2) (y true)))))
    (nested (M.t (a 1) (b (N.r (x 11) (y true)))))
    (by_expected <fun>)
    (by_annotation <fun>)
    (lexical <fun>)
    |}]
;;

let%expect_test "patterns resolve by the type of the scrutinee" =
  run
    (types
     ^ {|
let a = match M.t with {a = n; b = {x = x; y = _}} -> (n, x)
let annotated (v : N.r) = match v with {x = n} -> n
let in_constructor = match Some M.t.b with Some {x = n} -> n | None -> 0
let in_tuple = match (M.t, 1) with ({a = n}, _) -> n
let lexical v = match v with {x = n} -> n
let variant = match M.B 2 with A -> 0 | B n -> n
let in_poly_record = match {`p = M.t} with {`p = {a = n; b = _}} -> n
let in_poly_variant (v : [`T of M.t | `U]) = match v with `T {a = n; b = _} -> n | `U -> 0
|}
    );
  [%expect
    {|
    module N : sig
      type r = { x : int; y : bool }
    end
    module M : sig
      type t = { a : int; b : N.r }
      type v = A | B of int
      val t : t
    end
    type s = { x : string; z : int }
    val a : int * int
    val annotated : N.r -> int
    val in_constructor : int
    val in_tuple : int
    val lexical : s -> string
    val variant : int
    val in_poly_record : int
    val in_poly_variant : [`T of M.t | `U] -> int
    -- evaluated at compile time
    -- evaluated
    (M.t (M.t (a 1) (b (N.r (x 2) (y true)))))
    (a (Tuple 1 2))
    (annotated <fun>)
    (in_constructor 2)
    (in_tuple 1)
    (lexical <fun>)
    (variant 2)
    (in_poly_record 1)
    (in_poly_variant <fun>)
    |}]
;;

let%expect_test "an unknown type falls back to the fields in scope" =
  (* [v]'s type is not known when [v.x] is checked: [x] is [s]'s, and
     [(v : N.r)] then fails, as inference is not reordered. *)
  check (types ^ {|
let late v = (v.x, (v : N.r))
|});
  (* Known later, eg. by an application's argument, is too late: [y] is not in
     scope. *)
  check (types ^ {|
let apply f = f (M.t.b)
let y v = v.y
let late = apply y
|});
  (* Known first, it is used. *)
  run (types ^ {|
let early v = let _ = (v : N.r) in v.x
|});
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon N.r ()) (TCon s ()) (Base_types_not_equal N.r s)))
     @ t.stag:12:19-28)
    ((Unbound_field y) @ t.stag:13:12-13)
    module N : sig
      type r = { x : int; y : bool }
    end
    module M : sig
      type t = { a : int; b : N.r }
      type v = A | B of int
      val t : t
    end
    type s = { x : string; z : int }
    val early : N.r -> int
    -- evaluated at compile time
    -- evaluated
    (M.t (M.t (a 1) (b (N.r (x 2) (y true)))))
    (early <fun>)
    |}]
;;

let%expect_test "qualified fields and constructors name their declaration" =
  (* Even where another type is expected, the qualified name wins, and
     unification reports the mismatch, or for a record, the field. *)
  check (types ^ {|
let wrong (v : N.r) = v.M.a
|});
  check (types ^ {|
let wrong : N.r = {M.a = 1; b = M.t.b}
|});
  check (types ^ {|
let wrong : option[int] = M.A
|});
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon M.t ()) (TCon N.r ()) (Base_types_not_equal M.t N.r)))
     @ t.stag:12:22-23)
    ((Fields_of_different_types (field M.a) (expected_type N.r)
      (actual_type M.t))
     @ t.stag:12:19-22)
    ((Unification_error
      (Type_mismatch (TCon prelude.option ((ATy (TCon int ())))) (TCon M.v ())
       (Base_types_not_equal prelude.option M.v)))
     @ t.stag:12:26-29)
    |}]
;;

let%expect_test "a field or constructor the known type lacks" =
  check (types ^ {|
let missing = M.t.z
|});
  check (types ^ {|
let missing : N.r = {x = 1; z = 2}
|});
  check (types ^ {|
let missing (v : N.r) = {v with z = 1}
|});
  check (types ^ {|
let missing (v : N.r) = match v with {z = n} -> n
|});
  check (types ^ {|
let missing : M.v = C
|});
  check (types ^ {|
let missing (v : M.v) = match v with Some _ -> 1 | _ -> 0
|});
  (* Not a nominal record type: the field is looked up in scope. *)
  check (types ^ {|
let not_nominal = (1, 2).x
|});
  [%expect
    {|
    ((No_such_field (type_name M.t) (field z)) @ t.stag:12:18-19)
    ((No_such_field (type_name N.r) (field z)) @ t.stag:12:28-29)
    ((No_such_field (type_name N.r) (field z)) @ t.stag:12:32-33)
    ((No_such_field (type_name N.r) (field z)) @ t.stag:12:38-39)
    ((No_such_constructor (type_name M.v) (constructor C)) @ t.stag:12:20-21)
    ((No_such_constructor (type_name M.v) (constructor Some)) @ t.stag:12:37-41)
    ((Unification_error
      (Type_mismatch (TCon s ()) (TTuple ((TCon int ()) (TCon int ())))
       Structural_mismatch))
     @ t.stag:12:18-24)
    |}]
;;

let%expect_test "the errors as diagnostics" =
  File_check.check {|
type r = {x : int}
let missing (v : r) = v.y
|};
  File_check.check {|
type v = A
let missing : v = B
|};
  [%expect
    {|
    File "a.stag", line 3, characters 24-25:
    3 | let missing (v : r) = v.y
                                ^
    Error: The record type A.r has no field y
    File "a.stag", line 3, characters 18-19:
    3 | let missing : v = B
                          ^
    Error: The variant type A.v has no constructor B
    |}]
;;

let%expect_test "generated code's unqualified fields resolve by type" =
  (* [getters] makes [get_f (v : t) = v.f] for each field [f] of a record type,
     naming fields by name rather than by ref: [s] shadows [r]'s [x]. *)
  File_check.check
    ~run:true
    {|
let fst p = match p with (a, _) -> a

let getter ty r =
  let name = Stag.Field_ref.name r in
  Stag.Toplevel.let_ (String.append "get_" name) Nil
    (Stag.Expr.Let (Ptr.make ( ("f", Cons (Box.make ( (("v", Stag.Annot.ty (Stag.Type.expr ty)), Nil))), Stag.Annot.none,
       Stag.Expr.Field (Ptr.make ( (Stag.Expr.var "v", Stag.Path.Name name))),
       Stag.Expr.var "f"))))

let getters ty =
  let head = match Stag.Type.expr ty with Stag.Type_expr.Con (Stag.Path.Ref r, _) -> r in
  match Stag.Type.decl head with
  | Some d ->
    (match d.kind with
     | Record fields -> Stag.map (getter ty) (Stag.map fst fields)
     | _ -> Nil)
  | None -> Nil

module N = struct
  type r = {x : int; y : bool}
  let v = {x = 1; y = true}
end
type s = {x : string}
include .~(getters (type N.r))
let shown = print_int (get_x N.v)
|};
  [%expect
    {|
    module A : sig
      val fst : 'a * 'b -> 'a
      val getter : Stag.Type.t['a] -> Stag.Field_ref.t -> Stag.Toplevel.t
      val getters : Stag.Type.t['a] -> list[Stag.Toplevel.t]
      module N : sig
        type r = { x : int; y : bool }
        val v : r
      end
      type s = { x : string }
      val get_x : N.r -> int
      val get_y : N.r -> bool
      val shown : unit
    end
    --- output ---
    1
    |}]
;;

let%expect_test "unqualified constructors resolve by the expected type" =
  run
    (types
     ^ {|
let annotated : M.v = B 1
let in_list : list[M.v] = Cons (Box.make ( (A, Cons (Box.make ( (B 2, Nil))))))
let returned n : M.v = B n
let in_match b : M.v = match b with true -> A | false -> B 3
let pattern (v : M.v) = match v with A -> 0 | B n -> n
let nested (l : list[M.v]) = match l with Cons * (B n, _) -> n | _ -> 0
let ran = (pattern (B 5), nested (Cons (Box.make ( (B 6, Nil)))), in_match false)
|}
    );
  [%expect
    {|
    module N : sig
      type r = { x : int; y : bool }
    end
    module M : sig
      type t = { a : int; b : N.r }
      type v = A | B of int
      val t : t
    end
    type s = { x : string; z : int }
    val annotated : M.v
    val in_list : list[M.v]
    val returned : int -> M.v
    val in_match : bool -> M.v
    val pattern : M.v -> int
    val nested : list[M.v] -> int
    val ran : int * int * M.v
    -- evaluated at compile time
    -- evaluated
    (M.t (M.t (a 1) (b (N.r (x 2) (y true)))))
    (annotated (M.B 1))
    (in_list (Cons (M.A) (Cons (M.B 2) (Nil))))
    (returned <fun>)
    (in_match <fun>)
    (pattern <fun>)
    (nested <fun>)
    (ran (Tuple 5 6 (M.B 3)))
    |}]
;;

let%expect_test "aliases, parameters, quotes and match arms" =
  run
    (types
     ^ {|
type alias = N.r
module P = struct
  type p['a] = {v : 'a; w : int}
end
let via_alias (v : alias) = v.y
let alias_built : alias = {x = 1; y = true}
let param (p : P.p[bool]) = p.v
let param_built : P.p[string] = {v = "s"; w = 1}
let param_updated (p : P.p[int]) = {p with v = 5}
let quoted : <N.r>{} = .< {x = 2; y = false} >.
let arms b : N.r = match b with true -> {x = 3; y = true} | false -> {M.t.b with y = false}
let seq : N.r = (); {x = 4; y = true}
let ran = (via_alias alias_built, param {v = true; w = 2}, (param_updated {v = 1; w = 2}).v, arms false)
|}
    );
  [%expect
    {|
    module N : sig
      type r = { x : int; y : bool }
    end
    module M : sig
      type t = { a : int; b : N.r }
      type v = A | B of int
      val t : t
    end
    type s = { x : string; z : int }
    type alias = N.r
    module P : sig
      type p['a] = { v : 'a; w : int }
    end
    val via_alias : N.r -> bool
    val alias_built : N.r
    val param : P.p[bool] -> bool
    val param_built : P.p[string]
    val param_updated : P.p[int] -> P.p[int]
    val quoted : <N.r>{}
    val arms : bool -> N.r
    val seq : N.r
    val ran : bool * bool * int * N.r
    -- evaluated at compile time
    -- evaluated
    (M.t (M.t (a 1) (b (N.r (x 2) (y true)))))
    (via_alias <fun>)
    (alias_built (N.r (x 1) (y true)))
    (param <fun>)
    (param_built (P.p (v (String s)) (w 1)))
    (param_updated <fun>)
    (quoted
     (Code
      (Record (record N.r)
       (fields
        ((((type_name N.r) (name N.x) (index 0)) (Lit (Int 2) : int))
         (((type_name N.r) (name N.y) (index 1)) (Lit (Bool false) : bool))))
       : N.r)))
    (arms <fun>)
    (seq (N.r (x 4) (y true)))
    (ran (Tuple true true 5 (N.r (x 2) (y false))))
    |}]
;;

let%expect_test "the known type wins over the latest fields in scope" =
  run
    {|
module K = struct
  type u = {x : int; y : int}
  module L = struct
    type w = {x : bool}
    let latest v = v.x
    let picked (v : u) = v.x
    let built : u = {x = 1; y = 2}
    let updated (v : u) = {v with x = 3}
    let matched (v : u) = match v with {x = n} -> n
    let ran = (picked built, (updated built).x, matched built, latest {x = true})
  end
end
|};
  [%expect
    {|
    module K : sig
      type u = { x : int; y : int }
      module L : sig
        type w = { x : bool }
        val latest : w -> bool
        val picked : u -> int
        val built : u
        val updated : u -> u
        val matched : u -> int
        val ran : int * int * int * bool
      end
    end
    -- evaluated at compile time
    -- evaluated
    (K.L.latest <fun>)
    (K.L.picked <fun>)
    (K.L.built (K.u (x 1) (y 2)))
    (K.L.updated <fun>)
    (K.L.matched <fun>)
    (K.L.ran (Tuple 1 3 1 true))
    |}]
;;

let%expect_test "fields of different types" =
  check (types ^ {|
let mixed = {M.a = 1; N.x = 2}
|});
  check (types ^ {|
let mixed : M.t = {a = 1; N.x = 2}
|});
  (* At the qualified field, though [y] comes first and [M.t] lacks it. *)
  check (types ^ {|
let mixed : M.t = {y = true; N.x = 2}
|});
  check (types ^ {|
let mixed (v : M.t) = {v with N.x = 2}
|});
  [%expect
    {|
    ((Fields_of_different_types (field N.x) (expected_type M.t)
      (actual_type N.r))
     @ t.stag:12:22-25)
    ((Fields_of_different_types (field N.x) (expected_type M.t)
      (actual_type N.r))
     @ t.stag:12:26-29)
    ((Fields_of_different_types (field N.x) (expected_type M.t)
      (actual_type N.r))
     @ t.stag:12:29-32)
    ((Fields_of_different_types (field N.x) (expected_type M.t)
      (actual_type N.r))
     @ t.stag:12:30-33)
    |}]
;;

let%expect_test "with no type known, the first type in scope with all the \
                 fields"
  =
  (* [w]'s [x] is the latest in scope, but only [u] has [y], in either order. *)
  run
    {|
module K = struct
  type u = {x : int; y : int}
  module L = struct
    type w = {x : bool}
    let x_first = {x = 1; y = 2}
    let y_first = {y = 3; x = 4}
    let only_x = {x = true}
    let x_first_pattern v = match v with {x = n; y = m} -> Int.add n m
    let y_first_pattern v = match v with {y = m; x = n} -> Int.add n m
    let ran = (x_first_pattern x_first, y_first_pattern y_first)
  end
end
|};
  (* None has them all: the first field's type is reported as lacking one. *)
  check
    {|
module K = struct
  type u = {x : int; y : int}
  module L = struct
    type w = {x : bool; z : int}
    let none = {x = true; y = 1; z = 2}
  end
end
|};
  [%expect
    {|
    module K : sig
      type u = { x : int; y : int }
      module L : sig
        type w = { x : bool }
        val x_first : u
        val y_first : u
        val only_x : w
        val x_first_pattern : u -> int
        val y_first_pattern : u -> int
        val ran : int * int
      end
    end
    -- evaluated at compile time
    -- evaluated
    (K.L.x_first (K.u (x 1) (y 2)))
    (K.L.y_first (K.u (x 4) (y 3)))
    (K.L.only_x (K.L.w (x true)))
    (K.L.x_first_pattern <fun>)
    (K.L.y_first_pattern <fun>)
    (K.L.ran (Tuple 3 7))
    ((No_such_field (type_name K.L.w) (field y)) @ t.stag:6:26-27)
    |}]
;;

let%expect_test "an update may change the type parameters only in updated \
                 fields"
  =
  run
    {|
type p['a] = {v : 'a; w : int}
type q['a] = {s : 'a; t : 'a}
let a : p[int] = {v = 1; w = 2}
let changed = {a with v = "s"}
let kept = {a with w = 3}
let change r = {r with v = true}
let q : q[int] = {s = 1; t = 2}
let both = {q with s = "a"; t = "b"}
let ran = ((change a).v, changed.w)
|};
  (* [t] is not updated, so [q]'s parameter stays [int]. *)
  check
    {|
type q['a] = {s : 'a; t : 'a}
let q : q[int] = {s = 1; t = 2}
let one = {q with s = "a"}
|};
  check
    {|
type p['a] = {v : 'a; w : int}
let a : p[int] = {v = 1; w = 2}
let annotated : p[string] = {a with w = 3}
|};
  [%expect
    {|
    type p['a] = { v : 'a; w : int }
    type q['a] = { s : 'a; t : 'a }
    val a : p[int]
    val changed : p[string]
    val kept : p[int]
    val change : p['a] -> p[bool]
    val q : q[int]
    val both : q[string]
    val ran : bool * int
    -- evaluated at compile time
    -- evaluated
    (a (p (v 1) (w 2)))
    (changed (p (v (String s)) (w 2)))
    (kept (p (v 1) (w 3)))
    (change <fun>)
    (q (q (s 1) (t 2)))
    (both (q (s (String a)) (t (String b))))
    (ran (Tuple true 2))
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon string ())
       (Base_types_not_equal int string)))
     @ t.stag:4:22-25)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon string ())
       (Base_types_not_equal int string)))
     @ t.stag:4:28-42)
    |}]
;;

let%expect_test "a field given twice" =
  let given source = check ("type r = {v : int; w : int}\n" ^ source) in
  given "let a = {v = 1; v = 2; w = 3}";
  given "let b (a : r) = {a with v = 1; v = 2}";
  given "let c (a : r) = match a with {w = n; w = m} -> n";
  (* By the unqualified name, though one is qualified. *)
  given "module M = struct type r = {v : int} end\nlet d = {M.v = 1; v = 2}";
  File_check.check
    {|
type r = {v : int; w : int}
let b (a : r) = {a with v = 1; v = 2}
|};
  [%expect
    {|
    ((Repeated_field v) @ t.stag:2:16-17)
    ((Repeated_field v) @ t.stag:2:31-32)
    ((Repeated_field w) @ t.stag:2:37-38)
    ((Repeated_field v) @ t.stag:3:18-19)
    File "a.stag", line 3, characters 31-32:
    3 | let b (a : r) = {a with v = 1; v = 2}
                                       ^
    Error: The field v is given more than once in this record
    |}]
;;

let%expect_test "a missing field of the expected type" =
  check (types ^ {|
let missing : N.r = {x = 1}
|});
  [%expect {| ((Missing_field N.y) @ t.stag:12:20-27) |}]
;;

let%expect_test "splices and updates in quotes" =
  (* [.~e]'s expected type [T] gives [e] the expected type [<T>{..}]. *)
  run
    (types
     ^ {|
let spliced : <N.r>{} = .< let v : N.r = .~(.< {x = 1; y = true} >.) in v >.
let upd (c : <N.r>{}) = .< {.~c with x = 7} >.
let updated = upd .< {x = 1; y = true} >.
let ran = .~(upd spliced)
|}
    );
  [%expect
    {|
    module N : sig
      type r = { x : int; y : bool }
    end
    module M : sig
      type t = { a : int; b : N.r }
      type v = A | B of int
      val t : t
    end
    type s = { x : string; z : int }
    val spliced : <N.r>{}
    val upd : <N.r>{} -> <N.r>{..'a}
    val updated : <N.r>{..'_a}
    val ran : N.r
    -- evaluated at compile time
    (spliced
     (Code
      (Let (ident v/17) (args ())
       (result_annot ((Con (<N.r> @ t.stag:12:35-38) ()) @ t.stag:12:35-38))
       (generalized false)
       (body
        (Record (record N.r)
         (fields
          ((((type_name N.r) (name N.x) (index 0)) (Lit (Int 1) : int))
           (((type_name N.r) (name N.y) (index 1)) (Lit (Bool true) : bool))))
         : N.r))
       (in_ (Var (Bound (Local v/17)) : N.r)) : N.r)))
    (upd <fun>)
    -- evaluated
    (M.t (M.t (a 1) (b (N.r (x 2) (y true)))))
    (spliced
     (Code
      (Let (ident v/17) (args ())
       (result_annot ((Con (<N.r> @ t.stag:12:35-38) ()) @ t.stag:12:35-38))
       (generalized false)
       (body
        (Record (record N.r)
         (fields
          ((((type_name N.r) (name N.x) (index 0)) (Lit (Int 1) : int))
           (((type_name N.r) (name N.y) (index 1)) (Lit (Bool true) : bool))))
         : N.r))
       (in_ (Var (Bound (Local v/17)) : N.r)) : N.r)))
    (upd <fun>)
    (updated
     (Code
      (Record_update
       (Record (record N.r)
        (fields
         ((((type_name N.r) (name N.x) (index 0)) (Lit (Int 1) : int))
          (((type_name N.r) (name N.y) (index 1)) (Lit (Bool true) : bool))))
        : N.r)
       ((((type_name N.r) (name N.x) (index 0)) (Lit (Int 7) : int))) : N.r)))
    (ran (N.r (x 7) (y true)))
    |}]
;;

let%expect_test "a non-function toplevel in a quote is of unknown type" =
  (* [N.v] is a free variable of the quote, unrelated to [N.v]'s type, so [x] is
     the one in scope. *)
  let program use =
    {|
module N = struct
  type r = {x : int; y : bool}
  let v = {x = 1; y = true}
end
type s = {x : string}
|}
    ^ use
  in
  run (program "let a = .< N.v.x >.");
  check (program "let b = .< {N.v with x = 7} >.");
  [%expect
    {|
    module N : sig
      type r = { x : int; y : bool }
      val v : r
    end
    type s = { x : string }
    val a : <int>{..'a}
    -- evaluated at compile time
    -- evaluated
    (N.v (N.r (x 1) (y true)))
    (a
     (Code
      (Field (Var (Persistent (Toplevel N.v/4)) : N.r)
       ((type_name N.r) (name N.x) (index 0)) : int)))
    module N : sig
      type r = { x : int; y : bool }
      val v : r
    end
    type s = { x : string }
    val b : <N.r>{..'a}
    |}]
;;
