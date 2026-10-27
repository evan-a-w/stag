open! Import
open Source_helpers

let%expect_test "constructors, fields and empty matches of later types" =
  run
    {|
let a = M.A 1
let get r = r.M.x
module M = struct
  type t = A of int | B
  type r = {x : t}
end
let b = get {M.x = a}
|};
  [%expect
    {|
    val a : M.t
    val get : M.r -> M.t
    module M : sig
      type t = A of int | B
      type r = { x : t }
    end
    val b : M.t
    -- evaluated at compile time
    -- evaluated
    (a (M.A 1))
    (get <fun>)
    (b (M.A 1))
    |}]
;;

(* On the DSL: source can't write an empty match. *)
let%expect_test "an empty match on a later empty type" =
  let open Helpers in
  run_program
    [ top_let
        ("absurd", [ typed "v" (Ty.con "void") ], None, match_ (var "v") [])
    ; Decl.variant "void" []
    ];
  [%expect
    {|
    val absurd : void -> 'a
    type void = |
    -- evaluated at compile time
    -- evaluated
    (absurd <fun>)
    |}]
;;

let%expect_test "a toplevel splice using later types" =
  run
    {|
let x = .~(print_endline "splice"; gen (M.S M.Z))
let gen n = match n with M.Z -> .<{N.v = 0}>. | M.S _ -> .<{N.v = 1}>.
module M = struct
  type nat = Z | S of nat
end
module N = struct
  type r = {v : int}
end
|};
  [%expect
    {|
    splice
    val x : N.r
    val gen : M.nat -> <N.r>{..'a}
    module M : sig
      type nat = Z | S of nat
    end
    module N : sig
      type r = { v : int }
    end
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (gen <fun>)
    (x (Record 1))
    |}]
;;

let%expect_test "a type is checked once its constructors are used" =
  check
    {|
let x = A
let y = add 1 true
type t = A of unbound
|};
  [%expect {| ((Unbound_type unbound) @ t.stag:4:14-21) |}]
;;

let%expect_test "a type is not checked when only named" =
  check
    {|
let x (v : t) = v
let y = add 1 true
type t = A of unbound
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:14-18)
    |}]
;;

let%expect_test "errors are in order of declarations, used or not" =
  check
    {|
let y = add 1 true
type t = A of unbound
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:14-18)
    |}];
  check
    {|
type t = A of unbound
let y = add 1 true
|};
  [%expect {| ((Unbound_type unbound) @ t.stag:2:14-21) |}]
;;

let%expect_test "toplevel splices run before later declarations are checked" =
  check
    {|
let x = .~(print_endline "splice"; .<1>.)
type t = A of unbound
|};
  [%expect
    {|
    splice
    ((Unbound_type unbound) @ t.stag:3:14-21)
    |}]
;;
