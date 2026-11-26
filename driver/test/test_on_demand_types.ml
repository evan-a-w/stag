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
let x = .~(print_endline "splice"; gen (M.S (Ptr.make M.Z)))
let gen n = match n with M.Z -> .<{N.v = 0}>. | M.S _ -> .<{N.v = 1}>.
module M = struct
  type nat = Z | S of ptr[nat]
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
      type nat = Z | S of ptr[nat]
    end
    module N : sig
      type r = { v : int }
    end
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (gen <fun>)
    (x (N.r (v 1)))
    |}]
;;

let%expect_test "a type is checked once its constructors are used" =
  check {|
let x = A
let y = add 1 true
type t = A of unbound
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:14-18)
    ((Unbound_type unbound) @ t.stag:4:14-21)
    |}]
;;

let%expect_test "a type is not checked when only named" =
  check {|
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
    ((Unbound_type unbound) @ t.stag:4:14-21)
    |}]
;;

let%expect_test "errors are in order of declarations, used or not" =
  check {|
let y = add 1 true
type t = A of unbound
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:14-18)
    ((Unbound_type unbound) @ t.stag:3:14-21)
    |}];
  check {|
type t = A of unbound
let y = add 1 true
|};
  [%expect
    {|
    ((Unbound_type unbound) @ t.stag:2:14-21)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:14-18)
    |}]
;;

let%expect_test "toplevel splices run before later declarations are checked" =
  check {|
let x = .~(print_endline "splice"; .<1>.)
type t = A of unbound
|};
  [%expect {|
    splice
    ((Unbound_type unbound) @ t.stag:3:14-21)
    |}]
;;

let%expect_test "an alias cycle reached first through an annotation" =
  check {|
let f (x : a) = x
let y = add 1 true
type a = a
|};
  check {|
let f (x : b) = x
let y = add 1 true
type a = b
type b = a
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:14-18)
    ((Cyclic_type_alias a) @ t.stag:4:5-6)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:14-18)
    ((Cyclic_type_alias a) @ t.stag:4:5-6)
    ((Cyclic_type_alias b) @ t.stag:5:5-6)
    |}]
;;

let%expect_test "constructors and fields of later types in generated code" =
  run
    ~bodies:[ "x" ]
    {|
let x = .~(gen 0)
let gen n = .<({M.f = M.A 1}).M.f>.
module M = struct
  type t = A of int
  type r = {f : t}
end
|};
  [%expect
    {|
    val x : M.t
    val gen : 'a -> <M.t>{..'b}
    module M : sig
      type t = A of int
      type r = { f : t }
    end
    (x/4
     (Toplevel_splice
      (code
       (App (Var (Bound (Toplevel gen/5)) : "int -> <M.t>{}") (Lit (Int 0) : int)
        : <M.t>{}))
      (expansion
       (Field
        (Record (record M.r)
         (fields
          ((((type_name M.r) (name M.f) (index 0))
            (Construct ((type_name M.t) (name M.A) (index 0))
             ((Lit (Int 1) : int)) : M.t))))
         : M.r)
        ((type_name M.r) (name M.f) (index 0)) : M.t))
      : M.t))
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (gen <fun>)
    (x (M.A 1))
    |}]
;;

let%expect_test "an expect test in a module using a later type" =
  Option.iter
    (typecheck
       {x|
module M = struct
  let%expect_test "later" =
    print_int (match N.A 2 with N.A n -> n);
    [%expect {| 2 |}]
end
module N = struct
  type t = A of int
end
|x})
    ~f:(fun checked ->
      print_signature checked;
      let driver = driver checked in
      Driver.eval ~limits driver |> ok_exn;
      List.iter (Expect_test_runner.run ~limits driver) ~f:(fun result ->
        print_s [%sexp (result : Expect_test_runner.Test_result.t)]));
  [%expect
    {|
    module M : sig end
    module N : sig
      type t = A of int
    end
    ((name (later @ t.stag:3:18-25)) (mismatches ()) (error ()))
    |}]
;;

let%expect_test "a type in a module checked before its turn keeps its place" =
  run
    {|
let a = M.N.A
module M = struct
  type before = int
  module N = struct
    type t = A
    type u = B of int
  end
  type after = { x : N.t }
end
|};
  [%expect
    {|
    val a : M.N.t
    module M : sig
      type before = int
      module N : sig
        type t = A
        type u = B of int
      end
      type after = { x : N.t }
    end
    -- evaluated at compile time
    -- evaluated
    (a (M.N.A))
    |}]
;;

let%expect_test "names are checked unique before anything else" =
  check {|
let y = add 1 true
type t['a, 'a] = int
|};
  check
    {|
let y = add 1 true
module M = struct
  type t = A | A
end
module M = struct end
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:14-18)
    ((Duplicate_type_parameter (type_name t) (param a)) @ t.stag:3:11-13)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:14-18)
    ((Duplicate_constructor M.A) @ t.stag:4:15-16)
    ((Duplicate_module M) @ t.stag:6:7-8)
    |}]
;;

(* [t] is first elaborated while an attempt that fails is under way: what is
   cached of it must outlive the attempt's rollback. *)
let%expect_test "a later type first elaborated under a failed attempt" =
  print_s
    [%sexp
      (Mode_helpers.located_errors
         {|
let ident x = x
let use (x @ local) (k : t -> unit) = let r @ global = ident x in k r
let later (x : t) = match x with A n -> Int.add n 1 | B -> 0
type al = string
type t = A of al | B
|}
       : (unit, (Loc.t * string) list) Result.t)];
  [%expect
    {|
    (Error
     ((t.stag:3:68-69
       "This is regional, because it is annotated local (at t.stag:3:13-18), then x is passed to the function (at t.stag:3:38-69), then x is used (at t.stag:3:61-62), then its type turned out to be t, which has pointers, then it is passed to a function (at t.stag:3:55-62), but must be at most global, because it is annotated global (at t.stag:3:46-52), then it is bound by an annotated let (at t.stag:3:46-52), then the function returns it (at t.stag:3:55-62), then the function returns it (at t.stag:2:14-15), then x is used (at t.stag:2:14-15), then x is passed to the function (at t.stag:2:14-15)")
      (t.stag:4:48-49
       "This expression has type string but an expression was expected of type int")))
    |}]
;;
