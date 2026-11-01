open! Import
open Source_helpers

let%expect_test "qualified and unqualified values" =
  check
    {|
module M = struct
  let x = 1
  let f y = add y x
  let g y = M.f y
end
let a = M.x
let b = M.f 2
|};
  [%expect
    {|
    module M : sig
      val x : int
      val f : int -> int
      val g : int -> int
    end
    val a : int
    val b : int
    |}]
;;

let%expect_test "nested modules" =
  check
    {|
let root = "s"
module A = struct
  let v = 1
  module B = struct
    let x = true
    let from_a = v
    let from_root = root
    let self = B.x
    let absolute = A.v
  end
  let y = B.x
end
let z = A.B.x
let w = A.y
|};
  [%expect
    {|
    val root : string
    module A : sig
      val v : int
      module B : sig
        val x : bool
        val from_a : int
        val from_root : string
        val self : bool
        val absolute : int
      end
      val y : bool
    end
    val z : bool
    val w : bool
    |}]
;;

let%expect_test "results are in program order, depth first" =
  check
    {|
let a = 1
module A = struct
  let b = 2
  module B = struct
    let c = A.d
  end
  let d = 3
end
let e = A.B.c
|};
  [%expect
    {|
    val a : int
    module A : sig
      val b : int
      module B : sig
        val c : int
      end
      val d : int
    end
    val e : int
    |}]
;;

let%expect_test "lexical shadowing" =
  check
    {|
let x = 1
module M = struct
  let x = true
  let y = x
  module N = struct
    let z = x
  end
  let local x = x
  let local_let = let x = () in x
  let qualified_ignores_locals x = M.x
end
let root_x = x
|};
  [%expect
    {|
    val x : int
    module M : sig
      val x : bool
      val y : bool
      module N : sig
        val z : bool
      end
      val local : 'a -> 'a
      val local_let : unit
      val qualified_ignores_locals : 'a -> bool
    end
    val root_x : int
    |}]
;;

let%expect_test "modules shadow lexically" =
  check
    {|
module N = struct
  let x = 1
end
module M = struct
  module N = struct
    let x = true
  end
  let inner = N.x
  module K = struct
    let inner_too = N.x
  end
end
let outer = N.x
let explicit = M.N.x
|};
  [%expect
    {|
    module N : sig
      val x : int
    end
    module M : sig
      module N : sig
        val x : bool
      end
      val inner : bool
      module K : sig
        val inner_too : bool
      end
    end
    val outer : int
    val explicit : bool
    |}]
;;

let%expect_test "the rest of a path is looked up only in the module" =
  check
    {|
let x = 1
module M = struct end
let y = M.x
|};
  check
    {|
module N = struct
  let x = 1
end
module M = struct end
let y = M.N.x
|};
  check
    {|
module M = struct
  module N = struct end
end
let y = M.N.K.x
|};
  (* On the DSL: source reads [M.x.y] as the field [y] of [M.x]. *)
  check_program
    (parse_exn
       {|
module M = struct
  let x = 1
end
|}
     @ [ Helpers.top "y" [] (Helpers.var "M.x.y") ]);
  check
    {|
type t = A
module M = struct end
let y = M.A
|};
  check
    {|
type t = A
module M = struct end
let y : M.t = A
|};
  check
    {|
module N = struct
  type r = {x : int}
end
module M = struct end
let y r = r.M.N.x
|};
  [%expect
    {|
    ((Not_in_scope M.x) @ t.stag:4:8-11)
    ((Unbound_module M.N) @ t.stag:6:8-11)
    ((Unbound_module M.N.K) @ t.stag:5:8-13)
    (Unbound_module M.x)
    ((Unbound_constructor M.A) @ t.stag:4:8-11)
    ((Unbound_type M.t) @ t.stag:4:8-11)
    ((Unbound_module M.N) @ t.stag:6:12-15)
    |}]
;;

(* On the DSL: source can't name a value or a type with a capital, as [M]. *)
let%expect_test "modules and other members have separate namespaces" =
  let open Helpers in
  check_program
    [ module_ "M" [ top "M" [] (int 1) ]
    ; Decl.variant "M" [ "M", [] ]
    ; top "a" [] (var "M.M")
    ; top "b" [] ~annot:(Ty.con "M") (construct "M")
    ];
  [%expect
    {|
    module M : sig
      val M : int
    end
    type M = M
    val a : int
    val b : M
    |}]
;;

let%expect_test "values are order independent and recursive across modules" =
  check
    {|
let main = (A.even 4, B.later)
module A = struct
  let even n = cond (eq n 0) true (B.odd n)
end
module B = struct
  let odd n = cond (eq n 0) false (A.even n)
  let later = C.value
end
module C = struct
  let value = 'c'
end
|};
  [%expect
    {|
    val main : bool * char
    module A : sig
      val even : int -> bool
    end
    module B : sig
      val odd : int -> bool
      val later : char
    end
    module C : sig
      val value : char
    end
    |}]
;;

let%expect_test "SCCs across modules" =
  check
    {|
module A = struct
  let f x = B.g x
end
module B = struct
  let g x = A.f; x
end
let use = (A.f 1, B.g true)
|};
  check
    {|
module A = struct
  let f x = B.g 1
  let h = f true; B.g
end
module B = struct
  let g y = A.f true; y
end
|};
  check
    {|
module A = struct
  let w = B.g (id id)
end
module B = struct
  let g y = A.w; y
end
|};
  check
    {|
module A = struct
  let w = fun u -> B.g u
end
module B = struct
  let g y = A.w; y
end
|};
  [%expect
    {|
    module A : sig
      val f : 'a -> 'a
    end
    module B : sig
      val g : 'a -> 'a
    end
    val use : int * bool
    module A : sig
      val f : bool -> int
      val h : int -> int
    end
    module B : sig
      val g : int -> int
    end
    ((Toplevel_initialization_cycle (A.w B.g A.w)) @ t.stag:3:10-13)
    module A : sig
      val w : '_a -> '_a
    end
    module B : sig
      val g : '_a -> '_a
    end
    |}]
;;

let%expect_test "qualified and unqualified types, constructors and fields" =
  check
    {|
module M = struct
  type t = A | B of int
  type r = {x : int; y : t}
  let a = A
  let mk n = {x = n; y = B n}
  let get r = r.x
  let annotated v : M.t = v
end
let b = M.B 1
let r = {M.y = M.A; M.x = 2}
let get_x r = r.M.x
let annotated v : M.r = v
let matched v = match v with M.B n -> n | M.A -> 0
let record_pattern r = match r with {M.y = y} -> y
|};
  [%expect
    {|
    module M : sig
      type t = A | B of int
      type r = { x : int; y : t }
      val a : t
      val mk : int -> r
      val get : r -> int
      val annotated : t -> t
    end
    val b : M.t
    val r : M.r
    val get_x : M.r -> int
    val annotated : M.r -> M.r
    val matched : M.t -> int
    val record_pattern : M.r -> M.t
    |}]
;;

let%expect_test "types in different modules are distinct" =
  check
    {|
type t = A
module M = struct
  type t = A
  let inner = A
  let outer : t = M.A
end
let root = A
let qualified = M.A
|};
  check
    {|
type t = A
module M = struct
  type t = A
end
let mixed : t = M.A
|};
  [%expect
    {|
    type t = A
    module M : sig
      type t = A
      val inner : t
      val outer : t
    end
    val root : t
    val qualified : M.t
    ((Unification_error
      (Type_mismatch (TCon t ()) (TCon M.t ()) (Base_types_not_equal t M.t)))
     @ t.stag:6:16-19)
    |}]
;;

let%expect_test "types resolve lexically" =
  check
    {|
type t = R
type u = U
module M = struct
  type t = T
  module N = struct
    type s = S of t * u
    let s = S (T, U)
  end
  let t : t = T
  let root_u : u = U
end
let s = M.N.S (M.T, U)
|};
  [%expect
    {|
    type t = R
    type u = U
    module M : sig
      type t = T
      module N : sig
        type s = S of t * u
        val s : s
      end
      val t : t
      val root_u : u
    end
    val s : M.N.s
    |}]
;;

let%expect_test "types are order independent and recursive across modules" =
  check
    {|
let tree = A.Node (B.Cons (A.Leaf, B.Nil))
module A = struct
  type tree = Leaf | Node of B.forest
end
module B = struct
  type forest = Nil | Cons of A.tree * forest
  type trees = A.tree * A.tree
end
let pair : B.trees = (A.Leaf, tree)
|};
  [%expect
    {|
    val tree : A.tree
    module A : sig
      type tree = Leaf | Node of B.forest
    end
    module B : sig
      type forest = Nil | Cons of A.tree * forest
      type trees = A.tree * A.tree
    end
    val pair : A.tree * A.tree
    |}]
;;

let%expect_test "cyclic aliases across modules" =
  check
    {|
module A = struct
  type t = B.u
end
module B = struct
  type u = int -> A.t
end
|};
  check
    {|
module A = struct
  type t = u
  type u = int
end
type u = A.t
let x : u = 1
|};
  [%expect
    {|
    ((Cyclic_type_alias A.t) @ t.stag:3:7-8)
    module A : sig
      type t = int
      type u = int
    end
    type u = int
    val x : int
    |}]
;;

let%expect_test "builtin types are declared in the root module" =
  check
    {|
type int = I
|};
  check
    {|
type unit = bool
|};
  check
    {|
module M = struct
  type bool = B
end
type bool = int
|};
  check
    {|
module M = struct
  type unit = U
  type int = bool
  let u : unit = U
  let i : int = true
  module N = struct
    let nested : unit = U
  end
end
let root : unit = ()
let qualified : M.unit = M.U
|};
  check
    {|
module M = struct
  type unit = U
  let lit : unit = ()
end
|};
  [%expect
    {|
    ((Duplicate_type int) @ t.stag:2:5-8)
    ((Duplicate_type unit) @ t.stag:2:5-9)
    ((Duplicate_type bool) @ t.stag:5:5-9)
    module M : sig
      type unit = U
      type int = bool
      val u : unit
      val i : bool
      module N : sig
        val nested : unit
      end
    end
    val root : unit
    val qualified : M.unit
    ((Unification_error
      (Type_mismatch (TCon M.unit ()) (TCon unit ())
       (Base_types_not_equal M.unit unit)))
     @ t.stag:4:19-21)
    |}]
;;

let%expect_test "duplicates are per module" =
  check
    {|
module M = struct
  let x = 1
  type t = A
  type r = {f : int}
  module N = struct end
end
module K = struct
  let x = true
  type t = A
  type r = {f : bool}
  module N = struct end
end
let x = ()
type t = A
type r = {f : unit}
module N = struct end
let fields = ({M.f = 1}.M.f, {K.f = true}.K.f)
|};
  check
    {|
module M = struct end
module M = struct end
|};
  check
    {|
module M = struct
  module N = struct end
  module N = struct end
end
|};
  check
    {|
module M = struct
  let x = 1
  let x = 2
end
|};
  check
    {|
module M = struct
  type t = |
  type t = int
end
|};
  check
    {|
module M = struct
  type t = A
  type u = A
end
|};
  check
    {|
module M = struct
  type t = {x : int; x : int}
end
|};
  check
    {|
module M = struct
  type t['a, 'a] = |
end
|};
  [%expect
    {|
    module M : sig
      val x : int
      type t = A
      type r = { f : int }
      module N : sig end
    end
    module K : sig
      val x : bool
      type t = A
      type r = { f : bool }
      module N : sig end
    end
    val x : unit
    type t = A
    type r = { f : unit }
    module N : sig end
    val fields : int * bool
    ((Duplicate_module M) @ t.stag:3:7-8)
    ((Duplicate_module M.N) @ t.stag:4:9-10)
    ((Duplicate_toplevel M.x) @ t.stag:4:6-7)
    ((Duplicate_type M.t) @ t.stag:4:7-8)
    ((Duplicate_constructor M.A) @ t.stag:4:11-12)
    ((Duplicate_field M.x) @ t.stag:3:21-22)
    ((Duplicate_type_parameter (type_name M.t) (param a)) @ t.stag:3:13-15)
    |}]
;;

let%expect_test "unbound modules and members" =
  check
    {|
let v = M.x
|};
  check
    {|
let v : M.t = 1
|};
  check
    {|
let c = M.A
|};
  check
    {|
let f p = p.M.x
|};
  check
    {|
let r = {M.x = 1}
|};
  check
    {|
let p v = match v with M.A -> 1
|};
  check
    {|
let p v = match v with {M.x = _} -> 1
|};
  check
    {|
type t = A of M.t
|};
  check
    {|
module M = struct end
let v = M.x
|};
  check
    {|
module M = struct end
let v : M.t = 1
|};
  check
    {|
module M = struct end
let c = M.A
|};
  check
    {|
module M = struct end
let f p = p.M.x
|};
  check
    {|
module M = struct
  let x = y
end
|};
  [%expect
    {|
    ((Unbound_module M) @ t.stag:2:8-9)
    ((Unbound_module M) @ t.stag:2:8-9)
    ((Unbound_module M) @ t.stag:2:8-9)
    ((Unbound_module M) @ t.stag:2:12-13)
    ((Unbound_module M) @ t.stag:2:9-10)
    ((Unbound_module M) @ t.stag:2:23-24)
    ((Unbound_module M) @ t.stag:2:24-25)
    ((Unbound_module M) @ t.stag:2:14-15)
    ((Not_in_scope M.x) @ t.stag:3:8-11)
    ((Unbound_type M.t) @ t.stag:3:8-11)
    ((Unbound_constructor M.A) @ t.stag:3:8-11)
    ((Unbound_field M.x) @ t.stag:3:12-15)
    ((Not_in_scope y) @ t.stag:3:10-11)
    |}]
;;

let%expect_test "errors name canonical constructors, fields and types" =
  let m =
    {|
module M = struct
  type t = A of int
  type r = {x : int; y : int}
  type s = {z : int}
  type p['a] = |
end
|}
  in
  List.iter
    [ "let a = M.A"
    ; "let r = {M.x = 1}"
    ; "let r = {M.x = 1; M.y = 1; M.x = 1}"
    ; "let r = {M.x = 1; M.z = 1}"
    ; "let p : M.p = 1"
    ; "module M2 = struct let x = M.A true end"
    ]
    ~f:(fun source -> check (m ^ source));
  [%expect
    {|
    ((Constructor_arity_mismatch (constructor M.A) (expected 1) (got 0)) @
     t.stag:8:8-11)
    ((Missing_field M.y) @ t.stag:8:8-17)
    ((Repeated_field x) @ t.stag:8:27-30)
    ((Fields_of_different_types (field M.z) (expected_type M.r)
      (actual_type M.s))
     @ t.stag:8:18-21)
    ((Type_arity_mismatch (type_name M.p) (expected 1) (got 0)) @ t.stag:8:8-11)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:8:31-35)
    |}]
;;

let%expect_test "persistent toplevels of modules are usable in quotes" =
  check
    {|
module M = struct
  let f x = add x 1
  let inner = .<f 1>.
end
let outer = .<M.f 1>.
|};
  [%expect
    {|
    module M : sig
      val f : int -> int
      val inner : <int>{..'a}
    end
    val outer : <int>{..'a}
    |}]
;;

let%expect_test "non-persistent toplevels are free variables named canonically" =
  check
    {|
let n = true
module M = struct
  let n = 5
  let q u = .<n>.
  let both = .<(n, M.n)>.
  let local = .<let n = () in n>.
  let root_code = .<.~(q_root ())>.
end
let q u = .<M.n>.
let q_root u = .<n>.
let spliced = .<.~(M.q ())>.
let not_captured = .<let n = () in .~(M.q ())>.
let captured = .<let n = () in .~(q_root ())>.
|};
  [%expect
    {|
    val n : bool
    module M : sig
      val n : int
      val q : 'a -> <'b>{M.n : 'b; ..'c}
      val both : <'a * 'a>{M.n : 'a; ..'b}
      val local : <unit>{..'a}
      val root_code : <'_a>{n : '_a; ..'_b}
    end
    val q : 'a -> <'b>{M.n : 'b; ..'c}
    val q_root : 'a -> <'b>{n : 'b; ..'c}
    val spliced : <'_a>{M.n : '_a; ..'_b}
    val not_captured : <'_a>{M.n : '_a; ..'_b}
    val captured : <unit>{..'_a}
    |}]
;;

let%expect_test "splicing code with canonically named free variables" =
  (* [explicit] and [shorthand] on the DSL: source can't write a qualified name
     in a splice's substitutions. *)
  check_program
    (parse_exn
       {|
module M = struct
  let n = 5
end
let implicit = .~.<M.n>.
let local_does_not_provide n = cond n () (); .~.<M.n>.
|}
     @ Helpers.
         [ top
             "explicit"
             []
             (splice ~subs:[ "M.n", Some (string "s") ] (quote (var "M.n")))
         ; top "shorthand" [] (splice ~subs:[ "M.n", None ] (quote (var "M.n")))
         ]);
  check
    {|
module M = struct
  let n = 5
  let as_written = .~.<n>..{n}
end
|};
  [%expect
    {|
    module M : sig
      val n : int
    end
    val implicit : int
    val local_does_not_provide : bool -> int
    val explicit : string
    val shorthand : int
    module M : sig
      val n : int
      val as_written : int
    end
    |}]
;;

let%expect_test "toplevels of modules used at compile time" =
  check
    {|
module M = struct
  let n = 1
  let code = .<add n 1>.
end
let x = .~M.code
|};
  check
    {|
module M = struct
  let n = 1
end
let x = .~M.n
|};
  [%expect
    {|
    module M : sig
      val n : int
      val code : <int>{M.n : int; ..'a}
    end
    val x : int
    ((Toplevel_splice_of_neither (TCon int ())) @ t.stag:5:10-13)
    |}]
;;
