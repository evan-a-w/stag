open! Import
open Source_helpers

(* The source of [toplevels], one per line. *)
let lines toplevels = String.concat ("" :: toplevels) ~sep:"\n" ^ "\n"

let%expect_test "a generator used by a toplevel splice" =
  run
    {|
type nat = Z | S of ptr[nat]
type list['a] = Nil | Cons of ptr['a * list['a]]
let power n c = match n with Z -> .<Nil>. | S * m -> .<Cons (Ptr.make ((.~c, .~(power m c))))>.
let three = S (Ptr.make (S (Ptr.make (S (Ptr.make Z)))))
let cube x = .~(power three .<x>.)
let used = cube 7
|};
  [%expect
    {|
    type nat = Z | S of ptr[nat]
    type list['a] = Nil | Cons of ptr['a * list['a]]
    val power : nat -> <'a>{..'b} -> <list['a]>{..'c} where ..'b <= {..'c}
    val three : nat
    val cube : 'a -> list['a]
    val used : list[int]
    -- evaluated at compile time
    (power <fun>)
    (three (S (Ptr (S (Ptr (S (Ptr (Z))))))))
    -- evaluated
    (power <fun>)
    (three (S (Ptr (S (Ptr (S (Ptr (Z))))))))
    (cube <fun>)
    (used
     (Cons (Ptr (Tuple 7 (Cons (Ptr (Tuple 7 (Cons (Ptr (Tuple 7 (Nil)))))))))))
    |}]
;;

let%expect_test "re-inferred types are more precise than the quote's" =
  run
    ~bodies:[ "wrapped" ]
    {|
let wrap c = .<let y = .~c in (y, y)>.
let wrapped = .~(wrap .<1>.)
let generated = wrap .<1>.
|};
  [%expect
    {|
    val wrap : <'a>{y?'b : immut 'a; ..'c} -> <'a * 'a>{..'d} where ..'c <= {..'d}
    val wrapped : int * int
    val generated : <int * int>{..'_a}
    (wrapped/5
     (Toplevel_splice
      (code
       (App
        (Var (Bound (Toplevel wrap/4)) :
         "<int>{y?'_a : immut int; ..'_b} -> <int * int>{}")
        (Quote (Lit (Int 1) : int) : "<int>{y?'_a : immut int; ..'_b}") :
        "<int * int>{}"))
      (expansion
       (Let (ident y/18) (args ()) (generalized true) (body (Lit (Int 1) : int))
        (in_
         (Tuple
          ((Var (Bound (Local y/18)) : int) (Var (Bound (Local y/18)) : int)) :
          "int * int"))
        : "int * int"))
      : "int * int" (where "..'_b <= {}")))
    -- evaluated at compile time
    (wrap <fun>)
    -- evaluated
    (wrap <fun>)
    (wrapped (Tuple 1 1))
    (generated
     (Code
      (Let (ident y/17) (args ()) (generalized false) (body (Lit (Int 1) : int))
       (in_
        (Tuple ((Var (Bound (Local y/17)) : 'a) (Var (Bound (Local y/17)) : 'a))
         : "'a * 'a"))
       : "'a * 'a")))
    |}]
;;

let%expect_test "mutually dependent compile time helpers" =
  run
    {|
type nat = Z | S of ptr[nat]
let even n = match n with Z -> .<true>. | S * m -> odd m
let odd n = match n with Z -> .<false>. | S * m -> even m
let five_is_odd = .~(odd (S (Ptr.make (S (Ptr.make (S (Ptr.make (S (Ptr.make (S (Ptr.make Z)))))))))))
|};
  [%expect
    {|
    type nat = Z | S of ptr[nat]
    val even : nat -> <bool>{..'a}
    val odd : nat -> <bool>{..'a}
    val five_is_odd : bool
    -- evaluated at compile time
    (even <fun>)
    (odd <fun>)
    -- evaluated
    (even <fun>)
    (odd <fun>)
    (five_is_odd true)
    |}]
;;

let%expect_test "a toplevel used at both levels" =
  run
    {|
let pair x = (x, x)
let at_runtime = pair 1
let at_compile_time = .~(match pair .<2>. with (a, _) -> a)
let in_generated_code = .~(.<pair true>.)
|};
  [%expect
    {|
    val pair : 'a -> 'a * 'a
    val at_runtime : int * int
    val at_compile_time : int
    val in_generated_code : bool * bool
    -- evaluated at compile time
    (pair <fun>)
    -- evaluated
    (pair <fun>)
    (at_runtime (Tuple 1 1))
    (at_compile_time 2)
    (in_generated_code (Tuple true true))
    |}]
;;

let%expect_test "compile time use of a toplevel being checked" =
  run {|
let f x = .~(f; .<x>.)
|};
  run {|
let f x = g x
let g x = .~(f; .<x>.)
|};
  run {|
let f x = .~(.<f x>.)
|};
  [%expect
    {|
    ((Compile_time_use_of_toplevel_being_checked f) @ t.stag:2:13-14)
    ((Compile_time_use_of_toplevel_being_checked f) @ t.stag:3:13-14)
    val f : 'a -> 'b
    -- evaluated at compile time
    -- evaluated
    (f <fun>)
    |}]
;;

let%expect_test "level 0 locals are not usable at compile time" =
  run {|
let f x = .~(x; .<1>.)
|};
  run {|
let f = let x = 1 in .~(x; .<1>.)
|};
  [%expect
    {|
    ((Variable_used_at_wrong_level (name x) (var_level 0) (used_at -1)) @
     t.stag:2:13-14)
    ((Variable_used_at_wrong_level (name x) (var_level 0) (used_at -1)) @
     t.stag:2:24-25)
    |}]
;;

let%expect_test "toplevels are only evaluated if used at compile time" =
  run
    ~eval:false
    {|
type abc = A | B
let fails = match B with A -> 0
let loops = let f x = f x in f ()
let unused_code = .<0>.
let used_code = .<1>.
let spliced = .~used_code
|};
  run
    {|
type abc = A | B
let fails = match B with A -> 0
let spliced = .~(.<1>.)
|};
  run
    {|
type abc = A | B
let fails = match B with A -> 0
let spliced = .~(fails; .<1>.)
|};
  [%expect
    {|
    type abc = A | B
    val fails : int
    val loops : '_a
    val unused_code : <int>{..'a}
    val used_code : <int>{..'a}
    val spliced : int
    -- evaluated at compile time
    (used_code (Code (Lit (Int 1) : int)))
    type abc = A | B
    val fails : int
    val spliced : int
    -- evaluated at compile time
    -- evaluated
    ("Match failure: no case matches the value B" @ t.stag:3:12-31)
    ((Splice_evaluation_failed
      (code
       (Let (ident _/15) (args ()) (generalized true)
        (body (Var (Bound (Toplevel fails/4)) : int))
        (in_ (Quote (Lit (Int 1) : int) : <int>{})) : <int>{}))
      (error ("Match failure: no case matches the value B" @ t.stag:3:12-31)))
     @ t.stag:4:14-30)
    |}]
;;

let%expect_test "splice evaluation failures" =
  run {|
type abc = A | B
let x = .~(match B with A -> .<0>.)
|};
  [%expect
    {|
    ((Splice_evaluation_failed
      (code
       (Match
        (scrutinee (Construct ((type_name abc) (name B) (index 1)) () : abc))
        (cases
         (((Construct ((type_name abc) (name A) (index 0)) () : abc)
           (Quote (Lit (Int 0) : int) : <int>{}))))
        : <int>{}))
      (error ("Match failure: no case matches the value B" @ t.stag:3:10-35)))
     @ t.stag:3:8-35)
    |}]
;;

let%expect_test "substitutions at toplevel splices" =
  run
    ~bodies:[ "implicit"; "explicit"; "explicit_toplevel" ]
    {|
let n = 3
let code = .<(n, x)>.
let implicit x = .~code
let explicit = .~code.{n = true; x = 1}
let explicit_toplevel = .~code.{n; x = n}
let applied = implicit false
|};
  [%expect
    {|
    val n : int
    val code : <int * 'a>{x : 'a; ..'b}
    val implicit : 'a -> int * 'a
    val explicit : int * int
    val explicit_toplevel : int * int
    val applied : int * bool
    (implicit/6
     (Toplevel_splice
      (code (Var (Bound (Toplevel code/5)) : "<int * 'a>{x : immut 'a}"))
      (expansion
       (Tuple
        ((Var (Bound (Toplevel n/4)) : int) (Var (Bound (Local x/19)) : 'a)) :
        "int * 'a"))
      : "int * 'a"))
    (explicit/7
     (Toplevel_splice
      (code
       (Var (Bound (Toplevel code/5)) :
        "<int * int>{n?'_a : immut bool; x : immut int}"))
      (expansion
       (Let (ident n/20) (args ()) (generalized false)
        (body (Lit (Bool true) : bool))
        (in_
         (Let (ident x/21) (args ()) (generalized false)
          (body (Lit (Int 1) : int))
          (in_
           (Tuple
            ((Var (Bound (Toplevel n/4)) : int) (Var (Bound (Local x/21)) : int))
            : "int * int"))
          : "int * int"))
        : "int * int"))
      : "int * int"))
    (explicit_toplevel/8
     (Toplevel_splice
      (code
       (Var (Bound (Toplevel code/5)) :
        "<int * int>{n?'_a : immut int; x : immut int}"))
      (expansion
       (Tuple
        ((Var (Bound (Toplevel n/4)) : int) (Var (Bound (Toplevel n/4)) : int)) :
        "int * int"))
      : "int * int"))
    -- evaluated at compile time
    (code
     (Code
      (Tuple ((Var (Persistent (Toplevel n/4)) : int) (Var (Free x) : 'a)) :
       "int * 'a")))
    -- evaluated
    (n 3)
    (code
     (Code
      (Tuple ((Var (Persistent (Toplevel n/4)) : int) (Var (Free x) : 'a)) :
       "int * 'a")))
    (implicit <fun>)
    (explicit (Tuple 3 1))
    (explicit_toplevel (Tuple 3 3))
    (applied (Tuple 3 false))
    |}]
;;

let%expect_test "nested toplevel splices" =
  run
    ~bodies:[ "nested" ]
    {|
let inner = .< .<1>. >.
let nested = .~(.~inner)
let in_quote = .<.~(.~inner)>.
|};
  [%expect
    {|
    val inner : <<int>{..'a}>{..'b}
    val nested : int
    val in_quote : <int>{..'a}
    (nested/5
     (Toplevel_splice
      (code
       (Toplevel_splice (code (Var (Bound (Toplevel inner/4)) : <<int>{}>{}))
        (expansion (Quote (Lit (Int 1) : int) : <int>{})) : <int>{}))
      (expansion (Lit (Int 1) : int)) : int))
    -- evaluated at compile time
    (inner (Code (Quote (Lit (Int 1) : int) : <int>{..'a})))
    -- evaluated
    (inner (Code (Quote (Lit (Int 1) : int) : <int>{..'a})))
    (nested 1)
    (in_quote (Code (Lit (Int 1) : int)))
    |}]
;;

let%expect_test "compile time values embedded in generated code" =
  run
    ~bodies:[ "embeds" ]
    {|
let embeds = .~(let k x = (x, 1) in .<(k true, k 2)>.)
|};
  [%expect
    {|
    val embeds : (bool * int) * (int * int)
    (embeds/4
     (Toplevel_splice
      (code
       (Let (ident k/15) (args (x/14)) (generalized true)
        (body
         (Tuple ((Var (Bound (Local x/14)) : 'a) (Lit (Int 1) : int)) :
          "'a * int"))
        (in_
         (Quote
          (Tuple
           ((App (Var (Persistent (Local k/15)) : "bool -> bool * int")
             (Lit (Bool true) : bool) : "bool * int")
            (App (Var (Persistent (Local k/15)) : "int -> int * int")
             (Lit (Int 2) : int) : "int * int"))
           : "(bool * int) * (int * int)")
          : "<(bool * int) * (int * int)>{}"))
        : "<(bool * int) * (int * int)>{}"))
      (expansion
       (Tuple
        ((App (Var (Persistent (Local k/16)) : "bool -> bool * int")
          (Lit (Bool true) : bool) : "bool * int")
         (App (Var (Persistent (Local k/16)) : "int -> int * int")
          (Lit (Int 2) : int) : "int * int"))
        : "(bool * int) * (int * int)"))
      : "(bool * int) * (int * int)"))
    -- evaluated at compile time
    -- evaluated
    (embeds (Tuple (Tuple true 1) (Tuple 2 1)))
    |}]
;;

let%expect_test "closures made at compile time read the toplevels they capture \
                 only when called"
  =
  run
    ~eval:false
    {|
type abc = A | B
let fails = match B with A -> 0
let n = 3
let embeds =
  .~(let never_called x = x; fails in
     let k x = (x, n) in
     .<k true>.)
|};
  run {|
let n = 3
let embeds = .~(let k x = (x, n) in .<k true>.)
|};
  [%expect
    {|
    type abc = A | B
    val fails : int
    val n : int
    val embeds : bool * int
    -- evaluated at compile time
    val n : int
    val embeds : bool * int
    -- evaluated at compile time
    -- evaluated
    (n 3)
    (embeds (Tuple true 3))
    |}]
;;

let%expect_test "spliced code using the toplevel being checked" =
  run
    ~bodies:[ "f" ]
    {|
type nat = Z | S of ptr[nat]
let f n = match n with Z -> 0 | S * m -> .~code
let code = .<f m>.
let used = f (S (Ptr.make (S (Ptr.make Z))))
|};
  (* [f] must be checked to know if [code] embeds it, which needs [code]. *)
  run
    {|
type nat = Z | S of ptr[nat]
let code = .<f m>.
let f n = match n with Z -> 0 | S * m -> .~code
let used = f (S (Ptr.make (S (Ptr.make Z))))
|};
  [%expect
    {|
    type nat = Z | S of ptr[nat]
    val f : nat -> int
    val code : <'a>{f : 'b -> 'a; m : 'b; ..'c}
    val used : int
    (f/4
     (Match (scrutinee (Var (Bound (Local n/16)) : nat))
      (cases
       (((Construct ((type_name nat) (name Z) (index 0)) () : nat)
         (Lit (Int 0) : int))
        ((Construct ((type_name nat) (name S) (index 1))
          ((Deref (Var m/17 : nat) : ptr[nat])) : nat)
         (Toplevel_splice
          (code
           (Var (Bound (Toplevel code/5)) :
            "<int>{f : immut nat -> int; m : immut nat}"))
          (expansion
           (App (Var (Bound (Toplevel f/4)) : "nat -> int")
            (Var (Bound (Local m/17)) : nat) : int))
          : int))))
      : int))
    -- evaluated at compile time
    (code
     (Code (App (Var (Free_toplevel f) : "'a -> 'b") (Var (Free m) : 'a) : 'b)))
    -- evaluated
    (code
     (Code (App (Var (Free_toplevel f) : "'a -> 'b") (Var (Free m) : 'a) : 'b)))
    (f <fun>)
    (used 0)
    ((Compile_time_use_of_toplevel_being_checked code) @ t.stag:4:43-47)
    |}]
;;

let%expect_test "maybe-used entries at a toplevel splice" =
  (* [gen]'s result may use [f], so [f?] is in the context of the code spliced
     at level -1. *)
  run {|
let gen c = .<let f = 1 in .~c>.; c
let f = .~(.~(gen .< .<1>. >.))
|};
  (* Checking [f] on demand would need [x] at compile time. *)
  run
    {|
let gen c = .<let f = 1 in .~c>.; c
let x = .~(.~(gen .< .<1>. >.))
let f = .~(x; .<2>.)
|};
  [%expect
    {|
    val gen : <'a>{f?'b : immut int; ..'c} -> <'a>{f?'b : immut int; ..'c}
    val f : int
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (gen <fun>)
    (f 1)
    val gen : <'a>{f?'b : immut int; ..'c} -> <'a>{f?'b : immut int; ..'c}
    val x : int
    val f : int
    -- evaluated at compile time
    (gen <fun>)
    (x 1)
    -- evaluated
    (gen <fun>)
    (x 1)
    (f 2)
    |}]
;;

let%expect_test "toplevels of a component are evaluated separately" =
  run
    {|
type abc = A | B
let f x = v; .<x>.
let v = f; match B with A -> 0
let g y = .~(f; .<y>.)
|};
  (* [a] needs [b], computed when first needed, which only refers to [a] through
     [k] without calling it. *)
  run {|
let a = b; 1
let b = k; 2
let k u = u; a
|};
  [%expect
    {|
    type abc = A | B
    val f : '_a -> <'_b>{x : '_b; ..'_c}
    val v : int
    val g : 'a -> 'a
    -- evaluated at compile time
    (f <fun>)
    -- evaluated
    (f <fun>)
    ("Match failure: no case matches the value B" @ t.stag:4:11-30)
    val a : int
    val b : int
    val k : '_a -> int
    -- evaluated at compile time
    -- evaluated
    (a 1)
    (b 2)
    (k <fun>)
    |}]
;;

let%expect_test "a splice needing a toplevel whose checking runs a splice" =
  run
    {|
let user = .~(gen ())
let gen u = helper; .<helper>.
let helper = .~(gen2 ())
let gen2 u = .<5>.
|};
  [%expect
    {|
    val user : int
    val gen : 'a -> <int>{..'b}
    val helper : int
    val gen2 : 'a -> <int>{..'b}
    -- evaluated at compile time
    (gen2 <fun>)
    (helper 5)
    (gen <fun>)
    -- evaluated
    (gen2 <fun>)
    (helper 5)
    (gen <fun>)
    (user 5)
    |}]
;;

let%expect_test "constructors and fields resolve where quoted" =
  run
    ~bodies:[ "M.x"; "M.y" ]
    {|
type t = A | B
type r = {a : int}
let code = .<(B, {a = 1})>.
module M = struct
  type t = B | C
  type s = {a : bool}
  let x = .~code
  let y = match .~code with (A, _) -> 0 | _ -> 1
end
|};
  [%expect
    {|
    type t = A | B
    type r = { a : int }
    val code : <t * r>{..'a}
    module M : sig
      type t = B | C
      type s = { a : bool }
      val x : .t * r
      val y : int
    end
    (M.x/5
     (Toplevel_splice (code (Var (Bound (Toplevel code/4)) : "<t * r>{}"))
      (expansion
       (Tuple
        ((Construct ((type_name t) (name B) (index 1)) () : t)
         (Record ((((type_name r) (name a) (index 0)) (Lit (Int 1) : int))) : r))
        : "t * r"))
      : "t * r"))
    (M.y/6
     (Match
      (scrutinee
       (Toplevel_splice (code (Var (Bound (Toplevel code/4)) : "<t * r>{}"))
        (expansion
         (Tuple
          ((Construct ((type_name t) (name B) (index 1)) () : t)
           (Record ((((type_name r) (name a) (index 0)) (Lit (Int 1) : int))) :
            r))
          : "t * r"))
        : "t * r"))
      (cases
       (((Tuple ((Construct ((type_name t) (name A) (index 0)) () : t) (Any : r))
          : "t * r")
         (Lit (Int 0) : int))
        ((Any : "t * r") (Lit (Int 1) : int))))
      : int))
    -- evaluated at compile time
    (code
     (Code
      (Tuple
       ((Construct ((type_name t) (name B) (index 1)) () : t)
        (Record ((((type_name r) (name a) (index 0)) (Lit (Int 1) : int))) : r))
       : "t * r")))
    -- evaluated
    (code
     (Code
      (Tuple
       ((Construct ((type_name t) (name B) (index 1)) () : t)
        (Record ((((type_name r) (name a) (index 0)) (Lit (Int 1) : int))) : r))
       : "t * r")))
    (M.x (Tuple (B) (r (a 1))))
    (M.y 1)
    |}]
;;

let%expect_test "splicing polymorphic code twice" =
  run
    ~bodies:[ "twice" ]
    {|
let id_code = .<let f x = x in f>.
let twice = (.~id_code 1, .~id_code true)
|};
  [%expect
    {|
    val id_code : <'a -> 'a>{..'b}
    val twice : int * bool
    (twice/5
     (Tuple
      ((App
        (Toplevel_splice
         (code (Var (Bound (Toplevel id_code/4)) : "<int -> int>{}"))
         (expansion
          (Let (ident f/17) (args (x/18)) (generalized true)
           (body (Var (Bound (Local x/18)) : 'a))
           (in_ (Var (Bound (Local f/17)) : "int -> int")) : "int -> int"))
         : "int -> int")
        (Lit (Int 1) : int) : int)
       (App
        (Toplevel_splice
         (code (Var (Bound (Toplevel id_code/4)) : "<bool -> bool>{}"))
         (expansion
          (Let (ident f/19) (args (x/20)) (generalized true)
           (body (Var (Bound (Local x/20)) : 'b))
           (in_ (Var (Bound (Local f/19)) : "bool -> bool")) : "bool -> bool"))
         : "bool -> bool")
        (Lit (Bool true) : bool) : bool))
      : "int * bool"))
    -- evaluated at compile time
    (id_code
     (Code
      (Let (ident f/16) (args (x/15)) (generalized true)
       (body (Var (Bound (Local x/15)) : 'a))
       (in_ (Var (Bound (Local f/16)) : "'b -> 'b")) : "'b -> 'b")))
    -- evaluated
    (id_code
     (Code
      (Let (ident f/16) (args (x/15)) (generalized true)
       (body (Var (Bound (Local x/15)) : 'a))
       (in_ (Var (Bound (Local f/16)) : "'b -> 'b")) : "'b -> 'b")))
    (twice (Tuple 1 true))
    |}]
;;

let%expect_test "toplevel splices are generalized by their expansion" =
  run
    {|
let id_code = .<let f x = x in f>.
let spliced_id = .~id_code
let used = (spliced_id 1, spliced_id true)
let spliced_app = .~(.<id id>.)
|};
  [%expect
    {|
    val id_code : <'a -> 'a>{..'b}
    val spliced_id : 'a -> 'a
    val used : int * bool
    val spliced_app : '_a -> '_a
    -- evaluated at compile time
    (id_code
     (Code
      (Let (ident f/18) (args (x/17)) (generalized true)
       (body (Var (Bound (Local x/17)) : 'a))
       (in_ (Var (Bound (Local f/18)) : "'b -> 'b")) : "'b -> 'b")))
    -- evaluated
    (id_code
     (Code
      (Let (ident f/18) (args (x/17)) (generalized true)
       (body (Var (Bound (Local x/17)) : 'a))
       (in_ (Var (Bound (Local f/18)) : "'b -> 'b")) : "'b -> 'b")))
    (spliced_id <fun>)
    (used (Tuple 1 true))
    (spliced_app <fun>)
    |}]
;;

let%expect_test "toplevels used at level -2" =
  run ~bodies:[ "deep" ] {|
let n = 2
let deep = .~(.~(n; .< .<n>. >.))
|};
  [%expect
    {|
    val n : int
    val deep : int
    (deep/5
     (Toplevel_splice
      (code
       (Toplevel_splice
        (code
         (Let (ident _/15) (args ()) (generalized true)
          (body (Var (Bound (Toplevel n/4)) : int))
          (in_
           (Quote (Quote (Var (Persistent (Toplevel n/4)) : int) : <int>{}) :
            <<int>{}>{}))
          : <<int>{}>{}))
        (expansion (Quote (Var (Persistent (Toplevel n/4)) : int) : <int>{})) :
        <int>{}))
      (expansion (Var (Bound (Toplevel n/4)) : int)) : int))
    -- evaluated at compile time
    (n 2)
    -- evaluated
    (n 2)
    (deep 2)
    |}]
;;

let%expect_test "re-inference keeps the providers of implicit splices" =
  (* The annotation closes [c]'s context, so [x] doesn't provide its entries and
     is generalized. Without the annotation, re-inferring the generated code
     would make [x] a provider. *)
  run
    {|
let code u =
  u;
  .<let h c = .<let x = let g a = .~(c : <int>{}) in g in (x 1, x true)>. in h>.
let user = .~(code ())
let used = .~(user .<5>.)
|};
  [%expect
    {|
    val code : 'a -> <<int>{} -> <int * int>{..'b}>{..'c}
    val user : <int>{} -> <int * int>{..'a}
    val used : int * int
    -- evaluated at compile time
    (code <fun>)
    (user <fun>)
    -- evaluated
    (code <fun>)
    (user <fun>)
    (used (Tuple 5 5))
    |}]
;;

let%expect_test "re-inference keeps the providers of entries already known" =
  (* Re-inferring the first splice gives [c]'s context an entry [x], which the
     second splice must not provide. *)
  run
    {|
let code u =
  u;
  .<let h c =
      .<let x = 1 in .~c>.;
      .<let x = let g a = .~(c : <int>{}) in g in (x 1, x true)>.
    in
    h>.
let user = .~(code ())
let used = .~(user .<5>.)
|};
  [%expect
    {|
    val code : 'a -> <<int>{} -> <int * int>{..'b}>{..'c}
    val user : <int>{} -> <int * int>{..'a}
    val used : int * int
    -- evaluated at compile time
    (code <fun>)
    (user <fun>)
    -- evaluated
    (code <fun>)
    (user <fun>)
    (used (Tuple 5 5))
    |}]
;;

(* On the DSL: source can't write an empty match. *)
let%expect_test "re-inference of empty matches on empty nominal types" =
  let open Helpers in
  let program =
    [ Decl.variant "void" []
    ; top
        "code"
        [ "u" ]
        (seq
           (var "u")
           (quote
              (expr_let
                 ( "f"
                 , [ typed "x" (Ty.con "void") ]
                 , None
                 , match_ (var "x") []
                 , var "f" ))))
    ; top "user" [] (splice (app (var "code") [ unit ]))
    ]
  in
  run_program program;
  [%expect
    {|
    type void = |
    val code : 'a -> <void -> 'b>{..'c}
    val user : void -> 'a
    -- evaluated at compile time
    (code <fun>)
    -- evaluated
    (code <fun>)
    (user <fun>)
    |}]
;;

let%expect_test "maybe-used entries are not provided by checked toplevels" =
  run
    {|
let gen c = .<let f = 1 in .~c>.; c
let f = true
let x = .~(.~(gen .< .<1>. >.))
|};
  run
    {|
let gen c = .<let f = 1 in .~c>.; c
let x = .~(.~(gen .< .<1>. >.))
let f = true
|};
  [%expect
    {|
    val gen : <'a>{f?'b : immut int; ..'c} -> <'a>{f?'b : immut int; ..'c}
    val f : bool
    val x : int
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (gen <fun>)
    (f true)
    (x 1)
    val gen : <'a>{f?'b : immut int; ..'c} -> <'a>{f?'b : immut int; ..'c}
    val x : int
    val f : bool
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (gen <fun>)
    (x 1)
    (f true)
    |}]
;;

let%expect_test "re-inference without annotations meets presence constraints \
                 in another order"
  =
  (* [f]'s annotation decides that [c] doesn't use [x] before [c2]'s quote uses
     it; re-inferred without it, the quote's use comes first. *)
  run
    {|
let gen u =
  u;
  .<let g d = .<let x = 1 in .~d>.; d in
    let f (c : <int>{}) = let c2 = g c in .<add .~c2 x>. in
    f>.
let user = .~(gen ())
|};
  [%expect
    {|
    val gen : 'a -> <<int>{} -> <int>{x : int; ..'b}>{..'c}
    val user : <int>{} -> <int>{x : int; ..'a}
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (gen <fun>)
    (user <fun>)
    |}]
;;

let%expect_test "weak generators decide presences at their first toplevel \
                 splice"
  =
  (* [w]'s presence of [x] is weak. Splicing [w <1>] at toplevel forces it
     [Absent], as nothing there provides an [x] the code only may use, so a
     later [w <x>] is rejected; splicing [w <x>] first makes it [Present]. *)
  run
    {|
let x = 5
let k c = .<let x = 1 in .~c>.; c
let w = id k
let s = .~(w .<1>.)
let t = .~(w .<x>.)
|};
  run
    {|
let x = 5
let k c = .<let x = 1 in .~c>.; c
let w = id k
let t = .~(w .<x>.)
let s = .~(w .<1>.)
|};
  [%expect
    {|
    val x : int
    val k : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'b : immut int; ..'c}
    val w : <int>{} -> <int>{}
    val s : int
    val t : int
    -- evaluated at compile time
    (k <fun>)
    (w <fun>)
    -- evaluated
    (x 5)
    (k <fun>)
    (w <fun>)
    (s 1)
    (t 5)
    val x : int
    val k : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'b : immut int; ..'c}
    val w : <int>{} -> <int>{}
    val t : int
    val s : int
    -- evaluated at compile time
    (k <fun>)
    (w <fun>)
    -- evaluated
    (x 5)
    (k <fun>)
    (w <fun>)
    (t 5)
    (s 1)
    |}]
;;

let%expect_test "splices of code from a generator with presence bounds" =
  let gen = {|let gen c = .<let f = 1 in .~c>.; c|} in
  let f = {|let f = 7|} in
  let x = {|let x = .~(gen .<add f 1>.)|} in
  let y = {|let y = .~(.~(gen .< .<1>. >.))|} in
  (* Every order of the toplevels checks and evaluates. *)
  List.iter
    [ [ gen; f; x; y ]; [ x; y; gen; f ]; [ f; y; x; gen ]; [ y; f; gen; x ] ]
    ~f:(fun toplevels ->
      Option.iter
        (typecheck (lines toplevels))
        ~f:(fun t ->
          print_s
            [%sexp
              (List.map (members t) ~f:(fun m -> m.ident.name) : string list)
              , (Driver.eval ~limits (driver t) : unit Or_error.t)]));
  [%expect
    {|
    ((gen f x y) (Ok ()))
    ((gen f x y) (Ok ()))
    ((f gen y x) (Ok ()))
    ((gen y f x) (Ok ()))
    |}]
;;

let%expect_test "re-inference of a local polymorphic in a presence bounded by \
                 an annotation"
  =
  (* [h]'s [d] may use [x] only if [e] does, which its annotation decides before
     [h] is generalized, but re-inference only after. *)
  run
    {|
let closeit (c : <int>{}) = ()
let gen u =
  u;
  .<let f (e : <int>{x : int}) k =
      let h d = .<let x = 1 in .~d>.; cond true .<.~d>. e in
      h .<add x 1>.; h k; closeit k
    in
    f>.
let user = .~(gen ())
|};
  [%expect
    {|
    val closeit : <int>{} -> unit
    val gen : 'a -> <<int>{x : int} -> <int>{} -> unit>{..'b}
    val user : <int>{x : int} -> <int>{} -> unit
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (closeit <fun>)
    (gen <fun>)
    (user <fun>)
    |}]
;;

(* [tw c] bounds [c]'s row by a row with [x] and an unknown tail, so a local [h]
   using [tw e1] is bounded by [e1]'s row. *)
let tw = {|let tw c = .<let x = 1 in .~c>.; .<.~c>.|}
let needx = {|let needx (e : <int>{x : int}) = ()|}
let closeit = {|let closeit (c : <int>{}) = ()|}

let%expect_test "re-inference of a local bounded by an outer row" =
  let variants =
    [ "<int>{x : int}", needx, "needx"; "<int>{}", closeit, "closeit" ]
  in
  List.iter variants ~f:(fun (annot, use_k_decl, use_k) ->
    let f e1 =
      sprintf
        {|let f %s k =
    let h d = cond true (tw e1) d in
    h .<add x y>.; h k; %s k
  in
  f|}
        e1
        use_k
    in
    let annotated = f (sprintf "(e1 : %s)" annot) in
    print_endline "-- annotated";
    run ~eval:false (lines [ tw; use_k_decl; "let direct =\n  " ^ annotated ]);
    print_endline "-- not annotated";
    run ~eval:false (lines [ tw; use_k_decl; "let direct =\n  " ^ f "e1" ]);
    print_endline "-- generated";
    run
      (lines
         [ tw
         ; use_k_decl
         ; "let gen u = u; .<" ^ annotated ^ ">."
         ; "let user = .~(gen ())"
         ]));
  [%expect
    {|
    -- annotated
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val needx : <int>{x : int} -> unit
    val direct : <int>{x : immut int} -> <int>{x : int} -> unit
    -- evaluated at compile time
    -- not annotated
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val needx : <int>{x : int} -> unit
    val direct : <int>{x?'a : immut int; ..'b} -> <int>{x : mut?'c int} -> unit where ..'b <= {x : mut?'c int}, ..'b <= {x : int; y : int; ..'d}
    -- evaluated at compile time
    -- generated
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val needx : <int>{x : int} -> unit
    val gen : 'a -> <<int>{x : immut int} -> <int>{x : int} -> unit>{..'b}
    val user : <int>{x : immut int} -> <int>{x : int} -> unit
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (tw <fun>)
    (needx <fun>)
    (gen <fun>)
    (user <fun>)
    -- annotated
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val closeit : <int>{} -> unit
    val direct : <int>{} -> <int>{} -> unit
    -- evaluated at compile time
    -- not annotated
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val closeit : <int>{} -> unit
    val direct : <int>{-x; ..'a} -> <int>{} -> unit where ..'a <= {}
    -- evaluated at compile time
    -- generated
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val closeit : <int>{} -> unit
    val gen : 'a -> <<int>{} -> <int>{} -> unit>{..'b}
    val user : <int>{} -> <int>{} -> unit
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (tw <fun>)
    (closeit <fun>)
    (gen <fun>)
    (user <fun>)
    |}]
;;

let%expect_test "re-inference of a local bounded by an outer closed row" =
  (* Splicing [e1] with only [x] substituted closes its row, so [h]'s presence
     of [x] is only bounded below by [e1]'s, which must not keep [h] from being
     generalized: without [k]'s annotation, [h] is used with and without [x]. *)
  let f e1 ~use_k =
    sprintf
      {|let f %s k =
    .<.~e1.{x = 1}>.;
    let h d = cond true (tw e1) d in
    h .<add x y>.; h k; %s k
  in
  f|}
      e1
      use_k
  in
  List.iter
    [ "<int>{x : int}", needx, "needx"; "<int>{}", closeit, "closeit" ]
    ~f:(fun (annot, use_k_decl, use_k) ->
      let annotated = f (sprintf "(e1 : %s)" annot) ~use_k in
      print_endline "-- annotated";
      run ~eval:false (lines [ tw; use_k_decl; "let direct =\n  " ^ annotated ]);
      print_endline "-- not annotated";
      run
        ~eval:false
        (lines [ tw; use_k_decl; "let direct =\n  " ^ f "e1" ~use_k ]);
      print_endline "-- generated";
      run
        (lines
           [ tw
           ; use_k_decl
           ; "let gen u = u; .<" ^ annotated ^ ">."
           ; "let user = .~(gen ())"
           ]));
  [%expect
    {|
    -- annotated
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val needx : <int>{x : int} -> unit
    val direct : <int>{x : immut int} -> <int>{x : int} -> unit
    -- evaluated at compile time
    -- not annotated
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val needx : <int>{x : int} -> unit
    val direct : <int>{x?'a : immut int} -> <int>{x : int} -> unit
    -- evaluated at compile time
    -- generated
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val needx : <int>{x : int} -> unit
    val gen : 'a -> <<int>{x : immut int} -> <int>{x : int} -> unit>{..'b}
    val user : <int>{x : immut int} -> <int>{x : int} -> unit
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (tw <fun>)
    (needx <fun>)
    (gen <fun>)
    (user <fun>)
    -- annotated
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val closeit : <int>{} -> unit
    val direct : <int>{} -> <int>{} -> unit
    -- evaluated at compile time
    -- not annotated
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val closeit : <int>{} -> unit
    val direct : <int>{} -> <int>{} -> unit
    -- evaluated at compile time
    -- generated
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val closeit : <int>{} -> unit
    val gen : 'a -> <<int>{} -> <int>{} -> unit>{..'b}
    val user : <int>{} -> <int>{} -> unit
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (tw <fun>)
    (closeit <fun>)
    (gen <fun>)
    (user <fun>)
    |}]
;;

let%expect_test "the order of splices deciding a weak variable matters" =
  (* [w0]'s row is weak. Spliced under local [x] and [y] first, it is closed
     with neither, as nothing it may use is known; spliced with [x] substituted
     first, it may use [x], which [needx] then makes [Present]. *)
  let f0 = {|let f0 c d = d|} in
  let w0 = {|let w0 = f0 .<y>. .<2>.|} in
  let substituted = {|let substituted = .~w0.{x = 3}|} in
  let under_lets = {|let under_lets = let x = 1 in let y = 2 in .~w0|} in
  let use = {|let use = needx w0|} in
  run ~eval:false (lines [ needx; f0; w0; substituted; under_lets; use ]);
  run ~eval:false (lines [ needx; f0; w0; under_lets; substituted; use ]);
  run ~eval:false (lines [ needx; f0; w0; substituted ]);
  run ~eval:false (lines [ needx; f0; w0; under_lets ]);
  [%expect
    {|
    val needx : <int>{x : int} -> unit
    val f0 : 'a -> 'b -> 'b
    val w0 : <int>{x : immut int}
    val substituted : int
    val under_lets : int
    val use : unit
    -- evaluated at compile time
    (f0 <fun>)
    (w0 (Code (Lit (Int 2) : int)))
    ((Unification_error
      (Row_mismatch (RExtend x Present (TCon int ()) REmpty) REmpty
       (Missing_label x)))
     @ t.stag:7:16-18)
    val needx : <int>{x : int} -> unit
    val f0 : 'a -> 'b -> 'b
    val w0 : <int>{x?'_a : immut int}
    val substituted : int
    -- evaluated at compile time
    (f0 <fun>)
    (w0 (Code (Lit (Int 2) : int)))
    val needx : <int>{x : int} -> unit
    val f0 : 'a -> 'b -> 'b
    val w0 : <int>{}
    val under_lets : int
    -- evaluated at compile time
    (f0 <fun>)
    (w0 (Code (Lit (Int 2) : int)))
    |}]
;;

let%expect_test "weak toplevels used by generated code" =
  run
    {|
let weak = id id
let gen u = u; .<weak>.
let one = .~(gen ()) 1
let two = .~(gen ()) 2
|};
  run
    {|
let weak = id id
let gen u = u; .<weak>.
let one = .~(gen ()) 1
let other = .~(gen ()) true
|};
  run
    {|
let weak = id id
let gen u = u; .<weak>.
let one = weak 1
let other = .~(gen ()) true
|};
  [%expect
    {|
    val weak : int -> int
    val gen : 'a -> <'b>{weak : 'b; ..'c}
    val one : int
    val two : int
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (weak <fun>)
    (gen <fun>)
    (one 1)
    (two 2)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:5:23-27)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:5:23-27)
    |}]
;;

let%expect_test "entries of different types code may use, then closed" =
  (* [ki c] and [kb c] give [c]'s [x] the types [int] and [bool], which is fine
     as [c] is closed, so has no [x]: the generated [f] has lost the annotation,
     and closing [c] may come before or after. *)
  let ki = {|let ki c = .<let x = 1 in .~c>.; ()|} in
  let kb = {|let kb c = .<let x = true in .~c>.; ()|} in
  let use = {|let use = .~(gen ())|} in
  run (lines [ ki; kb; {|let plain = let f (c : <int>{}) = ki c; kb c in 0|} ]);
  run
    (lines
       [ ki
       ; kb
       ; {|let gen u = u; .<let f (c : <int>{}) = ki c; kb c in 0>.|}
       ; use
       ]);
  List.iter
    [ [ "closeit"; "ki"; "kb" ]
    ; [ "ki"; "closeit"; "kb" ]
    ; [ "ki"; "kb"; "closeit" ]
    ]
    ~f:(fun order ->
      print_endline ("-- " ^ String.concat order ~sep:"; ");
      (* Sequenced to the left, as [(a; b); c]. *)
      let calls =
        match List.map order ~f:(fun name -> name ^ " c") with
        | [ a; b; c ] -> sprintf "(%s; %s); %s" a b c
        | _ -> assert false
      in
      run (lines [ closeit; ki; kb; "let f c = " ^ calls ]);
      run
        (lines
           [ closeit
           ; ki
           ; kb
           ; sprintf "let gen u = u; .<let f c = %s in 0>." calls
           ; use
           ]));
  [%expect
    {|
    val ki : <'a>{x?'b : immut int; ..'c} -> unit
    val kb : <'a>{x?'b : immut bool; ..'c} -> unit
    val plain : int
    -- evaluated at compile time
    -- evaluated
    (ki <fun>)
    (kb <fun>)
    (plain 0)
    val ki : <'a>{x?'b : immut int; ..'c} -> unit
    val kb : <'a>{x?'b : immut bool; ..'c} -> unit
    val gen : 'a -> <int>{..'b}
    val use : int
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (ki <fun>)
    (kb <fun>)
    (gen <fun>)
    (use 0)
    -- closeit; ki; kb
    val closeit : <int>{} -> unit
    val ki : <'a>{x?'b : immut int; ..'c} -> unit
    val kb : <'a>{x?'b : immut bool; ..'c} -> unit
    val f : <int>{} -> unit
    -- evaluated at compile time
    -- evaluated
    (closeit <fun>)
    (ki <fun>)
    (kb <fun>)
    (f <fun>)
    val closeit : <int>{} -> unit
    val ki : <'a>{x?'b : immut int; ..'c} -> unit
    val kb : <'a>{x?'b : immut bool; ..'c} -> unit
    val gen : 'a -> <int>{..'b}
    val use : int
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (closeit <fun>)
    (ki <fun>)
    (kb <fun>)
    (gen <fun>)
    (use 0)
    -- ki; closeit; kb
    val closeit : <int>{} -> unit
    val ki : <'a>{x?'b : immut int; ..'c} -> unit
    val kb : <'a>{x?'b : immut bool; ..'c} -> unit
    val f : <int>{} -> unit
    -- evaluated at compile time
    -- evaluated
    (closeit <fun>)
    (ki <fun>)
    (kb <fun>)
    (f <fun>)
    val closeit : <int>{} -> unit
    val ki : <'a>{x?'b : immut int; ..'c} -> unit
    val kb : <'a>{x?'b : immut bool; ..'c} -> unit
    val gen : 'a -> <int>{..'b}
    val use : int
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (closeit <fun>)
    (ki <fun>)
    (kb <fun>)
    (gen <fun>)
    (use 0)
    -- ki; kb; closeit
    val closeit : <int>{} -> unit
    val ki : <'a>{x?'b : immut int; ..'c} -> unit
    val kb : <'a>{x?'b : immut bool; ..'c} -> unit
    val f : <int>{} -> unit
    -- evaluated at compile time
    -- evaluated
    (closeit <fun>)
    (ki <fun>)
    (kb <fun>)
    (f <fun>)
    val closeit : <int>{} -> unit
    val ki : <'a>{x?'b : immut int; ..'c} -> unit
    val kb : <'a>{x?'b : immut bool; ..'c} -> unit
    val gen : 'a -> <int>{..'b}
    val use : int
    -- evaluated at compile time
    (gen <fun>)
    -- evaluated
    (closeit <fun>)
    (ki <fun>)
    (kb <fun>)
    (gen <fun>)
    (use 0)
    |}]
;;
