open! Import
open Source_helpers

(* Evaluates [source], then prints its toplevels in evaluation order, then the
   error evaluation stopped at, if any. *)
let eval source = Option.iter (typecheck source) ~f:eval_and_print_values

let%expect_test "values" =
  eval
    {|
let lits = (1, true, "s", 'c', ())
let tags = (`A, `B 1)
let poly_record = {`a = 1; `b = false}
let fn x = x
|};
  [%expect
    {|
    (lits (Tuple 1 true (String s) (Char c) "()"))
    (tags (Tuple `A (`B 1)))
    (poly_record ((a 1) (b false)))
    (fn <fun>)
    |}]
;;

let%expect_test "recursive values" =
  (* [y] is evaluated after [f], whatever the order of the component. *)
  eval
    {|
let y = f `A
let f x = match x with `A -> 1 | `B -> y
|};
  [%expect
    {|
    (y 1)
    (f <fun>)
    |}];
  (* A value used by its own definition is an error. *)
  eval
    {|
let x = add x 1
|};
  [%expect
    {|
    ("x is used before its value is defined (a value may not depend on itself)" @
     t.stag:2:12-13)
    |}];
  eval
    {|
let z = g `B
let g x = match x with `A -> 1 | `B -> z
|};
  [%expect
    {|
    (g <fun>)
    ("z is used before its value is defined (a value may not depend on itself)" @
     t.stag:3:39-40)
    |}];
  (* A closure may refer to the value it is part of. *)
  eval
    {|
let h = let k u = h; u in k
let called = h 7
|};
  [%expect
    {|
    (h <fun>)
    (called 7)
    |}]
;;

let%expect_test "nominal types" =
  eval
    {|
type t = A | B of int * bool
type r = {x : int; y : bool}
let of_t t = match t with A -> 0 | B (n, _) -> n
let constructed = (A, B (1, true), of_t (B (2, true)))
let r = {y = true; x = 3}
let fields = (r.x, r.y)
let matched = match r with {y = y; x = x} -> (x, y)
let only_a t = match t with A -> 0
let failure = only_a (B (1, true))
|};
  [%expect
    {|
    (of_t <fun>)
    (constructed (Tuple (A) (B 1 true) 2))
    (r (Record 3 true))
    (fields (Tuple 3 true))
    (matched (Tuple 3 true))
    (only_a <fun>)
    ("Match failure: no case matches the value B (1, true)" @ t.stag:9:15-34)
    |}]
;;

let%expect_test "negative literals" =
  eval
    {|
type t = A of int
let lits = (-1, -4611686018427387904, Int.add -2 3, Int.neg -5)
let sign n = match n with -1 -> `Minus_one | 0 -> `Zero | _ -> `Other
let signs = (sign -1, sign 0, sign 1)
let only_minus_one t = match t with A -1 -> 0
let matched = only_minus_one (A -1)
let failure = only_minus_one (A -2)
|};
  eval "let tag_failure = match `B -3 with `B 0 -> 0";
  [%expect
    {|
    (lits (Tuple -1 -4611686018427387904 1 5))
    (sign <fun>)
    (signs (Tuple `Minus_one `Zero `Other))
    (only_minus_one <fun>)
    (matched 0)
    ("Match failure: no case matches the value A (-2)" @ t.stag:6:23-45)
    ("Match failure: no case matches the value `B (-3)" @ t.stag:1:18-44)
    |}]
;;

let%expect_test "printed negative ints parse back" =
  List.iter
    Treewalk.Value.
      [ Int (-1)
      ; Int Int.min_value
      ; Poly_variant ("A", Int (-2))
      ; Tuple [ Int (-3); Poly_variant ("B", Int 4) ]
      ]
    ~f:(fun value ->
      let printed = Treewalk.Value.to_string_hum value in
      match Parser.expr ~file:"t.stag" printed with
      | Ok _ -> print_endline printed
      | Error error -> print_endline (Parser.Error.to_string error));
  [%expect
    {|
    -1
    -4611686018427387904
    `A (-2)
    (-3, `B 4)
    |}]
;;

let%expect_test "polymorphic records and variants" =
  eval
    {|
let field = {`a = 1; `b = true}.`b
let extended = {`c = 2 | {`a = 1; `b = true}}
let restricted = {`a = 1; `b = true} \ `a
let updated = {{`a = 1; `b = true} with `a = 5}
let rest = match {`a = 1; `b = true} with {`a = a; ..rest} -> (a, rest)
let lits = match 2 with 1 -> `One | 2 -> `Two | _ -> `Other
let variant = match `B 3 with `A -> 0 | `B n -> n
|};
  [%expect
    {|
    (field true)
    (extended ((a 1) (b true) (c 2)))
    (restricted ((b true)))
    (updated ((a 5) (b true)))
    (rest (Tuple 1 ((b true))))
    (lits `Two)
    (variant 3)
    |}]
;;

let%expect_test "code values" =
  eval
    {|
let code = .<(1, x)>.
let wrap c = .<(.~c, .~c)>.
let wrapped = wrap .<2>.
|};
  [%expect
    {|
    (code (Code (Tuple ((Lit (Int 1) : int) (Var (Free x) : 'a)) : "int * 'a")))
    (wrap <fun>)
    (wrapped
     (Code (Tuple ((Lit (Int 2) : int) (Lit (Int 2) : int)) : "'a * 'a")))
    |}]
;;

let%expect_test "spliced code binding patterns and recursive closures" =
  eval
    {|
let copies =
  .~(let c = .<let count m = match m with `Z -> x | `S k -> count k in count (`S (`S `Z))>. in
     .<((match {`x = 1; `y = 0} with {`x = x; ..rest} -> (.~c, rest)),
        (match {`x = 2; `y = 0} with {`x = x; ..rest} -> (.~c, rest)))>.)
let open_rest = match {`a = 1; `b = 2} with {`a = a; ..} -> a
|};
  [%expect
    {|
    (copies (Tuple (Tuple 1 ((y 0))) (Tuple 2 ((y 0)))))
    (open_rest 1)
    |}]
;;

let%expect_test "nested quotes at runtime" =
  eval
    {|
let twice c = .<.<.~(.~c)>.>.
let built = twice .<.<1>.>.
let embedding x = let h u = x in .<h ()>.
let embedded = embedding 7
|};
  [%expect
    {|
    (twice <fun>)
    (built
     (Code
      (Quote
       (Splice (code (Quote (Lit (Int 1) : int) : <int>{..'_a}))
        (subs (Implicit ())) : 'b)
       : <'b>{..'c} (where "..'_a <= {..'_d}"))))
    (embedding <fun>)
    (embedded
     (Code
      (App (Var (Persistent (Local h/21)) : "unit -> 'a") (Lit Unit : unit) : 'a)
      ((h/21 <fun>))))
    |}]
;;

let%expect_test "expanded programs" =
  Option.iter
    (typecheck
       {|
let five = .~(.<5>.)
let captured x = .~(.<(x, x)>.)
let explicit y = .~(.<let z = x in z>.).{x = y}
let in_quote = .<.~(.~(.<.<1>.>.))>.
|})
    ~f:print_bodies;
  [%expect
    {|
    (five/4
     (Toplevel_splice (code (Quote (Lit (Int 5) : int) : <int>{}))
      (expansion (Lit (Int 5) : int)) : int))
    (captured/5
     (Toplevel_splice
      (code
       (Quote (Tuple ((Var (Free x) : 'a) (Var (Free x) : 'a)) : "'a * 'a") :
        "<'a * 'a>{x : 'a}"))
      (expansion
       (Tuple ((Var (Bound (Local x/17)) : 'a) (Var (Bound (Local x/17)) : 'a)) :
        "'a * 'a"))
      : "'a * 'a"))
    (explicit/6
     (Toplevel_splice
      (code
       (Quote
        (Let (ident z/19) (args ()) (generalized true) (body (Var (Free x) : 'a))
         (in_ (Var (Bound (Local z/19)) : 'a)) : 'a)
        : "<'a>{x : 'a}"))
      (expansion
       (Let (ident x/20) (args ()) (generalized false)
        (body (Var (Bound (Local y/18)) : 'a))
        (in_
         (Let (ident z/21) (args ()) (generalized true)
          (body (Var (Bound (Local x/20)) : 'a))
          (in_ (Var (Bound (Local z/21)) : 'a)) : 'a))
        : 'a))
      : 'a))
    (in_quote/7
     (Quote
      (Splice
       (code
        (Toplevel_splice
         (code
          (Quote (Quote (Lit (Int 1) : int) : <int>{..'_a}) : <<int>{..'_a}>{}))
         (expansion (Quote (Lit (Int 1) : int) : <int>{..'_a})) : <int>{..'_a}))
       (subs (Implicit ())) : int)
      : <int>{..'b} (where "..'_a <= {..'b}")))
    |}]
;;

let%expect_test
    "a failing toplevel splice fails checking, before anything is evaluated"
  =
  eval
    {|
type abc = A | B | C
let fails_at_runtime = match B with A -> 0
let fails_at_expansion = .~(match C with A -> .<0>.)
|};
  [%expect
    {|
    ((Splice_evaluation_failed
      (code
       (Match (Construct ((type_name abc) (name C) (index 2)) () : abc)
        (((Construct ((type_name abc) (name A) (index 0)) () : abc)
          (Quote (Lit (Int 0) : int) : <int>{})))
        : <int>{}))
      (error ("Match failure: no case matches the value C" @ t.stag:4:27-52)))
     @ t.stag:4:25-52)
    |}]
;;

let%expect_test "a constructor of one tuple argument, and one of two" =
  eval
    {|
type t = C of (int * int)
type u = D of int * int
let c = C ((1, 2))
let d = D (1, 2)
let pair = (3, 4)
let c' = C pair
let first = match c with C ((a, b)) -> a
let whole = match c' with C p -> p
let second = match d with D (a, b) -> b
|};
  [%expect
    {|
    (c (C (Tuple 1 2)))
    (d (D 1 2))
    (pair (Tuple 3 4))
    (c' (C (Tuple 3 4)))
    (first 1)
    (whole (Tuple 3 4))
    (second 2)
    |}];
  List.iter
    ~f:(fun source -> ignore (typecheck (source ^ "\n") : _ option))
    [ "type t = C of (int * int)\nlet c = C (1, 2)"
    ; "type t = C of (int * int)\nlet f x = match x with C (a, b) -> a"
    ; "type u = D of int * int\nlet d = D ((1, 2))"
    ; "type u = D of int * int\nlet f x = match x with D ((a, b)) -> a"
    ];
  [%expect
    {|
    ((Constructor_arity_mismatch (constructor C) (expected 1) (got 2)) @
     t.stag:2:8-16)
    ((Constructor_arity_mismatch (constructor C) (expected 1) (got 2)) @
     t.stag:2:23-31)
    ((Constructor_arity_mismatch (constructor D) (expected 2) (got 1)) @
     t.stag:2:8-18)
    ((Constructor_arity_mismatch (constructor D) (expected 2) (got 1)) @
     t.stag:2:23-33)
    |}]
;;
