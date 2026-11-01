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
  (* Computing a value may not use its own component, even through a call
     that wouldn't need the value: calls aren't followed. *)
  eval
    {|
let y = f `A
let f x = match x with `A -> 1 | `B -> y
|};
  [%expect {| ((Toplevel_initialization_cycle (y f y)) @ t.stag:2:8-9) |}];
  (* A value used by its own definition is an error. *)
  eval
    {|
let x = add x 1
|};
  [%expect {| ((Toplevel_initialization_cycle (x x)) @ t.stag:2:12-13) |}];
  eval
    {|
let z = g `B
let g x = match x with `A -> 1 | `B -> z
|};
  [%expect {| ((Toplevel_initialization_cycle (z g z)) @ t.stag:2:8-9) |}];
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
    (r (r (x 3) (y true)))
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

let%expect_test "nominal records print with their unqualified field names" =
  eval
    {|
module N = struct
  type r = {x : int; y : option[r]}
end
type s = {x : bool}
let v : N.r = {x = 1; y = Some {x = -2; y = None}}
let only_none (v : N.r) = match v with {x = _; y = None} -> 0
let failure = only_none v
|};
  [%expect
    {|
    (v (N.r (x 1) (y (Some (N.r (x -2) (y (None)))))))
    (only_none <fun>)
    ("Match failure: no case matches the value {x = 1; y = Some {x = -2; y = None}}"
     @ t.stag:7:26-61)
    |}]
;;

(* Each [name : ty = e] evaluates to a value whose printed form, checked
   against [ty] after [types], evaluates to a value printed the same. *)
let%expect_test "printed nominal records parse back" =
  let types =
    {|
module N = struct
  type r = {x : int; y : option[r]}
  type t = A of r | B of {`a : r} * r
end
type s = {x : bool}
|}
  in
  let value_of source =
    let%bind.Option t = typecheck (types ^ source) in
    let%bind.Option () = Driver.eval (driver t) |> Result.ok in
    List.find_map (members t) ~f:(fun { ident; _ } ->
      if String.equal ident.name "v"
      then Treewalk.find (Driver.interpreter (driver t)) ~ident |> Result.ok
      else None)
  in
  List.iter
    [ "N.r", "{x = 1; y = Some {x = -2; y = None}}"
    ; "N.t", "N.B ({`a = {x = 3; y = None}}, {x = 4; y = None})"
    ; "list[N.t]", "Cons (N.A {x = 5; y = None}, Nil)"
    ; "[`C of N.r]", "`C {x = 6; y = None}"
    ]
    ~f:(fun (ty, e) ->
      let printed source =
        Option.map (value_of source) ~f:Treewalk.Value.to_string_hum
      in
      let first = printed (sprintf "let v : %s = %s" ty e) in
      let second =
        Option.bind first ~f:(fun first ->
          printed (sprintf "let v : %s = %s" ty first))
      in
      print_s [%sexp (first : string option), (second : string option)]);
  [%expect
    {|
    (("{x = 1; y = Some {x = -2; y = None}}")
     ("{x = 1; y = Some {x = -2; y = None}}"))
    (("N.B ({`a = {x = 3; y = None}}, {x = 4; y = None})")
     ("N.B ({`a = {x = 3; y = None}}, {x = 4; y = None})"))
    (("Cons (N.A {x = 5; y = None}, Nil)") ("Cons (N.A {x = 5; y = None}, Nil)"))
    (("`C {x = 6; y = None}") ("`C {x = 6; y = None}"))
    |}]
;;

let%expect_test "printed constructors of one tuple keep its parentheses" =
  eval
    {|
type t = P of (int * int) | Q of int * int
let one = match P ((1, 2)) with Q (_, _) -> 0
|};
  eval
    {|
type t = P of (int * int) | Q of int * int
let two = match Q (1, 2) with P _ -> 0
|};
  [%expect
    {|
    ("Match failure: no case matches the value P ((1, 2))" @ t.stag:3:10-45)
    ("Match failure: no case matches the value Q (1, 2)" @ t.stag:3:10-38)
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
let embedding (x : int) = let h u = x in .<h ()>.
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
      (App (Var (Persistent (Local h/21)) : "unit -> int") (Lit Unit : unit) :
       int)
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
        "<'a * 'a>{x : immut 'a}"))
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
        : "<'a>{x : immut 'a}"))
      (expansion
       (Let (ident z/20) (args ()) (generalized true)
        (body (Var (Bound (Local y/18)) : 'a))
        (in_ (Var (Bound (Local z/20)) : 'a)) : 'a))
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
       (Match
        (scrutinee (Construct ((type_name abc) (name C) (index 2)) () : abc))
        (cases
         (((Construct ((type_name abc) (name A) (index 0)) () : abc)
           (Quote (Lit (Int 0) : int) : <int>{}))))
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
type box['a] = Box of 'a
let c = C ((1, 2))
let c2 = C (1, 2)
let d = D (1, 2)
let pair = (3, 4)
let c' = C pair
let first = match c with C ((a, b)) -> a
let first2 = match c2 with C (a, b) -> a
let whole = match c' with C p -> p
let second = match d with D (a, b) -> b
let some = Some (1, true)
let boxed = Box (1, 2, 3)
let nested = match Some (Box (1, 2), D (3, 4)) with Some (Box (a, b), D (c, d)) -> (a, b, c, d)
let none = match some with None -> 0 | Some (n, _) -> n
|};
  [%expect
    {|
    (c (C (Tuple 1 2)))
    (c2 (C (Tuple 1 2)))
    (d (D 1 2))
    (pair (Tuple 3 4))
    (c' (C (Tuple 3 4)))
    (first 1)
    (first2 1)
    (whole (Tuple 3 4))
    (second 2)
    (some (Some (Tuple 1 true)))
    (boxed (Box (Tuple 1 2 3)))
    (nested (Tuple 1 2 3 4))
    (none 1)
    |}];
  List.iter
    ~f:(fun source -> ignore (typecheck (source ^ "\n") : _ option))
    [ "type t = C of (int * int)\nlet c = C (1, 2, 3)"
    ; "type t = C of (int * int)\nlet f x = match x with C (a, b, c) -> a"
    ; "type u = D of int * int\nlet d = D ((1, 2))"
    ; "type u = D of int * int\nlet f x = match x with D ((a, b)) -> a"
    ; "type u = D of int * int\nlet d = D (1, 2, 3)"
    ; "type u = D of int * int\nlet d = D 1"
    ; "type v = E\nlet e = E (1, 2)"
    ; "let s = Some"
    ; "let f x = match x with Some -> 1"
    ];
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TTuple ((TCon int ()) (TCon int ())))
       (TTuple ((TCon int ()) (TCon int ()) (TCon int ())))
       (Args_length_mismatch 2 3)))
     @ t.stag:2:11-18)
    ((Unification_error
      (Type_mismatch (TTuple ((TCon int ()) (TCon int ())))
       (TTuple
        ((TVar (Link (TVar (Unbound 23 1)))) (TVar (Link (TVar (Unbound 24 1))))
         (TVar (Link (TVar (Unbound 25 1))))))
       (Args_length_mismatch 2 3)))
     @ t.stag:2:26-33)
    ((Constructor_arity_mismatch (constructor D) (expected 2) (got 1)) @
     t.stag:2:8-18)
    ((Constructor_arity_mismatch (constructor D) (expected 2) (got 1)) @
     t.stag:2:23-33)
    ((Constructor_arity_mismatch (constructor D) (expected 2) (got 3)) @
     t.stag:2:8-19)
    ((Constructor_arity_mismatch (constructor D) (expected 2) (got 1)) @
     t.stag:2:8-11)
    ((Constructor_arity_mismatch (constructor E) (expected 0) (got 2)) @
     t.stag:2:8-16)
    ((Constructor_arity_mismatch (constructor prelude.Some) (expected 1) (got 0))
     @ t.stag:1:8-12)
    ((Constructor_arity_mismatch (constructor prelude.Some) (expected 1) (got 0))
     @ t.stag:1:23-27)
    |}]
;;
