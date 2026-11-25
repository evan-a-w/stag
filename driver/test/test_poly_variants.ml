open! Import
open Helpers
open Source_helpers

(* [fun x -> body] *)
let fn x body = let_ "fn" [ x ] body (var "fn")

let%expect_test "injection" =
  check
    {|
let a = `A 1
let b = `B
let unit_arg = `B ()
let c x = `C x
let nested = `A (`B 1)
let pair = (`A, `B)
let same_type = cond true `A `B
|};
  [%expect
    {|
    val a : [`A of int; ..'a]
    val b : [`B; ..'a]
    val unit_arg : [`B; ..'a]
    val c : 'a -> [`C of 'a; ..'b]
    val nested : [`A of [`B of int; ..'a]; ..'b]
    val pair : [`A; ..'a] * [`B; ..'b]
    val same_type : [`A | `B; ..'_a]
    |}]
;;

let%expect_test "matches without a catch-all are closed" =
  check {|
let f v = match v with `A n -> n | `B -> 0
let applied = f `B
|};
  (* On the DSL: source can't write an empty match. *)
  check_program [ top "absurd" [ "v" ] (match_ (var "v") []) ];
  check
    {|
let with_any v = match v with `A -> 1 | _ -> 0
let with_var v = match v with `A -> 1 | x -> 0
let var_first v = match v with x -> x | `A -> `B
let repeated v = match v with `A 1 -> 1 | `A _ -> 2
|};
  check {|
let f v = match v with `A n -> n | `B -> 0
let unhandled = f `C
|};
  check_program [ top "absurd_applied" [] (match_ (tag "A") []) ];
  check {|
let argument_mismatch v = match v with `A 1 -> 1 | `A -> 2
|};
  [%expect
    {|
    val f : [`A of int | `B] -> int
    val applied : int
    val absurd : [] -> 'a
    val with_any : [`A; ..'a] -> int
    val with_var : [`A; ..'a] -> int
    val var_first : [`A | `B; ..'a] -> [`A | `B; ..'a]
    val repeated : [`A of int] -> int
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend C Present (TCon unit ()) (RVar (Unbound 47 1)))
       (Missing_label C)))
     @ t.stag:3:18-20)
    (Unification_error
     (Row_mismatch REmpty
      (RExtend A Present (TCon unit ()) (RVar (Unbound 23 1))) (Missing_label A)))
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon unit ())
       (Base_types_not_equal int unit)))
     @ t.stag:2:51-53)
    |}]
;;

(* On the DSL: source can't write an empty match ([match_ x []]). *)
let%expect_test "residuals of a catch-all" =
  let handle name =
    top
      ("handle_" ^ String.lowercase name)
      [ "e"; "k" ]
      (match_
         (var "e")
         [ Pat.tag name ~arg:(Pat.var "n"), var "n"
         ; Pat.var "other", app (var "k") [ var "other" ]
         ])
  in
  check_program
    [ handle "A"
    ; handle "B"
    ; top
        "handle_both"
        [ "e" ]
        (app
           (var "handle_a")
           [ var "e"
           ; fn
               "e"
               (app (var "handle_b") [ var "e"; fn "e" (match_ (var "e") []) ])
           ])
    ; top "both_a" [] (app (var "handle_both") [ tag "A" ~arg:(int 1) ])
    ; top
        "open_ended"
        [ "e"; "k" ]
        (app
           (var "handle_a")
           [ var "e"; fn "e" (app (var "handle_b") [ var "e"; var "k" ]) ])
    ];
  check_program
    [ handle "A"
    ; handle "B"
    ; top
        "handle_both"
        [ "e" ]
        (app
           (var "handle_a")
           [ var "e"
           ; fn
               "e"
               (app (var "handle_b") [ var "e"; fn "e" (match_ (var "e") []) ])
           ])
    ; top "unhandled" [] (app (var "handle_both") [ tag "C" ])
    ];
  [%expect
    {|
    val handle_a : [`A of 'a; ..'b] -> ([..'b] -> 'a) -> 'a
    val handle_b : [`B of 'a; ..'b] -> ([..'b] -> 'a) -> 'a
    val handle_both : [`A of 'a | `B of 'a] -> 'a
    val both_a : int
    val open_ended : [`A of 'a | `B of 'a; ..'b] -> ([..'b] -> 'a) -> 'a
    (Unification_error
     (Row_mismatch REmpty
      (RExtend C Present (TCon unit ()) (RVar (Unbound 119 1)))
      (Missing_label C)))
    |}]
;;

let%expect_test "which cases are removed from residuals" =
  check
    {|
type option['a] = None | Some of 'a
let two_tags v k = match v with `A -> 1 | `B -> 2 | other -> k other
let refutable v k = match v with `A 0 -> 0 | other -> k other
let refutable_then_irrefutable v k =
  match v with `A 0 -> 0 | `A _ -> 1 | other -> k other
let tuple v k = match v with `A (x, _) -> x | other -> k other
let refutable_tuple v k = match v with `A (x, 1) -> x | other -> k other
let record v k = match v with `A {`x = x; ..r} -> x | other -> k other
let nominal v k = match v with `A (Some x) -> x | other -> k other
let nested_variant v k = match v with `A `B -> 1 | other -> k other
let removed_once v k = match v with `A x -> x | `A y -> y | other -> k other
let shadowed_tag v =
  match v with
  | `A n -> n
  | other -> (match other with `A b -> cond b 1 2 | _ -> 0)
let later_cases v k = match v with `A -> 1 | other -> k other | `B -> 2
|};
  [%expect
    {|
    type option['a] = None | Some of 'a
    val two_tags : [`A | `B; ..'a] -> ([..'a] -> int) -> int
    val refutable : [`A of int; ..'a] -> ([`A of int; ..'a] -> int) -> int
    val refutable_then_irrefutable : [`A of int; ..'a] -> ([..'a] -> int) -> int
    val tuple : [`A of 'a * 'b; ..'c] -> ([..'c] -> 'a) -> 'a
    val refutable_tuple : [`A of 'a * int; ..'b] -> ([`A of 'a * int; ..'b] -> 'a) -> 'a
    val record : [`A of {`x : 'a; ..'b}; ..'c] -> ([..'c] -> 'a) -> 'a
    val nominal : [`A of option['a]; ..'b] -> ([`A of option['a]; ..'b] -> 'a) -> 'a
    val nested_variant : [`A of [`B; ..'a]; ..'b] -> ([`A of [`B; ..'a]; ..'b] -> int) -> int
    val removed_once : [`A of 'a; ..'b] -> ([..'b] -> 'a) -> 'a
    val shadowed_tag : [`A of int | `A of bool; ..'a] -> int
    val later_cases : [`A | `B; ..'a] -> ([`B; ..'a] -> int) -> int
    |}]
;;

let%expect_test "nested variant patterns stay open" =
  check
    {|
type option['a] = None | Some of 'a
let pair p = match p with (`A, `B) -> 1
let inner v = match v with `A `B -> 1
let in_option o = match o with Some `A -> 1 | None -> 0
let option_in_tag v = match v with `Some (Some x) -> x | `None -> 0
let in_record r = match r with {`x = `A} -> 1
let tuple_of_tags = (`A (Some 1), `B (1, true))
|};
  [%expect
    {|
    type option['a] = None | Some of 'a
    val pair : [`A; ..'a] * [`B; ..'b] -> int
    val inner : [`A of [`B; ..'a]] -> int
    val in_option : option[[`A; ..'a]] -> int
    val option_in_tag : [`None | `Some of option[int]] -> int
    val in_record : {`x : [`A; ..'a]} -> int
    val tuple_of_tags : [`A of option[int]; ..'a] * [`B of int * bool; ..'b]
    |}]
;;

let%expect_test "aliases" =
  check
    {|
type ab = [`A of int | `B]
type with_c[{..'r}] = [`C of bool; ..'r]
type wrapper = W of ab
let a : ab = `A 1
let c : with_c[{`D : unit}] = `D
let w = W `B
let open_param (v : with_c[{..'r}]) = v
|};
  check {|
type ab = [`A of int | `B]
let bad : ab = `C
|};
  check {|
type dup = [`A of int | `A of bool]
|};
  [%expect
    {|
    type ab = [`A of int | `B]
    type with_c[{..'a}] = [`C of bool; ..'a]
    type wrapper = W of [`A of int | `B]
    val a : [`A of int | `B]
    val c : [`C of bool | `D]
    val w : wrapper
    val open_param : [`C of bool; ..'a] -> [`C of bool; ..'a]
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend C Present (TCon unit ()) (RVar (Unbound 31 1)))
       (Missing_label C)))
     @ t.stag:3:15-17)
    ((Duplicate_row_entry A) @ t.stag:2:24-26)
    |}]
;;

let%expect_test "generalization and the value restriction" =
  check
    {|
let v = `A id
let weak = `A (id id)
let weak_row = id `A
let matched = match `A with `A -> id
let use = (cond true v `B, cond true v `C)
|};
  [%expect
    {|
    val v : [`A of 'a -> 'a; ..'b]
    val weak : [`A of '_a -> '_a; ..'_b]
    val weak_row : [`A; ..'_a]
    val matched : 'a -> 'a
    val use : [`A of '_a -> '_a | `B; ..'_b] * [`A of '_c -> '_c | `C; ..'_d]
    |}]
;;

let%expect_test "tags in quotes are not free variables" =
  check
    {|
let inject = .< `A x >.
let match_inside = .< match `A 1 with `A x -> x | other -> 0 >.
let residual_spliced k = .< match v with `A -> 0 | other -> .~(k .< other >.) >.
|};
  [%expect
    {|
    val inject : <[`A of 'a; ..'b]>{x : 'a; ..'c}
    val match_inside : <int>{..'a}
    val residual_spliced : (<'a>{other : 'a; ..'b} -> <int>{other?'c : immut [..'d]; ..'e}) -> <int>{v : mut?'f [`A; ..'d]; ..'g} where ..'e <= {v : mut?'f [`A; ..'d]; ..'g}
    |}]
;;

let%expect_test "presences reach variant rows through contexts" =
  check
    {|
let unknown c (v : [..'r]) =
  .< let y = 1 in .~(c : <int>{..'r}) >.;
  v
let absent c (v : [..'d]) =
  .< .< let z = 1 in .~(.~(c : <<'a>{..'c}>{..'d})) >. >.;
  v
|};
  [%expect
    {|
    val unknown : <int>{y?'a : immut int; ..'b} -> [`y?'a of int; ..'b] -> [`y?'a of int; ..'b]
    val absent : <<'a>{z?'b : immut int; ..'c}>{-z; ..'d} -> [-`z; ..'d] -> [-`z; ..'d]
    |}]
;;

let%expect_test "unification errors" =
  check {|
let variant_vs_record = match {} with `A -> 1
|};
  check {|
let variant_vs_tuple = add `A 1
|};
  check {|
let argument = cond true (`A 1) (`A true)
|};
  check {|
let closed = match `C with `A -> 1 | `B -> 2
|};
  check {|
let maybe_c v =
  cond true v `C;
  match v with `A -> 1
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TRecord REmpty)
       (TVariant (RExtend A Present (TCon unit ()) (RVar (Unbound 25 1))))
       Structural_mismatch))
     @ t.stag:2:38-40)
    ((Unification_error
      (Type_mismatch (TCon int ())
       (TVariant (RExtend A Present (TCon unit ()) (RVar (Unbound 27 1))))
       Structural_mismatch))
     @ t.stag:2:27-29)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:32-41)
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend C Present (TCon unit ()) (RVar (Unbound 36 1)))
       (Missing_label C)))
     @ t.stag:2:13-44)
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend C Present (TCon unit ()) (RVar (Unbound 41 1)))
       (Missing_label C)))
     @ t.stag:4:2-22)
    |}]
;;

let%expect_test "empty tuples print as ()" =
  (* On the DSL: in source, [()] is the unit literal, not the empty tuple. *)
  check_program
    [ top "empty" [] (tuple []); top "arg" [] (tag "A" ~arg:(tuple [])) ];
  check {|
let unit_arg = `A ()
type t = [`A of ()]
type c = C of ()
|};
  [%expect
    {|
    val empty : ()
    val arg : [`A of (); ..'a]
    val unit_arg : [`A; ..'a]
    type t = [`A of ()]
    type c = C of ()
    |}]
;;

let%expect_test "literal unit and closed records as tag arguments" =
  check
    {|
let unit_literal v k = match v with `A () -> 0 | other -> k other
let closed_record v k = match v with `A {`x = n} -> n | other -> k other
|};
  [%expect
    {|
    val unit_literal : [`A; ..'a] -> ([`A; ..'a] -> int) -> int
    val closed_record : [`A of {`x : 'a}; ..'b] -> ([..'b] -> 'a) -> 'a
    |}]
;;

let%expect_test "local polymorphic matches" =
  check
    {|
let closed =
  let f v = match v with `A x -> x in
  (f (`A 1), f (`A true))
let residual =
  let f v k = match v with `A -> 1 | other -> k other in
  ( f (`B 1) (let fn o = match o with `B n -> n in fn),
    f (`C true) (let fn o = match o with `C b -> cond b 1 2 in fn) )
|};
  [%expect {|
    val closed : int * bool
    val residual : int * int
    |}]
;;

let%expect_test "closing scrutinees with shadowed tags" =
  let shadowed = {|
type t[{..'r}] = [`A of int; ..'r]
|} in
  check
    (shadowed
     ^ {|
let residual (v : t[{`A : bool}]) =
  match v with
  | `A n -> n
  | other -> (match other with `A b -> cond b 1 2)
|}
    );
  (* Closing gives each tag one entry, so the shadowed [`A of bool] is extra. *)
  check
    (shadowed ^ {|
let closed (v : t[{`A : bool}]) = match v with `A n -> 1
|});
  check
    (shadowed
     ^ {|
let repeated_tag (v : t[{`A : bool}]) = match v with `A n -> 1 | `A b -> 2
|}
    );
  [%expect
    {|
    type t[{..'a}] = [`A of int; ..'a]
    val residual : [`A of int | `A of bool] -> int
    ((Unification_error
      (Row_mismatch REmpty (RExtend A Present (TCon bool ()) REmpty)
       (Missing_label A)))
     @ t.stag:4:34-56)
    ((Unification_error
      (Row_mismatch REmpty (RExtend A Present (TCon bool ()) REmpty)
       (Missing_label A)))
     @ t.stag:4:40-74)
    |}]
;;

let%expect_test "rows shared between records and variants" =
  check
    {|
let same (r : {..'r}) (v : [..'r]) = (r, v)
let ok = same {`A = 1} (`A 2)
let matched r v =
  same r v;
  cond true r.`A (match v with `A n -> n)
|};
  check
    {|
let same (r : {..'r}) (v : [..'r]) = (r, v)
let missing = same {`A = 1} `B
|};
  [%expect
    {|
    val same : {..'a} -> [..'a] -> {..'a} * [..'a]
    val ok : {`A : int} * [`A of int]
    val matched : {`A : 'a} -> [`A of 'a] -> 'a
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend B Present (TCon unit ()) (RVar (Unbound 41 1)))
       (Missing_label B)))
     @ t.stag:3:28-30)
    |}]
;;

let%expect_test "structural types may be recursive" =
  check {|
let occurs v = cond true v (`A v)
|};
  check {|
let cyclic (a : [`A; ..'r]) (b : [`B; ..'r]) = cond true a b
|};
  check {|
type t = [`Nil | `Cons of int * t]
|};
  check
    {|
type t = T of [`Nil | `Cons of int * t]
let two = T (`Cons (1, T `Nil))
|};
  (* Through a pointer, as they must be. *)
  check {|
let occurs v = cond true v (`A (Ptr.make v))
|};
  check {|
type t = [`Nil | `Cons of int * ptr[t]]
|};
  check
    {|
type t = T of [`Nil | `Cons of int * ptr[t]]
let two = T (`Cons (1, Ptr.make (T `Nil)))
|};
  [%expect
    {|
    ((Unification_error (Occurs_check_failed (ATy (TVar (Unbound 32 1))))) @
     t.stag:2:27-33)
    ((Unification_error
      (Row_mismatch
       (RExtend A Present (TCon unit ())
        (RVar
         (Link
          (RExtend A (PVar (Unbound 36 1)) (TCon unit ()) (RVar (Unbound 37 1))))))
       (RExtend B Present (TCon unit ())
        (RVar
         (Link
          (RExtend A (PVar (Unbound 36 1)) (TCon unit ()) (RVar (Unbound 37 1))))))
       Cyclic_row))
     @ t.stag:2:59-60)
    ((Infinite_type_layout t) @ t.stag:2:5-6)
    ((Infinite_type_layout t) @ t.stag:2:5-6)
    val occurs : ([`A of ptr['a]; ..'b] as 'a) -> 'a
    type t = [`Cons of int * ptr[t] | `Nil]
    type t = T of [`Cons of int * ptr[t] | `Nil]
    val two : t
    |}]
;;

(* On the DSL: source can't write an empty match ([match_ x []]). *)
let%expect_test "empty matches on empty nominal types" =
  let void = Decl.variant "void" [] in
  let absurd ?(name = "absurd") ty =
    top_let (name, [ typed "x" ty ], None, match_ (var "x") [])
  in
  check_program
    [ void
    ; absurd (Ty.con "void")
    ; top "poly" [ "x" ] (match_ (var "x") [])
    ; top_let
        ( "through_let"
        , [ typed "v" (Ty.con "void") ]
        , None
        , let_ "y" [] (var "v") (match_ (var "y") []) )
    ];
  check_program
    [ module_ "M" [ void; absurd (Ty.con "void") ]; absurd (Ty.con "M.void") ];
  check_program
    [ void; Decl.alias "empty" (Ty.con "void"); absurd (Ty.con "empty") ];
  [%expect
    {|
    type void = |
    val absurd : void -> 'a
    val poly : [] -> 'a
    val through_let : void -> 'a
    module M : sig
      type void = |
      val absurd : void -> 'a
    end
    val absurd : M.void -> 'a
    type void = |
    type empty = void
    val absurd : void -> 'a
    |}];
  check_program [ Decl.variant "t" [ "A", [] ]; absurd (Ty.con "t") ];
  check_program [ absurd Ty.int ];
  [%expect
    {|
    (Unification_error
     (Type_mismatch (TVariant REmpty) (TCon t ()) Structural_mismatch))
    (Unification_error
     (Type_mismatch (TVariant REmpty) (TCon int ()) Structural_mismatch))
    |}];
  (* Order dependent: [f]'s scrutinee is not yet known to be [void]. *)
  check_program
    [ void
    ; top "f" [ "x" ] (match_ (var "x") [])
    ; top_let
        ("g", [ typed "v" (Ty.con "void") ], None, app (var "f") [ var "v" ])
    ];
  check_program
    [ void
    ; top_let
        ( "local"
        , [ typed "v" (Ty.con "void") ]
        , None
        , let_ "f" [ "x" ] (match_ (var "x") []) (app (var "f") [ var "v" ]) )
    ];
  [%expect
    {|
    (Unification_error
     (Type_mismatch (TVariant REmpty) (TCon void ()) Structural_mismatch))
    (Unification_error
     (Type_mismatch (TVariant REmpty) (TCon void ()) Structural_mismatch))
    |}]
;;
