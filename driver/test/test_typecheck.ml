open! Import
open Source_helpers

let%expect_test "primitives" =
  check ~prelude:false prelude;
  [%expect
    {|
    val add : int -> int -> int
    val eq : int -> int -> bool
    val cond : bool -> 'a -> 'a -> 'a
    val id : 'a -> 'a
    |}]
;;

let%expect_test "typechecking twice with the same state" =
  let typecheck = typechecker () in
  let run source =
    match Typecheck.typecheck typecheck ~program:(parse_exn source) with
    | Ok { signature; _ } -> print_string (Signature.to_string_hum signature)
    | Error error ->
      List.iter (error : Typecheck.Failed.t).errors ~f:(fun error ->
        print_s [%sexp (error : Typecheck_error.t)])
  in
  run prelude;
  run {|
let id x = x
let n = id 1
|};
  [%expect
    {|
    val add : int -> int -> int
    val eq : int -> int -> bool
    val cond : bool -> 'a -> 'a -> 'a
    val id : 'a -> 'a
    val id : 'a -> 'a
    val n : int
    |}]
;;

let%expect_test "literals and application" =
  check
    {|
let i = 1
let b = false
let s = "s"
let c = 'c'
let u = ()
let sum = add 1 2
let partial = add 1
|};
  [%expect
    {|
    val i : int
    val b : bool
    val s : string
    val c : char
    val u : unit
    val sum : int
    val partial : int -> int
    |}]
;;

let%expect_test "application errors" =
  check {|
let bad = add true
|};
  check {|
let bad = 1 2
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:14-18)
    ((Unification_error
      (Type_mismatch (TArrow (TVar (Unbound 23 1)) (TVar (Unbound 24 1)))
       (TCon int ()) Structural_mismatch))
     @ t.stag:2:10-11)
    |}]
;;

let%expect_test "functions are generalized" =
  check {|
let k x y = x
let flip f x y = f y x
let both = k 1 true; k true 1
|};
  [%expect
    {|
    val k : 'a -> 'b -> 'a
    val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
    val both : bool
    |}]
;;

let%expect_test "value restriction" =
  check {|
let weak = id id
|};
  check {|
let weak = id id
let use = weak 1; weak true
|};
  [%expect
    {|
    val weak : '_a -> '_a
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:23-27)
    |}]
;;

let%expect_test "later bindings don't generalize weak variables" =
  check
    {|
let f u =
  let weak = id id in
  let through c = weak c in
  (through 1, through true)
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:5:22-26)
    |}]
;;

let%expect_test "later toplevels don't generalize weak variables" =
  let weak = "let weak = id id\n" in
  let through = "let through c = weak c\n" in
  check (weak ^ through);
  check (through ^ weak);
  check (weak ^ through ^ "let use = (through 1, through true)\n");
  check (weak ^ through ^ "let use = through 1\nlet after = weak\n");
  [%expect
    {|
    val weak : '_a -> '_a
    val through : '_a -> '_a
    val through : '_a -> '_a
    val weak : '_a -> '_a
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:30-34)
    val weak : int -> int
    val through : int -> int
    val use : int
    val after : int -> int
    |}]
;;

let%expect_test "annotations" =
  check {|
let f x : int = x
|};
  check {|
let f = (id : bool -> bool)
|};
  check {|
let f x : bool = add x 1
|};
  [%expect
    {|
    val f : int -> int
    val f : bool -> bool
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:2:17-24)
    |}]
;;

let%expect_test "scope errors" =
  check {|
let f = nope
|};
  check {|
let f x = x
let f = 1
|};
  check {|
let f x = g
let g y = x
|};
  [%expect
    {|
    ((Not_in_scope nope) @ t.stag:2:8-12)
    ((Duplicate_toplevel f) @ t.stag:3:4-5)
    ((Not_in_scope x) @ t.stag:3:10-11)
    |}]
;;

let%expect_test "toplevels are order independent and generalized per SCC" =
  check {|
let main = f 1; f true
let f x = g x
let g x = x
|};
  [%expect
    {|
    val main : bool
    val f : 'a -> 'a
    val g : 'a -> 'a
    |}]
;;

let%expect_test "mutual recursion is monomorphic within an SCC" =
  check {|
let a x = b 1
let b y = a true; y
let c = a true; b 2
|};
  check {|
let a x = b 1
let b y = a true; a 1
|};
  [%expect
    {|
    val a : bool -> int
    val b : int -> int
    val c : int
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:3:20-21)
    |}]
;;

let%expect_test "initialization cycles" =
  (* Recursion through functions, and values only building closures, are fine. *)
  run
    {|
let even n = match n with 0 -> true | _ -> odd (Int.sub n 1)
let odd n = match n with 0 -> false | _ -> even (Int.sub n 1)
let fns = `Fns (even, fun u -> Ptr.make fns)
let stream = `Cons (1, fun u -> Ptr.make stream)
let first = match stream with `Cons (n, _) -> n
let local = let h (u : int) = Int.add 1 (local u) in h
let unused = let f u = unused in 1
let called = even 4
let stored_local = let k = fun u -> Ptr.make stored_local in `Pair (k, 1)
let stored_alias = let k = fun u -> Ptr.make stored_alias in let j = k in `Pair (j, 2)
|};
  (* A local closure needing the value, then called, through an alias. *)
  check {|
let x = let k = fun u -> x in let j = k in j ()
|};
  (* Conservative: safe, but rejected, as calls are assumed to need all the
     callee refers to, and functions to call closures passed to them. *)
  check {|
let f u = (1, fun w -> v)
let v = match f () with (n, _) -> n
|};
  check
    {|
let make u = `Default (fun w -> Ptr.make default)
let default = make ()
|};
  check
    {|
let register h = h
let dispatch x = handlers; x
let handlers = register (fun x -> dispatch x)
|};
  (* A value needing itself, directly or through other toplevels. *)
  check "let x = Int.add x 1";
  check {|
let x = y
let y = Int.add x 1
|};
  check {|
let x = f 1
let f n = g n
let g n = Int.add x n
|};
  (* Calls are assumed to need all the callee refers to, and passing a closure
     to a function, to call it. *)
  check {|
let x = f `A
let f t = match t with `A -> 1 | `B -> x
|};
  check {|
let x = Int.add (id (fun u -> x) ()) 1
|};
  check {|
let x = let h u = x in h 1
|};
  check
    {|
let x = ((fun u -> Int.add y 1), 1)
let y = match x with (f, _) -> f ()
|};
  [%expect
    {|
    val even : int -> bool
    val odd : int -> bool
    val fns : ([`Fns of (int -> bool) * ('_a -> ptr['_b]); ..'_c] as '_b)
    val stream : ([`Cons of int * ('_a -> ptr['_b])] as '_b)
    val first : int
    val local : int -> int
    val unused : int
    val called : bool
    val stored_local : ([`Pair of ('_a -> ptr['_b]) * int; ..'_c] as '_b)
    val stored_alias : ([`Pair of ('_a -> ptr['_b]) * int; ..'_c] as '_b)
    -- evaluated at compile time
    -- evaluated
    (even <fun>)
    (odd <fun>)
    (fns (`Fns (Tuple <fun> <fun>)))
    (stream (`Cons (Tuple 1 <fun>)))
    (first 1)
    (local <fun>)
    (unused 1)
    (called true)
    (stored_local (`Pair (Tuple <fun> 1)))
    (stored_alias (`Pair (Tuple <fun> 2)))
    ((Toplevel_initialization_cycle (x x)) @ t.stag:2:25-26)
    ((Toplevel_initialization_cycle (v f v)) @ t.stag:3:14-15)
    ((Toplevel_initialization_cycle (default make default)) @ t.stag:3:14-18)
    ((Toplevel_initialization_cycle (handlers dispatch handlers)) @
     t.stag:4:34-42)
    ((Toplevel_initialization_cycle (x x)) @ t.stag:1:16-17)
    ((Toplevel_initialization_cycle (y x y)) @ t.stag:3:16-17)
    ((Toplevel_initialization_cycle (x f g x)) @ t.stag:2:8-9)
    ((Toplevel_initialization_cycle (x f x)) @ t.stag:2:8-9)
    ((Toplevel_initialization_cycle (x x)) @ t.stag:2:30-31)
    ((Toplevel_initialization_cycle (x x)) @ t.stag:2:18-19)
    ((Toplevel_initialization_cycle (y x y)) @ t.stag:3:14-15)
    |}]
;;

let%expect_test "SCC containing a non-value is not generalized" =
  check {|
let x = (id 1, fun u -> f u)
let f y = x; y
|};
  [%expect {|
    val x : int * ('_a -> '_a)
    val f : '_a -> '_a
    |}]
;;

let%expect_test "an SCC below an in-progress toplevel is generalized" =
  check
    {|
let a x = b a; c 1
let b x = x
let c x = d x
let d x = c x; a x; x
let e = b 1; b true
|};
  [%expect
    {|
    val a : int -> int
    val b : 'a -> 'a
    val c : int -> int
    val d : int -> int
    val e : bool
    |}]
;;

let%expect_test "a recursive group is generalized at once, in any order" =
  (* The members share variables, which generalizing one at a time would
     simplify while the others still use them: generalizing [g] first gave its
     [y] type [<int>{x?'d : int; ..'e}]. The orders give the same schemes, up to
     which side of an equality is an entry type. *)
  let both_orders f g =
    check (f ^ "\n" ^ g);
    print_endline "---";
    check (g ^ "\n" ^ f)
  in
  both_orders
    "let f c d = let _ = .< let x = .~d in .~c >. in g c d"
    "let g c d = let _ = .< .~c >. in let _ = .< let x = 1 in .~c >. in f c d";
  [%expect
    {|
    val f : <'a>{x?'b : immut 'c; ..'d} -> <'c>{x?'e : immut 'c; ..'f} -> 'g where 'b <= 'h, 'b => 'c = int, 'b => int = 'i, ..'d <= {..'j}, ..'d <= {x?'h : 'i; ..'k}, ..'f <= {..'j}
    val g : <'a>{x?'b : immut int; ..'c} -> <'d>{x?'e : immut 'd; ..'f} -> 'g where 'b <= 'h, 'b => 'd = int, 'b => int = 'i, ..'c <= {..'j}, ..'c <= {x?'h : 'i; ..'k}, ..'f <= {..'j}
    ---
    val g : <'a>{x?'b : immut int; ..'c} -> <'d>{x?'e : immut 'd; ..'f} -> 'g where 'b <= 'h, 'b => int = 'd, 'b => int = 'i, ..'c <= {x?'h : 'i; ..'j}, ..'c <= {..'k}, ..'f <= {..'k}
    val f : <'a>{x?'b : immut 'c; ..'d} -> <'c>{x?'e : immut 'c; ..'f} -> 'g where 'b <= 'h, 'b => int = 'c, 'b => int = 'i, ..'d <= {x?'h : 'i; ..'j}, ..'d <= {..'k}, ..'f <= {..'k}
    |}];
  both_orders
    "let f c = let _ = g c in .< let x = 1 in .~c >."
    "let g c y = cond true (f c) .< let x = .~y in .~c >.";
  [%expect
    {|
    val f : <'a>{x?'b : immut 'c; ..'d} -> <'a>{..'e} where 'b => 'c = int, ..'d <= {..'e}
    val g : <'a>{x?'b : immut 'c; ..'d} -> <'c>{x?'e : immut 'c; ..'f} -> <'a>{..'g} where 'b => 'c = int, ..'d <= {..'g}, ..'f <= {..'g}
    ---
    val g : <'a>{x?'b : immut int; ..'c} -> <'d>{x?'e : immut 'd; ..'f} -> <'a>{..'g} where 'b => int = 'd, ..'c <= {..'g}, ..'f <= {..'g}
    val f : <'a>{x?'b : immut int; ..'c} -> <'a>{..'d} where 'b => int = 'e, ..'c <= {..'d}
    |}]
;;

let%expect_test "a kept bound is added by instances of every group member" =
  (* The scheme of the group keeps an equality on [w]'s weak presence of [x]
     with ['a] of [g] and ['a] of [f]. [f]'s instance must add it again, or the
     first [r] would be accepted with ill-typed generated code. In the
     signatures, [w] has it from the generalization, [r]'s instance of [g] and
     [q]'s of [f]. *)
  let prelude =
    {|let w = let k c = c in id k
let pre d e = .< let y = .~d in let x = .~e in .~(w .<2>.) >.|}
  in
  let f = "let f d c = .< let y = .~d in let x = (.~(g ()), y) in .~(w c) >." in
  let g = "let g u = let _ = f in .< let ident z = z in ident >." in
  let use = "let r = .~(f .< true >. .< match x with | (a, b) -> b >.)" in
  List.iter
    [ [ f; g ]; [ g; f ] ]
    ~f:(fun group ->
      check (String.concat ~sep:"\n" ((prelude :: group) @ [ use ]));
      print_endline "---");
  check
    (String.concat
       ~sep:"\n"
       [ prelude
       ; f
       ; g
       ; "let r = g ()"
       ; "let t = .~r true"
       ; "let q = f .< false >."
       ]);
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:5:24-56)
    ---
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:5:24-56)
    ---
    val w : <int>{x?'_a : immut '_b; y?'_c : immut '_d; ..'_e} -> <int>{x?'_a : immut '_b; y?'_c : immut '_d; ..'_e} where '_a => '_b = ('_f -> '_f) * bool, '_a => '_b = (bool -> bool) * '_g, '_a => '_b = ('_h -> '_h) * '_i, '_c => '_d = bool, '_c => '_d = '_g, '_c => '_d = '_i, ..'_e <= {..'_j}, ..'_e <= {..'_k}
    val pre : <'_a>{y?'b : immut '_a; ..'c} -> <'_d>{x?'e : immut '_d; y?'f : immut '_a; ..'g} -> <int>{..'h} where ..'c <= {..'h}, ..'g <= {..'h}, ..'_i <= {..'h}, ..'_i <= {..'_j}, ..'_i <= {..'_k}
    val f : <'a>{y?'b : immut 'a; ..'c} -> <int>{x?'_d : immut '_e; y?'_f : immut '_g; ..'_h} -> <int>{..'i} where '_f => '_g = 'a, '_d => '_e = ('j -> 'j) * 'a, ..'c <= {..'i}, '_d => '_e = ('_k -> '_k) * bool, '_d => '_e = (bool -> bool) * '_l, '_d => '_e = ('_m -> '_m) * '_n, '_f => '_g = bool, '_f => '_g = '_l, '_f => '_g = '_n, ..'_h <= {..'_o}, ..'_h <= {..'_p}, ..'_h <= {..'i}
    val g : unit -> <'a -> 'a>{x?'b : immut ('a -> 'a) * 'c; y?'d : immut 'c; ..'e} where '_f => '_g = ('a -> 'a) * 'c, '_h => '_i = 'c, ..'e <= {..'j}, '_f => '_g = ('_k -> '_k) * bool, '_f => '_g = (bool -> bool) * '_l, '_f => '_g = ('_m -> '_m) * '_n, '_h => '_i = bool, '_h => '_i = '_l, '_h => '_i = '_n, ..'_o <= {..'j}, ..'_o <= {..'_p}, ..'_o <= {..'_q}
    val r : <bool -> bool>{}
    val t : bool
    val q : <int>{x?'_a : immut '_b; y?'_c : immut '_d; ..'_e} -> <int>{..'_f} where '_a => '_b = ('_g -> '_g) * bool, '_a => '_b = (bool -> bool) * '_h, '_a => '_b = ('_i -> '_i) * '_j, '_c => '_d = bool, '_c => '_d = '_h, '_c => '_d = '_j, ..'_e <= {..'_f}, ..'_e <= {..'_k}
    |}]
;;

let%expect_test "local lets" =
  check
    {|
let poly = let i x = x in i 1; i true
let rec_ = let loop x = loop x in loop
let shadow x = let x = 1 in x
let arg_scope x = (let f x = true in f); x
|};
  check {|
let mono = let i = id id in i 1; i true
|};
  check {|
let mono_rec = let f x = f 1; f true in f
|};
  [%expect
    {|
    val poly : bool
    val rec_ : 'a -> 'b
    val shadow : 'a -> int
    val arg_scope : 'a -> 'a
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:35-39)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:32-36)
    |}]
;;

let%expect_test "quotes" =
  check
    {|
let closed = .<1>.
let open = .<add y (add x y)>.
let local = .<let y = 1 in add y y>.
let persistent = .<id 1>.
let n = 5
let not_persistent = .<n>.
let outer_arg x = .<x>.
let nested = .< .<add x 1>. >.
|};
  [%expect
    {|
    val closed : <int>{..'a}
    val open : <int>{x : int; y : int; ..'a}
    val local : <int>{..'a}
    val persistent : <int>{..'a}
    val n : int
    val not_persistent : <int>{..'a}
    val outer_arg : 'a -> <'b>{x : 'b; ..'c}
    val nested : <<int>{x : int; ..'a}>{..'b}
    |}]
;;

let%expect_test "free variables of a quote are not generalized by inner lets" =
  check {|
let f u = .<let g v = x in add (g 1) 1; g true>.
|};
  [%expect {| val f : 'a -> <int>{x : int; ..'b} |}]
;;

let%expect_test "splice with explicit substitutions" =
  check
    {|
let values = .<.~(.<add x y>.).{x = 1; y = 2}>.
let reorder = .<.~(.<add x y>.).{y = 1; x = 2}>.
let from_scope = .<let x = 1 in .~(.<add x y>.).{x; y}>.
let infers_sub = .<.~(.<add x y>.).{x = z; y = 1}>.
let closed = .<.~(.<1>.).{}>.
|};
  [%expect
    {|
    val values : <int>{..'a}
    val reorder : <int>{..'a}
    val from_scope : <int>{y : int; ..'a}
    val infers_sub : <int>{z : int; ..'a}
    val closed : <int>{..'a}
    |}]
;;

let%expect_test "splice substitution errors" =
  check {|
let wrong_type = .<.~(.<add x 1>.).{x = true}>.
|};
  check {|
let missing = .<.~(.<add x 1>.).{}>.
|};
  check {|
let extra = .<.~(.<add x 1>.).{x = 1; y = 1}>.
|};
  check {|
let dup = .<.~(.<add x 1>.).{x = 1; x = 1}>.
|};
  check {|
let not_code = .<.~(1).{}>.
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:19-45)
    ((Unification_error
      (Row_mismatch
       (RExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
        (RVar (Unbound 39 1)))
       REmpty (Missing_label x)))
     @ t.stag:2:16-34)
    val extra : <int>{..'a}
    ((Duplicate_substitution x) @ t.stag:2:36-37)
    ((Unification_error
      (Type_mismatch
       (TCode (inner (TVar (Unbound 27 1))) (context (RVar (Unbound 28 1))))
       (TCon int ()) Structural_mismatch))
     @ t.stag:2:19-22)
    |}]
;;

let%expect_test "splice with implicit substitutions" =
  check
    {|
let bound = .<let x = 1 in .~(.<add x 1>.)>.
let propagates = .<.~(.<add x 1>.)>.
let unknown_context c = .<.~c>.
let explicit_unknown c = .<.~c.{x = 1}>.
|};
  check
    {|
let two_unknown c1 c2 = .<add .~c1 .~c2>.
let known_and_unknown c = .<let x = 1 in .~(c : <int>{x : int; ..'r})>.
let with_free_var c = .<add y .~c>.
let instantiated = with_free_var .<z>.
|};
  check
    {|
let two_unknown c1 c2 = .<add .~c1 .~c2>.
let conflict = two_unknown .<add y 1>. .<cond y 1 1>.
|};
  check
    {|
let toplevel_unknown =
  .~(let opaque u = opaque u in match 1 with 2 -> opaque 1)
|};
  check {|
let wrong_type = .<let x = true in .~(.<add x 1>.)>.
|};
  [%expect
    {|
    val bound : <int>{..'a}
    val propagates : <int>{x : int; ..'a}
    val unknown_context : <'a>{..'b} -> <'a>{..'c} where ..'b <= {..'c}
    val explicit_unknown : <'a>{x?'b : immut int} -> <'a>{..'c}
    val two_unknown : <int>{..'a} -> <int>{..'b} -> <int>{..'c} where ..'a <= {..'c}, ..'b <= {..'c}
    val known_and_unknown : <int>{x : immut int; ..'a} -> <int>{..'b} where ..'a <= {..'b}
    val with_free_var : <int>{..'a} -> <int>{y : mut?'b int; ..'c} where ..'a <= {y : mut?'b int; ..'c}
    val instantiated : <int>{y : int; z : mut?'_a int; ..'_b} where '_c <= '_a
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:3:39-53)
    ((Splice_evaluation_failed
      (code
       (Let (ident opaque/14) (args (u/13)) (generalized true)
        (body
         (App (Var (Bound (Local opaque/14)) : "'a -> 'b")
          (Var (Bound (Local u/13)) : 'a) : 'b))
        (in_
         (Match (scrutinee (Lit (Int 1) : int))
          (cases
           (((Lit (Int 2) : int)
             (App (Var (Bound (Local opaque/14)) : "int -> <'_c>{}")
              (Lit (Int 1) : int) : <'_c>{}))))
          : <'_c>{}))
        : <'_c>{}))
      (error ("Match failure: no case matches the value 1" @ t.stag:3:32-58)))
     @ t.stag:3:2-59)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:44-45)
    |}]
;;

let%expect_test "levels" =
  check {|
let inner_in_splice = .<let y = 1 in .~y>.
|};
  check
    {|
let outer_in_splice c = .<.~(id c)>.
let rebind_inner = .<let y = 1 in .~(.<y>.)>.
let toplevel_splice = .~(.<1>.)
|};
  check
    {|
let toplevel_splice_arg x = .~(.<x>.)
let not_captured = .<let x = 1 in .~(.<x>.).{x}>.
|};
  check {|
let toplevel_splice_open = .~(.<x>.)
|};
  check {|
let captured = .<let x = true in .~(.<x>.).{}>.
|};
  check {|
let toplevel_splice_level x = .~x
|};
  [%expect
    {|
    ((Variable_used_at_wrong_level (name y) (var_level 1) (used_at 0)) @
     t.stag:2:39-40)
    val outer_in_splice : <'a>{..'b} -> <'a>{..'c} where ..'b <= {..'c}
    val rebind_inner : <int>{..'a}
    val toplevel_splice : int
    val toplevel_splice_arg : 'a -> 'a
    val not_captured : <int>{..'a}
    ((Not_in_scope x) @ t.stag:2:27-36)
    val captured : <bool>{..'a}
    ((Variable_used_at_wrong_level (name x) (var_level 0) (used_at -1)) @
     t.stag:2:32-33)
    |}]
;;

let%expect_test "staged power" =
  let power =
    "let power n x = cond (eq n 0) .<1>. .<add .~x .~(power (add n -1) x)>.\n"
  in
  check (power ^ "let cube = power 3 .<2>.\n");
  check (power ^ "let open = power 3 .<y>.\n");
  check (power ^ "let open_as_closed : <int>{} = power 3 .<y>.\n");
  check (power ^ "let wrong_inner = power 3 .<true>.\n");
  [%expect
    {|
    val power : int -> <int>{..'a} -> <int>{..'b} where ..'a <= {..'b}
    val cube : <int>{..'_a}
    val power : int -> <int>{..'a} -> <int>{..'b} where ..'a <= {..'b}
    val open : <int>{y : mut?'_a int; ..'_b} where '_c <= '_a
    ((Unification_error
      (Row_mismatch
       (RExtend y (PVar (Link Present)) (TVar (Link (TCon int ())))
        (RVar (Unbound 85 1)))
       (RVar (Link REmpty)) (Missing_label y)))
     @ t.stag:2:39-44)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:26-34)
    |}]
;;

let%expect_test "splices capture binders of the quote even once the code is \
                 known"
  =
  let captures_x = "let captures_x c = .<let x = true in .~c>.\n" in
  check
    (captures_x
     ^ {|let closed : <int>{} = .<1>.
let uses_x = captures_x .<cond x 1 2>.
let uses_y = captures_x .<add y 1>.
let uses_closed = captures_x closed
|}
    );
  check (captures_x ^ "let wrong_type = captures_x .<add x 1>.\n");
  [%expect
    {|
    val captures_x : <'a>{x?'b : immut bool; ..'c} -> <'a>{..'d} where ..'c <= {..'d}
    val closed : <int>{}
    val uses_x : <int>{..'_a}
    val uses_y : <int>{y : mut?'_a int; ..'_b} where '_c <= '_a
    val uses_closed : <int>{..'_a}
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:2:28-39)
    |}]
;;

let%expect_test "splices capture the same whether the context is known or not" =
  check
    {|
let known = .<let x = 1 in .~(.<add x y>.)>.
let unknown c = .<let x = 1 in .~c>.
let unknown_applied = unknown .<add x y>.
|};
  [%expect
    {|
    val known : <int>{y : int; ..'a}
    val unknown : <'a>{x?'b : immut int; ..'c} -> <'a>{..'d} where ..'c <= {..'d}
    val unknown_applied : <int>{y : mut?'_a int; ..'_b} where '_c <= '_a
    |}]
;;

let%expect_test "spliced code that may use a name propagates it as such" =
  check
    {|
let twice c = .<let x = true in .~c>.; .<.~c>.
let propagates = twice .<cond x 1 2>.
let not_used = twice .<1>.
(* Instantiation keeps the order of bounds. *)
let instance c = twice c
|};
  check {|
let direct_use c = .<let y = .~(.<let x = true in .~c>.) in x>.
|};
  [%expect
    {|
    val twice : <'a>{x?'b : immut bool; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => bool = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val propagates : <int>{x : bool; ..'_a}
    val not_used : <int>{x?'_a : '_b; ..'_c} where '_d <= '_a, '_d => bool = '_b
    val instance : <'a>{x?'b : immut bool; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => bool = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val direct_use : <'a>{x?'b : immut bool; ..'c} -> <'d>{x : mut?'e 'd; ..'f} where ..'c <= {y?'g : immut 'a; ..'h}, ..'h <= {x : mut?'e 'd; ..'f}
    |}]
;;

let%expect_test "only binders of the enclosing quote capture" =
  check
    {|
let outer_persistent c = let g v = v in .<.~c>.
let add_free add = .<add>.
let not_toplevel = .<.~(add_free 1)>.
|};
  [%expect
    {|
    val outer_persistent : <'a>{..'b} -> <'a>{..'c} where ..'b <= {..'c}
    val add_free : 'a -> <'b>{add : 'b; ..'c}
    val not_toplevel : <'_a>{add : mut?'_b '_a; ..'_c} where '_d <= '_b
    |}]
;;

let%expect_test "names bound at a later level are absent from spliced code" =
  let inner_splice = "let inner_splice c = .< .<let z = 1 in .~(.~c)>. >.\n" in
  check (inner_splice ^ "let uses_z = inner_splice .< .<z>. >.\n");
  check (inner_splice ^ "let uses_z_too_early = inner_splice .<z; .<1>.>.\n");
  check
    {|
let known_z c =
  .< .<let z = 1 in .~(.~(c : <<int>{..'r1}>{z : int; ..'r2}))>. >.
|};
  [%expect
    {|
    val inner_splice : <<'a>{z?'b : immut int; ..'c}>{-z; ..'d} -> <<'a>{..'e}>{..'f} where ..'c <= {..'e}, ..'d <= {..'f}
    val uses_z : <<int>{..'_a}>{..'_b}
    ((Unification_error
      (Row_mismatch
       (RExtend z Absent (TVar (Unbound 54 1)) (RVar (Unbound 55 1)))
       (RExtend z (PVar (Link Present)) (TVar (Unbound 65 1))
        (RVar (Unbound 66 1)))
       (Presence_mismatch z)))
     @ t.stag:2:36-48)
    ((Variable_used_at_wrong_level (name z) (var_level 2) (used_at 1)) @
     t.stag:3:22-62)
    |}]
;;

let%expect_test "free variables of a quote are not duplicated by spliced code" =
  let f = "let f c = .<add x .~c>.\n" in
  check (f ^ "let same = f .<add x 1>.\nlet other = f .<add y 1>.\n");
  check (f ^ "let conflict = f .<cond x 1 1>.\n");
  check
    {|
let later_use c = .<add .~c x>.
let conflict = later_use .<cond x 1 1>.
|};
  [%expect
    {|
    val f : <int>{..'a} -> <int>{x : mut?'b int; ..'c} where ..'a <= {x : mut?'b int; ..'c}
    val same : <int>{x : mut?'_a int; ..'_b} where '_c <= '_a
    val other : <int>{x : int; y : mut?'_a int; ..'_b} where '_c <= '_a
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:2:17-31)
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:3:25-39)
    |}]
;;

let%expect_test "spliced code does not take on the quote's free variables" =
  check
    {|
let closed : <int>{} = .<1>.
let before = .<add x .~closed>.
let after = .<add .~closed x>.
|};
  [%expect
    {|
    val closed : <int>{}
    val before : <int>{x : int; ..'a}
    val after : <int>{x : int; ..'a}
    |}]
;;

let%expect_test "mutually recursive staged functions" =
  let ping_pong =
    {|
let ping n c = cond (eq n 0) c .<let x = 1 in .~(pong n c)>.
let pong n c = .<add .~(ping (add n -1) c) y>.
|}
  in
  check ping_pong;
  check (ping_pong ^ "let uses_x = ping 2 .<x>.\nlet uses_z = ping 2 .<z>.\n");
  check (ping_pong ^ "let uses_z_as_closed : <int>{} = ping 2 .<z>.\n");
  [%expect
    {|
    val ping : int -> <int>{y : mut?'a int; ..'b} -> <int>{y : mut?'a int; ..'b} where 'a <= 'c, ..'b <= {x?'d : immut int; y : mut?'c int; ..'e}, 'c <= 'a, ..'e <= {y : mut?'a int; ..'b}
    val pong : int -> <int>{y : mut?'a int; ..'b} -> <int>{x?'c : immut int; y : mut?'d int; ..'e} where 'a <= 'd, ..'b <= {x?'c : immut int; y : mut?'d int; ..'e}, 'd <= 'a, ..'e <= {y : mut?'a int; ..'b}
    val ping : int -> <int>{y : mut?'a int; ..'b} -> <int>{y : mut?'a int; ..'b} where 'a <= 'c, ..'b <= {x?'d : immut int; y : mut?'c int; ..'e}, 'c <= 'a, ..'e <= {y : mut?'a int; ..'b}
    val pong : int -> <int>{y : mut?'a int; ..'b} -> <int>{x?'c : immut int; y : mut?'d int; ..'e} where 'a <= 'd, ..'b <= {x?'c : immut int; y : mut?'d int; ..'e}, 'd <= 'a, ..'e <= {y : mut?'a int; ..'b}
    val uses_x : <int>{x : immut int; y : mut?'_a int; ..'_b} where '_a <= '_c, ..'_b <= {x : immut int; y : mut?'_c int; ..'_d}, '_c <= '_a, ..'_d <= {x : immut int; y : mut?'_a int; ..'_b}
    val uses_z : <int>{y : mut?'_a int; z : mut?'_b int; ..'_c} where '_a <= '_d, '_b <= '_e, ..'_c <= {x?'_f : immut int; y : mut?'_d int; z : mut?'_e int; ..'_g}, '_d <= '_a, '_e <= '_b, ..'_g <= {y : mut?'_a int; z : mut?'_b int; ..'_c}
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend y (PVar (Link Present)) (TVar (Link (TCon int ())))
        (RVar (Unbound 94 1)))
       (Missing_label y)))
     @ t.stag:4:33-45)
    |}]
;;

let%expect_test "toplevel splices drop names spliced code only may use" =
  check {|
let unused = .~(let g c = .<let x = true in .~c>.; c in g .<1>.)
|};
  check
    {|
let used = .~(let g c = .<let x = true in .~c>.; c in g .<cond x 1 2>.)
|};
  (* The generated code is at level 0, where the toplevel [x] is in scope. *)
  check
    {|
let x = true
let used_toplevel = .~(let g c = .<let x = true in .~c>.; c in g .<x>.)
|};
  (* A name that may be used is not provided by a toplevel, so needn't have its
     type. *)
  check
    {|
let unused_of_another_type =
  .~(let g c = .<let id = true in .~c>.; c in g .<1>.)
|};
  [%expect
    {|
    val unused : int
    ((Not_in_scope x) @ t.stag:2:11-71)
    val x : bool
    val used_toplevel : bool
    val unused_of_another_type : int
    |}]
;;

let%expect_test "using a name before or after splicing code that may use it" =
  (* Splicing [c] into a quote that uses [x] only bounds [c]'s [x] by a used
     one, which decides nothing, whichever comes first. *)
  let closed = "let closed : <int>{} = .<1>.\n" in
  let uses_x_then_splices =
    "let uses_x_then_splices c = .<let x = 1 in .~c>.; .<add x .~c>.; c\n"
  in
  let splices_then_uses_x =
    "let splices_then_uses_x c = .<let x = 1 in .~c>.; .<add .~c x>.; c\n"
  in
  check
    (closed
     ^ uses_x_then_splices
     ^ splices_then_uses_x
     ^ "let uses_x_then_splices_closed = uses_x_then_splices closed\n");
  check
    (closed
     ^ splices_then_uses_x
     ^ "let splices_then_uses_x_closed = splices_then_uses_x closed\n");
  [%expect
    {|
    val closed : <int>{}
    val uses_x_then_splices : <int>{x?'a : immut int; ..'b} -> <int>{x?'a : immut int; ..'b} where ..'b <= {x : int; ..'c}
    val splices_then_uses_x : <int>{x?'a : immut int; ..'b} -> <int>{x?'a : immut int; ..'b} where ..'b <= {x : int; ..'c}
    val uses_x_then_splices_closed : <int>{}
    val closed : <int>{}
    val splices_then_uses_x : <int>{x?'a : immut int; ..'b} -> <int>{x?'a : immut int; ..'b} where ..'b <= {x : int; ..'c}
    val splices_then_uses_x_closed : <int>{}
    |}]
;;

let%expect_test "presence constraints are solved in any order" =
  let program body =
    {|
let closeit (c : <int>{}) = ()
let g d = .<let x = 1 in .~d>.; d
let f c = |}
    ^ body
    ^ "\n"
  in
  (* [g] bounds [c]'s [x] by its quote's, and [.<add .~c2 x>.] makes the quote's
     [x] used, while [closeit] makes [c]'s absent: all orders agree. *)
  List.iter
    [ "close first", "closeit c; let c2 = g c in .<add .~c2 x>."
    ; "close last", "let c2 = g c in let r = .<add .~c2 x>. in closeit c; r"
    ; "close between", "let c2 = g c in closeit c; .<add .~c2 x>."
    ]
    ~f:(fun (name, body) ->
      print_endline name;
      check (program body));
  [%expect
    {|
    close first
    val closeit : <int>{} -> unit
    val g : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'b : immut int; ..'c}
    val f : <int>{} -> <int>{x : int; ..'a}
    close last
    val closeit : <int>{} -> unit
    val g : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'b : immut int; ..'c}
    val f : <int>{} -> <int>{x : int; ..'a}
    close between
    val closeit : <int>{} -> unit
    val g : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'b : immut int; ..'c}
    val f : <int>{} -> <int>{x : int; ..'a}
    |}]
;;

let twice = "let twice c = .<let x = true in .~c>.; .<.~c>.\n"
let closeit = "let closeit (c : <int>{}) = ()\n"
let uses_x = ".<cond x 1 2>."
let needx = "let needx (e : <int>{x : int}) = ()\n"

(* [h]'s argument may use [x] only if [e] does. *)
let local_bounded_by_outer ~in_ =
  "let h d = .<let x = 1 in .~d>.; cond true .<.~d>. e in " ^ in_
;;

let%expect_test "generalization does not depend on when outer presences are \
                 known"
  =
  let uses = "h .<add x 1>.; h k; closeit k" in
  check
    (closeit
     ^ needx
     ^ sprintf
         "let known_before e k = needx e; %s\n"
         (local_bounded_by_outer ~in_:uses)
     ^ sprintf
         "let known_after e k = %s\n"
         (local_bounded_by_outer ~in_:(sprintf "(%s); needx e" uses))
     ^ sprintf "let never_known e k = %s\n" (local_bounded_by_outer ~in_:uses));
  [%expect
    {|
    val closeit : <int>{} -> unit
    val needx : <int>{x : int} -> unit
    val known_before : <int>{x : int} -> <int>{} -> unit
    val known_after : <int>{x : int} -> <int>{} -> unit
    val never_known : <int>{x : int; ..'a} -> <int>{} -> unit
    |}]
;;

let%expect_test "instances of a scheme bounded by an outer presence" =
  (* Each use of [h] constrains [e] and [h]'s scheme keeps the bound. *)
  let program in_ = sprintf "let f e = %s\n" (local_bounded_by_outer ~in_) in
  check (closeit ^ program "h .<1>.; h .<2>.");
  check (closeit ^ program "h .<1>.; h .<add x 1>.");
  check (closeit ^ program "closeit e; h .<add x 1>.");
  check (closeit ^ needx ^ program "needx e; h .<1>.; h .<add x 1>.");
  [%expect
    {|
    val closeit : <int>{} -> unit
    val f : <int>{x?'a : mut?'b 'c; ..'d} -> <int>{x?'a : mut?'b 'c; ..'d}
    val closeit : <int>{} -> unit
    val f : <int>{x : mut?'a int; ..'b} -> <int>{x : mut?'a int; ..'b}
    ((Unification_error
      (Row_mismatch
       (RExtend x (PVar (Link Absent)) (TVar (Link (TCon int ())))
        (RVar (Link (RVar (Unbound 70 1)))))
       (RExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
        (RVar (Unbound 81 1)))
       (Presence_mismatch x)))
     @ t.stag:2:78-89)
    val closeit : <int>{} -> unit
    val needx : <int>{x : int} -> unit
    val f : <int>{x : mut?'a int} -> <int>{x : mut?'a int}
    |}]
;;

let%expect_test "bounds and presences no instance can use are dropped" =
  (* [h]'s instance in [used] bounds [e]'s context by a row used nowhere else,
     and in [spliced] gives presences of [x] the spliced code doesn't use. *)
  check
    {|let closeit (c : <int>{}) = ()
let unused e = let h c = .< let _ = .~e in .~c >. in e
let used e = let h c = .< let _ = .~e in .~c >. in let _ = h .< 1 >. in e
let partial e = let h c = .< let _ = .~e in .~c >. in let _ = h .< 1 >. in h
let lh e = .< let h d = let _ = .< let x = 1 in .~d >. in cond true .< .~d >. e in h >.
let not_spliced e k =
  let r = .< let h d = let _ = .< let x = 1 in .~d >. in cond true .< .~d >. e in 0 >. in
  let _ = closeit k in r
let spliced e k =
  let r = .< let h d = let _ = .< let x = 1 in .~d >. in cond true .< .~d >. e in .~k >. in
  let _ = closeit k in r|};
  [%expect
    {|
    val closeit : <int>{} -> unit
    val unused : <'a>{_?'b : immut 'a; ..'c} -> <'a>{_?'b : immut 'a; ..'c}
    val used : <'a>{_?'b : immut 'a; ..'c} -> <'a>{_?'b : immut 'a; ..'c}
    val partial : <'a>{_?'b : immut 'a; ..'c} -> <'d>{_?'e : immut 'a; ..'f} -> <'d>{..'g} where ..'c <= {..'g}, ..'f <= {..'g}
    val lh : 'a -> <<'b>{x?'c : immut int; ..'d} -> <'b>{x?'e : mut?'f 'g; ..'h}>{e : <'b>{x?'e : mut?'f 'g; ..'h}; ..'i} where 'c <= 'e, 'c => int = 'g, ..'d <= {x?'e : mut?'f 'g; ..'h}
    val not_spliced : 'a -> <int>{} -> <int>{e : <'b>{x?'c : 'd; ..'e}; ..'f}
    val spliced : 'a -> <int>{} -> <int>{e : <'b>{x?'c : 'd; ..'e}; ..'f}
    |}]
;;

let%expect_test "a witness left out still constrains through the scheme" =
  (* [h c]'s context has [e]'s entries through a row of [h]'s type, so the
     witness of [e]'s kept bound adds nothing. *)
  let f =
    {|let closeit (c : <int>{}) = ()
let f e = let h c = .< let _ = .~e in .~c >. in h .< 1 >.
let partial e = let h c = .< let _ = .~e in .~c >. in let _ = h .< 1 >. in h
|}
  in
  check f;
  check (f ^ "let ok = .< let y = 2 in .~(f .< y >.) >.");
  check (f ^ "let bad = closeit (f .< y >.)");
  check (f ^ "let bad = closeit (partial .< y >. .< 3 >.)");
  [%expect
    {|
    val closeit : <int>{} -> unit
    val f : <'a>{_?'b : immut 'a; ..'c} -> <int>{..'d} where ..'c <= {..'d}
    val partial : <'a>{_?'b : immut 'a; ..'c} -> <'d>{_?'e : immut 'a; ..'f} -> <'d>{..'g} where ..'c <= {..'g}, ..'f <= {..'g}
    val closeit : <int>{} -> unit
    val f : <'a>{_?'b : immut 'a; ..'c} -> <int>{..'d} where ..'c <= {..'d}
    val partial : <'a>{_?'b : immut 'a; ..'c} -> <'d>{_?'e : immut 'a; ..'f} -> <'d>{..'g} where ..'c <= {..'g}, ..'f <= {..'g}
    val ok : <int>{..'_a}
    ((Unification_error
      (Row_mismatch
       (RExtend y (PVar (Link Present)) (TVar (Unbound 125 1))
        (RVar (Unbound 128 1)))
       (RVar (Link REmpty)) (Missing_label y)))
     @ t.stag:4:21-28)
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend y (PVar (Link Present)) (TVar (Unbound 139 1))
        (RVar (Unbound 140 1)))
       (Missing_label y)))
     @ t.stag:4:18-43)
    |}]
;;

let%expect_test "presence bounds of polymorphic functions" =
  (* [twice]'s argument may use [x] only if its result does: each use decides
     differently. *)
  check
    (twice
     ^ closeit
     ^ sprintf
         {|let arg_uses_x = twice %s
let result_closed c = closeit (twice c); c
let arg_closed (c : <int>{}) = twice c
|}
         uses_x);
  [%expect
    {|
    val twice : <'a>{x?'b : immut bool; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => bool = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val closeit : <int>{} -> unit
    val arg_uses_x : <int>{x : bool; ..'_a}
    val result_closed : <int>{-x; ..'a} -> <int>{-x; ..'a} where ..'a <= {}
    val arg_closed : <int>{} -> <int>{x?'a : 'b; ..'c}
    |}]
;;

let%expect_test "conflicting presence constraints" =
  check (twice ^ closeit ^ sprintf "let conflict = closeit (twice %s)\n" uses_x);
  check
    (twice
     ^ closeit
     ^ sprintf
         {|let conflict_later c = let r = twice c in closeit r; .<.~c>.
let use = conflict_later %s
|}
         uses_x);
  [%expect
    {|
    ((Unification_error
      (Row_mismatch
       (RExtend x (PVar (Link Absent)) (TVar (Link (TCon bool ())))
        (RVar (Link (RVar (Unbound 60 1)))))
       (RExtend x (PVar (Link Present)) (TVar (Link (TCon bool ())))
        (RVar (Unbound 84 1)))
       (Presence_mismatch x)))
     @ t.stag:3:30-44)
    ((Unification_error
      (Row_mismatch
       (RExtend x (PVar (Link Absent)) (TVar (Link (TCon bool ())))
        (RVar (Link (RVar (Unbound 88 1)))))
       (RExtend x (PVar (Link Present)) (TVar (Link (TCon bool ())))
        (RVar (Unbound 109 1)))
       (Presence_mismatch x)))
     @ t.stag:4:25-39)
    |}]
;;

let%expect_test "references to toplevels in quotes don't join their components" =
  let a = "let a = .<b>.\n" in
  let b = "let b = (a, 1)\n" in
  check (a ^ b);
  check (b ^ a);
  [%expect
    {|
    val a : <'_a>{b : '_a; ..'_b}
    val b : <'_a>{b : '_a; ..'_b} * int
    val b : <'a>{b : 'a; ..'b} * int
    val a : <'a>{b : 'a; ..'b}
    |}]
;;

let tw = "let tw c = .<let x = 1 in .~c>.; .<.~c>.\n"
let needy = "let needy (e : <int>{y : int; ..'r}) = ()\n"

(* [h]'s argument is bounded by [e]'s row through [tw]'s bound. *)
let local_bounded_by_outer_row ~in_ = "let h d = cond true (tw e) d in " ^ in_

let%expect_test "row bounds are solved in any order" =
  let program body = closeit ^ needx ^ tw ^ sprintf "let f e k = %s\n" body in
  List.iter
    [ ( "closed before"
      , "closeit e; "
        ^ local_bounded_by_outer_row ~in_:"h .<add x y>.; h k; closeit k" )
    ; ( "closed between"
      , local_bounded_by_outer_row
          ~in_:"h .<add x y>.; closeit e; h k; closeit k" )
    ; ( "closed after"
      , local_bounded_by_outer_row
          ~in_:"h .<add x y>.; h k; closeit k; closeit e" )
    ; ( "never closed"
      , local_bounded_by_outer_row ~in_:"h .<add x y>.; h k; closeit k" )
    ]
    ~f:(fun (name, body) ->
      print_endline name;
      check (program body));
  [%expect
    {|
    closed before
    val closeit : <int>{} -> unit
    val needx : <int>{x : int} -> unit
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val f : <int>{} -> <int>{} -> unit
    closed between
    val closeit : <int>{} -> unit
    val needx : <int>{x : int} -> unit
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val f : <int>{} -> <int>{} -> unit
    closed after
    val closeit : <int>{} -> unit
    val needx : <int>{x : int} -> unit
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val f : <int>{} -> <int>{} -> unit
    never closed
    val closeit : <int>{} -> unit
    val needx : <int>{x : int} -> unit
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val f : <int>{-x; ..'a} -> <int>{} -> unit where ..'a <= {}
    |}]
;;

let%expect_test "instances of a scheme bounded by an outer row" =
  (* Each use of [h] bounds [e]'s row by its argument's, so what [e] uses every
     argument must allow, whether [e] uses it before or after. *)
  let uses = "(h .<1>.; h k)" in
  let program body = closeit ^ needy ^ tw ^ sprintf "let f e k = %s\n" body in
  List.iter
    [ "unused", local_bounded_by_outer_row ~in_:uses
    ; "unused, k closed", local_bounded_by_outer_row ~in_:(uses ^ "; closeit k")
    ; "used before", "needy e; " ^ local_bounded_by_outer_row ~in_:uses
    ; "used after", local_bounded_by_outer_row ~in_:(uses ^ "; needy e")
    ; ( "used before, k closed"
      , "needy e; " ^ local_bounded_by_outer_row ~in_:(uses ^ "; closeit k") )
    ; ( "used after, k closed"
      , local_bounded_by_outer_row ~in_:(uses ^ "; closeit k; needy e") )
    ]
    ~f:(fun (name, body) ->
      print_endline name;
      check (program body));
  [%expect
    {|
    unused
    val closeit : <int>{} -> unit
    val needy : <int>{y : int; ..'a} -> unit
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val f : <int>{x?'a : immut int; ..'b} -> <int>{x?'c : mut?'d 'e; ..'f} -> <int>{x?'c : mut?'d 'e; ..'f} where 'a <= 'c, 'a => int = 'e, ..'b <= {x?'c : mut?'d 'e; ..'f}
    unused, k closed
    val closeit : <int>{} -> unit
    val needy : <int>{y : int; ..'a} -> unit
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val f : <int>{-x; ..'a} -> <int>{} -> unit where ..'a <= {}
    used before
    val closeit : <int>{} -> unit
    val needy : <int>{y : int; ..'a} -> unit
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val f : <int>{x?'a : immut int; y : int; ..'b} -> <int>{x?'c : mut?'d 'e; y : mut?'f int; ..'g} -> <int>{x?'c : mut?'d 'e; y : mut?'f int; ..'g} where 'a <= 'c, 'a => int = 'e, ..'b <= {x?'c : mut?'d 'e; y : mut?'f int; ..'g}, 'h <= 'f
    used after
    val closeit : <int>{} -> unit
    val needy : <int>{y : int; ..'a} -> unit
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val f : <int>{x?'a : immut int; y : int; ..'b} -> <int>{x?'c : mut?'d 'e; y : mut?'f int; ..'g} -> unit where 'a <= 'c, 'a => int = 'e, ..'b <= {x?'c : mut?'d 'e; y : mut?'f int; ..'g}, 'h <= 'f
    used before, k closed
    ((Unification_error
      (Row_mismatch REmpty
       (RExtend y (PVar (Link Present)) (TVar (Link (TCon int ())))
        (RVar (Unbound 140 1)))
       (Missing_label y)))
     @ t.stag:4:77-78)
    used after, k closed
    ((Unification_error
      (Row_mismatch (RExtend y Present (TCon int ()) (RVar (Unbound 128 1)))
       (RExtend x (PVar (Link Absent)) (TVar (Link (TCon int ())))
        (RVar
         (Link
          (RExtend y (PVar (Link Absent)) (TCon int ()) (RVar (Unbound 130 1))))))
       (Presence_mismatch y)))
     @ t.stag:4:77-78)
    |}]
;;

let%expect_test "weak toplevels in a recursive group or a module" =
  let at_two_types f = sprintf "let use = (%s 1, %s true)\n" f f in
  let scc = "let weak = other; id id\nlet other u = weak u\n" in
  check scc;
  check (scc ^ at_two_types "other");
  check (scc ^ at_two_types "weak");
  check ("module M = struct let weak = id id end\n" ^ at_two_types "M.weak");
  let annotated = "let weak : 'a -> 'a = id id\n" in
  check annotated;
  check (annotated ^ at_two_types "weak");
  [%expect
    {|
    val weak : '_a -> '_a
    val other : '_a -> '_a
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:26-30)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:3:24-28)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:28-32)
    val weak : '_a -> '_a
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:24-28)
    |}]
;;

let%expect_test "an entry code may use only gets a type if it is present" =
  (* [tw c] and the quote give [c]'s [x] different types, which only conflict if
     [c] has [x]: closing [c] makes it absent, whenever it is. *)
  let binds_bool = ".<let x = true in .~c>." in
  let orders =
    [ "closed_first", "closeit c; tw c; " ^ binds_bool
    ; "closed_between", "tw c; closeit c; " ^ binds_bool
    ; "closed_last", "tw c; " ^ binds_bool ^ "; closeit c"
    ; "never_closed", "tw c; " ^ binds_bool
    ]
  in
  List.iter orders ~f:(fun (name, body) ->
    check (closeit ^ tw ^ sprintf "let %s c = %s\n" name body));
  [%expect
    {|
    val closeit : <int>{} -> unit
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val closed_first : <int>{} -> <int>{..'a}
    val closeit : <int>{} -> unit
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val closed_between : <int>{} -> <int>{..'a}
    val closeit : <int>{} -> unit
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val closed_last : <int>{} -> unit
    val closeit : <int>{} -> unit
    val tw : <'a>{x?'b : immut int; ..'c} -> <'a>{x?'d : mut?'e 'f; ..'g} where 'b <= 'd, 'b => int = 'f, ..'c <= {x?'d : mut?'e 'f; ..'g}
    val never_closed : <'a>{-x; ..'b} -> <'a>{..'c} where ..'b <= {..'c}, ..'b <= {x?'d : 'e; ..'f}
    |}]
;;

let%expect_test "a chain of lets each instantiating the previous one" =
  let chain n =
    let rec go i =
      let earlier = if i = 0 then "tw e" else sprintf "h%d (tw e)" (i - 1) in
      let rest = if i = n - 1 then sprintf "h%d k" i else go (i + 1) in
      sprintf "let h%d d = cond true (%s) d in h%d .<1>.; %s" i earlier i rest
    in
    sprintf "let f e k = %s\n" (go 0)
  in
  let typecheck n =
    let t = Option.value_exn (typecheck (tw ^ chain n)) in
    List.last_exn (checked t).signature
  in
  print_string (Signature.to_string_hum [ typecheck 2 ]);
  let f = Signature.to_string_hum [ typecheck 200 ] in
  let constraints =
    match String.substr_index f ~pattern:" where " with
    | None -> 0
    | Some at -> List.length (String.split (String.drop_prefix f at) ~on:',')
  in
  print_s [%message (constraints : int)];
  [%expect
    {|
    val f : <int>{x?'a : immut int; ..'b} -> <int>{x?'c : mut?'d 'e; ..'f} -> <int>{x?'c : mut?'d 'e; ..'f} where 'a <= 'c, 'a => int = 'e, ..'b <= {x?'c : mut?'d 'e; ..'f}
    (constraints 3)
    |}]
;;
