open! Import
open Source_helpers

let%expect_test "mutable locals" =
  run
    {|
let counted =
  let mut n = 0 in
  n <- Int.add n 1;
  n <- Int.add n 1;
  n
let shadowed =
  let mut x = 1 in
  let mut x = Int.add x 10 in
  x
let annotated = let mut xs : list[int] = [] in xs <- [1]; xs
let assign_is_unit = let mut x = 1 in x <- 2
let chained = let mut x = 1 in let mut y = 2 in x <- (y <- 3; Int.add y 1); (x, y)
let not_chained = let mut x = () in let mut y = 2 in x <- y <- 3; (x, y)
let assigned_tuple = let mut p = (1, 2) in p <- 3, 4; p
|};
  [%expect
    {|
    val counted : int
    val shadowed : int
    val annotated : list[int]
    val assign_is_unit : unit
    val chained : int * int
    val not_chained : unit * int
    val assigned_tuple : int * int
    -- evaluated at compile time
    -- evaluated
    (counted 2)
    (shadowed 11)
    (annotated (Cons 1 (Nil)))
    (assign_is_unit "()")
    (chained (Tuple 4 3))
    (not_chained (Tuple "()" 3))
    (assigned_tuple (Tuple 3 4))
    |}]
;;

let%expect_test "closures capture mutable locals by reference" =
  run
    {|
let counter u =
  let mut n = 0 in
  let incr u = n <- Int.add n 1; n in
  let get u = n in
  (incr, get)
let counted =
  match counter () with
  | (incr, get) -> let _ = incr () in let _ = incr () in (incr (), get ())
let separate =
  match (counter (), counter ()) with
  | ((incr1, get1), (incr2, get2)) -> let _ = incr1 () in (get1 (), get2 ())
let sum xs =
  let mut total = 0 in
  let go xs = match xs with [] -> () | Cons * (x, rest) -> total <- Int.add total x; go rest in
  go xs;
  total
let summed = sum [1; 2; 3; 4]
let from_lambda = let mut x = 0 in let set = fun v -> x <- v in set 5; x
|};
  [%expect
    {|
    val counter : 'a -> ('b -> int) * ('c -> int)
    val counted : int * int
    val separate : int * int
    val sum : list[int] -> int
    val summed : int
    val from_lambda : int
    -- evaluated at compile time
    -- evaluated
    (counter <fun>)
    (counted (Tuple 3 3))
    (separate (Tuple 1 0))
    (sum <fun>)
    (summed 10)
    (from_lambda 5)
    |}]
;;

let%expect_test "let mut is not recursive nor generalized" =
  run
    {|
let x = 10
let outer = let mut x = Int.add x 1 in x
let weak = let mut l = [] in l <- [1]; l
|};
  check {|
let bad = let mut l = [] in l <- [1]; l <- [true]
|};
  check {|
let bad = let mut id = fun x -> x in (id 1, id true)
|};
  [%expect
    {|
    val x : int
    val outer : int
    val weak : list[int]
    -- evaluated at compile time
    -- evaluated
    (x 10)
    (outer 11)
    (weak (Cons 1 (Nil)))
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:43-49)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:47-51)
    |}]
;;

let%expect_test "assigning what isn't mutable" =
  check {|
let bad = let x = 1 in x <- 2
|};
  check {|
let x = 1
let bad u = x <- 2
|};
  check {|
let bad u = Int.add <- Int.sub
|};
  check {|
let bad u = nope <- 1
|};
  check {|
let bad = let mut x = 1 in x <- true
|};
  check {|
let bad f = (fun x -> x <- 1) 2
|};
  [%expect
    {|
    ((Assign_to_immutable x) @ t.stag:2:23-29)
    ((Assign_to_immutable x) @ t.stag:3:12-18)
    ((Assign_to_immutable Int.add) @ t.stag:2:12-30)
    ((Not_in_scope nope) @ t.stag:2:12-21)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:32-36)
    ((Assign_to_immutable x) @ t.stag:2:22-28)
    |}]
;;

let%expect_test "mutable locals can't cross levels" =
  check {|
let bad = let mut x = 1 in .<x>.
|};
  check {|
let bad = let mut x = 1 in .<x <- 2>.
|};
  check {|
let bad = let mut x = 1 in .~(let _ = x in .<1>.)
|};
  check {|
let bad = .~(let mut x = 1 in .<x>.)
|};
  run
    {|
let in_code = .<let mut x = 1 in x <- Int.add x 1; x>.
let ran = .~in_code
let at_compile_time =
  .~(let mut x = 1 in x <- 2; match x with 2 -> .<true>. | _ -> .<false>.)
|};
  [%expect
    {|
    ((Mutable_used_outside_its_code (name x) (var_level 0) (used_at 1)) @
     t.stag:2:29-30)
    ((Mutable_used_outside_its_code (name x) (var_level 0) (used_at 1)) @
     t.stag:2:29-35)
    ((Mutable_used_outside_its_code (name x) (var_level 0) (used_at -1)) @
     t.stag:2:38-39)
    ((Mutable_used_outside_its_code (name x) (var_level -1) (used_at 0)) @
     t.stag:2:32-33)
    val in_code : <int>{..'a}
    val ran : int
    val at_compile_time : bool
    -- evaluated at compile time
    (in_code
     (Code
      (Let_mut (ident x/15) (body (Lit (Int 1) : int))
       (in_
        (Seq
         (Assign (Bound (Local x/15))
          (App
           (App (Var (Prim Int.add) : "int -> int -> int")
            (Var (Bound (Local x/15)) : int) : "int -> int")
           (Lit (Int 1) : int) : int)
          : unit)
         (Var (Bound (Local x/15)) : int) : int))
       : int)))
    -- evaluated
    (in_code
     (Code
      (Let_mut (ident x/15) (body (Lit (Int 1) : int))
       (in_
        (Seq
         (Assign (Bound (Local x/15))
          (App
           (App (Var (Prim Int.add) : "int -> int -> int")
            (Var (Bound (Local x/15)) : int) : "int -> int")
           (Lit (Int 1) : int) : int)
          : unit)
         (Var (Bound (Local x/15)) : int) : int))
       : int)))
    (ran 2)
    (at_compile_time true)
    |}]
;;

let%expect_test "closures reaching a mutable local are free variables of quotes"
  =
  (* They are [nonportable], so not usable in quotes: the code leaves them to
     its context. *)
  (* Directly. *)
  check {|
let bad = let mut n = 0 in let f u = n in .<f ()>.
|};
  (* Through a closure it captures. *)
  check
    {|
let bad = let mut n = 0 in let f u = n in let g u = f () in .<g ()>.
|};
  (* Through data holding a closure. *)
  check
    {|
let bad =
  let mut n = 0 in
  let h = fun u -> n <- 1 in
  let g u = h () in
  .<g ()>.
|};
  check
    {|
let bad =
  let mut n = 0 in
  let pair = (1, fun u -> n) in
  let g u = match pair with (_, h) -> h () in
  .<g ()>.
|};
  check
    {|
let bad =
  let mut n = 0 in
  let opt = Some (fun u -> n) in
  let g u = match opt with Some h -> h () | None -> 0 in
  .<g ()>.
|};
  (* Through an argument, which a caller could make capture one. *)
  check {|
let bad (h : unit -> int) = let g u = h () in .<g ()>.
|};
  check {|
let bad x = let g u = x in .<g ()>.
|};
  check {|
let bad (x : option[unit -> int]) = let g u = x in .<g ()>.
|};
  (* Arguments that can't hold a closure, but aren't portable either. *)
  check {|
let not_portable (x : int) (y : list[string]) = let g u = (x, y) in .<g ()>.
|};
  check {|
let not_portable_code (c : <int>{}) = let g u = c in .<g ()>.
|};
  run
    {|
let not_mentioned = let mut n = 0 in let g u = 1 in .<g ()>.
let only_data = let mut n = 0 in let v = n in let g u = v in .<g ()>.
let at_runtime = let mut n = 0 in let f u = n in f ()
|};
  [%expect
    {|
    val bad : <'_a>{f : unit -> '_a; ..'_b}
    val bad : <'_a>{g : unit -> '_a; ..'_b}
    val bad : <'_a>{g : unit -> '_a; ..'_b}
    val bad : <'_a>{g : unit -> '_a; ..'_b}
    val bad : <'_a>{g : unit -> '_a; ..'_b}
    val bad : (unit -> int) -> <'a>{g : unit -> 'a; ..'b}
    val bad : 'a -> <'b>{g : unit -> 'b; ..'c}
    val bad : option[unit -> int] -> <'a>{g : unit -> 'a; ..'b}
    val ok : int -> list[string] -> <'a>{g : unit -> 'a; ..'b}
    val ok_code : <int>{} -> <'a>{g : unit -> 'a; ..'b}
    val not_mentioned : <int>{..'a}
    val only_data : <int>{..'_a}
    val at_runtime : int
    -- evaluated at compile time
    -- evaluated
    (ok <fun>)
    (ok_code <fun>)
    (not_mentioned
     (Code
      (App (Var (Persistent (Local g/34)) : "unit -> int") (Lit Unit : unit) :
       int)
      ((g/34 <fun>))))
    (only_data
     (Code
      (App (Var (Persistent (Local g/35)) : "unit -> int") (Lit Unit : unit) :
       int)
      ((g/35 <fun>))))
    (at_runtime 0)
    |}]
;;

let%expect_test "mutable toplevels" =
  run
    {|
let mut counter = 0
let incr u = counter <- Int.add counter 1
let after = incr (); incr (); counter
let mut log : list[string] = []
let push s = log <- Cons (Ptr.make ( (s, log)))
let logged = push "a"; push "b"; log
let mut weak = []
let fixed = weak <- [true]; weak
|};
  [%expect
    {|
    val counter : int
    val incr : 'a -> unit
    val after : int
    val log : list[string]
    val push : string -> unit
    val logged : list[string]
    val weak : list[bool]
    val fixed : list[bool]
    -- evaluated at compile time
    -- evaluated
    (counter 2)
    (incr <fun>)
    (after 2)
    (log (Cons (String b) (Cons (String a) (Nil))))
    (push <fun>)
    (logged (Cons (String b) (Cons (String a) (Nil))))
    (weak (Cons true (Nil)))
    (fixed (Cons true (Nil)))
    |}]
;;

let%expect_test "mutable toplevels are initialized on demand" =
  run
    {|
let early = counter <- Int.add counter 1; counter
let mut counter = print_endline "init"; 10
let late = counter
|};
  [%expect
    {|
    val early : int
    val counter : int
    val late : int
    -- evaluated at compile time
    -- evaluated
    init
    (counter 11)
    (early 11)
    (late 11)
    |}]
;;

let%expect_test "a mutable toplevel can't need itself" =
  check {|
let mut x = Int.add x 1
|};
  check {|
let mut x = (fun u -> x) ()
|};
  run {|
let mut x = y
let y = x
|};
  (* A function it holds may use it: it reads its current value when called. *)
  run
    {|
let mut count = fun n -> match n with 0 -> 0 | _ -> Int.add 1 (count (Int.sub n 1))
let three = count 3
let replaced = count <- (fun n -> n)
let still = count 5
|};
  [%expect
    {|
    ((Toplevel_initialization_cycle (x x)) @ t.stag:2:20-21)
    ((Toplevel_initialization_cycle (x x)) @ t.stag:2:22-23)
    ((Toplevel_initialization_cycle (y x y)) @ t.stag:3:8-9)
    val count : int -> int
    val three : int
    val replaced : unit
    val still : int
    -- evaluated at compile time
    -- evaluated
    (count <fun>)
    (three 3)
    (replaced "()")
    (still 5)
    |}]
;;

let%expect_test "mutable toplevels across modules" =
  run
    {|
module M = struct
  let mut x = 1
  let get u = x
end
let set = M.x <- 5; M.get ()
let read = M.x
|};
  [%expect
    {|
    module M : sig
      val x : int
      val get : 'a -> int
    end
    val set : int
    val read : int
    -- evaluated at compile time
    -- evaluated
    (M.x 5)
    (M.get <fun>)
    (set 5)
    (read 5)
    |}]
;;

let%expect_test "generated code mutates the runtime instance" =
  run
    {|
let mut counter = 0
let incr_code = .<counter <- Int.add counter 1>.
let bump u = .~incr_code; .~(.<counter <- Int.add counter 10>.); counter
let bumped = bump ()
let bumped_again = bump ()
|};
  [%expect
    {|
    val counter : int
    val incr_code : <unit>{..'a}
    val bump : 'a -> int
    val bumped : int
    val bumped_again : int
    -- evaluated at compile time
    (incr_code
     (Code
      (Assign (Persistent (Toplevel counter/4))
       (App
        (App (Var (Prim Int.add) : "int -> int -> int")
         (Var (Persistent (Toplevel counter/4)) : int) : "int -> int")
        (Lit (Int 1) : int) : int)
       : unit)))
    -- evaluated
    (counter 22)
    (incr_code
     (Code
      (Assign (Persistent (Toplevel counter/4))
       (App
        (App (Var (Prim Int.add) : "int -> int -> int")
         (Var (Persistent (Toplevel counter/4)) : int) : "int -> int")
        (Lit (Int 1) : int) : int)
       : unit)))
    (bump <fun>)
    (bumped 11)
    (bumped_again 22)
    |}]
;;

let%expect_test "mutable toplevels can't be used at compile time" =
  check {|
let mut counter = 0
let bad = .~(counter <- 1; .<1>.)
|};
  check {|
let mut counter = 0
let bad = .~(let _ = counter in .<1>.)
|};
  (* Through toplevel functions. *)
  check
    {|
let mut counter = 0
let incr u = counter <- Int.add counter 1
let incr_twice u = incr (); incr ()
let bad = .~(incr_twice (); .<1>.)
|};
  (* Through a mutually recursive component. *)
  check
    {|
let mut counter = 0
let ping n = match n with 0 -> counter | _ -> pong (Int.sub n 1)
let pong n = ping n
let bad = .~(let _ = pong 3 in .<1>.)
|};
  (* Across modules. *)
  check
    {|
module M = struct
  let mut x = 1
  let get u = x
end
module N = struct
  let get u = M.get ()
end
let bad = .~(let _ = N.get () in .<1>.)
|};
  (* Through a local closure. *)
  check
    {|
let mut counter = 0
let f u = let g u = counter in g
let bad = .~(let _ = f () () in .<1>.)
|};
  [%expect
    {|
    ((Compile_time_use_of_mutable (toplevel counter) (mutable_ counter)) @
     t.stag:3:13-25)
    ((Compile_time_use_of_mutable (toplevel counter) (mutable_ counter)) @
     t.stag:3:21-28)
    ((Compile_time_use_of_mutable (toplevel incr_twice) (mutable_ counter)) @
     t.stag:5:13-23)
    ((Compile_time_use_of_mutable (toplevel pong) (mutable_ counter)) @
     t.stag:5:21-25)
    ((Compile_time_use_of_mutable (toplevel N.get) (mutable_ M.x)) @
     t.stag:9:21-26)
    ((Compile_time_use_of_mutable (toplevel f) (mutable_ counter)) @
     t.stag:4:21-22)
    |}]
;;

let%expect_test "code using mutable toplevels can be built at compile time" =
  run
    {|
let mut counter = 0
let builds u = .<counter <- Int.add counter 1>.
let pure u = 1
let at_compile_time = .~(match pure () with 1 -> .<true>. | _ -> .<false>.)
let uses = .~(builds ()); .~(.<counter <- Int.add counter 1>.); counter
|};
  [%expect
    {|
    val counter : int
    val builds : 'a -> <unit>{..'b}
    val pure : 'a -> int
    val at_compile_time : bool
    val uses : int
    -- evaluated at compile time
    (builds <fun>)
    (pure <fun>)
    -- evaluated
    (counter 2)
    (builds <fun>)
    (pure <fun>)
    (at_compile_time true)
    (uses 2)
    |}]
;;

let%expect_test "expect tests share mutable toplevels" =
  Test_expect_test_runner.test
    {x|let mut n = 0
let%expect_test "a" =
  n <- Int.add n 1;
  print_int n;
  [%expect {| 1 |}]
let%expect_test "b" =
  n <- Int.add n 1;
  print_int n;
  [%expect {| 2 |}]
|x};
  [%expect
    {|
    ((name (a @ t.stag:2:16-19)) (mismatches ()) (error ()))
    ((name (b @ t.stag:6:16-19)) (mismatches ()) (error ()))
    (no corrections)
    |}]
;;

let%expect_test "same-level mutable locals in code built by a splice" =
  (* Read through a free variable of the inner quote, provided at the splice. *)
  run
    {|
let read = .<let mut x = 3 in x <- 4; .~(.<x>.)>.
let ran = .~read
let at_runtime = let mut x = 1 in x <- 2; .~(.<x>.)
|};
  run
    {|
let assigned = .<let mut x = 0 in .~(.<x <- 2>.); x>.
let ran_assigned = .~assigned
|};
  [%expect
    {|
    val read : <int>{..'a}
    val ran : int
    val at_runtime : int
    -- evaluated at compile time
    (read
     (Code
      (Let_mut (ident x/15) (body (Lit (Int 3) : int))
       (in_
        (Seq (Assign (Bound (Local x/15)) (Lit (Int 4) : int) : unit)
         (Var (Bound (Local x/15)) : int) : int))
       : int)))
    -- evaluated
    (read
     (Code
      (Let_mut (ident x/15) (body (Lit (Int 3) : int))
       (in_
        (Seq (Assign (Bound (Local x/15)) (Lit (Int 4) : int) : unit)
         (Var (Bound (Local x/15)) : int) : int))
       : int)))
    (ran 4)
    (at_runtime 2)
    val assigned : <int>{..'a}
    val ran_assigned : int
    -- evaluated at compile time
    (assigned
     (Code
      (Let_mut (ident x/14) (body (Lit (Int 0) : int))
       (in_
        (Seq (Assign (Bound (Local x/14)) (Lit (Int 2) : int) : unit)
         (Var (Bound (Local x/14)) : int) : int))
       : int)))
    -- evaluated
    (assigned
     (Code
      (Let_mut (ident x/14) (body (Lit (Int 0) : int))
       (in_
        (Seq (Assign (Bound (Local x/14)) (Lit (Int 2) : int) : unit)
         (Var (Bound (Local x/14)) : int) : int))
       : int)))
    (ran_assigned 2)
    |}]
;;

let%expect_test "closures capturing arguments are free variables of quotes" =
  (* Arguments are [nonportable], whatever their type, until mode crossing by
     kind: even [ok]'s [g], capturing an [int], is not usable in its quote. *)
  check {|
let bad x = let g u = x in .<g ()>.
|};
  check
    {|
let bad (h : unit -> int) = let k = h in let g u = k () in .<g ()>.
|};
  check
    {|
let bad = let mut n = 0 in let f u = n in let g u = f () in .<g ()>.
|};
  run {|
let ok (x : int) = let g u = x in .<g ()>.
let ran = .~(ok 1)
|};
  [%expect
    {|
    val bad : 'a -> <'b>{g : unit -> 'b; ..'c}
    val bad : (unit -> int) -> <'a>{g : unit -> 'a; ..'b}
    val bad : <'_a>{g : unit -> '_a; ..'_b}
    val ok : int -> <int>{..'a}
    val ran : int
    -- evaluated at compile time
    (ok <fun>)
    -- evaluated
    (ok <fun>)
    (ran 1)
    |}]
;;

let%expect_test "a mutable toplevel is persistent whatever its component" =
  run
    {|
let v = fun u -> x
let mut x = let _ = fun u -> v in 0
let d = .<x <- 1>.
let ran = .~d; x
|};
  [%expect
    {|
    val v : '_a -> int
    val x : int
    val d : <unit>{..'a}
    val ran : int
    -- evaluated at compile time
    (d (Code (Assign (Persistent (Toplevel x/5)) (Lit (Int 1) : int) : unit)))
    -- evaluated
    (v <fun>)
    (x 1)
    (d (Code (Assign (Persistent (Toplevel x/5)) (Lit (Int 1) : int) : unit)))
    (ran 1)
    |}]
;;

let%expect_test "generated code with a closure reaching a mutable local" =
  check
    {|
let bad =
  .~(Stag.Expr.Let_mut (Ptr.make (
       ( "n"
       , Stag.Annot.none
       , Stag.Expr.int 0
       , Stag.Expr.Let (Ptr.make (
           ( "f"
           , [ ("u", Stag.Annot.none) ]
           , Stag.Annot.none
           , Stag.Expr.var "n"
           , Stag.Expr.Quote (Ptr.make ( (Stag.Expr.App (Ptr.make ( (Stag.Expr.var "f", Stag.Expr.Lit Stag.Lit.Unit)))))) ))) ))))
|};
  [%expect {| val bad : <'_a>{f : unit -> '_a; ..'_b} |}]
;;

(* The signature, then the values of the toplevels named [*_value] at runtime. *)
let run_values source =
  Option.iter (typecheck source) ~f:(fun t ->
    print_signature t;
    print_endline "-- values";
    (match Driver.eval ~limits (driver t) with
     | Ok () -> ()
     | Error error -> print_s [%sexp (error : Error.t)]);
    List.iter (members t) ~f:(fun { ident; _ } ->
      if String.is_suffix ident.name ~suffix:"_value"
      then (
        match Interpreter.find (Driver.interpreter (driver t)) ~ident with
        | Ok value ->
          print_s [%sexp (ident.name : string), (value : Interpreter.Value.t)]
        | Error _ -> ())))
;;

let%expect_test "code assigning a free variable" =
  run_values
    {|
let incr = .<x <- Int.add x 1>.
let read = .<x>.
let ran = .<let mut x = 0 in .~incr; .~incr; x>.
let ran_value = .~ran
let mixed = .<.~read; .~incr>.
let generator c = .<y <- Int.add y .~c>.
let twice = .<let mut y = 1 in let mut z = 10 in .~(generator .<2>.); .~(generator .<3>.).{y = z}; (y, z)>.
let twice_value = .~twice
let mut global = 5
let global_incr = .<let _ = .~incr.{x = global} in global>.
let global_value = .~global_incr
let explicit = .<let mut y = 1 in let mut x = 10 in .~incr.{x = y}; .~incr.{x}; (x, y)>.
let explicit_value = .~explicit
let reads_mut = .<let mut x = 4 in .~read.{x = x}>.
let reads_mut_value = .~reads_mut
let annotated : <unit>{x : mut int} = incr
let spliced
  : .~(Stag.Type_expr.Code (Ptr.make (
         { `inner = Stag.Type_expr.con "unit" []
         ; `context =
             Stag.Type_expr.Row.Extend (Ptr.make (
               ("x", Stag.Type_expr.Mut (Ptr.make ( (Stag.Type_expr.con "int" []))), Stag.Type_expr.Row.Empty)))
         })))
  = incr
|};
  [%expect
    {|
    val incr : <unit>{x : mut int; ..'a}
    val read : <'a>{x : 'a; ..'b}
    val ran : <int>{..'a}
    val ran_value : int
    val mixed : <unit>{x : mut int; ..'a}
    val generator : <int>{..'a} -> <unit>{y : mut int; ..'b} where ..'a <= {y : mut int; ..'b}
    val twice : <int * int>{}
    val twice_value : int * int
    val global : int
    val global_incr : <int>{..'a}
    val global_value : int
    val explicit : <int * int>{..'a}
    val explicit_value : int * int
    val reads_mut : <int>{..'a}
    val reads_mut_value : int
    val annotated : <unit>{x : mut int}
    val spliced : <unit>{x : mut int}
    -- values
    (ran_value 2)
    (twice_value (Tuple 3 13))
    (global_value 6)
    (explicit_value (Tuple 11 2))
    (reads_mut_value 4)
    |}];
  check
    {|
let incr = .<x <- Int.add x 1>.
let plain = .<let x = 0 in .~incr; x>.
|};
  check {|
let incr = .<x <- Int.add x 1>.
let f y = .<.~incr.{x = 1}>.
|};
  check
    {|
let incr = .<x <- Int.add x 1>.
let f (y : int) = .<.~incr.{x = Int.add y 1}>.
|};
  check
    {|
let incr = .<x <- Int.add x 1>.
let f y = .<let z = 1 in .~incr.{x = z}>.
|};
  check
    {|
let incr = .<x <- Int.add x 1>.
let g = 1
let f y = .<.~incr.{x = g}>.
|};
  check
    {|
let incr = .<x <- Int.add x 1>.
let wrong : <unit>{x : immut int} = incr
|};
  check
    {|
let reading_is_fine : <int>{x : mut int} = .<x>.
let wrong_place : {`a : mut int} -> int = fun r -> 1
|};
  check {|
let wrong_place : {`a : immut int} -> int = fun r -> 1
|};
  [%expect
    {|
    ((Spliced_code_assigns_immutable x) @ t.stag:3:27-33)
    ((Spliced_code_assigns_immutable x) @ t.stag:3:12-26)
    ((Spliced_code_assigns_immutable x) @ t.stag:3:12-36)
    ((Spliced_code_assigns_immutable x) @ t.stag:3:25-39)
    ((Spliced_code_assigns_immutable x) @ t.stag:4:12-26)
    ((Unification_error
      (Row_mismatch (RExtend x Present (TCon int ()) REmpty)
       (RExtend x (PVar (Link Present)) Assigned (TVar (Link (TCon int ())))
        (RVar (Unbound 37 1)))
       (Assigned x)))
     @ t.stag:3:36-40)
    (Misplaced_mut @ t.stag:3:24-31)
    (Misplaced_mut @ t.stag:2:24-33)
    |}]
;;

let%expect_test "assignment markers in code types" =
  check
    {|
let f c = .<let mut x = 1 in .~c>.
let g c = .<let x = 1 in .~c>.
let reads = .<x>.
let assigns = .<x <- 1>.
let shared c = .<.~c; x>.
let unmarked_reads : <int>{x : int} = reads
let unmarked_assigns : <unit>{x : int} = assigns
let marked : <unit>{x : mut int} = assigns
let immut_reads : <int>{x : immut int} = reads
let unknown (c : <unit>{x : int}) = .<let x = 1 in .~c>.
let unknown_mut (c : <unit>{x : int}) = .<let mut x = 1 in .~c>.
let applied = unknown_mut assigns
|};
  [%expect
    {|
    val f : <'a>{x?'b : int; ..'c} -> <'a>{..'d} where ..'c <= {..'d}
    val g : <'a>{x?'b : immut int; ..'c} -> <'a>{..'d} where ..'c <= {..'d}
    val reads : <'a>{x : 'a; ..'b}
    val assigns : <unit>{x : mut int; ..'a}
    val shared : <'a>{..'b} -> <'c>{x : mut?'d 'c; ..'e} where ..'b <= {x : mut?'d 'c; ..'e}
    val unmarked_reads : <int>{x : int}
    val unmarked_assigns : <unit>{x : mut int}
    val marked : <unit>{x : mut int}
    val immut_reads : <int>{x : immut int}
    val unknown : <unit>{x : immut int} -> <unit>{..'a}
    val unknown_mut : <unit>{x : int} -> <unit>{..'a}
    val applied : <unit>{..'_a}
    |}];
  check
    {|
let assigns = .<x <- 1>.
let g c = .<let x = 1 in .~c>.
let rejected = g assigns
|};
  check
    {|
let assigns = .<x <- 1>.
let unknown (c : <unit>{x : int}) = .<let x = 1 in .~c>.
let rejected = unknown assigns
|};
  [%expect
    {|
    ((Unification_error
      (Row_mismatch
       (RExtend x (PVar (Link Present)) (TVar (Link (TCon int ())))
        (RVar (Unbound 51 1)))
       (RExtend x (PVar (Link Present)) Assigned (TVar (Link (TCon int ())))
        (RVar (Unbound 56 1)))
       (Assigned x)))
     @ t.stag:4:17-24)
    ((Unification_error
      (Row_mismatch (RExtend x Present (TCon int ()) REmpty)
       (RExtend x (PVar (Link Present)) Assigned (TVar (Link (TCon int ())))
        (RVar (Unbound 49 1)))
       (Assigned x)))
     @ t.stag:4:23-30)
    |}]
;;

let%expect_test "records' and variants' entries are never mut" =
  check {|
let f (c : <unit>{..'r}) (r : {..'r}) = r
let g = f .<y <- 1>.
|};
  check
    {|
let f (r : {..'r}) (c : <unit>{..'r}) = r
let g = f {`y = 1} .<y <- 1>.
|};
  check {|
let f (c : <unit>{..'r}) (v : [..'r]) = v
let g = f .<y <- 1>.
|};
  check
    {|
type with_x[{..'r}] = {`x : int; ..'r}
let f (r : with_x[{`a : mut int}]) = r
|};
  check
    {|
type ctx[{..'r}] = <unit>{..'r}
let fine : ctx[{`x : mut int}] = .<x <- 1>.
|};
  check
    {|
let f (c : <unit>{..'r}) = let _ = (c : <unit>{y : mut int}) in fun (r : {..'r}) -> r
|};
  [%expect
    {|
    ((Unification_error
      (Assigned_in_record y
       (RExtend y (PVar (Link Present)) Assigned (TVar (Link (TCon int ())))
        (RVar (Unbound 39 1)))))
     @ t.stag:3:10-20)
    ((Unification_error
      (Row_mismatch (RExtend y Present (TCon int ()) REmpty)
       (RExtend y (PVar (Link Present)) Assigned (TVar (Link (TCon int ())))
        (RVar (Unbound 42 1)))
       (Assigned y)))
     @ t.stag:3:19-29)
    ((Unification_error
      (Assigned_in_record y
       (RExtend y (PVar (Link Present)) Assigned (TVar (Link (TCon int ())))
        (RVar (Unbound 39 1)))))
     @ t.stag:3:10-20)
    ((Unification_error
      (Assigned_in_record a
       (RExtend x Present (TCon int ())
        (RExtend a Present Assigned (TCon int ()) REmpty))))
     @ t.stag:3:11-33)
    type ctx[{..'a}] = <unit>{..'a}
    val fine : <unit>{x : mut int}
    ((Unification_error
      (Assigned_in_record y
       (RVar (Link (RExtend y Present Assigned (TCon int ()) REmpty)))))
     @ t.stag:2:73-79)
    |}]
;;

let%expect_test "explicit substitutions at toplevel splices" =
  run
    {|
let incr = .<x <- Int.add x 1>.
let read = .<fun u -> x>.
let pair = .<(a, b)>.
let mut g = 10
let renamed = let mut y = 0 in .~incr.{x = y}; .~incr.{x = y}; y
let same_name = let mut x = 5 in .~incr.{x}; x
let global = .~incr.{x = g}; g
let reads_current = let mut y = 0 in let f = .~read.{x = y} in y <- 5; f ()
let in_quote = .~(.<let mut y = 0 in let f = .~read.{x = y} in y <- 5; f ()>.)
let order = let mut y = 0 in .~pair.{a = y; b = y <- 5}
let order_in_quote = .~(.<let mut y = 0 in .~pair.{a = y; b = y <- 5}>.)
let value_once =
  let mut n = 0 in
  let next u = n <- Int.add n 1; n in
  let p = .~pair.{a = next (); b = next ()} in
  (p, n)
|};
  [%expect
    {|
    val incr : <unit>{x : mut int; ..'a}
    val read : <'a -> 'b>{x : 'b; ..'c}
    val pair : <'a * 'b>{a : 'a; b : 'b; ..'c}
    val g : int
    val renamed : int
    val same_name : int
    val global : int
    val reads_current : int
    val in_quote : int
    val order : int * unit
    val order_in_quote : int * unit
    val value_once : (int * int) * int
    -- evaluated at compile time
    (incr
     (Code
      (Assign (Free x)
       (App
        (App (Var (Prim Int.add) : "int -> int -> int") (Var (Free x) : int) :
         "int -> int")
        (Lit (Int 1) : int) : int)
       : unit)))
    (read (Code (Fun (args (u/24)) (body (Var (Free x) : 'a)) : "'b -> 'a")))
    (pair (Code (Tuple ((Var (Free a) : 'a) (Var (Free b) : 'b)) : "'a * 'b")))
    -- evaluated
    (incr
     (Code
      (Assign (Free x)
       (App
        (App (Var (Prim Int.add) : "int -> int -> int") (Var (Free x) : int) :
         "int -> int")
        (Lit (Int 1) : int) : int)
       : unit)))
    (read (Code (Fun (args (u/24)) (body (Var (Free x) : 'a)) : "'b -> 'a")))
    (pair (Code (Tuple ((Var (Free a) : 'a) (Var (Free b) : 'b)) : "'a * 'b")))
    (g 11)
    (renamed 2)
    (same_name 6)
    (global 11)
    (reads_current 5)
    (in_quote 5)
    (order (Tuple 5 "()"))
    (order_in_quote (Tuple 5 "()"))
    (value_once (Tuple (Tuple 1 2) 2))
    |}];
  check
    {|
let incr = .<x <- Int.add x 1>.
let immutable = let y = 0 in .~incr.{x = y}
let expression = .~incr.{x = 1}
|};
  [%expect
    {|
    ((Spliced_code_assigns_immutable x) @ t.stag:3:29-43)
    ((Spliced_code_assigns_immutable x) @ t.stag:4:17-31)
    |}]
;;

let%expect_test "closures assigning through spliced code are not persistent" =
  check
    {|
let incr = .<x <- Int.add x 1>.
let implicit = .<let mut x = 0 in let g u = .~incr in .<g ()>.>.
let explicit = .<let mut y = 0 in let g u = .~incr.{x = y} in .<g ()>.>.
let fine = .<let mut x = 0 in let g u = .~incr in g (); x>.
|};
  [%expect
    {|
    val incr : <unit>{x : mut int; ..'a}
    val implicit : <<'a>{g : unit -> 'a; ..'b}>{..'c}
    val explicit : <<'a>{g : unit -> 'a; ..'b}>{..'c}
    val fine : <int>{..'a}
    |}]
;;
