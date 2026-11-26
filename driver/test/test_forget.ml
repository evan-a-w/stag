open! Import

let show source =
  match Mode_helpers.signature source with
  | Ok s -> print_string s
  | Error errors -> List.iter errors ~f:print_endline
;;

let eval source =
  Option.iter
    (Source_helpers.typecheck source)
    ~f:Source_helpers.eval_and_print_values
;;

let%expect_test "a local the quote can't embed is an error, not a free variable"
  =
  show {|
let argument x = .< x >.
|};
  show {|
let computed = let k = Int.to_string 3 in .< k >.
|};
  [%expect
    {|
    The quote can't embed x, as it may not be portable. Bind it as (x @ portable) to embed it, or write [forget x in ...] to make x a free variable of the quote
    The quote can't embed k, as it isn't portable. Write [forget k in ...] to make k a free variable of the quote
    |}]
;;

let%expect_test "forget makes names free variables of quotes" =
  show
    {|
let argument x = forget x in .< x >.
let inside x = .< forget x in x >.
let two x y = forget x, y in .< Int.add x y >.
let filled x = forget x in .< Int.add x 1 >.
let used = .~(filled 0).{x = 41}
let spliced x = .~(filled 0)
let rebound x = forget x in let x = 3 in .< x >.
let nested x = forget x in .< .< x >. >.
let toplevel_forgotten = forget used in .< used >.
let qualified = forget used in .< Int.add 1 2 >.
|};
  [%expect
    {|
    val argument : 'a @ _ -> <'b>{x : 'b; ..'c} @ _
    val inside : 'a @ _ -> <'b>{x : 'b; ..'c} @ _
    val two : 'a @ 'm0 -> ('b @ _ -> <int>{x : int; y : int; ..'c} @ _) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    val filled : 'a @ _ -> <int>{x : int; ..'b} @ _
    val used : int
    val spliced : int @ _ -> int @ _
    val rebound : 'a @ _ -> <int>{..'b} @ _
    val nested : 'a @ _ -> <<'b>{x : 'b; ..'c}>{..'d} @ _
    val toplevel_forgotten : <'a>{used : 'a; ..'b}
    val qualified : <int>{..'a}
    |}]
;;

let%expect_test "a forgotten name is out of scope outside quotes" =
  show {|
let f x = forget x in x
|};
  show {|
let used = 1
let g u = forget used in used
|};
  show {|
let h x = forget x, x in 1
|};
  show {|
let shadow x = let y = 1 in forget y in let y = 2 in y
|};
  [%expect
    {|
    x is not in scope here: it was forgotten by [forget x in ...]
    used is not in scope here: it was forgotten by [forget used in ...]
    x is forgotten twice
    val shadow : 'a @ _ -> int @ _
    |}]
;;

let%expect_test "erased code keeps forgotten names free, though it drops \
                 [forget]"
  =
  show
    {|
let code = Stag.Code.erase .< fun x -> forget x in .< x >. >.
let rechecked = .~(code)
|};
  [%expect
    {|
    val code : Stag.Expr.t
    val rechecked : 'a @ _ -> <'b>{x : 'b; ..'c} @ _
    |}]
;;

let%expect_test "code refers to a toplevel it can't embed by symbol" =
  show {|
let s = Int.to_string 3
let c = .< .s >.
let bare = .< s >.
|};
  [%expect
    {|
    val s : string
    val c : <string>{..'a}
    val bare : <string>{..'a}
    |}];
  (* Neither a local where the code is spliced nor a substitution captures it. *)
  eval
    {|
let s = Int.to_string 3
let c = .< .s >.
let f (s : string) = .~c
let captured = f "captured"
let k = .< fun (s : string) -> .~c >.
let in_quote = .~k "captured"
let forgotten = forget s in .~c
|};
  [%expect
    {|
    (s (String 3))
    (c (Code (Var (Persistent (Toplevel s/4)) : string)))
    (f <fun>)
    (captured (String 3))
    (k
     (Code
      (Fun (args (s/20))
       (annots
        ((s/20 ((ty ((Con (<string> @ t.stag:6:20-26) ()) @ t.stag:6:20-26))))))
       (body (Var (Persistent (Toplevel s/4)) : string)) : "string -> string")))
    (in_quote (String 3))
    (forgotten (String 3))
    |}];
  show {|
let s = Int.to_string 3
let c = .< s >.
let d = .~c.{s = "other"}
|};
  [%expect {| s is substituted, but the spliced code has no free variable s |}]
;;

let%expect_test "toplevels by symbol in recursive generators" =
  (* A toplevel referred to by symbol in its own code is a recursive use, of its
     type. Code calling the generator that made it has no type, being of a code
     type containing itself. *)
  show {|
let gen n = forget n in .< let _ = gen in n >.
|};
  [%expect {| val gen : 'a @ _ -> <'b>{n : 'b; ..'c} @ _ |}];
  show {|
let gen n = forget n in .< gen n >.
|};
  [%expect {| The type variable 'a occurs inside its own definition |}];
  (* [code] is finished before [f], whose splice uses it: its use of [f] is
     checked once [f] is. *)
  eval
    {|
type nat = Z | S of ptr[nat]
let f n = match n with | Z -> 0 | S *m -> .~code
let code = forget m in .< f m >.
let used = f (S (Ptr.make (S (Ptr.make Z))))
|};
  [%expect
    {|
    (code
     (Code
      (App (Var (Persistent (Toplevel f/4)) : "nat -> int") (Var (Free m) : nat)
       : int)))
    (f <fun>)
    (used 0)
    |}];
  show {|
let a = .< b >.
let b = .< a >.
|};
  [%expect
    {| In this instance of b, a use in its quote is wrong: The type variable 'a occurs inside its own definition |}]
;;

let%expect_test "spliced code's free variables pass mutable locals by" =
  show
    {|
let f (c : <int>{m : int}) = let mut m = 0 in .< .~c >.
let h c = let m = Int.to_string 0 in .< .~c >.
let g c = let mut m = 0 in m <- 1; .< .~c >.
|};
  [%expect
    {|
    val f : <int>{m : int} @ 'm0 -> <int>{m : mut?'a int; ..'b} @ 'm1 where 'c <= 'a with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and gc <= 'm1
    val h : <'a>{..'b} @ 'm0 -> <'a>{..'c} @ 'm1 where ..'b <= {..'c} with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and nonportable gc <= 'm1
    val g : <'a>{..'b} @ 'm0 -> <'a>{..'c} @ 'm1 where ..'b <= {..'c} with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and nonportable gc <= 'm1
    |}]
;;

let%expect_test "forget in a quote doesn't hide names from splices back out" =
  show {|
let f (x : int) = .< forget x in .~(Some x; .< 1 >.) >.
|};
  [%expect {| val f : int @ _ -> <int>{..'a} @ 'm0 with gc <= 'm0 |}];
  show {|
let g (x : int) = .< forget x in .~(.< x >.) >.
|};
  [%expect {| val g : int @ _ -> <'a>{x : 'a; ..'b} @ _ |}];
  (* Written at level 0, it hides [x] in splices back to level 0. *)
  show {|
let h (x : int) = forget x in .< .~(Some x; .< 1 >.) >.
|};
  [%expect {| x is not in scope here: it was forgotten by [forget x in ...] |}]
;;

let%expect_test "a name bound in another quote, used by code spliced outside it"
  =
  show
    {|
let cell = Ptr.make .< 1 >.
let q = .< fun (y : string) -> .~(cell.* <- .< String.length y >.; .< y >.) >.
let r = .~(cell.*)
|};
  [%expect
    {| Unbound value y, which the spliced code uses: y is a free variable of the quote that made it, as the y in scope there, inside 1 quote(s) (level 1), is bound in another quote |}]
;;

let%expect_test "the explanation of a free variable of spliced code is that of \
                 the quote that made it"
  =
  show {|
let s = .< y >.
let r = .~s
|};
  [%expect {| Unbound value y |}];
  (* [s]'s [y] has nothing to do with [q]'s. *)
  show
    {|
let cell = Ptr.make .< 1 >.
let q = .< fun (y : int) -> .~(cell.* <- .< y >.; .< y >.) >.
let s = .< y >.
let r = .~s
|};
  [%expect {| Unbound value y |}]
;;

let%expect_test "the explanation of a free variable of spliced code survives \
                 let-polymorphism"
  =
  show
    {|
let cell = Ptr.make .< 1 >.
let q = .< fun (y : int) -> .~(let c = .< y >. in cell.* <- c; c) >.
let r = .~(cell.*)
|};
  [%expect
    {| Unbound value y, which the spliced code uses: y is a free variable of the quote that made it, as the y in scope there, inside 1 quote(s) (level 1), is bound in another quote |}];
  show
    {|
let cell = Ptr.make .< 1 >.
let q = .< fun (y : int) -> .~(let f u = .< y >. in cell.* <- f (); .< y >.) >.
let r = .~(cell.*)
|};
  [%expect
    {| Unbound value y, which the spliced code uses: y is a free variable of the quote that made it, as the y in scope there, inside 1 quote(s) (level 1), is bound in another quote |}];
  (* [s]'s [y] has nothing to do with [f]'s. *)
  show
    {|
let cell = Ptr.make .< 1 >.
let q = .< fun (y : int) -> .~(let f u = .< y >. in cell.* <- f (); .< y >.) >.
let s = .< y >.
let r = .~s
|};
  [%expect {| Unbound value y |}]
;;

let%expect_test "a crossing local bound in another quote isn't embedded" =
  show
    {|
let cell = Ptr.make .< 1 >.
let q = .< fun (y : int) -> .~(cell.* <- .< y >.; .< y >.) >.
let r = .~(cell.*)
|};
  [%expect
    {| Unbound value y, which the spliced code uses: y is a free variable of the quote that made it, as the y in scope there, inside 1 quote(s) (level 1), is bound in another quote |}]
;;

let%expect_test "a quote's use of a toplevel being checked is checked once it \
                 is"
  =
  (* [c] is finished before [f], whose splice uses it. *)
  show
    {|
let f (x : int) : int = .~(let _ = c in .< x >.)
let c : <bool -> string>{} = .< f >.
|};
  [%expect
    {| This expression has type int but an expression was expected of type bool |}];
  show
    {|
let f (x : int) : int = .~(let _ = c2 in .< x >.)
let c2 = .< (f true, f "s") >.
|};
  [%expect
    {|
    This expression has type int but an expression was expected of type bool
    This expression has type int but an expression was expected of type string
    |}];
  (* Its type is then the toplevel's, as if [f] was finished first. *)
  show {|
let f (x : int) : int = .~(let _ = c in .< x >.)
let c = .< f >.
|};
  [%expect
    {|
    val f : int @ _ -> int @ _
    val c : <int @ _ -> int @ _>{..'a}
    |}];
  show {|
let f (x : int) : int = x
let c = .< f >.
|};
  [%expect
    {|
    val f : int @ _ -> int @ _
    val c : <int @ _ -> int @ _>{..'a}
    |}];
  (* Each use is of an instance. *)
  show
    {|
let ident x = .~(let _ = c in .< x >.)
let c = .< (ident 1, ident "s") >.
|};
  [%expect
    {|
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val c : <int * string>{..'a}
    |}]
;;

let%expect_test "a quote's use of a toplevel of its component is a recursive \
                 one"
  =
  (* [c] uses [f] directly too, so is checked with it: its quote's use is
     related to [f]'s type before it is generalized. *)
  show {|
let f x = let _ = c in x
let c (u : unit) = (f u, .< f >.)
|};
  [%expect
    {|
    val f : unit @ 'm0 -> unit @ _ with portable <= 'm0
    val c : unit @ _ -> unit * <unit @ _ -> unit @ _>{..'a} @ _
    |}];
  show
    {|
let f x = let _ = c in x
let c (u : unit) = (f u, .< (f 1, f "s") >.)
|};
  [%expect
    {|
    This expression has type unit but an expression was expected of type int
    This expression has type unit but an expression was expected of type string
    |}];
  (* Without the direct use, [c] is finished first, and uses an instance. *)
  show {|
let f x = let _ = c in x
let c (u : unit) = .< (f 1, f "s") >.
|};
  [%expect
    {|
    val f : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val c : unit @ _ -> <int * string>{..'a} @ _
    |}]
;;

let%expect_test "a quote's use of a toplevel being checked is generalized with \
                 it"
  =
  (* [c] is finished before [ident], whose splice uses [c] at two types: the use
     of [ident] in [c]'s quote is generalized with [c], so each instance of [c]
     has its own, unified with an instance of [ident] once it is finished. *)
  show
    {|
let ident x = .~(let _ = (c .< 1 >., c .< "s" >.) in .< x >.)
let c (y : <'a>{..'r}) = .< ident .~y >.
let both = (c .< 1 >., c .< "s" >.)
|};
  [%expect
    {|
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val c : <'a>{..'b} @ 'm0 -> <'a>{..'c} @ 'm1 where ..'b <= {..'c} with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and gc <= 'm1
    val both : <int>{..'_a} * <string>{..'_b}
    |}];
  (* An instance's use fails once [g] is finished. *)
  show
    {|
let g (x : int) : int = .~(let _ = d .< "s" >. in .< x >.)
let d (y : <'a>{..'r}) = .< g .~y >.
|};
  [%expect
    {| In this instance of d, a use in its quote is wrong: This expression has type int but an expression was expected of type string |}];
  (* Uses by instances made before and after it is refined agree. *)
  show
    {|
let h x = .~(let _ = e .< 1 >. in .< (x, x) >.)
let e (y : <'a>{..'r}) = .< h .~y >.
let later = e .< true >.
|};
  [%expect
    {|
    val h : 'a @ 'm0 -> 'a * 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val e : <'a>{..'b} @ 'm0 -> <'a * 'a>{..'c} @ 'm1 where ..'b <= {..'c} with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and gc <= 'm1
    val later : <bool * bool>{..'_a}
    |}]
;;

let%expect_test "a toplevel checked by compile-time code is finished with the \
                 toplevel it was checked for"
  =
  show
    {|
let b = let _ = c in 1
let c : .~(let _ = .< y >. in Stag.Type_expr.Con (Stag.Path.Name "int", Nil)) = 1
let y = let _ = b in fun a -> a
|};
  [%expect
    {|
    val b : int
    val c : int
    val y : '_a @ '_m0 -> '_a @ '_m1 with stage('_m0) <= stage('_m1) unless pointer_free('_a) and provenance('_m0) <= provenance('_m1) unless pointer_free('_a) and release(locality('_m0)) <= locality('_m1) unless pointer_free('_a)
    |}];
  show
    {|
let b = let _ = M.zz in 1
module M = .~(let _ = .< y >. in Cons (Box.make (Stag.Toplevel.let_ "zz" Nil (Stag.Expr.int 1), Nil)))
let y = let _ = b in fun a -> a
|};
  [%expect
    {|
    val b : int
    module M : sig
      val zz : int
    end
    val y : '_a @ '_m0 -> '_a @ '_m1 with stage('_m0) <= stage('_m1) unless pointer_free('_a) and provenance('_m0) <= provenance('_m1) unless pointer_free('_a) and release(locality('_m0)) <= locality('_m1) unless pointer_free('_a)
    |}]
;;

let%expect_test "schemes awaiting schemes that await others are refined in turn"
  =
  (* [n] is finished awaiting [m], which is finished awaiting [k]: once [k] is,
     both are refined in place, and [n]'s use of [m], at [string], fails. *)
  show
    {|
let k (x : int) : int = .~(let _ = m .< 1 >. in .< x >.)
let m y = let _ = .~(let _ = n () in .< 0 >.) in .< k .~y >.
let n (u : unit) = .< m .< 2 >. >.
let both = n ()
|};
  [%expect
    {|
    val k : int @ _ -> int @ _
    val m : <int>{..'a} @ 'm0 -> <int>{..'b} @ 'm1 where ..'a <= {..'b} with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and gc <= 'm1
    val n : unit @ _ -> <<int>{..'a}>{..'b} @ _
    val both : <<int>{..'_a}>{..'_b}
    |}];
  show
    {|
let k (x : int) : int = .~(let _ = m .< 1 >. in .< x >.)
let m y = let _ = .~(let _ = n () in .< 0 >.) in .< k .~y >.
let n (u : unit) = .< m .< "s" >. >.
|};
  [%expect
    {| In this instance of m, a use in its quote is wrong: This expression has type int but an expression was expected of type string |}]
;;

let%expect_test "a toplevel finished in a splice's code has a scheme of its own"
  =
  (* [c], checked for [ident]'s splice through [p], quotes [ident] while it is
     being checked; [p] is finished in a component of its own. *)
  show
    {|
let ident (x : string) = .~(.< x >.)
let p = let _ = c in 1
let c (_ : int) = .< ident "s" >.
|};
  [%expect
    {|
    val ident : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val p : int
    val c : int @ _ -> <string>{..'a} @ _
    |}];
  show
    {|
let ident (x : string) = .~(let _ = p in .< x >.)
let p = let _ = c in 1
let c (_ : int) = .< ident "s" >.
let q = .~(let _ = (fun (u : int) -> p) in .< 1 >.)
|};
  [%expect
    {|
    val ident : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val p : int
    val c : int @ _ -> <string>{..'a} @ _
    val q : int
    |}]
;;

let%expect_test "abandoning a splice keeps the errors of the components it \
                 finished"
  =
  (* The include's code finishes [x], then needs [c], whose component is being
     checked outside it ([b] is after the include), so it is abandoned. *)
  show
    {|
let c = b
let x : int = "s"
include .~ (let _ = x in let _ = c in Nil)
let b = zzz
|};
  [%expect
    {|
    This expression has type string but an expression was expected of type int
    Unbound value zzz
    |}]
;;

let%expect_test "a toplevel whose quote's use turns out wrong has failed" =
  (* [c] is finished in [ident]'s splice, awaiting [ident], which it uses at
     [int]: once [ident] is finished, [c] fails, and its error is reported once,
     at its use, not again at its instance in [ident]'s splice. [c] isn't run at
     compile time then, and neither is [ident], whose splice used it. *)
  Source_helpers.check
    {|
let ident (x : string) = .~(let _ = c in .< x >.)
let c (_ : int) = .< ident 1 >.
let d = .~(let _ = c 0 in let _ = print_endline "ran c" in .< 1 >.)
let e = .~(let _ = ident "a" in let _ = print_endline "ran ident" in .< 1 >.)
|};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon string ())
       (Base_types_not_equal int string)))
     @ t.stag:3:21-26)
    |}]
;;

let%expect_test "a wrong use in an instance is reported at the instance" =
  (* [d]'s quote uses [g] at its argument's type: [d .< "s" >.] is the wrong
     one, through [d]'s quote. *)
  Source_helpers.check
    {|
let g (x : int) : int = .~(let _ = d .< "s" >. in .< x >.)
let d (y : <'a>{..'r}) = .< g .~y >.
|};
  [%expect
    {|
    ((Via_quote (toplevel d) (quote t.stag:3:28-29)
      (error
       (Unification_error
        (Type_mismatch (TCon string ()) (TCon int ())
         (Base_types_not_equal string int)))))
     @ t.stag:2:35-36)
    |}];
  (* Through two quotes: [n]'s quote's instance of [m] is the wrong one, and the
     instance of [n] in [m]'s splice, through [n]'s quote, isn't reported again. *)
  Source_helpers.check
    {|
let k (x : int) : int = .~(let _ = m .< 1 >. in .< x >.)
let m y = let _ = .~(let _ = n () in .< 0 >.) in .< k .~y >.
let n (u : unit) = .< m .< "s" >. >.
|};
  [%expect
    {|
    ((Via_quote (toplevel m) (quote t.stag:3:52-53)
      (error
       (Unification_error
        (Type_mismatch (TCon string ()) (TCon int ())
         (Base_types_not_equal string int)))))
     @ t.stag:4:22-23)
    |}]
;;

let%expect_test "schemes of a component awaiting a toplevel are refined \
                 together"
  =
  show
    {|
let ident (x : string) = .~(let _ = (a .< 1 >., b) in .< x >.)
let a y = let _ = b in .< ident >.
let b z = let _ = a z in .< ident >.
|};
  [%expect
    {|
    val ident : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val a : 'a @ 'm0 -> <string @ 'm1 -> string @ 'm2>{..'b} @ _ with portable <= 'm0 and 'm1 <= 'm2 and release(locality('m1)) <= locality('m2)
    val b : 'a @ _ -> <string @ 'm0 -> string @ 'm1>{..'b} @ _ with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    |}]
;;

let%expect_test "a local bound to an awaiting instance is generalized with its \
                 uses"
  =
  let program h =
    sprintf
      {|
let ident x = .~(let _ = h () in .< x >.)
let c y = .< ident .~y >.
let first p = match p with (a, _) -> a
let h (u : unit) = %s
|}
      h
  in
  List.iter
    ~f:(fun h ->
      print_endline h;
      match Mode_helpers.signature (program h) with
      | Ok signature ->
        String.split_lines signature
        |> List.filter ~f:(String.is_prefix ~prefix:"val h")
        |> List.iter ~f:print_endline
      | Error errors -> List.iter errors ~f:print_endline)
    [ {|(c .< 1 >., c .< "s" >.)|}
    ; {|let f = c in (f .< 1 >., f .< "s" >.)|}
    ; {|let f = c in let g = f in (g .< 1 >., g .< "s" >.)|}
    ; {|let f y = c y in (f .< 1 >., f .< "s" >.)|}
    ; {|let p = (c, 0) in (first p .< 1 >., first p .< "s" >.)|}
    ; {|let f = c in let g (z : unit) = (f .< 1 >., f .< "s" >.) in g ()|}
    ; {|let g (z : unit) = let f = c in (f .< 1 >., f .< "s" >.) in g ()|}
    ; {|.< let f = c in (f .< 1 >., f .< "s" >.) >.|}
    ];
  [%expect
    {|
    (c .< 1 >., c .< "s" >.)
    val h : unit @ _ -> <int>{..'a} * <string>{..'b} @ 'm0 with gc <= 'm0
    let f = c in (f .< 1 >., f .< "s" >.)
    val h : unit @ _ -> <int>{..'a} * <string>{..'b} @ 'm0 with gc <= 'm0
    let f = c in let g = f in (g .< 1 >., g .< "s" >.)
    val h : unit @ _ -> <int>{..'a} * <string>{..'b} @ 'm0 with gc <= 'm0
    let f y = c y in (f .< 1 >., f .< "s" >.)
    val h : unit @ _ -> <int>{..'a} * <string>{..'b} @ 'm0 with gc <= 'm0
    let p = (c, 0) in (first p .< 1 >., first p .< "s" >.)
    val h : unit @ _ -> <int>{..'a} * <string>{..'b} @ 'm0 with gc <= 'm0
    let f = c in let g (z : unit) = (f .< 1 >., f .< "s" >.) in g ()
    val h : unit @ _ -> <int>{..'a} * <string>{..'b} @ 'm0 with gc <= 'm0
    let g (z : unit) = let f = c in (f .< 1 >., f .< "s" >.) in g ()
    val h : unit @ _ -> <int>{..'a} * <string>{..'b} @ 'm0 with gc <= 'm0
    .< let f = c in (f .< 1 >., f .< "s" >.) >.
    val h : unit @ _ -> <<int>{..'a} * <string>{..'b}>{..'c} @ _
    |}]
;;

let%expect_test "a local bound to an awaiting instance is as its toplevel, in \
                 any order of the toplevels"
  =
  let ident = {|let ident x = .~(let _ = h () in .< x >.)|} in
  let c = {|let c y = .< ident .~y >.|} in
  let h body = sprintf {|let h (u : unit) = %s|} body in
  let direct = h {|(c .< 1 >., c .< "s" >.)|} in
  let local = h {|let f = c in (f .< 1 >., f .< "s" >.)|} in
  let show_order order =
    let program h =
      String.concat
        ~sep:"\n"
        (""
         :: List.map order ~f:(function
           | `Ident -> ident
           | `C -> c
           | `H -> h))
    in
    let result h =
      match Mode_helpers.signature (program h) with
      | Ok signature -> String.split_lines signature
      | Error errors -> errors
    in
    let names =
      List.map order ~f:(function
        | `Ident -> "ident"
        | `C -> "c"
        | `H -> "h")
    in
    let direct = result direct in
    let local = result local in
    printf
      "%s: %s\n"
      (String.concat ~sep:", " names)
      (if [%equal: string list] direct local then "as directly" else "differs");
    List.iter local ~f:(printf "  %s\n")
  in
  List.iter
    ~f:show_order
    [ [ `Ident; `C; `H ]
    ; [ `Ident; `H; `C ]
    ; [ `C; `Ident; `H ]
    ; [ `C; `H; `Ident ]
    ; [ `H; `Ident; `C ]
    ; [ `H; `C; `Ident ]
    ];
  [%expect
    {|
    ident, c, h: as directly
      val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
      val c : <'a>{..'b} @ 'm0 -> <'a>{..'c} @ 'm1 where ..'b <= {..'c} with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and gc <= 'm1
      val h : unit @ _ -> <int>{..'a} * <string>{..'b} @ 'm0 with gc <= 'm0
    ident, h, c: as directly
      val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
      val h : unit @ _ -> <int>{..'a} * <string>{..'b} @ 'm0 with gc <= 'm0
      val c : <'a>{..'b} @ 'm0 -> <'a>{..'c} @ 'm1 where ..'b <= {..'c} with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and gc <= 'm1
    c, ident, h: as directly
      The toplevel h is used at compile time before it is fully checked (it is in the same recursive group as this splice)
      This expression has type string but an expression was expected of type int
    c, h, ident: as directly
      This expression has type string but an expression was expected of type int
      The toplevel h is used at compile time before it is fully checked (it is in the same recursive group as this splice)
    h, ident, c: as directly
      The toplevel h is used at compile time before it is fully checked (it is in the same recursive group as this splice)
    h, c, ident: as directly
      The toplevel h is used at compile time before it is fully checked (it is in the same recursive group as this splice)
    |}]
;;

let%expect_test "a wrong use in a local's instance is reported at the \
                 instance, once"
  =
  let program h =
    sprintf
      {|
let ident (x : int) = .~(let _ = h () in .< x >.)
let c y = .< ident .~y >.
let h (u : unit) = %s
|}
      h
  in
  List.iter
    ~f:(fun h ->
      print_endline h;
      match Mode_helpers.located_errors (program h) with
      | Ok () -> print_endline "  ok"
      | Error errors ->
        List.iter errors ~f:(fun (loc, message) ->
          printf "  %s: %s\n" (Sexp.to_string [%sexp (loc : Loc.t)]) message))
    [ {|let f = c in (f .< 1 >., f .< "s" >.)|}
    ; {|let f = c in let g = f in (g .< "s" >., g .< "s" >.)|}
      (* The local's own use is wrong: reported there, not again at its
         instances, and even if it has none. *)
    ; {|let f = fun (v : unit) -> c .< "s" >. in (f (), f ())|}
    ; {|let f = fun (v : unit) -> c .< "s" >. in 0|}
    ];
  [%expect
    {|
    let f = c in (f .< 1 >., f .< "s" >.)
      t.stag:4:44-45: In this instance of c, a use in its quote is wrong: This expression has type int but an expression was expected of type string
    let f = c in let g = f in (g .< "s" >., g .< "s" >.)
      t.stag:4:46-47: In this instance of c, a use in its quote is wrong: This expression has type int but an expression was expected of type string
      t.stag:4:59-60: In this instance of c, a use in its quote is wrong: This expression has type int but an expression was expected of type string
    let f = fun (v : unit) -> c .< "s" >. in (f (), f ())
      t.stag:4:45-46: In this instance of c, a use in its quote is wrong: This expression has type int but an expression was expected of type string
    let f = fun (v : unit) -> c .< "s" >. in 0
      t.stag:4:45-46: In this instance of c, a use in its quote is wrong: This expression has type int but an expression was expected of type string
    |}]
;;

let%expect_test "a local bound to an awaiting instance in the splice of the \
                 toplevel awaited"
  =
  show
    {|
let ident x = .~(let f = c in let _ = (f .< 1 >., f .< "s" >.) in .< x >.)
let c (y : <'a>{..'r}) = .< ident .~y >.
|};
  show
    {|
let ident (x : int) = .~(let f = c in let _ = (f .< 1 >., f .< "s" >.) in .< x >.)
let c (y : <'a>{..'r}) = .< ident .~y >.
|};
  [%expect
    {|
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val c : <'a>{..'b} @ 'm0 -> <'a>{..'c} @ 'm1 where ..'b <= {..'c} with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and gc <= 'm1
    In this instance of c, a use in its quote is wrong: This expression has type int but an expression was expected of type string
    |}]
;;

let%expect_test "an awaiting scheme's joins are refined in place, in either \
                 order"
  =
  (* [c] is finished before [ident]: its arms join code types with modes, and
     the refinement keeps what the join had either way round. *)
  let program ~arms =
    sprintf
      {|
let ident x = .~(let _ = c 0 .< 1 >. .< 2 >. in .< x >.)
let c (n : int) y z = let j = match n with %s in .< ident .~j >.
|}
      arms
  in
  show (program ~arms:"| 0 -> y | _ -> z");
  show (program ~arms:"| 0 -> z | _ -> y");
  [%expect
    {|
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val c : int @ 'm0 -> (<'a>{..'b} @ 'm2 -> (<'a>{..'b} @ 'm4 -> <'a>{..'c} @ 'm5) @ 'm3) @ 'm1 where ..'b <= {..'c} with 'm0 <= 'm3 and 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= 'm3 and 'm4 <= 'm5 and release(locality('m4)) <= locality('m5) and gc <= 'm5 and gc <= 'm3 and gc <= 'm1
    val ident : 'a @ 'm0 -> 'a @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val c : int @ 'm0 -> (<'a>{..'b} @ 'm2 -> (<'a>{..'b} @ 'm4 -> <'a>{..'c} @ 'm5) @ 'm3) @ 'm1 where ..'b <= {..'c} with 'm0 <= 'm3 and 'm0 <= 'm1 and 'm2 <= 'm5 and release(locality('m2)) <= locality('m5) and 'm2 <= 'm3 and 'm4 <= 'm5 and release(locality('m4)) <= locality('m5) and gc <= 'm5 and gc <= 'm3 and gc <= 'm1
    |}]
;;

let%expect_test "a local generalizes the modes of its uses of toplevels being \
                 checked, as their types"
  =
  let program h =
    sprintf
      {|
let ident (x : ptr[int]) = .~(let _ = h () in .< x >.)
let same (a : 'a) (b : 'a) = a
let h (u : unit) = %s
|}
      h
  in
  let direct = {|.< same ident (fun (p : ptr[int]) -> Ptr.make 0) >.|} in
  let local =
    {|let f = .< same ident (fun (p : ptr[int]) -> Ptr.make 0) >. in f|}
  in
  List.iter [ direct; local ] ~f:(fun h ->
    print_endline h;
    match Mode_helpers.signature (program h) with
    | Ok signature ->
      String.split_lines signature
      |> List.filter ~f:(String.is_prefix ~prefix:"val h")
      |> List.iter ~f:(printf "  %s\n")
    | Error errors -> List.iter errors ~f:(printf "  %s\n"));
  (* So a local value passed to [ident] is not taken for a global one. *)
  show
    (program local
     ^ {|
let mut g = Ptr.make 0
let k (u : unit) = .< fun (q : ptr[int] @ local) -> g <- .~(h ()) q >.
|}
    );
  [%expect
    {|
    .< same ident (fun (p : ptr[int]) -> Ptr.make 0) >.
      val h : unit @ _ -> <ptr[int] @ 'm0 -> ptr[int] @ 'm1>{..'a} @ _ with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and nonportable gc <= 'm1
    let f = .< same ident (fun (p : ptr[int]) -> Ptr.make 0) >. in f
      val h : unit @ _ -> <ptr[int] @ 'm0 -> ptr[int] @ 'm1>{..'a} @ _ with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and nonportable gc <= 'm1
    This is local, because it is annotated local (at t.stag:7:42-47), then q is passed to the function (at t.stag:7:22-67), then q is used (at t.stag:7:66-67), then it is passed to a function (at t.stag:7:57-67), then a function is used at a type with other modes, then x is passed to the function (at t.stag:2:27-54), then x is used (at t.stag:2:49-50), then the function returns it (at t.stag:2:27-54), then a function is used at a type with other modes, then the function returns it (at t.stag:7:57-67), then it is assigned to g (at t.stag:7:52-67), but must be at most global, because g is a toplevel (at t.stag:6:12-22)
    |}]
;;

let%expect_test "a local's instances have their own modes of its uses of \
                 toplevels being checked"
  =
  let program f =
    sprintf
      {|
let ident (x : ptr[int]) = .~(let _ = h () in .< x >.)
let mut g = Ptr.make 0
let h (u : unit) =
  let f = %s in
  .< fun (q : ptr[int] @ local) (r : ptr[int]) -> let _ = .~f q in g <- .~f r >.
|}
      f
  in
  show (program {|.< ident >.|});
  (* Not generalized, it has one. *)
  show (program {|(fun (z : unit) -> .< ident >.) ()|});
  [%expect
    {|
    val ident : ptr[int] @ 'm0 -> ptr[int] @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    val g : ptr[int]
    val h : unit @ _ -> <ptr[int] @ 'm0 -> (ptr[int] @ 'm2 -> unit @ _) @ 'm1>{..'a} @ _ with local <= 'm0 and 'm0 <= 'm1 and 'm2 <= global and nonportable gc <= 'm1
    This expression has type ptr[int] @ regional -> ptr[int] @ local but an expression was expected of type ptr[int] @ regional -> ptr[int]
    Their modes differ: This is local, because it is annotated local (at t.stag:6:25-30), then q is passed to the function (at t.stag:6:5-77), then q is used (at t.stag:6:62-63), then it is passed to a function (at t.stag:6:58-63), then a function is used at a type with other modes, then x is passed to the function (at t.stag:2:27-54), then x is used (at t.stag:2:49-50), then the function returns it (at t.stag:2:27-54), then a function is used at a type with other modes, but must be at most global, because g is a toplevel (at t.stag:3:12-22), then it is assigned to g (at t.stag:6:67-77), then the function returns it (at t.stag:6:72-77), then a function is used at a type with other modes
    |}]
;;

let%expect_test "a local's use holding a variable of an enclosing local is \
                 generalized with each"
  =
  (* [f]'s use of [c2] is [g]'s too, so each instance of [g] has its own. *)
  let program ~ident ~g =
    sprintf
      {|
let ident2 %s y = .~(let _ = h () in .< x >.)
let c2 w z = .< ident2 .~w .~z >.
let h (u : unit) = %s
|}
      ident
      g
  in
  List.iter
    [ {|let g w = let f = fun z -> c2 w z in 0 in (g .< 1 >., g .< "s" >.)|}
    ; {|let g w = let f = fun z -> c2 w z in f in (g .< 1 >. .< 2 >., g .< "s" >. .< 3 >.)|}
    ]
    ~f:(fun g ->
      print_endline g;
      List.iter [ "(x : int)"; "x" ] ~f:(fun ident ->
        match Mode_helpers.located_errors (program ~ident ~g) with
        | Ok () ->
          (match Mode_helpers.signature (program ~ident ~g) with
           | Ok signature ->
             String.split_lines signature
             |> List.filter ~f:(String.is_prefix ~prefix:"val h")
             |> List.iter ~f:(printf "  %s: %s\n" ident)
           | Error _ -> ())
        | Error errors ->
          List.iter errors ~f:(fun (loc, message) ->
            printf
              "  %s: %s: %s\n"
              ident
              (Sexp.to_string [%sexp (loc : Loc.t)])
              message)));
  [%expect
    {|
    let g w = let f = fun z -> c2 w z in 0 in (g .< 1 >., g .< "s" >.)
      (x : int): t.stag:4:73-74: In this instance of c2, a use in its quote is wrong: This expression has type int but an expression was expected of type string
      x: val h : unit @ _ -> int * int @ _
    let g w = let f = fun z -> c2 w z in f in (g .< 1 >. .< 2 >., g .< "s" >. .< 3 >.)
      (x : int): t.stag:4:81-82: In this instance of c2, a use in its quote is wrong: This expression has type int but an expression was expected of type string
      x: val h : unit @ _ -> <int>{..'a} * <string>{..'b} @ 'm0 with gc <= 'm0
    |}]
;;

let%expect_test "a local decides the layouts of its uses of toplevels being \
                 checked as it generalizes them"
  =
  (* As for any variable a [let] generalizes, its unknown layout has pointers:
     [v] is taken to have pointers, though [ident] turns out to make it [int],
     unless the [let] is the toplevel's own, not generalized. *)
  let program h =
    sprintf
      {|
let ident (x : int) = .~(let _ = h () in .< x >.)
let c y = .< ident .~y >.
let tie (v : 'a @ local) (q : <'a>{..'r}) = 0
let app f = f
let h = app (fun (u : unit) -> %s)
|}
      h
  in
  let body =
    {|fun (v @ local) q -> let _ = tie v (c q) in let s @ global = v in s|}
  in
  List.iter
    [ body; sprintf "let f = %s in f" body ]
    ~f:(fun h ->
      print_endline h;
      match Mode_helpers.located_errors (program h) with
      | Ok () -> print_endline "  ok"
      | Error errors ->
        List.iter errors ~f:(fun (loc, message) ->
          printf "  %s: %s\n" (Sexp.to_string [%sexp (loc : Loc.t)]) message));
  [%expect
    {|
    fun (v @ local) q -> let _ = tie v (c q) in let s @ global = v in s
      ok
    let f = fun (v @ local) q -> let _ = tie v (c q) in let s @ global = v in s in f
      t.stag:6:31-111: This is regional, because it is annotated local (at t.stag:6:48-53), then v is passed to the function (at t.stag:6:39-106), then v is used (at t.stag:6:100-101), but must be at most global, because it is annotated global (at t.stag:6:91-97), then it is bound by an annotated let (at t.stag:6:91-97)
    |}]
;;
