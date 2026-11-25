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
    val argument : 'a @ 'm0 -> <'b>{x : 'b; ..'c} @ 'm1
    val inside : 'a @ 'm0 -> <'b>{x : 'b; ..'c} @ 'm1
    val two : 'a @ 'm0 -> ('b @ 'm2 -> <int>{x : int; y : int; ..'c} @ 'm3) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    val filled : 'a @ 'm0 -> <int>{x : int; ..'b} @ 'm1
    val used : int
    val spliced : int @ 'm0 -> int @ 'm1
    val rebound : 'a @ 'm0 -> <int>{..'b} @ 'm1
    val nested : 'a @ 'm0 -> <<'b>{x : 'b; ..'c}>{..'d} @ 'm1
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
    val shadow : 'a @ 'm0 -> int @ 'm1
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
    val rechecked : 'a @ 'm0 -> <'b>{x : 'b; ..'c} @ 'm1
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
      (Fun (args (s/21))
       (annots
        ((s/21 ((ty ((Con (<string> @ t.stag:6:20-26) ()) @ t.stag:6:20-26))))))
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
  [%expect {| val gen : 'a @ 'm0 -> <'b>{n : 'b; ..'c} @ 'm1 |}];
  show {|
let gen n = forget n in .< gen n >.
|};
  [%expect {| The type variable '_a occurs inside its own definition |}];
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
  [%expect {| The type variable '_a occurs inside its own definition |}]
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
  [%expect {| val f : int @ 'm0 -> <int>{..'a} @ 'm1 with gc <= 'm1 |}];
  show {|
let g (x : int) = .< forget x in .~(.< x >.) >.
|};
  [%expect {| val g : int @ 'm0 -> <'a>{x : 'a; ..'b} @ 'm1 |}];
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
    val f : int @ 'm0 -> int @ 'm1
    val c : <int @ 'm0 -> int @ 'm1>{..'a}
    |}];
  show {|
let f (x : int) : int = x
let c = .< f >.
|};
  [%expect
    {|
    val f : int @ 'm0 -> int @ 'm1
    val c : <int @ 'm0 -> int @ 'm1>{..'a}
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
     unified with [f]'s type before it is generalized. *)
  show {|
let f x = let _ = c in x
let c (u : unit) = (f u, .< f >.)
|};
  [%expect
    {|
    val f : unit @ 'm0 -> unit @ 'm1 with portable <= 'm0 and 'm1 <= regional
    val c : unit @ 'm0 -> unit * <unit @ 'm2 -> unit @ 'm3>{..'a} @ 'm1 with portable <= 'm2 and 'm3 <= 'm1 and release(locality('m3)) <= locality('m1) and 'm3 <= regional
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
    val c : unit @ 'm0 -> <int * string>{..'a} @ 'm1
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
    {| This expression has type int but an expression was expected of type string |}];
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
    val y : '_a @ '_m0 -> '_a @ '_m1 with '_m0 <= '_m1 and release(locality('_m0)) <= locality('_m1)
    |}];
  show
    {|
let b = let _ = M.zz in 1
module M = .~(let _ = .< y >. in Cons (Ptr.make (Stag.Toplevel.let_ "zz" Nil (Stag.Expr.int 1), Nil)))
let y = let _ = b in fun a -> a
|};
  [%expect
    {|
    val b : int
    module M : sig
      val zz : int
    end
    val y : '_a @ '_m0 -> '_a @ '_m1 with '_m0 <= '_m1 and release(locality('_m0)) <= locality('_m1)
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
    val k : int @ 'm0 -> int @ 'm1
    val m : <int>{..'a} @ 'm0 -> <int>{..'b} @ 'm1 where ..'a <= {..'b} with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and gc <= 'm1
    val n : unit @ 'm0 -> <<int>{..'a}>{..'b} @ 'm1
    val both : <<int>{..'_a}>{..'_b}
    |}];
  show
    {|
let k (x : int) : int = .~(let _ = m .< 1 >. in .< x >.)
let m y = let _ = .~(let _ = n () in .< 0 >.) in .< k .~y >.
let n (u : unit) = .< m .< "s" >. >.
|};
  [%expect
    {| This expression has type int but an expression was expected of type string |}]
;;
