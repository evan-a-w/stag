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
    The quote can't embed k, as it may not be portable. Bind it as (k @ portable) to embed it, or write [forget k in ...] to make k a free variable of the quote
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
    val argument : 'a -> <'b>{x : 'b; ..'c}
    val inside : 'a -> <'b>{x : 'b; ..'c}
    val two : 'a @ 'm0 -> ('b -> <int>{x : int; y : int; ..'c}) @ 'm1 with 'm0 <= 'm1 and gc <= 'm1
    val filled : 'a -> <int>{x : int; ..'b}
    val used : int
    val spliced : int -> int
    val rebound : 'a -> <int>{..'b}
    val nested : 'a -> <<'b>{x : 'b; ..'c}>{..'d}
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
    val shadow : 'a -> int
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
    val rechecked : 'a -> <'b>{x : 'b; ..'c}
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
  (* A toplevel being checked is referred to by symbol, and doesn't join the
     quote's component: the code's type for it is fresh, the expansion being
     checked where it is spliced. *)
  show {|
let gen n = forget n in .< gen n >.
|};
  [%expect {| val gen : 'a -> <'b>{n : 'c; ..'d} |}];
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
      (App (Var (Persistent (Toplevel f/4)) : "'a -> 'b") (Var (Free m) : 'a) :
       'b)))
    (f <fun>)
    (used 0)
    |}];
  (* Code that would contain itself has no type. *)
  show {|
let a = .< b >.
let b = .< a >.
|};
  [%expect {|
    val a : <<'a>{..'b}>{..'c}
    val b : <'a>{..'b}
    |}]
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
  [%expect {| val f : int -> <int>{..'a} @ 'm0 with gc <= 'm0 |}];
  show {|
let g (x : int) = .< forget x in .~(.< x >.) >.
|};
  [%expect {| val g : int -> <'a>{x : 'a; ..'b} |}];
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
    {| Unbound value y, which the spliced code uses: y is a variable of its quote's context, as the y bound at inside 1 quote(s) (level 1) can't be used in the quote (level 1), as it is not portable |}]
;;
