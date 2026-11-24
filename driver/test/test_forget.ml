open! Import

let show source =
  match Mode_helpers.signature source with
  | Ok s -> print_string s
  | Error errors -> List.iter errors ~f:print_endline
;;

let%expect_test "a local the quote can't embed is an error, not a free variable" =
  show {|
let argument x = .< x >.
|};
  show {|
let computed = let k = Int.to_string 3 in .< k >.
|};
  [%expect {|
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
  [%expect {|
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
  [%expect {|
    x is not in scope here: it was forgotten by [forget x in ...]
    used is not in scope here: it was forgotten by [forget used in ...]
    x is forgotten twice
    val shadow : 'a -> int
    |}]
;;

let%expect_test "forget round trips through quoted syntax" =
  show
    {|
let code = Stag.Code.erase .< fun x -> forget x in .< x >. >.
let rechecked = .~(code)
|};
  [%expect {|
    val code : Stag.Expr.t
    val rechecked : 'a -> <'b>{x : 'b; ..'c}
    |}]
;;
