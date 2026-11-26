open! Import
open Source_helpers

let errors source =
  match Mode_helpers.located_errors source with
  | Ok () -> print_endline "ok"
  | Error errors ->
    List.iter errors ~f:(fun (loc, message) ->
      print_endline (Loc.to_string loc ^ ": " ^ message))
;;

let%expect_test "evaluation: the first alternative that matches binds" =
  run
    {|
type t = A of int | B of int | C of string
let f x = match x with | A n | B n -> n | C _ -> 0
let a = f (A 1)
let b = f (B 2)
let c = f (C "s")
let first p = match p with | (A n, _) | (_, A n) -> n | _ -> 0
let left = first (A 3, A 4)
let right = first (C "s", A 5)
let neither = first (B 6, B 7)
let nested x = match x with | Some (A n | B n) -> n | Some (C _) | None -> 8
let n1 = nested (Some (B 9))
let n2 = nested None
let poly x = match x with | `X | `Y -> 1 | `Z -> 2
let p = (poly `X, poly `Y, poly `Z)
let lit x = match x with | 1 | 2 | 3 -> "small" | _ -> "big"
let l = (lit 2, lit 4)
let raises (u : unit) =
  match raise (Invalid_argument "x") with
  | v -> v
  | exception (Failure m | Invalid_argument m) -> m
let r = raises ()
let raises2 (u : unit) =
  match raise (Failure "y") with
  | v -> v
  | exception Invalid_argument m | exception Failure m -> m
let r2 = raises2 ()
|};
  [%expect
    {|
    type t = A of int | B of int | C of string
    val f : t -> int
    val a : int
    val b : int
    val c : int
    val first : t * t -> int
    val left : int
    val right : int
    val neither : int
    val nested : option[t] -> int
    val n1 : int
    val n2 : int
    val poly : [`X | `Y | `Z] -> int
    val p : int * int * int
    val lit : int -> string
    val l : string * string
    val raises : unit -> string
    val r : string
    val raises2 : unit -> string
    val r2 : string
    -- evaluated at compile time
    -- evaluated
    (f <fun>)
    (a 1)
    (b 2)
    (c 0)
    (first <fun>)
    (left 3)
    (right 5)
    (neither 0)
    (nested <fun>)
    (n1 9)
    (n2 8)
    (poly <fun>)
    (p (Tuple 1 1 2))
    (lit <fun>)
    (l (Tuple (String small) (String big)))
    (raises <fun>)
    (r (String x))
    (raises2 <fun>)
    (r2 (String y))
    |}]
;;

let%expect_test "both alternatives must bind the same variables at the same \
                 types"
  =
  errors
    {|
type t = A of int | B of int | C of string
let missing_right x = match x with | A n | B _ -> 0 | C _ -> 1
let missing_left x = match x with | A _ | B n -> n | C _ -> 1
let types x = match x with | A n | C n -> 0 | B _ -> 1
let nested x = match x with | Some (A n | B m) -> n | _ -> 0
let ok x = match x with | (A n, m) | (m, A n) -> (n, m) | _ -> (0, A 0)
let rests x = match x with {`x = n; ..r} | {`y = n; ..r} -> n
let sides x = match x with | A n | 1 -> 0 | _ -> 1
|};
  [%expect
    {|
    t.stag:3:39-40: The variable n is bound by only one side of this or-pattern, but both sides must bind the same variables
    t.stag:4:44-45: The variable n is bound by only one side of this or-pattern, but both sides must bind the same variables
    t.stag:5:37-38: The variable n has different types on the two sides of this or-pattern: This expression has type string but an expression was expected of type int
    t.stag:6:38-39: The variable n is bound by only one side of this or-pattern, but both sides must bind the same variables
    t.stag:6:44-45: The variable m is bound by only one side of this or-pattern, but both sides must bind the same variables
    t.stag:8:54-55: The variable r has different types on the two sides of this or-pattern: The row {`x : 'a; `y?'b : 'a; ..'c} is incompatible with the row {`y : 'a; `y?'b : 'a; ..'c}
    The row would be cyclic.
    t.stag:9:31-32: The variable n is bound by only one side of this or-pattern, but both sides must bind the same variables
    t.stag:9:35-36: This expression has type int but an expression was expected of type t
    |}]
;;

let%expect_test "a polymorphic variant's alternatives close the match" =
  run
    ~eval:false
    {|
let closed x = match x with | `A | `B -> 1 | `C n -> n
let rest x = match x with | `A | `B -> 1 | other -> 2
let open_ x = match x with | `A | _ -> 1
|};
  [%expect
    {|
    val closed : [`A | `B | `C of int] -> int
    val rest : [`A | `B; ..'a] -> int
    val open_ : [`A; ..'a] -> int
    -- evaluated at compile time
    |}]
;;

let%expect_test "a variable's mode is the join of what each alternative binds \
                 it to"
  =
  let check source =
    match Mode_helpers.signature source with
    | Ok signature -> print_string signature
    | Error _ -> errors source
  in
  check
    {|
let ext (x : string) = let y @ external = x in y
let either (k : string) =
  match (k, ext "v") with
  | (a, "") | ("", a) -> a
  | _ -> k
let left_only (k : string) =
  match (k, ext "v") with
  | (a, "") | (a, _) -> a
|};
  check
    {|
let keep : ptr[string] = Ptr.make ""
let store (k : string) (l @ local) =
  match (k, l) with
  | (a, "") | ("", a) -> keep.* <- a
  | _ -> ()
|};
  check
    {|
let keep : ptr[string] = Ptr.make ""
let fine (k : string) (l @ local) =
  match (k, l) with
  | (a, "") | (a, _) -> keep.* <- a
|};
  [%expect
    {|
    val ext : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val either : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1) and external <= 'm1
    val left_only : string @ 'm0 -> string @ 'm1 with 'm0 <= 'm1 and release(locality('m0)) <= locality('m1)
    t.stag:5:25-36: This is regional, because it is annotated local (at t.stag:3:28-33), then l is passed to the function (at t.stag:4:2-6:11), then l is used (at t.stag:4:12-13), then its type turned out to be string, which has pointers, then it is bound by an alternative of an or-pattern (at t.stag:5:5-6), then a is used (at t.stag:5:35-36), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:5:35-36)
    val keep : ptr[string]
    val fine : string @ 'm0 -> (string @ 'm2 -> unit @ _) @ 'm1 with 'm0 <= 'm1 and 'm0 <= global gc and local <= 'm2 and gc <= 'm1
    |}]
;;

let%expect_test "quotes of or-patterns round trip through code" =
  run
    {|
type t = A of int | B of int
let code = .< fun x -> match x with | A n | B n -> n >.
let applied = .~code (B 4)
include .~([Stag.Toplevel.let_ "generated" ["x"]
  (Stag.Expr.match_ (Stag.Expr.var "x")
    [(Stag.Pattern.Or (Ptr.make (Stag.Pattern.construct "A" [Stag.Pattern.Var "n"],
                                 Stag.Pattern.construct "B" [Stag.Pattern.Var "n"])),
      Stag.Expr.var "n")])])
let from_a = generated (A 5)
let from_b = generated (B 6)
|};
  [%expect
    {|
    type t = A of int | B of int
    val code : <t -> int>{..'a}
    val applied : int
    val generated : t -> int
    val from_a : int
    val from_b : int
    -- evaluated at compile time
    (code
     (Code
      (Fun (args (x/16))
       (body
        (Match (scrutinee (Var (Bound (Local x/16)) : t))
         (cases
          (((Or
             (Construct ((type_name t) (name A) (index 0)) ((Var n/17 : int)) :
              t)
             (Construct ((type_name t) (name B) (index 1)) ((Var n/17 : int)) :
              t)
             : t)
            (Var (Bound (Local n/17)) : int))))
         : int))
       : "t -> int")))
    -- evaluated
    (code
     (Code
      (Fun (args (x/16))
       (body
        (Match (scrutinee (Var (Bound (Local x/16)) : t))
         (cases
          (((Or
             (Construct ((type_name t) (name A) (index 0)) ((Var n/17 : int)) :
              t)
             (Construct ((type_name t) (name B) (index 1)) ((Var n/17 : int)) :
              t)
             : t)
            (Var (Bound (Local n/17)) : int))))
         : int))
       : "t -> int")))
    (applied 4)
    (generated <fun>)
    (from_a 5)
    (from_b 6)
    |}]
;;

let%expect_test "the region oracle: an alternative's stack value is freed with \
                 its region"
  =
  let run source ~entry =
    print_s [%sexp (Region_check.run source ~entry : Region_check.Outcome.t)]
  in
  run
    ~entry:"use"
    {|
let sum (b : box[int]) = match b with *n -> n
let use (u : unit) =
  let p = ((stack_ (Box.make 1)), Box.make 2) in
  match p with
  | (b, _) | (_, b) -> Int.add (sum b) 0
|};
  run
    ~entry:"second"
    {|
let sum (b : box[int]) = match b with *n -> n
let second (u : unit) =
  region_ (
    let p = ((stack_ (Box.make 1)), (stack_ (Box.make 2))) in
    match (0, p) with
    | (1, (b, _)) | (_, (_, b)) -> Int.add (sum b) 0)
|};
  run
    ~entry:"escape"
    {|
let keep : ptr[box[int]] = Ptr.make (Box.make 0)
let escape (u : unit) =
  region_ (
    let p = ((stack_ (Box.make 1)), Box.make 2) in
    match p with
    | (b, _) | (_, b) -> keep.* <- b);
  match keep.* with *n -> n
|};
  [%expect
    {|
    (Returned (value 1) (on_heap 0) (in_regions 2))
    (Returned (value 2) (on_heap 0) (in_regions 2))
    (Rejected
     ("This is local, because it is allocated by [stack_] (at t.stag:5:13-34), then it is part of the value (at t.stag:5:13-34), then p is used (at t.stag:6:10-11), then it is bound by an alternative of an or-pattern (at t.stag:7:7-8), then b is used (at t.stag:7:35-36), but must be at most global, because it is stored through a pointer, which may outlive any region (at t.stag:7:35-36)"))
    |}]
;;

let%expect_test "alternatives binding a name to the same part keep its mode" =
  let run pattern =
    print_s
      [%sexp
        (Region_check.run
           ~entry:"pick"
           (sprintf
              {|
let pick (k : int) =
  let p = ((stack_ (Box.make 1)), Box.make k) in
  match p with
  | %s -> b
|}
              pattern)
         : Region_check.Outcome.t)]
  in
  run "(a, b) | (a, b)";
  run "(a, b) | (b, a)";
  run "(a, b)";
  [%expect
    {|
    (Returned (value <value>) (on_heap 1) (in_regions 1))
    (Rejected
     ("This is local, because it is allocated by [stack_] (at t.stag:3:11-32), then it is part of the value (at t.stag:3:11-32), then p is used (at t.stag:4:8-9), then it is bound by an alternative of an or-pattern (at t.stag:5:8-9), then b is used (at t.stag:5:23-24), but must be at most regional, because the function returns it, ending its region (return it with [exclave_] instead) (at t.stag:3:2-5:24)"))
    (Returned (value <value>) (on_heap 1) (in_regions 1))
    |}]
;;
