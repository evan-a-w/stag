open! Import

(* The errors of [source], else what running it gives [result]. *)
let check source =
  match Mode_helpers.located_errors source with
  | Error errors ->
    List.iter errors ~f:(fun (loc, message) ->
      printf "%s: %s\n" (Loc.to_string loc) message)
  | Ok () ->
    Option.iter
      (Source_helpers.typecheck (source ^ "\nlet result = main ()"))
      ~f:Source_helpers.eval_and_print_values
;;

let%expect_test "a local value may not use itself" =
  check {|let main (u : unit) = let n = Int.add n 1 in n|};
  check {|let main (u : unit) = let n = n in n|};
  check {|let main (u : unit) = let b = Box.make b in 0|};
  check {|let main (u : unit) = let n = (let k = n in k) in 0|};
  check {|let main (u : unit) = let n = match n with _ -> 0 in n|};
  check {|let main (u : unit) = let n = (n; 0) in n|};
  [%expect
    {|
    t.stag:1:38-39: n is used before its value is defined (a value may not depend on itself)
    t.stag:1:30-31: n is used before its value is defined (a value may not depend on itself)
    t.stag:1:39-40: b is used before its value is defined (a value may not depend on itself)
    t.stag:1:39-40: n is used before its value is defined (a value may not depend on itself)
    t.stag:1:36-37: n is used before its value is defined (a value may not depend on itself)
    t.stag:1:31-32: n is used before its value is defined (a value may not depend on itself)
    |}]
;;

let%expect_test "an error in its definition caused by using it is not reported" =
  (* [m]'s type would be cyclic. *)
  check {|let main (u : unit) = let m = (1, m) in 0|};
  check {|let main (u : unit) = let m = Some m in 0|};
  (* Other errors are. *)
  check {|let main (u : unit) = let q = Int.add q "s" in q|};
  [%expect
    {|
    t.stag:1:34-35: m is used before its value is defined (a value may not depend on itself)
    t.stag:1:35-36: m is used before its value is defined (a value may not depend on itself)
    t.stag:1:38-39: q is used before its value is defined (a value may not depend on itself)
    t.stag:1:40-43: This expression has type string but an expression was expected of type int
    |}]
;;

let%expect_test "lets are recursive, so can't refer to what they shadow" =
  check {|let main (u : unit) = let s = "a" in let s = String.length s in s|};
  check {|let main (n : int) = let n = Int.add n 1 in n|};
  check {|
let n = 1
let main (u : unit) = let n = Int.add n 1 in n|};
  [%expect
    {|
    t.stag:1:59-60: s is used before its value is defined (a value may not depend on itself). Lets are recursive, so this s is the one being defined, not the s outside it: rename one of them
    t.stag:1:37-38: n is used before its value is defined (a value may not depend on itself). Lets are recursive, so this n is the one being defined, not the n outside it: rename one of them
    t.stag:3:38-39: n is used before its value is defined (a value may not depend on itself). Lets are recursive, so this n is the one being defined, not the n outside it: rename one of them
    |}]
;;

let%expect_test "a function only built may refer to it" =
  check
    {|
let main (u : unit) =
  let f = fun x -> match Int.equal x 0 with true -> 0 | false -> f (Int.sub x 1) in
  f 3|};
  check
    {|
let main (u : unit) =
  let p = (1, fun (x : int) -> match p with (k, _) -> k) in
  match p with (_, g) -> g 0|};
  check {|
let main (u : unit) =
  let g = (let h x = g in 1) in
  g|};
  check
    {|
let main (u : unit) =
  let f = stack_ (fun x -> match Int.equal x 0 with true -> 0 | false -> f (Int.sub x 1)) in
  f 3 [@nontail]|};
  [%expect
    {|
    (main <fun>)
    (result 0)
    (main <fun>)
    (result 1)
    (main <fun>)
    (result 1)
    (main <fun>)
    (result 0)
    |}]
;;

let%expect_test "a function called while computing it may not" =
  check {|let main (u : unit) = let g = (let h x = g in h 1) in g|};
  check {|let main (u : unit) = let g = (fun x -> g) 1 in 0|};
  (* Calls are assumed to need what the callee refers to, as for toplevels. *)
  check
    {|
let apply x = x
let main (u : unit) = let f = apply (fun x -> f x) in 0|};
  [%expect
    {|
    t.stag:1:41-42: g is used before its value is defined (a value may not depend on itself)
    t.stag:1:40-41: g is used before its value is defined (a value may not depend on itself)
    t.stag:3:46-47: f is used before its value is defined (a value may not depend on itself)
    |}]
;;

let%expect_test "in nested lets and quotes" =
  check
    {|let main (u : unit) = let a = 1 in let b = (let c = Int.add c a in c) in b|};
  check {|let main (u : unit) = .< let n = Int.add n 1 in n >.|};
  (* A splice's code may need the binder: only if it surely does is it used. *)
  check
    {|
let make (c : <int>{..'r}) = .< let x = .~c in x >.
let main (u : unit) = make .< 1 >.|};
  check {|let main (u : unit) = .< let x = .~(.< x >.) in x >.|};
  [%expect
    {|
    t.stag:1:60-61: c is used before its value is defined (a value may not depend on itself)
    t.stag:1:41-42: n is used before its value is defined (a value may not depend on itself)
    (make <fun>)
    (main <fun>)
    (result
     (Code
      (Let (ident x/16) (args ()) (generalized false) (body (Lit (Int 1) : int))
       (in_ (Var (Bound (Local x/16)) : int)) : int)))
    t.stag:1:33-44: x is used before its value is defined (a value may not depend on itself)
    |}]
;;
