open! Import

(* The errors of [source], else its signature and values. *)
let check source =
  match Mode_helpers.located_errors source with
  | Error errors ->
    List.iter errors ~f:(fun (loc, message) ->
      printf "%s: %s\n" (Loc.to_string loc) message)
  | Ok () -> Source_helpers.run source
;;

let round_trip source =
  let program = Source_helpers.parse_exn source in
  print_string (Printer.program program);
  Round_trip.program program;
  print_s
    (Round_trip.strip_locs
       [%sexp
         (List.map program ~f:(fun t -> t.txt) : Syntax.Toplevel.desc list)])
;;

let%expect_test "parsing and printing" =
  round_trip
    {|
let nonrec f x = let nonrec x = Int.add x 1 in x
type nonrec t['a] = list[t['a]]
type nonrec u = | A | B
|};
  [%expect
    {|
    let nonrec f x =
      let nonrec x = Int.add x 1 in
      x

    type nonrec t['a] = list[t['a]]
    type nonrec u = A | B
    ((Let_nonrec f ((x ())) ()
      (Let_nonrec x () () (App (App (Var Int.add ()) (Var x ())) (Lit (Int 1)))
       (Var x ())))
     (Type
      ((name t) (params ((Ty a)))
       (kind (Alias (Con list ((Ty (Con t ((Ty (Var a))))))))) (nonrec_)))
     (Type ((name u) (params ()) (kind (Variant ((A ()) (B ())))) (nonrec_))))
    |}]
;;

let%expect_test "let mut is never recursive, so takes no nonrec" =
  let parse source =
    match Parser.program ~file:"t.stag" source with
    | Ok _ -> print_endline "parsed"
    | Error error -> print_endline (Parser.Error.to_string error)
  in
  parse {|let x = let mut nonrec y = 1 in y|};
  parse {|let x = let nonrec mut y = 1 in y|};
  parse {|let mut nonrec y = 1|};
  parse {|let nonrec mut y = 1|};
  parse {|let nonrec nonrec y = 1|};
  [%expect
    {|
    t.stag:1:16-22: a mutable variable is never recursive; remove `nonrec`
    t.stag:1:12-18: a mutable variable is never recursive; remove `nonrec`
    t.stag:1:8-14: a mutable variable is never recursive; remove `nonrec`
    t.stag:1:4-10: a mutable variable is never recursive; remove `nonrec`
    t.stag:1:11-17: `nonrec` is written once, directly after `let`
    |}]
;;

let%expect_test "a nonrec local refers to the binding it shadows" =
  check
    {|
let s = "toplevel"
let main (u : unit) =
  let s = "abc" in
  let nonrec s = String.length s in
  s
let from_toplevel (u : unit) = let nonrec s = String.length s in s
let from_prelude (u : unit) =
  let nonrec print_endline s = print_endline (String.append s "!") in
  print_endline "prelude"
let function_ (u : unit) =
  let f x = Int.add x 1 in
  let nonrec f x = f (f x) in
  f 0
let result = (main (), from_toplevel (), from_prelude (), function_ ())
|};
  [%expect
    {|
    val s : string
    val main : unit -> int
    val from_toplevel : unit -> int
    val from_prelude : unit -> unit
    val function_ : unit -> int
    val result : int * int * unit * int
    -- evaluated at compile time
    -- evaluated
    prelude!
    (s (String toplevel))
    (main <fun>)
    (from_toplevel <fun>)
    (from_prelude <fun>)
    (function_ <fun>)
    (result (Tuple 3 8 "()" 2))
    |}]
;;

let%expect_test "a nonrec toplevel refers to the outer one" =
  check
    {|
let n = 1
module M = struct
  let nonrec n = Int.add n 10
  let nonrec length s = Int.add (String.length s) n
end
let result = (M.n, M.length "ab")
|};
  [%expect
    {|
    val n : int
    module M : sig
      val n : int
      val length : string -> int
    end
    val result : int * int
    -- evaluated at compile time
    -- evaluated
    (n 1)
    (M.n 11)
    (M.length <fun>)
    (result (Tuple 11 13))
    |}]
;;

let%expect_test "a nonrec binding is not in scope in its definition" =
  check {|let main (u : unit) = let nonrec x = x in x|};
  check {|let main (u : unit) = let nonrec f x = f x in f|};
  check {|let nonrec y = y|};
  check {|let nonrec g x = g x|};
  [%expect
    {|
    t.stag:1:37-38: Unbound value x
    t.stag:1:39-40: Unbound value f
    t.stag:1:15-16: Unbound value y
    t.stag:1:17-18: Unbound value g
    |}]
;;

let%expect_test "a nonrec type refers to the outer type" =
  check
    {|
type t = A | B
module M = struct
  type nonrec t = list[t]
  let x : t = [A; B]
end
type nonrec option = option[int]
let y : option = Some 1
let result = (M.x, y)
|};
  check {|type nonrec v = list[v]|};
  [%expect
    {|
    type t = A | B
    module M : sig
      type t = list[.t]
      val x : list[.t]
    end
    type option = .option[int]
    val y : .option[int]
    val result : list[t] * .option[int]
    -- evaluated at compile time
    -- evaluated
    (M.x (Cons (A) (Cons (B) (Nil))))
    (y (Some 1))
    (result (Tuple (Cons (A) (Cons (B) (Nil))) (Some 1)))
    t.stag:1:21-22: Unbound type v
    |}]
;;

let%expect_test "the shadowing hint suggests let nonrec" =
  check {|let main (u : unit) = let s = "a" in let s = String.length s in s|};
  [%expect
    {| t.stag:1:59-60: s is used before its value is defined (a value may not depend on itself). Lets are recursive, so this s is the one being defined, not the s outside it: write `let nonrec s` to use the outer one, or rename one of them |}]
;;

let%expect_test "quotes of nonrec lets" =
  check
    {|
let code = .< fun (s : string) -> let nonrec s = String.length s in s >.
let result = .~code "abc"
let lifted = Stag.Code.erase .< fun (s : string) -> let nonrec s = String.length s in s >.
|};
  [%expect
    {|
    val code : <string -> int>{..'a}
    val result : int
    val lifted : Stag.Expr.t
    -- evaluated at compile time
    (code
     (Code
      (Fun (args (s/15))
       (annots
        ((s/15 ((ty ((Con (<string> @ t.stag:2:23-29) ()) @ t.stag:2:23-29))))))
       (body
        (Let (ident s/16) (args ()) (generalized false)
         (body
          (App (Var (Prim String.length) : "string -> int")
           (Var (Bound (Local s/15)) : string) : int))
         (in_ (Var (Bound (Local s/16)) : int)) : int))
       : "string -> int")))
    -- evaluated
    (code
     (Code
      (Fun (args (s/15))
       (annots
        ((s/15 ((ty ((Con (<string> @ t.stag:2:23-29) ()) @ t.stag:2:23-29))))))
       (body
        (Let (ident s/16) (args ()) (generalized false)
         (body
          (App (Var (Prim String.length) : "string -> int")
           (Var (Bound (Local s/15)) : string) : int))
         (in_ (Var (Bound (Local s/16)) : int)) : int))
       : "string -> int")))
    (result 3)
    (lifted
     (Stag.Expr.Fun
      (Ptr
       (Tuple
        (Cons
         (Tuple (String s)
          ((mode (None))
           (ty
            (Some (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type string))) (Nil))))))
         (Nil))
        (Stag.Expr.Let
         (Ptr
          (Tuple (String s_1) (Nil) ((mode (None)) (ty (None)))
           (Stag.Expr.App
            (Ptr
             (Tuple
              (Stag.Expr.Var (Stag.Path.Ref (Ref (value Prim.String.length)))
               ((mode (None)) (ty (None))))
              (Stag.Expr.Var (Stag.Path.Name (String s))
               ((mode (None)) (ty (None)))))))
           (Stag.Expr.Var (Stag.Path.Name (String s_1))
            ((mode (None)) (ty (None)))))))))))
    |}]
;;
