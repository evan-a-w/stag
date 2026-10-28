open! Import

(* A toplevel splice of a [Stag.Expr.t]: the syntax is checked in place of the
   splice. *)

let check ?run contents = File_check.check ?run contents

let%expect_test "syntax is checked where it is spliced" =
  check
    ~run:true
    {|
let one = .~(Stag.Expr.int 1)
let add x = .~(Stag.Expr.apply (Stag.Expr.var "Core.Int.add") [Stag.Expr.var "x"; Stag.Expr.int 2])
let shadowed = let one = "local" in .~(Stag.Expr.var "one")
let code = .~(.< 3 >.)
let main = print_endline (Core.Int.to_string (add one))
|};
  [%expect
    {|
    module A : sig
      val one : int
      val add : int -> int
      val shadowed : string
      val code : int
      val main : unit
    end
    --- output ---
    3
    |}]
;;

let%expect_test "in functions, at the expected type" =
  check
    ~run:true
    {|
let gen name = Stag.Expr.var name
let f (x : string) = .~(gen "x")
let g x = Core.String.append .~(gen "x") "!"
let main = print_endline (g (f "a"))
|};
  [%expect
    {|
    module A : sig
      val gen : string -> Stag.Expr.t
      val f : string -> string
      val g : string -> string
      val main : unit
    end
    --- output ---
    a!
    |}]
;;

let%expect_test "errors in the syntax are at the splice, with the syntax" =
  check
    {|
let x = .~(Stag.Expr.apply (Stag.Expr.var "Core.Int.add") [Stag.Expr.var "nope"; Stag.Expr.int 1])
|};
  check
    {|
let x = .~(Stag.Expr.apply (Stag.Expr.var "Core.Int.add") [Stag.Expr.string "s"; Stag.Expr.int 1])
|};
  check
    {|
let x : int = .~(Stag.Expr.string "s")
|};
  [%expect
    {|
    File "a.stag", line 2, characters 8-98:
    2 | let x = .~(Stag.Expr.apply (Stag.Expr.var "Core.Int.add") [Stag.Expr.var "nope"; Stag.Expr.int 1])
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the expression this splice gave: Unbound value nope
    Note: The splice gave the expression:
      Core.Int.add nope 1
    File "a.stag", line 2, characters 8-98:
    2 | let x = .~(Stag.Expr.apply (Stag.Expr.var "Core.Int.add") [Stag.Expr.string "s"; Stag.Expr.int 1])
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the expression this splice gave: This expression has type string but an expression was expected of type int
    Note: The splice gave the expression:
      Core.Int.add "s" 1
    File "a.stag", line 2, characters 14-38:
    2 | let x : int = .~(Stag.Expr.string "s")
                      ^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This expression has type string but an expression was expected of type int
    |}]
;;

let%expect_test "evaluation errors" =
  check
    {|
let fail _ : Stag.Expr.t = raise (Failure "no")
let x = .~(fail ())
|};
  check
    {|
let x = .~(Stag.Expr.var "not a name")
|};
  [%expect
    {|
    File "a.stag", line 3, characters 8-19:
    3 | let x = .~(fail ())
                ^^^^^^^^^^^
    Error: This toplevel splice failed at compile time: Uncaught exception Failure "no"
    File "a.stag", line 2, characters 27-47:
    2 | let fail _ : Stag.Expr.t = raise (Failure "no")
                                   ^^^^^^^^^^^^^^^^^^^^
    Note: The error was raised here
    File "a.stag", line 2, characters 8-38:
    2 | let x = .~(Stag.Expr.var "not a name")
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This splice gave malformed syntax: Malformed syntax: expected a path, optionally from the root ([.M.x]), of capitalized identifiers ending in a lowercase identifier, got "not a name", within Stag.Expr.Var argument 0 > Stag.Path.Name
    |}]
;;

let%expect_test "the splice's expression is at compile time" =
  check
    {|
let f e = .~e
|};
  check
    {|
let e = Stag.Expr.int 1
let f (x : Stag.Expr.t) = .~x
|};
  [%expect
    {|
    File "a.stag", line 2, characters 12-13:
    2 | let f e = .~e
                    ^
    Error: The variable e is bound at runtime (level 0) but used at compile time (level -1); only toplevel functions may be used at other levels
    File "a.stag", line 3, characters 28-29:
    3 | let f (x : Stag.Expr.t) = .~x
                                    ^
    Error: The variable x is bound at runtime (level 0) but used at compile time (level -1); only toplevel functions may be used at other levels
    |}]
;;

let%expect_test "substitutions" =
  check
    {|
let x = .~(Stag.Expr.var "y").{y = 1}
|};
  [%expect
    {|
    File "a.stag", line 2, characters 8-37:
    2 | let x = .~(Stag.Expr.var "y").{y = 1}
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: A splice of syntax (a Stag.Expr.t) is checked where it is, so it takes no substitutions
    |}]
;;

let%expect_test "within quotes, splices are of code" =
  check
    {|
let c = .< .~(Stag.Expr.int 1) >.
|};
  [%expect
    {|
    File "a.stag", line 2, characters 13-30:
    2 | let c = .< .~(Stag.Expr.int 1) >.
                     ^^^^^^^^^^^^^^^^^
    Error: This expression has type Stag.Expr.t but an expression was expected of type <'_a>{..'_b}
    |}]
;;

let%expect_test "a toplevel being checked" =
  check
    {|
let gen _ = Stag.Expr.int .~(gen ())
|};
  check
    {|
let a = .~(gen ())
let gen _ = Stag.Expr.int 1
|};
  [%expect
    {|
    File "a.stag", line 2, characters 29-32:
    2 | let gen _ = Stag.Expr.int .~(gen ())
                                     ^^^
    Error: The toplevel A.gen is used at compile time before it is fully checked (it is in the same recursive group as this splice)
    module A : sig
      val a : int
      val gen : 'a -> Stag.Expr.t
    end
    |}]
;;

let%expect_test "a type variable's conversion is needed where it is spliced" =
  check
    {|
let f v = .~(Core.Sexp.sexp_of (type list['a])) v
|};
  [%expect
    {|
    File "a.stag", line 2, characters 10-47:
    2 | let f v = .~(Core.Sexp.sexp_of (type list['a])) v
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the expression this splice gave: Unbound value _of_a
    Note: The splice gave the expression:
      .Core.Sexp.sexp_of_list _of_a
    |}]
;;
