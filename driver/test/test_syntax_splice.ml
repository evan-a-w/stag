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
    Error: In the expression this splice gave: This expression has type string but an expression was expected of type int
    Note: The splice gave the expression:
      "s"
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

let%expect_test
    "a splice of syntax can't give an [%expect], which could not be corrected"
  =
  check
    ~run:true
    {|
let%expect_test "t" =
  print_endline "hi";
  .~(Stag.Expr.Expect "")
let%expect_test "nested" =
  print_endline "hi";
  .~(Stag.Expr.Let ("u", [], None, Stag.Expr.Lit Stag.Lit.Unit, Stag.Expr.Expect ""))
|};
  [%expect
    {|
    File "a.stag", line 4, characters 2-25:
    4 |   .~(Stag.Expr.Expect "")
          ^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the expression this splice gave: A splice of syntax can't give an [%expect]: its expected output is generated, so it could not be corrected
    Note: The splice gave the expression:
      [%expect]
    File "a.stag", line 7, characters 2-85:
    7 |   .~(Stag.Expr.Let ("u", [], None, Stag.Expr.Lit Stag.Lit.Unit, Stag.Expr.Expect ""))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the expression this splice gave: A splice of syntax can't give an [%expect]: its expected output is generated, so it could not be corrected
    Note: The splice gave the expression:
      let u = () in
      [%expect]
    |}]
;;

let%expect_test "a mismatch with the expected type is the expression's" =
  check
    {|
let g : string -> Core.Sexp.t = .~(Core.Sexp.sexp_of (type int))
|};
  [%expect
    {|
    File "a.stag", line 2, characters 32-64:
    2 | let g : string -> Core.Sexp.t = .~(Core.Sexp.sexp_of (type int))
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the expression this splice gave: This expression has type int but an expression was expected of type string
    Note: The splice gave the expression:
      .Core.Sexp.sexp_of_int
    |}]
;;

let%expect_test "the expected type tells constructors apart" =
  check
    ~run:true
    {|
module M = struct
  type t = A | B
end
type u = B
let z : M.t = .~(Stag.Expr.construct "B" [])
let main = print_endline (match z with M.A -> "A" | M.B -> "M.B")
|};
  [%expect
    {|
    module A : sig
      module M : sig
        type t = A | B
      end
      type u = B
      val z : M.t
      val main : unit
    end
    --- output ---
    M.B
    |}]
;;

let%expect_test "a toplevel splice of neither code nor syntax" =
  check
    {|
let x = .~1
|};
  [%expect
    {|
    File "a.stag", line 2, characters 10-11:
    2 | let x = .~1
                  ^
    Error: This expression has type int, but a toplevel splice is of code (of type <'a>{..'r}) or of syntax (of type Stag.Expr.t)
    |}]
;;

let%expect_test "substitutions are rejected before being checked" =
  check
    {|
let x = .~(Stag.Expr.var "y").{y = nope}
|};
  check
    {|
let x = .~(Stag.Expr.var "y").{y = 1; y = 2}
|};
  [%expect
    {|
    File "a.stag", line 2, characters 8-40:
    2 | let x = .~(Stag.Expr.var "y").{y = nope}
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: A splice of syntax (a Stag.Expr.t) is checked where it is, so it takes no substitutions
    File "a.stag", line 2, characters 8-44:
    2 | let x = .~(Stag.Expr.var "y").{y = 1; y = 2}
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: A splice of syntax (a Stag.Expr.t) is checked where it is, so it takes no substitutions
    |}]
;;

let%expect_test "a splice of syntax is generalized" =
  check
    ~run:true
    {|
let id = .~(Stag.Expr.fun_ ["x"] (Stag.Expr.var "x"))
let main = print_endline (Core.String.append (id "a") (Core.Int.to_string (id 1)))
|};
  [%expect
    {|
    module A : sig
      val id : 'a -> 'a
      val main : unit
    end
    --- output ---
    a1
    |}]
;;

let%expect_test "within a compile-time splice" =
  check
    ~run:true
    {|
let x = .~(Stag.Expr.int .~(Stag.Expr.int 1))
let main = print_endline (Core.Int.to_string x)
|};
  [%expect
    {|
    module A : sig
      val x : int
      val main : unit
    end
    --- output ---
    1
    |}]
;;

let%expect_test "names in the syntax are resolved where it is spliced" =
  check
    ~run:true
    {|
module Core = struct
  let x = "local Core"
end
let s = .~(Stag.Expr.var "Core.x")
module M = struct
  module Core = struct end
  let shown = .~(.Core.Sexp.sexp_of (type list[int])) [1; 2]
end
let main = print_endline s; print_endline (.Core.Sexp.to_string M.shown)
|};
  [%expect
    {|
    module A : sig
      module Core : sig
        val x : string
      end
      val s : string
      module M : sig
        module Core : sig end
        val shown : .Core.Sexp.t
      end
      val main : unit
    end
    --- output ---
    local Core
    (1 2)
    |}]
;;

let%expect_test "compile-time failures of the generators" =
  File_check.check
    ~filter_diagnostic:File_check.without_core_location
    {|
let f = .~(Core.Sexp.sexp_of (type (int -> int)))
|};
  [%expect
    {|
    File "a.stag", line 2, characters 8-49:
    2 | let f = .~(Core.Sexp.sexp_of (type (int -> int)))
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This toplevel splice failed at compile time: Uncaught exception Failure "Sexp.sexp_of: functions have no sexp conversion"
    |}]
;;

let%expect_test "a conversion has exactly its type, its rows closed" =
  check
    {|
let of_sexp = .~(Core.Sexp.of_sexp (type option[[`A | `B of int]]))
let sexp_of = .~(Core.Sexp.sexp_of (type list[[`A | `B of int] * int]))
let raising = .~(Core.Sexp.of_sexp_raising (type [`A]))
|};
  [%expect
    {|
    module A : sig
      val of_sexp : Core.Sexp.t -> result[option[[`A | `B of int]], Core.Sexp.Of_sexp_error.t]
      val sexp_of : list[[`A | `B of int] * int] -> Core.Sexp.t
      val raising : Core.Sexp.t -> [`A]
    end
    |}]
;;

let%expect_test "mutable variables in generated syntax and declarations" =
  check
    ~run:true
    {|
let counted =
  .~(Stag.Expr.Let_mut
       ( "n"
       , None
       , Stag.Expr.int 1
       , Stag.Expr.Let
           ( "_"
           , Nil
           , None
           , Stag.Expr.Assign
               ( Stag.Path.Name "n"
               , Stag.Expr.apply (Stag.Expr.var "Core.Int.add") (Cons (Stag.Expr.var "n", Cons (Stag.Expr.int 2, Nil))) )
           , Stag.Expr.var "n" ) ))
include .~(.< struct
  let mut total = 0
  let add_to_total n = total <- Core.Int.add total n
end >.)
module Generated = .~(Cons (Stag.Toplevel.Let_mut ("count", None, Stag.Expr.int 5), Nil))
let main =
  add_to_total 3;
  add_to_total 4;
  Generated.count <- Core.Int.add Generated.count 1;
  print_endline (Core.Int.to_string counted);
  print_endline (Core.Int.to_string total);
  print_endline (Core.Int.to_string Generated.count)
|};
  [%expect
    {|
    module A : sig
      val counted : int
      val total : int
      val add_to_total : int -> unit
      module Generated : sig
        val count : int
      end
      val main : unit
    end
    --- output ---
    3
    7
    6
    |}]
;;
