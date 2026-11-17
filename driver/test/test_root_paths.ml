open! Import

let check = File_check.check
let check_files = File_check.check_files

let%expect_test "root paths in every position, under local shadowing" =
  check
    ~run:true
    {|
type int = bool
type option = Some | None
type r = { x : .int }
let raise = 3
module M = struct
  type r = { x : bool }
  let v = { .A.x = 1 }
  let w = { v with .A.x = 2 }
  let x = 5
  module A = struct let x = true end
  let field = v.(.A.x)
  let from_root = .A.M.x
  let matched = match .Some w with .Some { .A.x = n } -> n | .None -> 0
  let local = A.x
end
let a : .int = 1
let b : .option[.int] = .None
let f (x : .int) : .list[.int] = Cons (x, Nil)
let s = .String.length "abc"
let t = .Core.Int.to_string 3
let caught =
  match .raise (.Failure "boom") with
  | () -> ""
  | exception .Failure m -> m
let c = .< .Some (.String.length "ab") >.
let d = match .~c with .Some n -> n | .None -> 0
let shown = print_endline (.Core.Int.to_string (.Int.add M.field (.Int.add M.from_root d)))
|};
  [%expect
    {|
    module A : sig
      type int = bool
      type option = Some | None
      type r = { x : .int }
      val raise : .int
      module M : sig
        type r = { x : bool }
        val v : .A.r
        val w : .A.r
        val x : .int
        module A : sig
          val x : bool
        end
        val field : .int
        val from_root : .int
        val matched : .int
        val local : bool
      end
      val a : .int
      val b : .option[.int]
      val f : .int -> list[.int]
      val s : .int
      val t : string
      val caught : string
      val c : <.option[.int]>{..'a}
      val d : .int
      val shown : unit
    end
    --- output ---
    8
    |}]
;;

let%expect_test "a root path is not resolved locally" =
  check {|let f x = .x|};
  [%expect
    {|
    File "a.stag", line 1, characters 10-12:
    1 | let f x = .x
                  ^^
    Error: Unbound value .x
    |}];
  check {|module M = struct let x = 1 end
let y = M.x
let z = .M.x|};
  [%expect
    {|
    File "a.stag", line 3, characters 8-10:
    3 | let z = .M.x
                ^^
    Error: Unbound module .M
    |}];
  check {|let y = .Nope.x|};
  [%expect
    {|
    File "a.stag", line 1, characters 8-13:
    1 | let y = .Nope.x
                ^^^^^
    Error: Unbound module .Nope
    |}];
  check {|let y : .Nope.t = 1|};
  [%expect
    {|
    File "a.stag", line 1, characters 8-13:
    1 | let y : .Nope.t = 1
                ^^^^^
    Error: Unbound module .Nope
    |}];
  check {|let y = match 1 with .Nope.C -> 1|};
  [%expect
    {|
    File "a.stag", line 1, characters 21-26:
    1 | let y = match 1 with .Nope.C -> 1
                             ^^^^^
    Error: Unbound module .Nope
    |}];
  check {|let y r = r.(.Nope.x)|};
  [%expect
    {|
    File "a.stag", line 1, characters 13-18:
    1 | let y r = r.(.Nope.x)
                     ^^^^^
    Error: Unbound module .Nope
    |}];
  check {|let y r = r.(x)|};
  [%expect
    {|
    File "a.stag", line 1, characters 13-14:
    1 | let y r = r.(x)
                     ^
    Error: Expected a root path like `.M.x`: `e.(.M.x)`, found `x`
    |}]
;;

let%expect_test "root paths across files" =
  check_files
    [ "a.stag", {|let x = 1
module B = struct let x = "inner" end|}
    ; ( "b.stag"
      , {|let x = "b"
let y = .Int.add .A.x 1
let z = .Core.String.concat "" (Cons (.B.x, Cons (A.B.x, Nil)))|}
      )
    ];
  [%expect
    {|
    module A : sig
      val x : int
      module B : sig
        val x : string
      end
    end
    module B : sig
      val x : string
      val y : int
      val z : string
    end
    |}]
;;

let%expect_test "a root module shadowing the prelude's" =
  check_files
    [ "core.stag", {|let x = 1|}
    ; "a.stag", {|let y = .Core.x
let z = .Core.Int.to_string 1|}
    ];
  [%expect
    {|
    File "a.stag", line 2, characters 8-17:
    2 | let z = .Core.Int.to_string 1
                ^^^^^^^^^
    Error: Unbound module .Core.Int (the prelude's module Core is shadowed by the program's Core)
    File "core.stag", line 1, characters 0-0:
    1 | let x = 1
        ^
    Note: The program's module Core is here
    |}]
;;

let%expect_test "generated root paths" =
  check
    ~run:true
    {|
let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
type option = Some | None
let raise = 0
include .~(value "x" (Stag.Expr.var ".raise"))
include .~(value "y" (Stag.Expr.construct ".Some" (Cons (Stag.Expr.int 1, Nil))))
include .~(value "z" (Stag.Expr.var ".A.raise"))
let shown = print_endline (.Core.Int.to_string z)
|};
  [%expect
    {|
    module A : sig
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      type option = Some | None
      val raise : int
      val x : exn -> 'a
      val y : .option[int]
      val z : int
      val shown : unit
    end
    --- output ---
    0
    |}];
  check
    {|
let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
include .~(value "bad" (Stag.Expr.var ".a.b"))
|};
  [%expect
    {|
    File "a.stag", line 3, characters 0-46:
    3 | include .~(value "bad" (Stag.Expr.var ".a.b"))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice generated malformed declarations: Malformed syntax: expected a path, optionally from the root ([.M.x]), of capitalized identifiers ending in a lowercase identifier, got ".a.b", within element 0 > Stag.Toplevel.Let argument 3 > Stag.Expr.Var argument 0 > Stag.Path.Name
    |}]
;;

let gen =
  ( "gen.stag"
  , {|
let r = .< raise >.
let captured = .< fun raise -> (raise, .~r, Some 1, String.length "s") >.
let declare name code =
  Cons
    ( Stag.Toplevel.let_ name Nil
        (Stag.Expr.App (Stag.Expr.int 0, Stag.Code.erase code))
    , Nil )
|}
  )
;;

let%expect_test "erased refs print as root paths, reading back the same" =
  check_files
    [ gen
    ; ( "a.stag"
      , {|
type option = Some | None
let raise = 1
module String = struct end
include .~(Gen.declare "bad" Gen.captured)
|}
      )
    ];
  [%expect
    {|
    File "a.stag", line 5, characters 0-42:
    5 | include .~(Gen.declare "bad" Gen.captured)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the declaration `let bad` generated by this splice: This expression has type int but an expression was expected of type '_a -> '_b
    Note: The generated declaration is:
      let bad = 0 (fun raise -> (raise, .raise, .Some 1, .String.length "s"))
    |}]
;;
