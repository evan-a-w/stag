open! Import

(* Checks [contents] as the file [a.stag], printing its signature, or the
   diagnostic. With [run], also evaluates it, printing its output, and runs its
   expect tests, printing those that fail and the corrected file. *)
let check ?(run = false) contents =
  let file = { Source_file.path = "a.stag"; contents } in
  let print_diagnostic diagnostic =
    print_string
      (Diagnostic.to_string_hum ~contents:(fun _ -> Some contents) diagnostic)
  in
  match Driver.check_files [ file ] with
  | Error error -> print_diagnostic (Diagnostic.of_driver_error error)
  | Ok driver ->
    print_string (Signature.to_string_hum (Driver.checked driver).signature);
    if run
    then (
      let output = Buffer.create 64 in
      Treewalk.set_output (Driver.interpreter driver) (Buffer.add_string output);
      (match Driver.eval driver with
       | Ok () -> ()
       | Error error -> print_diagnostic (Diagnostic.of_runtime_error error));
      printf "--- output ---\n%s" (Buffer.contents output);
      let results = Expect_test_runner.run driver in
      List.iter results ~f:(fun result ->
        if not (Expect_test_runner.Test_result.passed result)
        then print_s [%sexp (result : Expect_test_runner.Test_result.t)]);
      Option.iter
        (Expect_test_runner.corrected ~path:file.path ~contents results)
        ~f:(printf "--- corrected ---\n%s"))
;;

(* [enum] generates a variant of the constructors [names] and its
   [to_string]. *)
let generators =
  {|
let constructor name = (name, Nil)
let arm name = (Stag.Pattern.Construct (name, Nil), Stag.Expr.string name)

let enum names =
  Cons
    ( Stag.Toplevel.Type
        { `name = "t"
        ; `params = Nil
        ; `kind = Stag.Type_decl.Kind.Variant (Stag.map constructor names) }
    , Cons
        ( Stag.Toplevel.let_ "to_string" (Cons ("x", Nil))
            (Stag.Expr.Match (Stag.Expr.var "x", Stag.map arm names))
        , Nil ) )

let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
|}
;;

let%expect_test "a generated variant and a function over it" =
  check
    ~run:true
    (generators
     ^ {|
module Color = .~(enum (Cons ("Red", Cons ("Green", Nil))))

let shown = print_endline (Color.to_string Color.Green)
|}
    );
  [%expect
    {|
    module A : sig
      val constructor : 'a -> 'a * list['b]
      val arm : string -> Stag.Pattern.t * Stag.Expr.t
      val enum : list[string] -> list[Stag.Toplevel.t]
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      module Color : sig
        type t = Red | Green
        val to_string : t -> string
      end
      val shown : unit
    end
    --- output ---
    Green
    |}]
;;

let%expect_test "include, in nested modules, and module splices in modules" =
  check
    ~run:true
    (generators
     ^ {|
module Outer = struct
  let before = 0
  include .~(value "x" (Stag.Expr.int 1))
  module Inner = struct
    include .~(value "y" (Stag.Expr.apply (Stag.Expr.var "Int.add") (Cons (Stag.Expr.var "x", Cons (Stag.Expr.int 1, Nil)))))
    module E = .~(enum (Cons ("A", Nil)))
  end
  let after = Inner.y
end

let shown = print_int (Int.add Outer.x Outer.after)
|}
    );
  [%expect
    {|
    module A : sig
      val constructor : 'a -> 'a * list['b]
      val arm : string -> Stag.Pattern.t * Stag.Expr.t
      val enum : list[string] -> list[Stag.Toplevel.t]
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      module Outer : sig
        val before : int
        val x : int
        module Inner : sig
          val y : int
          module E : sig
            type t = A
            val to_string : t -> string
          end
        end
        val after : int
      end
      val shown : unit
    end
    --- output ---
    3
    |}]
;;

let%expect_test
    "spliced toplevels are evaluated at run time at the splice's position, and \
     their expect tests run"
  =
  check
    ~run:true
    {|
let a = print_endline "a"
include .~(Cons (Stag.Toplevel.let_ "b" Nil (Stag.Expr.App (Stag.Expr.var "print_endline", Stag.Expr.string "b")), Nil))
let c = print_endline "c"
include .~(Cons
  (Stag.Toplevel.Expect_test
     { `name = "generated"
     ; `body =
         Stag.Expr.Let ("u", Nil, None,
           Stag.Expr.App (Stag.Expr.var "print_endline", Stag.Expr.string "hi"),
           Stag.Expr.Expect "wrong") },
   Nil))
let%expect_test "written" = print_endline "w"; [%expect "wrong"]
|};
  [%expect
    {xxx|
    module A : sig
      val a : unit
      val b : unit
      val c : unit
    end
    --- output ---
    a
    b
    c
    ((name (generated @ a.stag:5:0-12:8))
     (mismatches (((loc a.stag:5:0-12:8) (expected (wrong)) (actual hi))))
     (error ()) (generated))
    ((name (written @ a.stag:13:16-25))
     (mismatches (((loc a.stag:13:56-63) (expected (wrong)) (actual w))))
     (error ()))
    --- corrected ---

    let a = print_endline "a"
    include .~(Cons (Stag.Toplevel.let_ "b" Nil (Stag.Expr.App (Stag.Expr.var "print_endline", Stag.Expr.string "b")), Nil))
    let c = print_endline "c"
    include .~(Cons
      (Stag.Toplevel.Expect_test
         { `name = "generated"
         ; `body =
             Stag.Expr.Let ("u", Nil, None,
               Stag.Expr.App (Stag.Expr.var "print_endline", Stag.Expr.string "hi"),
               Stag.Expr.Expect "wrong") },
       Nil))
    let%expect_test "written" = print_endline "w"; [%expect {| w |}]
    |xxx}]
;;

let%expect_test "negative int literals" =
  check
    ~run:true
    {|
include .~(Cons (Stag.Toplevel.let_ "x" Nil (Stag.Expr.int (Int.neg 1)), Nil))
let shown = print_int x
|};
  [%expect
    {|
    module A : sig
      val x : int
      val shown : unit
    end
    --- output ---
    -1
    |}]
;;

let%expect_test "names are forced via paths and unqualified lookups" =
  (* [M.x] forces [M]'s splice, which comes later; [y] forces the include. *)
  check
    (generators
     ^ {|
let a = M.x
let b = y
module M = .~(value "x" (Stag.Expr.int 1))
include .~(value "y" (Stag.Expr.string "y"))
|}
    );
  [%expect
    {|
    module A : sig
      val constructor : 'a -> 'a * list['b]
      val arm : string -> Stag.Pattern.t * Stag.Expr.t
      val enum : list[string] -> list[Stag.Toplevel.t]
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val a : int
      val b : string
      module M : sig
        val x : int
      end
      val y : string
    end
    |}];
  (* A type through a module splice. *)
  check
    (generators
     ^ {|
let f (c : E.t) = E.to_string c
module E = .~(enum (Cons ("A", Nil)))
|}
    );
  [%expect
    {|
    module A : sig
      val constructor : 'a -> 'a * list['b]
      val arm : string -> Stag.Pattern.t * Stag.Expr.t
      val enum : list[string] -> list[Stag.Toplevel.t]
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val f : E.t -> string
      module E : sig
        type t = A
        val to_string : t -> string
      end
    end
    |}]
;;

let%expect_test
    "includes in one module sharing a generator, and a generator using what \
     another include defines"
  =
  check
    ~run:true
    (generators
     ^ {|
include .~(value "a" (Stag.Expr.int (Int.add b 1)))
include .~(value "b" (Stag.Expr.int 1))
module N = struct
  let c = Int.add a b
end
let shown = print_int N.c
|}
    );
  [%expect
    {|
    module A : sig
      val constructor : 'a -> 'a * list['b]
      val arm : string -> Stag.Pattern.t * Stag.Expr.t
      val enum : list[string] -> list[Stag.Toplevel.t]
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val a : int
      val b : int
      module N : sig
        val c : int
      end
      val shown : unit
    end
    --- output ---
    3
    |}]
;;

let%expect_test
    "a name used while running a declaration splice, found elsewhere, can't \
     then be defined by an include"
  =
  (* Found as a prim. *)
  check
    (generators
     ^ {|
include .~(let p = print_string in value "x" (Stag.Expr.int 1))
include .~(value "print_string" (Stag.Expr.int 2))
|}
    );
  [%expect
    {|
    File "a.stag", line 18, characters 19-31:
    18 | include .~(let p = print_string in value "x" (Stag.Expr.int 1))
                            ^^^^^^^^^^^^
    Error: The value print_string is used here while running a declaration splice, so is not looked for among what the declaration splice at a.stag:19:0-50 generates, but that defines it
    File "a.stag", line 19, characters 0-50:
    19 | include .~(value "print_string" (Stag.Expr.int 2))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
    |}];
  (* Found in an enclosing module: [x] is not looked for in [N]'s second
     include while running its first. *)
  check
    (generators
     ^ {|
let x = 1
module N = struct
  include .~(value "y" (Stag.Expr.int x))
  include .~(value "x" (Stag.Expr.int 2))
end
|}
    );
  [%expect
    {|
    File "a.stag", line 20, characters 38-39:
    20 |   include .~(value "y" (Stag.Expr.int x))
                                               ^
    Error: The value x is used here while running a declaration splice, so is not looked for among what the declaration splice at a.stag:21:2-41 generates, but that defines it
    File "a.stag", line 21, characters 2-41:
    21 |   include .~(value "x" (Stag.Expr.int 2))
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
    |}];
  (* Outside running a splice, includes are generated before looking further
     out, so an included name shadows. *)
  check
    ~run:true
    (generators
     ^ {|
let x = 1
module N = struct
  let y = x
  include .~(value "x" (Stag.Expr.int 2))
end
include .~(value "print_string" (Stag.Expr.int 3))
let shown = print_int (Int.add N.y print_string)
|}
    );
  [%expect
    {|
    module A : sig
      val constructor : 'a -> 'a * list['b]
      val arm : string -> Stag.Pattern.t * Stag.Expr.t
      val enum : list[string] -> list[Stag.Toplevel.t]
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val x : int
      module N : sig
        val y : int
        val x : int
      end
      val print_string : int
      val shown : unit
    end
    --- output ---
    5
    |}]
;;

let%expect_test
    "shadowing: a written name in an inner module shadows an included one in \
     an outer"
  =
  check
    ~run:true
    (generators
     ^ {|
include .~(value "x" (Stag.Expr.string "outer, included"))
module Inner = struct
  let x = "inner, written"
  let seen = x
end
module Inner2 = struct
  include .~(value "x" (Stag.Expr.string "inner, included"))
  let seen = x
end
let shown = print_endline Inner.seen; print_endline Inner2.seen; print_endline x
|}
    );
  [%expect
    {|
    module A : sig
      val constructor : 'a -> 'a * list['b]
      val arm : string -> Stag.Pattern.t * Stag.Expr.t
      val enum : list[string] -> list[Stag.Toplevel.t]
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val x : string
      module Inner : sig
        val x : string
        val seen : string
      end
      module Inner2 : sig
        val x : string
        val seen : string
      end
      val shown : unit
    end
    --- output ---
    inner, written
    inner, included
    outer, included
    |}]
;;

let%expect_test "duplicates between spliced and written declarations" =
  check
    (generators
     ^ {|
let x = 1
include .~(value "x" (Stag.Expr.int 2))
|}
    );
  [%expect
    {|
    File "a.stag", line 19, characters 0-39:
    19 | include .~(value "x" (Stag.Expr.int 2))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: The value A.x is defined more than once in this module
    |}];
  check
    (generators
     ^ {|
include .~(value "x" (Stag.Expr.int 2))
let x = 1
|}
    );
  [%expect
    {|
    File "a.stag", line 18, characters 0-39:
    18 | include .~(value "x" (Stag.Expr.int 2))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: The value A.x is defined more than once in this module
    |}];
  check
    (generators
     ^ {|
module M = struct end
module M = .~(value "x" (Stag.Expr.int 2))
|}
    );
  [%expect
    {|
    File "a.stag", line 19, characters 7-8:
    19 | module M = .~(value "x" (Stag.Expr.int 2))
                ^
    Error: The module A.M is defined more than once
    |}];
  check
    (generators
     ^ {|
include .~(value "x" (Stag.Expr.int 2))
include .~(value "x" (Stag.Expr.int 3))
|}
    );
  [%expect
    {|
    File "a.stag", line 19, characters 0-39:
    19 | include .~(value "x" (Stag.Expr.int 3))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: The value A.x is defined more than once in this module
    |}]
;;

let%expect_test "cycles" =
  (* The generator uses a value the splice defines. *)
  check
    (generators
     ^ {|
include .~(value "x" (Stag.Expr.int x))
|}
    );
  [%expect
    {|
    File "a.stag", line 18, characters 36-37:
    18 | include .~(value "x" (Stag.Expr.int x))
                                             ^
    Error: A declaration splice depends on what it defines: this needs the declarations generated by the splice at a.stag:18:0-39, to generate them
    File "a.stag", line 18, characters 0-39:
    18 | include .~(value "x" (Stag.Expr.int x))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
    |}];
  (* A type the splice defines. *)
  check
    (generators
     ^ {|
module E = .~(let f (e : E.t) = e in enum Nil)
|}
    );
  [%expect
    {|
    File "a.stag", line 18, characters 25-28:
    18 | module E = .~(let f (e : E.t) = e in enum Nil)
                                  ^^^
    Error: A declaration splice depends on what it defines: this needs the declarations generated by the splice at a.stag:18:0-46, to generate them
    File "a.stag", line 18, characters 0-46:
    18 | module E = .~(let f (e : E.t) = e in enum Nil)
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
    |}];
  (* Via another module. *)
  check
    (generators
     ^ {|
module A = struct
  let a = B.b
end
module B = .~(value "b" (Stag.Expr.int A.a))
|}
    );
  [%expect
    {|
    File "a.stag", line 21, characters 39-42:
    21 | module B = .~(value "b" (Stag.Expr.int A.a))
                                                ^^^
    Error: The toplevel A.A.a is used at compile time before it is fully checked (it is in the same recursive group as this splice)
    |}];
  (* An unqualified lookup searching the module of a pending include, from
     within its own generator. *)
  check
    (generators
     ^ {|
let one = 1
include .~(value "x" (Stag.Expr.int one))
|}
    );
  [%expect
    {|
    module A : sig
      val constructor : 'a -> 'a * list['b]
      val arm : string -> Stag.Pattern.t * Stag.Expr.t
      val enum : list[string] -> list[Stag.Toplevel.t]
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val one : int
      val x : int
    end
    |}]
;;

let%expect_test "splicing a generator twice gives distinct types" =
  check
    (generators
     ^ {|
module A = .~(enum (Cons ("C", Nil)))
module B = .~(enum (Cons ("C", Nil)))
let ok = (A.to_string A.C, B.to_string B.C)
let wrong = A.to_string B.C
|}
    );
  [%expect
    {|
    File "a.stag", line 21, characters 24-27:
    21 | let wrong = A.to_string B.C
                                 ^^^
    Error: This expression has type A.B.t but an expression was expected of type A.A.t
    |}]
;;

let%expect_test
    "a program shadowing list and option, and with its own module Stag"
  =
  check
    ~run:true
    {|
type list['a] = Nil | Cons of 'a * list['a] | Extra
module Stag = struct
  let mine = 1
end
let l = Extra
let m = Stag.mine
let o = Some 1
|};
  [%expect
    {|
    module A : sig
      type list['a] = Nil | Cons of 'a * list['a] | Extra
      module Stag : sig
        val mine : int
      end
      val l : list['a]
      val m : int
      val o : option[int]
    end
    --- output ---
    |}];
  (* A generator must then name the prelude's list, which it can't: its
     [Cons] is the program's. *)
  check
    {|
type list['a] = Nil | Cons of 'a * list['a]
include .~(Cons (Stag.Toplevel.let_ "x" Nil (Stag.Expr.int 1), Nil))
|};
  [%expect
    {|
    File "a.stag", line 3, characters 40-43:
    3 | include .~(Cons (Stag.Toplevel.let_ "x" Nil (Stag.Expr.int 1), Nil))
                                                ^^^
    Error: This expression has type A.list['_a] but an expression was expected of type list[string]
    |}]
;;

let%expect_test "malformed generated values" =
  check
    (generators
     ^ {|
include .~(value "X" (Stag.Expr.int 1))
|}
    );
  [%expect
    {|
    File "a.stag", line 18, characters 0-39:
    18 | include .~(value "X" (Stag.Expr.int 1))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice generated malformed declarations: Malformed syntax: expected a lowercase identifier or _, got "X", within element 0 > Stag.Toplevel.Let argument 0
    |}];
  check
    (generators
     ^ {|
include .~(Cons (Stag.Toplevel.Type { `name = "t"; `params = Nil; `kind = Stag.Type_decl.Kind.Record Nil }, Nil))
|}
    );
  [%expect
    {|
    File "a.stag", line 18, characters 0-113:
    18 | include .~(Cons (Stag.Toplevel.Type { `name = "t"; `params = Nil; `kind = Stag.Type_decl.Kind.Record Nil }, Nil))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice generated malformed declarations: Malformed syntax: expected at least one field, got Nil, within element 0 > Stag.Toplevel.Type > field `kind > Stag.Type_decl.Kind.Record
    |}]
;;

let%expect_test "errors in generated code" =
  check
    (generators
     ^ {|
module M = .~(value "x" (Stag.Expr.apply (Stag.Expr.var "Int.add") (Cons (Stag.Expr.int 1, Cons (Stag.Expr.string "a", Nil)))))
|}
    );
  [%expect
    {|
    File "a.stag", line 18, characters 0-127:
    18 | module M = .~(value "x" (Stag.Expr.apply (Stag.Expr.var "Int.add") (Cons (Stag.Expr.int 1, Cons (Stag.Expr.string "a", Nil)))))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the declaration `let x` generated by this splice: This expression has type string but an expression was expected of type int
    |}];
  check
    (generators
     ^ {|
include .~(value "x" (Stag.Expr.var "nope"))
|}
    );
  [%expect
    {|
    File "a.stag", line 18, characters 0-44:
    18 | include .~(value "x" (Stag.Expr.var "nope"))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the declaration `let x` generated by this splice: Unbound value nope
    |}];
  (* Failing at compile time. *)
  check
    (generators
     ^ {|
include .~(Int.div 1 0; Nil)
|}
    );
  [%expect
    {|
    File "a.stag", line 18, characters 0-28:
    18 | include .~(Int.div 1 0; Nil)
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Division by zero
    File "a.stag", line 18, characters 11-22:
    18 | include .~(Int.div 1 0; Nil)
                    ^^^^^^^^^^^
    Note: The error was raised here
    |}];
  (* Not a declaration list. *)
  check
    {|
include .~1
|};
  [%expect
    {|
    File "a.stag", line 2, characters 10-11:
    2 | include .~1
                  ^
    Error: This expression has type int but an expression was expected of type list[Stag.Toplevel.t]
    |}]
;;
