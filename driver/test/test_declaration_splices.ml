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
let arm name = (Stag.Pattern.construct name Nil, Stag.Expr.string name)

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
  (* [M.x] forces [M]'s splice, which comes later; [b] forces the include
     before it. *)
  check
    (generators
     ^ {|
let a = M.x
module M = .~(value "x" (Stag.Expr.int 1))
include .~(value "y" (Stag.Expr.string "y"))
let b = y
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
      module M : sig
        val x : int
      end
      val y : string
      val b : string
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
include .~(value "b" (Stag.Expr.int 1))
include .~(value "a" (Stag.Expr.int (Int.add b 1)))
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
      val b : int
      val a : int
      module N : sig
        val c : int
      end
      val shown : unit
    end
    --- output ---
    3
    |}]
;;

let%expect_test "a generator before its include, in the same module" =
  check
    ~run:true
    {|
let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
include .~(value "x" (Stag.Expr.int 1))
let shown = print_int x
|};
  [%expect
    {|
    module A : sig
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val x : int
      val shown : unit
    end
    --- output ---
    1
    |}]
;;

let%expect_test "a generator in a submodule" =
  check
    ~run:true
    {|
module Gen = struct
  let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
end
include .~(Gen.value "x" (Stag.Expr.int 1))
let shown = print_int x
|};
  [%expect
    {|
    module A : sig
      module Gen : sig
        val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      end
      val x : int
      val shown : unit
    end
    --- output ---
    1
    |}];
  (* After the include, the submodule sees what it generates. *)
  check
    {|
include .~(Gen.value "x" (Stag.Expr.int 1))
module Gen = struct
  let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
end
|};
  [%expect
    {|
    File "a.stag", line 4, characters 21-25:
    4 |   let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
                             ^^^^
    Error: Cons can't be resolved here: the include at a.stag:2:0-43 comes before this point and may define it, but it is still being generated (its expression uses this code). Move this code before the include or into another module
    File "a.stag", line 2, characters 0-43:
    2 | include .~(Gen.value "x" (Stag.Expr.int 1))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
    |}]
;;

let%expect_test "a generator after its include" =
  check
    {|
include .~(value "x" (Stag.Expr.int 1))
let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
|};
  [%expect
    {|
    File "a.stag", line 3, characters 19-23:
    3 | let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
                           ^^^^
    Error: Cons can't be resolved here: the include at a.stag:2:0-39 comes before this point and may define it, but it is still being generated (its expression uses this code). Move this code before the include or into another module
    File "a.stag", line 2, characters 0-39:
    2 | include .~(value "x" (Stag.Expr.int 1))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
    |}]
;;

let%expect_test "what an include generates is visible only after it" =
  (* [before] and [p_before] resolve outward, to the outer [x] and the prim;
     [after] and [p_after] to what the include generates, shadowing them. *)
  check
    ~run:true
    (generators
     ^ {|
let two a e b f =
  Cons (Stag.Toplevel.let_ a Nil e, Cons (Stag.Toplevel.let_ b Nil f, Nil))
let x = "outer"
module N = struct
  let before = x
  let p_before = print_string
  include .~(two "x" (Stag.Expr.int 1) "print_string" (Stag.Expr.int 2))
  let after = x
  let p_after = print_string
end
let outside = (N.x, N.print_string)
let shown = N.p_before N.before; print_int (Int.add N.after N.p_after)
|}
    );
  [%expect
    {|
    module A : sig
      val constructor : 'a -> 'a * list['b]
      val arm : string -> Stag.Pattern.t * Stag.Expr.t
      val enum : list[string] -> list[Stag.Toplevel.t]
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val two : string -> Stag.Expr.t -> string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val x : string
      module N : sig
        val before : string
        val p_before : string -> unit
        val x : int
        val print_string : int
        val after : int
        val p_after : int
      end
      val outside : int * int
      val shown : unit
    end
    --- output ---
    outer3
    |}];
  (* A generator before an include in the same module resolves outward too. *)
  check
    ~run:true
    (generators
     ^ {|
let x = 1
module N = struct
  include .~(value "y" (Stag.Expr.int x))
  include .~(value "x" (Stag.Expr.int 2))
  let z = x
end
let shown = print_int N.y; print_int N.z
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
        val z : int
      end
      val shown : unit
    end
    --- output ---
    12
    |}]
;;

let%expect_test "outward resolution does not depend on check order" =
  (* [before] is checked first, then [N]'s include run; or, [first] runs it
     first, by a path, then [before] is checked while it runs. *)
  let program ~first =
    generators
    ^ (if first then "let first = N.y\n" else "")
    ^ {|
let two a e b f =
  Cons (Stag.Toplevel.let_ a Nil e, Cons (Stag.Toplevel.let_ b Nil f, Nil))
let x = "outer"
module N = struct
  let before = x
  include .~(let _ = before in two "x" (Stag.Expr.int 1) "y" (Stag.Expr.var "before"))
end
|}
  in
  check (program ~first:false);
  [%expect
    {|
    module A : sig
      val constructor : 'a -> 'a * list['b]
      val arm : string -> Stag.Pattern.t * Stag.Expr.t
      val enum : list[string] -> list[Stag.Toplevel.t]
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val two : string -> Stag.Expr.t -> string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val x : string
      module N : sig
        val before : string
        val x : int
        val y : string
      end
    end
    |}];
  check (program ~first:true);
  [%expect
    {|
    module A : sig
      val constructor : 'a -> 'a * list['b]
      val arm : string -> Stag.Pattern.t * Stag.Expr.t
      val enum : list[string] -> list[Stag.Toplevel.t]
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val first : string
      val two : string -> Stag.Expr.t -> string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val x : string
      module N : sig
        val before : string
        val x : int
        val y : string
      end
    end
    |}]
;;

let%expect_test "a failed splice" =
  (* Reported as its own error wherever it is first reached. *)
  check
    (generators
     ^ {|
let a = M.x
module M = .~(Int.div 1 0; Nil)
let b = M.y
|}
    );
  [%expect
    {|
    File "a.stag", line 19, characters 0-31:
    19 | module M = .~(Int.div 1 0; Nil)
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Division by zero
    File "a.stag", line 19, characters 14-25:
    19 | module M = .~(Int.div 1 0; Nil)
                       ^^^^^^^^^^^
    Note: The error was raised here
    |}];
  (* A module of a prim's name shadows the prim even when its splice fails. *)
  check
    (generators
     ^ {|
let y = Int.add 1 2
module Int = .~(let u = Nope.x in Nil)
|}
    );
  [%expect
    {|
    File "a.stag", line 19, characters 24-28:
    19 | module Int = .~(let u = Nope.x in Nil)
                                 ^^^^
    Error: Unbound module Nope
    |}]
;;

let%expect_test "an unbound name in a generator" =
  check
    (generators
     ^ {|
include .~(let u = typo in value "x" (Stag.Expr.int 1))
|}
    );
  [%expect
    {|
    File "a.stag", line 18, characters 19-23:
    18 | include .~(let u = typo in value "x" (Stag.Expr.int 1))
                            ^^^^
    Error: Unbound value typo
    |}];
  check
    (generators
     ^ {|
module M = .~(let u = typo in value "x" (Stag.Expr.int 1))
|}
    );
  [%expect
    {|
    File "a.stag", line 18, characters 22-26:
    18 | module M = .~(let u = typo in value "x" (Stag.Expr.int 1))
                               ^^^^
    Error: Unbound value typo
    |}]
;;

let%expect_test "generated declaration splices" =
  (* [y]'s include and [Inner]'s splice are generated by the outer include, in
     its place, so see what is before it. *)
  check
    ~run:true
    (generators
     ^ {|
let call f args = Stag.Expr.apply (Stag.Expr.var f) args
let int n = call "Stag.Expr.int" (Cons (Stag.Expr.int n, Nil))
include .~(
  Cons (Stag.Toplevel.let_ "x" Nil (Stag.Expr.int 1),
  Cons (Stag.Toplevel.Include (call "value" (Cons (Stag.Expr.string "y", Cons (int 2, Nil)))),
  Cons (Stag.Toplevel.Module_splice ("Inner", call "value" (Cons (Stag.Expr.string "z", Cons (int 3, Nil)))),
  Nil))))
let shown = print_int x; print_int y; print_int Inner.z
|}
    );
  [%expect
    {|
    module A : sig
      val constructor : 'a -> 'a * list['b]
      val arm : string -> Stag.Pattern.t * Stag.Expr.t
      val enum : list[string] -> list[Stag.Toplevel.t]
      val value : string -> Stag.Expr.t -> list[Stag.Toplevel.t]
      val call : string -> list[Stag.Expr.t] -> Stag.Expr.t
      val int : int -> Stag.Expr.t
      val x : int
      val y : int
      module Inner : sig
        val z : int
      end
      val shown : unit
    end
    --- output ---
    123
    |}];
  (* A generated declaration using a spliced module after the include. *)
  check
    ~run:true
    (generators
     ^ {|
include .~(value "a" (Stag.Expr.var "M.x"))
module M = .~(value "x" (Stag.Expr.int 1))
let shown = print_int a
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
      module M : sig
        val x : int
      end
      val shown : unit
    end
    --- output ---
    1
    |}]
;;

let%expect_test "a cycle through a module splice and an include" =
  check
    (generators
     ^ {|
module P = struct
  include .~(let _ = Q.b in value "a" (Stag.Expr.int 1))
end
module Q = .~(let _ = P.a in value "b" (Stag.Expr.int 2))
|}
    );
  [%expect
    {|
    File "a.stag", line 21, characters 22-25:
    21 | module Q = .~(let _ = P.a in value "b" (Stag.Expr.int 2))
                               ^^^
    Error: Unbound value P.a
    File "a.stag", line 19, characters 2-56:
    19 |   include .~(let _ = Q.b in value "a" (Stag.Expr.int 1))
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include, still being generated because its expression uses this code, may define it: move this code before the include or into another module
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
  (* An include's expression does not see what it generates. *)
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
    Error: Unbound value x
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
    Error: A declaration splice depends on what it generates: E.t needs the declarations of the module A.E, which the splice at a.stag:18:0-46 is generating, as its expression uses this code
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
    File "a.stag", line 19, characters 10-13:
    19 |   let a = B.b
                   ^^^
    Error: A declaration splice depends on what it generates: B.b needs the declarations of the module A.B, which the splice at a.stag:21:0-44 is generating, as its expression uses this code
    File "a.stag", line 21, characters 0-44:
    21 | module B = .~(value "b" (Stag.Expr.int A.a))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
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
    Error: This expression has type A.list['_a] but an expression was expected of type prelude.list[string]
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
    Note: The generated declaration is (Let x () () (App (App (Var Int.add ()) (Lit (Int 1))) (Lit (String a))))
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
    Note: The generated declaration is (Let x () () (Var nope ()))
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

let%expect_test "a program shadowing option, in a type error" =
  check
    {|
type option['a] = None | Some of 'a
let v = Stag.Expr.Var ("x", Some 1)
|};
  [%expect
    {|
    File "a.stag", line 3, characters 28-34:
    3 | let v = Stag.Expr.Var ("x", Some 1)
                                    ^^^^^^
    Error: This expression has type A.option[int] but an expression was expected of type prelude.option[Stag.Type_expr.t]
    |}]
;;

let%expect_test "a file shadowing the prelude's Stag" =
  let files =
    [ { Source_file.path = "stag.stag"; contents = "let mine = 1\n" }
    ; { Source_file.path = "a.stag"
      ; contents =
          "include .~(Cons (Stag.Toplevel.let_ \"x\" Nil (Stag.Expr.int 1), \
           Nil))\n"
      }
    ]
  in
  match Driver.check_files files with
  | Ok _ -> print_endline "ok"
  | Error error ->
    print_string
      (Diagnostic.to_string_hum
         ~contents:(fun path ->
           List.find_map files ~f:(fun file ->
             Option.some_if (String.equal file.path path) file.contents))
         (Diagnostic.of_driver_error error));
    [%expect
      {|
    File "a.stag", line 1, characters 17-30:
    1 | include .~(Cons (Stag.Toplevel.let_ "x" Nil (Stag.Expr.int 1), Nil))
                         ^^^^^^^^^^^^^
    Error: Unbound module Stag.Toplevel (the prelude's module Stag is shadowed by the program's Stag)
    File "stag.stag", line 1, characters 0-0:
    1 | let mine = 1
        ^
    Note: The program's module Stag is here
    |}]
;;

(* [contents] as a.stag, with gen.stag's [Gen], printing A's signature. *)
let check_with_gen contents =
  let files =
    [ { Source_file.path = "gen.stag"
      ; contents =
          {|let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
let module_ name decls = Cons (Stag.Toplevel.Module (name, decls), Nil)
let call f args = Stag.Expr.apply (Stag.Expr.var f) args
let include_value name e =
  Stag.Toplevel.Include
    (call "Gen.value" (Cons (Stag.Expr.string name, Cons (e, Nil))))
let type_ name kind =
  Cons (Stag.Toplevel.Type { `name = name; `params = Nil; `kind = kind }, Nil)
|}
      }
    ; { Source_file.path = "a.stag"; contents }
    ]
  in
  match Driver.check_files files with
  | Ok driver ->
    List.iter (Driver.checked driver).signature ~f:(function
      | Module { name = "A"; _ } as item ->
        print_string (Signature.to_string_hum [ item ])
      | _ -> ())
  | Error error ->
    print_string
      (Diagnostic.to_string_hum
         ~contents:(fun path ->
           List.find_map files ~f:(fun file ->
             Option.some_if (String.equal file.path path) file.contents))
         (Diagnostic.of_driver_error error))
;;

(* [G], generated by an include, has an include of its own: [before] doesn't
   see the [x] it generates, [after] does. *)
let generated_g =
  {|include .~(Gen.module_ "G"
  (Cons (Stag.Toplevel.let_ "before" Nil (Stag.Expr.var "x"),
   Cons (Gen.include_value "x" (Gen.call "Stag.Expr.int" (Cons (Stag.Expr.int 5, Nil))),
   Cons (Stag.Toplevel.let_ "after" Nil (Stag.Expr.var "x"), Nil)))))
|}
;;

let%expect_test "before and after an include in a generated module" =
  check_with_gen
    ({|let x = "outer"
|}
     ^ generated_g
     ^ {|let r = (G.before, G.after, G.x)
|}
    );
  [%expect
    {|
    module A : sig
      val x : string
      module G : sig
        val before : string
        val x : int
        val after : int
      end
      val r : string * int * int
    end
    |}];
  (* Used before the include generating it. *)
  check_with_gen
    ({|let probe = G.after
let x = "outer"
|}
     ^ generated_g);
  [%expect
    {|
    File "a.stag", line 1, characters 12-13:
    1 | let probe = G.after
                    ^
    Error: Unbound module G
    File "a.stag", lines 3-6, characters 0-69:
    3 | include .~(Gen.module_ "G"
    4 |   (Cons (Stag.Toplevel.let_ "before" Nil (Stag.Expr.var "x"),
    5 |    Cons (Gen.include_value "x" (Gen.call "Stag.Expr.int" (Cons (Stag.Expr.int 5, Nil))),
    6 |    Cons (Stag.Toplevel.let_ "after" Nil (Stag.Expr.var "x"), Nil)))))
    Note: This include generates it, but it is visible only after the include
    |}];
  (* The same, once the include has run. *)
  check_with_gen
    ({|let a = b
let probe = G.after
let x = "outer"
|}
     ^ generated_g
     ^ {|let b = G.x
|}
    );
  [%expect
    {|
    File "a.stag", line 2, characters 12-13:
    2 | let probe = G.after
                    ^
    Error: Unbound module G
    File "a.stag", lines 4-7, characters 0-69:
    4 | include .~(Gen.module_ "G"
    5 |   (Cons (Stag.Toplevel.let_ "before" Nil (Stag.Expr.var "x"),
    6 |    Cons (Gen.include_value "x" (Gen.call "Stag.Expr.int" (Cons (Stag.Expr.int 5, Nil))),
    7 |    Cons (Stag.Toplevel.let_ "after" Nil (Stag.Expr.var "x"), Nil)))))
    Note: This include generates it, but it is visible only after the include
    |}];
  check_with_gen
    {|let probe = y
include .~(Gen.value "y" (Stag.Expr.int 1))
|};
  [%expect
    {|
    File "a.stag", line 1, characters 12-13:
    1 | let probe = y
                    ^
    Error: Unbound value y
    File "a.stag", line 2, characters 0-43:
    2 | include .~(Gen.value "y" (Stag.Expr.int 1))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include generates it, but it is visible only after the include
    |}];
  (* In an enclosing module. *)
  check_with_gen
    {|module M = struct
  let probe = y
end
include .~(Gen.value "y" (Stag.Expr.int 1))
|};
  [%expect
    {|
    File "a.stag", line 2, characters 14-15:
    2 |   let probe = y
                      ^
    Error: Unbound value y
    File "a.stag", line 4, characters 0-43:
    4 | include .~(Gen.value "y" (Stag.Expr.int 1))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include generates it, but it is visible only after the include
    |}]
;;

let%expect_test "includes in mutually dependent modules, in both orders" =
  let modules =
    {|module A = struct
  include .~(Gen.value "a" (Stag.Expr.int B.k))
  let k = 10
  let use_b = B.b
end
module B = struct
  include .~(Gen.value "b" (Stag.Expr.int A.k))
  let k = 20
  let use_a = A.a
end
|}
  in
  check_with_gen modules;
  [%expect
    {|
    module A : sig
      module A : sig
        val a : int
        val k : int
        val use_b : int
      end
      module B : sig
        val b : int
        val k : int
        val use_a : int
      end
    end
    |}];
  check_with_gen ("let probe = B.use_a\n" ^ modules);
  [%expect
    {|
    module A : sig
      val probe : int
      module A : sig
        val a : int
        val k : int
        val use_b : int
      end
      module B : sig
        val b : int
        val k : int
        val use_a : int
      end
    end
    |}]
;;

let%expect_test "a derive in a nested module, its generator written later" =
  check_with_gen
    {|type t = A | B
include .~(Gen.value "show" (Stag.Expr.string "t"))
include .~(Gen.value "eq" (Stag.Expr.var "show"))
module N = struct
  type u = C
  include .~(derive "nshow")
  let z = nshow
end
let derive name = Gen.value name (Stag.Expr.int 3)
let r = (show, eq, N.z)
|};
  [%expect
    {|
    module A : sig
      type t = A | B
      val show : string
      val eq : string
      module N : sig
        type u = C
        val nshow : int
        val z : int
      end
      val derive : string -> list[Stag.Toplevel.t]
      val r : string * string * int
    end
    |}];
  (* Modules and includes nested in one with includes. *)
  check_with_gen
    {|let first = N.x
let x = "outer"
module N = struct
  module Inner = struct let f = x let t = (Inner2.h, 1) end
  include .~(Gen.value "x" (Stag.Expr.int 1))
  module Inner2 = struct let h = x end
  include .~(Gen.module_ "Inner3" (Gen.value "q" (Stag.Expr.var "x")))
  include .~(Gen.value "w" (Stag.Expr.var "Inner3.q"))
end
|};
  [%expect
    {|
    module A : sig
      val first : int
      val x : string
      module N : sig
        module Inner : sig
          val f : string
          val t : int * int
        end
        val x : int
        module Inner2 : sig
          val h : int
        end
        module Inner3 : sig
          val q : int
        end
        val w : int
      end
    end
    |}];
  check_with_gen
    {|module N = struct
  include .~(Gen.module_ "Sub" (Cons (Gen.include_value "k" (Gen.call "Stag.Expr.var" (Cons (Stag.Expr.string "outer_v", Nil))), Nil)))
  let v = Sub.k
end
let outer_v = 1
|};
  [%expect
    {|
    module A : sig
      module N : sig
        module Sub : sig
          val k : int
        end
        val v : int
      end
      val outer_v : int
    end
    |}]
;;

(* [f], used by the include's expression, looks past the include. *)
let past_running_include f =
  check_with_gen
    (sprintf
       {|module X = struct
  include .~(Gen.value "a" (Stag.Expr.int (f 0)))
  let f %s
end
|}
       f)
;;

let%expect_test "names not found past a running include" =
  past_running_include "n = zzz";
  [%expect
    {|
    File "a.stag", line 3, characters 12-15:
    3 |   let f n = zzz
                    ^^^
    Error: Unbound value zzz
    File "a.stag", line 2, characters 2-49:
    2 |   include .~(Gen.value "a" (Stag.Expr.int (f 0)))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include, still being generated because its expression uses this code, may define it: move this code before the include or into another module
    |}];
  past_running_include "n = Q.zzz";
  [%expect
    {|
    File "a.stag", line 3, characters 12-13:
    3 |   let f n = Q.zzz
                    ^
    Error: Unbound module Q
    File "a.stag", line 2, characters 2-49:
    2 |   include .~(Gen.value "a" (Stag.Expr.int (f 0)))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include, still being generated because its expression uses this code, may define it: move this code before the include or into another module
    |}];
  past_running_include "n = Int.nope";
  [%expect
    {|
    File "a.stag", line 3, characters 12-20:
    3 |   let f n = Int.nope
                    ^^^^^^^^
    Error: Unbound value Int.nope
    File "a.stag", line 2, characters 2-49:
    2 |   include .~(Gen.value "a" (Stag.Expr.int (f 0)))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include, still being generated because its expression uses this code, may define it: move this code before the include or into another module
    |}];
  past_running_include "(n : nope) = 1";
  [%expect
    {|
    File "a.stag", line 3, characters 13-17:
    3 |   let f (n : nope) = 1
                     ^^^^
    Error: Unbound type nope
    File "a.stag", line 2, characters 2-49:
    2 |   include .~(Gen.value "a" (Stag.Expr.int (f 0)))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include, still being generated because its expression uses this code, may define it: move this code before the include or into another module
    |}];
  (* Found further out, as a prim. *)
  past_running_include "n = Int.add n 1";
  [%expect
    {|
    File "a.stag", line 3, characters 12-19:
    3 |   let f n = Int.add n 1
                    ^^^^^^^
    Error: Int.add can't be resolved here: the include at a.stag:2:2-49 comes before this point and may define it, but it is still being generated (its expression uses this code). Move this code before the include or into another module
    File "a.stag", line 2, characters 2-49:
    2 |   include .~(Gen.value "a" (Stag.Expr.int (f 0)))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
    |}];
  (* Even when it does define it. *)
  check_with_gen
    {|module X = struct
  include .~(Gen.value "zzz" (Stag.Expr.int (f 0)))
  let f n = zzz
end
|};
  [%expect
    {|
    File "a.stag", line 3, characters 12-15:
    3 |   let f n = zzz
                    ^^^
    Error: Unbound value zzz
    File "a.stag", line 2, characters 2-51:
    2 |   include .~(Gen.value "zzz" (Stag.Expr.int (f 0)))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include, still being generated because its expression uses this code, may define it: move this code before the include or into another module
    |}];
  (* A path into a module with a running include. *)
  check_with_gen
    {|module N = struct
  include .~(Gen.value "a" (Stag.Expr.int A.N.zzz))
end
|};
  [%expect
    {|
    File "a.stag", line 2, characters 42-49:
    2 |   include .~(Gen.value "a" (Stag.Expr.int A.N.zzz))
                                                  ^^^^^^^
    Error: Unbound value A.N.zzz
    File "a.stag", line 2, characters 2-51:
    2 |   include .~(Gen.value "a" (Stag.Expr.int A.N.zzz))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include, still being generated because its expression uses this code, may define it: move this code before the include or into another module
    |}]
;;

let%expect_test "an include using a toplevel being checked, in both orders" =
  let x =
    {|module X = struct
  include .~(Gen.value "a" (Stag.Expr.int (f 0)))
  let f n = Int.add n y
end
|}
  in
  check_with_gen ("let y = 1\n" ^ x);
  [%expect
    {|
    File "a.stag", line 4, characters 12-19:
    4 |   let f n = Int.add n y
                    ^^^^^^^
    Error: Int.add can't be resolved here: the include at a.stag:3:2-49 comes before this point and may define it, but it is still being generated (its expression uses this code). Move this code before the include or into another module
    File "a.stag", line 3, characters 2-49:
    3 |   include .~(Gen.value "a" (Stag.Expr.int (f 0)))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
    |}];
  check_with_gen ("let first = X.f\nlet y = 1\n" ^ x);
  [%expect
    {|
    File "a.stag", line 5, characters 12-19:
    5 |   let f n = Int.add n y
                    ^^^^^^^
    Error: Int.add can't be resolved here: the include at a.stag:4:2-49 comes before this point and may define it, but it is still being generated (its expression uses this code). Move this code before the include or into another module
    File "a.stag", line 4, characters 2-49:
    4 |   include .~(Gen.value "a" (Stag.Expr.int (f 0)))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
    |}];
  let x =
    {|module X = struct
  include .~(Gen.value "zzz" (Stag.Expr.int (f 0)))
  let f n = zzz
end
|}
  in
  check_with_gen x;
  [%expect
    {|
    File "a.stag", line 3, characters 12-15:
    3 |   let f n = zzz
                    ^^^
    Error: Unbound value zzz
    File "a.stag", line 2, characters 2-51:
    2 |   include .~(Gen.value "zzz" (Stag.Expr.int (f 0)))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include, still being generated because its expression uses this code, may define it: move this code before the include or into another module
    |}];
  check_with_gen ("let g = X.f\n" ^ x);
  [%expect
    {|
    File "a.stag", line 4, characters 12-15:
    4 |   let f n = zzz
                    ^^^
    Error: Unbound value zzz
    File "a.stag", line 3, characters 2-51:
    3 |   include .~(Gen.value "zzz" (Stag.Expr.int (f 0)))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include, still being generated because its expression uses this code, may define it: move this code before the include or into another module
    |}]
;;

let%expect_test "prelude names in messages, with generated shadowing types" =
  let m =
    {|module M = .~(Cons (Stag.Toplevel.Type {`name = "option"; `params = Nil; `kind = Stag.Type_decl.Kind.Abstract}, Gen.value "x" (Stag.Expr.int 1)))
|}
  in
  check_with_gen ("let bad : option[int] = 1\n" ^ m);
  [%expect
    {|
    File "a.stag", line 1, characters 24-25:
    1 | let bad : option[int] = 1
                                ^
    Error: This expression has type int but an expression was expected of type option[int]
    |}];
  check_with_gen ("let m = M.x\nlet bad : option[int] = 1\n" ^ m);
  [%expect
    {|
    File "a.stag", line 2, characters 24-25:
    2 | let bad : option[int] = 1
                                ^
    Error: This expression has type int but an expression was expected of type option[int]
    |}];
  (* In the module generated. *)
  check_with_gen
    {|module M = .~(Cons (Stag.Toplevel.Type {`name = "option"; `params = Nil; `kind = Stag.Type_decl.Kind.Abstract}, Gen.value "x" (Gen.call "Int.add" (Cons (Stag.Expr.construct "None" Nil, Nil)))))
|};
  [%expect
    {|
    File "a.stag", line 1, characters 0-193:
    1 | module M = .~(Cons (Stag.Toplevel.Type {`name = "option"; `params = Nil; `kind = Stag.Type_decl.Kind.Abstract}, Gen.value "x" (Gen.call "Int.add" (Cons (Stag.Expr.construct "None" Nil, Nil)))))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the declaration `let x` generated by this splice: This expression has type prelude.option['_a] but an expression was expected of type int
    Note: The generated declaration is (Let x () () (App (Var Int.add ()) (Construct None ())))
    |}];
  (* By an include: not counted, whether or not it was run. *)
  check_with_gen
    {|include .~(Gen.type_ "option" Stag.Type_decl.Kind.Abstract)
let bad : int = Some 1
|};
  [%expect
    {|
    File "a.stag", line 2, characters 16-22:
    2 | let bad : int = Some 1
                        ^^^^^^
    Error: This expression has type option[int] but an expression was expected of type int
    |}];
  check_with_gen
    {|let bad : int = Some 1
include .~(Gen.type_ "option" Stag.Type_decl.Kind.Abstract)
|};
  [%expect
    {|
    File "a.stag", line 1, characters 16-22:
    1 | let bad : int = Some 1
                        ^^^^^^
    Error: This expression has type option[int] but an expression was expected of type int
    |}]
;;

let%expect_test "a nested module shadowing a prelude type or module" =
  check_with_gen
    {|module N = struct
  type list = L
  let bad : int = Cons (1, Nil)
end
let also_bad : int = Cons (1, Nil)
|};
  [%expect
    {|
    File "a.stag", line 3, characters 18-31:
    3 |   let bad : int = Cons (1, Nil)
                          ^^^^^^^^^^^^^
    Error: This expression has type prelude.list[int] but an expression was expected of type int
    |}];
  check_with_gen
    {|let bad : int = Cons (1, Nil)
module N = struct
  type list = L
end
|};
  [%expect
    {|
    File "a.stag", line 1, characters 16-29:
    1 | let bad : int = Cons (1, Nil)
                        ^^^^^^^^^^^^^
    Error: This expression has type list[int] but an expression was expected of type int
    |}];
  check_with_gen
    {|module N = struct
  module Stag = struct end
  let bad : int = Gen.call "f" Nil
end
|};
  [%expect
    {|
    File "a.stag", line 3, characters 18-34:
    3 |   let bad : int = Gen.call "f" Nil
                          ^^^^^^^^^^^^^^^^
    Error: This expression has type prelude.Stag.Expr.t but an expression was expected of type int
    |}];
  check_with_gen
    {|module N = struct
  module Stag = struct end
  let bad : int = Some 1
end
|};
  [%expect
    {|
    File "a.stag", line 3, characters 18-24:
    3 |   let bad : int = Some 1
                          ^^^^^^
    Error: This expression has type option[int] but an expression was expected of type int
    |}]
;;
