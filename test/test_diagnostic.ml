open! Core
open Stag

let show ?contents loc message =
  print_string (Diagnostic.to_string_hum ?contents { loc; message })
;;

let loc (line, col) (line', col') =
  { Loc.file = "a.stag"
  ; start = { line; col; offset = 0 }
  ; stop = { line = line'; col = col'; offset = 0 }
  }
;;

let%expect_test "rendering" =
  let source = List.init 10 ~f:(sprintf "line %d") |> String.concat ~sep:"\n" in
  let contents = function
    | "a.stag" -> Some source
    | _ -> None
  in
  show ~contents (loc (3, 2) (3, 5)) "one line";
  show ~contents (loc (3, 6) (3, 6)) "zero width";
  show ~contents (loc (2, 1) (4, 3)) "a few lines";
  show ~contents (loc (1, 0) (10, 3)) "many lines";
  show (loc (3, 2) (3, 5)) "no contents";
  show ~contents Loc.none "no location";
  show ~contents (loc (30, 0) (30, 1)) "out of the file";
  [%expect
    {|
    File "a.stag", line 3, characters 2-5:
    3 | line 2
          ^^^
    Error: one line
    File "a.stag", line 3, characters 6-6:
    3 | line 2
              ^
    Error: zero width
    File "a.stag", lines 2-4, characters 1-3:
    2 | line 1
    3 | line 2
    4 | line 3
    Error: a few lines
    File "a.stag", lines 1-10, characters 0-3:
     1 | line 0
     2 | line 1
     3 | line 2
       | ...
     9 | line 8
    10 | line 9
    Error: many lines
    File "a.stag", line 3, characters 2-5:
    Error: no contents
    Error: no location
    File "a.stag", line 30, characters 0-1:
    Error: out of the file
    |}]
;;

let%expect_test "messages" =
  let check contents =
    match Driver.check_files [ { Source_file.path = "a.stag"; contents } ] with
    | Ok _ -> print_endline "ok"
    | Error error ->
      print_string
        (Diagnostic.to_string_hum
           ~contents:(fun _ -> Some contents)
           (Diagnostic.of_driver_error error))
  in
  List.iter
    ~f:check
    [ "let x = Int.add 1 true"
    ; "let f x = x x"
    ; "let r = {`a = 1}.`b"
    ; "let x = y"
    ; "let x = 1 let x = 2"
    ; "let x = M.y"
    ; "type t = A let x : u = A"
    ; "type t = A of int let x = A"
    ; "type t = {a : int; b : int} let x = {a = 1}"
    ; "type t = t"
    ; "let f p = match p with (x, x) -> 1"
    ; "let g = let y = 1 in .~(let z = y in .<1>.)"
    ; "let c = .<.~(.<1>.).{x = 1; x = 2}>."
    ; "let c = .~(Int.div 1 0; .<1>.)"
    ; "let x = [%expect]"
    ; "let x = Int.foo"
    ; "type t['a] = A let x : t = A"
    ; "module M = struct end module M = struct end"
    ];
  [%expect
    {|
    File "a.stag", line 1, characters 18-22:
    1 | let x = Int.add 1 true
                          ^^^^
    Error: This expression has type bool but an expression was expected of type int
    File "a.stag", line 1, characters 12-13:
    1 | let f x = x x
                    ^
    Error: The type variable '_a occurs inside its own definition
    File "a.stag", line 1, characters 8-16:
    1 | let r = {`a = 1}.`b
                ^^^^^^^^
    Error: The row {`a : int} is incompatible with the row {`b : '_a; ..'_b}
    The label b is missing.
    File "a.stag", line 1, characters 8-9:
    1 | let x = y
                ^
    Error: Unbound value y
    File "a.stag", line 1, characters 14-15:
    1 | let x = 1 let x = 2
                      ^
    Error: The value A.x is defined more than once in this module
    File "a.stag", line 1, characters 8-9:
    1 | let x = M.y
                ^
    Error: Unbound module M
    File "a.stag", line 1, characters 19-20:
    1 | type t = A let x : u = A
                           ^
    Error: Unbound type u
    File "a.stag", line 1, characters 26-27:
    1 | type t = A of int let x = A
                                  ^
    Error: The constructor A.A expects 1 argument(s), but is applied to 0
    File "a.stag", line 1, characters 36-43:
    1 | type t = {a : int; b : int} let x = {a = 1}
                                            ^^^^^^^
    Error: Some record fields are undefined: A.b
    File "a.stag", line 1, characters 5-6:
    1 | type t = t
             ^
    Error: The type abbreviation A.t is cyclic
    File "a.stag", line 1, characters 27-28:
    1 | let f p = match p with (x, x) -> 1
                                   ^
    Error: The variable x is bound several times in this matching
    File "a.stag", line 1, characters 32-33:
    1 | let g = let y = 1 in .~(let z = y in .<1>.)
                                        ^
    Error: The variable y is bound at runtime (level 0) but used at compile time (level -1); only toplevel functions may be used at other levels
    File "a.stag", line 1, characters 28-29:
    1 | let c = .<.~(.<1>.).{x = 1; x = 2}>.
                                    ^
    Error: The variable x is substituted more than once
    File "a.stag", line 1, characters 8-30:
    1 | let c = .~(Int.div 1 0; .<1>.)
                ^^^^^^^^^^^^^^^^^^^^^^
    Error: This toplevel splice failed at compile time: Division by zero, at a.stag:1:11-22
    File "a.stag", line 1, characters 8-17:
    1 | let x = [%expect]
                ^^^^^^^^^
    Error: [%expect] is only allowed in the body of a let%expect_test, outside quotes and toplevel splices
    File "a.stag", line 1, characters 8-15:
    1 | let x = Int.foo
                ^^^^^^^
    Error: Unbound value Int.foo
    File "a.stag", line 1, characters 23-24:
    1 | type t['a] = A let x : t = A
                               ^
    Error: The type t expects 1 argument(s), but is given 0
    File "a.stag", line 1, characters 29-30:
    1 | module M = struct end module M = struct end
                                     ^
    Error: The module A.M is defined more than once
    |}]
;;

let%expect_test "end to end: source to rendered errors" =
  let check contents =
    match Driver.check_files [ { Source_file.path = "a.stag"; contents } ] with
    | Ok _ -> print_endline "ok"
    | Error error ->
      print_string
        (Diagnostic.to_string_hum
           ~contents:(fun _ -> Some contents)
           (Diagnostic.of_driver_error error))
  in
  List.iter
    ~f:check
    [ (* lexing *)
      "let s = \"abc"
    ; "let c = '\\q'"
    ; "let n = 99999999999999999999"
    ; "let n = 1_000 let s = \"a\\\n    b\""
    ; (* parsing *)
      "let rec f x = f x"
    ; "let g = let rec f x = f x in f"
    ; "let x = f.{y}"
    ; "type t = C of int -> int"
    ; "type t = C of int * int list"
    ; "let f (x : int list) = x"
    ; "type t = list[int][int]"
    ; "let x @ const = 1"
    ; "type t[] = int"
    ; "type t = [`A | ]"
    ; "type t = with_x[{x : int}]"
    ; "type t = <int>{`x : int}"
    ; "let r = {x = let a = 1 in a; y}"
    ; "let x = (1, 2"
    ; "module M = struct\n  let x = 1\n"
    ; (* typechecking *)
      "module M = struct module N = struct let y = 1 end end let x = M.N.z"
    ; "module M = struct end let x = M.N.y"
    ; "module M = struct module N = struct end end let x = M.N.O.y"
    ; "let x : M.t = 1"
    ; "let x : list[int, int] = 1"
    ; "type t = A of int let x = A (1, 2)"
    ; "let f r = r.`x let y = f {`y = 1}"
    ; "let f x =\n  match x with\n  | `A -> 1\n  | `B -> true"
    ; "let p = {`x = 1} \\ `y"
    ; "let f r = match r with {`x = a; ..a} -> a"
    ; "let c = .<1>. let d = .~c.{y = 1}"
    ; "let%expect_test \"t\" = 1"
    ; "let x = .<[%expect]>."
    ];
  [%expect
    {|
    File "a.stag", line 1, characters 8-9:
    1 | let s = "abc
                ^
    Error: Unterminated string literal
    File "a.stag", line 1, characters 9-11:
    1 | let c = '\q'
                 ^^
    Error: Illegal escape sequence \q
    File "a.stag", line 1, characters 8-28:
    1 | let n = 99999999999999999999
                ^^^^^^^^^^^^^^^^^^^^
    Error: Integer literal 99999999999999999999 exceeds the range of int
    ok
    File "a.stag", line 1, characters 4-7:
    1 | let rec f x = f x
            ^^^
    Error: Lets are always recursive; remove `rec`
    File "a.stag", line 1, characters 12-15:
    1 | let g = let rec f x = f x in f
                    ^^^
    Error: Lets are always recursive; remove `rec`
    File "a.stag", line 1, characters 9-11:
    1 | let x = f.{y}
                 ^^
    Error: Substitutions `.{...}` are only written after a splice `.~e`
    File "a.stag", line 1, characters 18-20:
    1 | type t = C of int -> int
                          ^^
    Error: A constructor argument of function type must be parenthesized: `C of (a -> b)`
    File "a.stag", line 1, characters 24-28:
    1 | type t = C of int * int list
                                ^^^^
    Error: Type arguments are written after the name, in brackets: `list[int]`
    File "a.stag", line 1, characters 15-19:
    1 | let f (x : int list) = x
                       ^^^^
    Error: Type arguments are written after the name, in brackets: `list[int]`
    File "a.stag", line 1, characters 18-19:
    1 | type t = list[int][int]
                          ^
    Error: Type arguments are written once
    File "a.stag", line 1, characters 6-7:
    1 | let x @ const = 1
              ^
    Error: `@` is reserved (for modes)
    File "a.stag", line 1, characters 7-8:
    1 | type t[] = int
               ^
    Error: Expected a type parameter, found `]`
    File "a.stag", line 1, characters 15-16:
    1 | type t = [`A | ]
                       ^
    Error: Expected a polymorphic variant tag like `A, found `]`
    File "a.stag", line 1, characters 17-18:
    1 | type t = with_x[{x : int}]
                         ^
    Error: Polymorphic record labels are written with a backtick: `x
    File "a.stag", line 1, characters 15-17:
    1 | type t = <int>{`x : int}
                       ^^
    Error: The variables of a code context are written without a backtick: x
    File "a.stag", line 1, characters 30-31:
    1 | let r = {x = let a = 1 in a; y}
                                      ^
    Error: Expected `=`, found `}`
    File "a.stag", line 1, characters 13-13:
    1 | let x = (1, 2
                     ^
    Error: Expected `)` to close this `(` (opened at 1:8), found end of file
    File "a.stag", line 3, characters 0-0:
    3 |
        ^
    Error: Expected `end` to close this `struct` (opened at 1:11), found end of file
    File "a.stag", line 1, characters 62-67:
    1 | module M = struct module N = struct let y = 1 end end let x = M.N.z
                                                                      ^^^^^
    Error: Unbound value M.N.z
    File "a.stag", line 1, characters 30-33:
    1 | module M = struct end let x = M.N.y
                                      ^^^
    Error: Unbound module M.N
    File "a.stag", line 1, characters 52-57:
    1 | module M = struct module N = struct end end let x = M.N.O.y
                                                            ^^^^^
    Error: Unbound module M.N.O
    File "a.stag", line 1, characters 8-9:
    1 | let x : M.t = 1
                ^
    Error: Unbound module M
    File "a.stag", line 1, characters 8-12:
    1 | let x : list[int, int] = 1
                ^^^^
    Error: Unbound type list
    File "a.stag", line 1, characters 26-34:
    1 | type t = A of int let x = A (1, 2)
                                  ^^^^^^^^
    Error: The constructor A.A expects 1 argument(s), but is applied to 2
    File "a.stag", line 1, characters 25-33:
    1 | let f r = r.`x let y = f {`y = 1}
                                 ^^^^^^^^
    Error: The row {`y : int} is incompatible with the row {`x : '_a; ..'_b}
    The label x is missing.
    File "a.stag", line 4, characters 10-14:
    4 |   | `B -> true
                  ^^^^
    Error: This expression has type bool but an expression was expected of type int
    File "a.stag", line 1, characters 8-16:
    1 | let p = {`x = 1} \ `y
                ^^^^^^^^
    Error: The row {`x : int} is incompatible with the row {`y : '_a; ..'_b}
    The label y is missing.
    File "a.stag", line 1, characters 34-35:
    1 | let f r = match r with {`x = a; ..a} -> a
                                          ^
    Error: The variable a is bound several times in this matching
    ok
    File "a.stag", line 1, characters 22-23:
    1 | let%expect_test "t" = 1
                              ^
    Error: This expression has type int but an expression was expected of type unit
    File "a.stag", line 1, characters 10-19:
    1 | let x = .<[%expect]>.
                  ^^^^^^^^^
    Error: [%expect] is only allowed in the body of a let%expect_test, outside quotes and toplevel splices
    |}]
;;
