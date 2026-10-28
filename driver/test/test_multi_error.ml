open! Import
open File_check

let%expect_test "several type errors in one file" =
  check
    {|let a : int = "a"
let b = Int.add 1 true
let c = nope
let d = 1
|};
  [%expect
    {|
    File "a.stag", line 1, characters 14-17:
    1 | let a : int = "a"
                      ^^^
    Error: This expression has type string but an expression was expected of type int
    File "a.stag", line 2, characters 18-22:
    2 | let b = Int.add 1 true
                          ^^^^
    Error: This expression has type bool but an expression was expected of type int
    File "a.stag", line 3, characters 8-12:
    3 | let c = nope
                ^^^^
    Error: Unbound value nope
    |}]
;;

let%expect_test "type errors across files, sorted by location" =
  check_files
    [ ( "b.stag"
      , {|let y : string = 1
let z = Int.add A.x 1
|}
      )
    ; ( "a.stag"
      , {|let x : int = true
let w = Int.add B.y 1
|}
      )
    ];
  [%expect
    {|
    File "a.stag", line 1, characters 14-18:
    1 | let x : int = true
                      ^^^^
    Error: This expression has type bool but an expression was expected of type int
    File "a.stag", line 2, characters 16-19:
    2 | let w = Int.add B.y 1
                        ^^^
    Error: This expression has type string but an expression was expected of type int
    File "b.stag", line 1, characters 17-18:
    1 | let y : string = 1
                         ^
    Error: This expression has type int but an expression was expected of type string
    |}]
;;

let%expect_test "uses of what failed to check are not errors" =
  check
    {|let broken = nope 1
let f x = Int.add broken x
let g = broken.field
let h = match broken with Some x -> x | None -> 0
let i = broken.`x
let j : string = broken
let k = Int.add (broken : int) (broken : string)
|};
  [%expect
    {|
    File "a.stag", line 1, characters 13-17:
    1 | let broken = nope 1
                     ^^^^
    Error: Unbound value nope
    |}]
;;

let%expect_test "a failed annotation is ignored" =
  check
    {|let f (x : nope) : int = Int.add x 1
let y : string = f 1
|};
  [%expect
    {|
    File "a.stag", line 1, characters 11-15:
    1 | let f (x : nope) : int = Int.add x 1
                   ^^^^
    Error: Unbound type nope
    File "a.stag", line 2, characters 17-20:
    2 | let y : string = f 1
                         ^^^
    Error: This expression has type int but an expression was expected of type string
    |}]
;;

let%expect_test "fields of a type that failed" =
  check
    {|type t = {x : nope}
type s = {z : int}
let r = {x = 1}
let v = r.x
let w = r.y
let u = {z = 1}.y
|};
  [%expect
    {|
    File "a.stag", line 1, characters 14-18:
    1 | type t = {x : nope}
                      ^^^^
    Error: Unbound type nope
    File "a.stag", line 6, characters 16-17:
    6 | let u = {z = 1}.y
                        ^
    Error: The record type A.s has no field y
    |}]
;;

let%expect_test "duplicates are reported, keeping the first" =
  check
    {|type t = A | B | A
let x = 1
let x = "x"
let a : t = B
let b : int = x
|};
  [%expect
    {|
    File "a.stag", line 1, characters 17-18:
    1 | type t = A | B | A
                         ^
    Error: The constructor A.A is defined more than once in this module
    File "a.stag", line 3, characters 4-5:
    3 | let x = "x"
            ^
    Error: The value A.x is defined more than once in this module
    |}]
;;

let%expect_test "names a failed splice might define are not errors" =
  check
    {|include .~(nope 1)
let a = generated
let b = Also.generated
let c : int = "c"
|};
  [%expect
    {|
    File "a.stag", line 1, characters 11-15:
    1 | include .~(nope 1)
                   ^^^^
    Error: Unbound value nope
    File "a.stag", line 4, characters 14-17:
    4 | let c : int = "c"
                      ^^^
    Error: This expression has type string but an expression was expected of type int
    |}]
;;

let%expect_test "a splice raising at compile time" =
  check
    {|exception E
include .~(raise E)
let a = generated
let b : int = "b"
|};
  [%expect
    {|
    File "a.stag", line 2, characters 0-19:
    2 | include .~(raise E)
        ^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception A.E
    File "a.stag", line 2, characters 10-19:
    2 | include .~(raise E)
                  ^^^^^^^^^
    Note: The error was raised here
    File "a.stag", line 4, characters 14-17:
    4 | let b : int = "b"
                      ^^^
    Error: This expression has type string but an expression was expected of type int
    |}]
;;

let%expect_test "syntax and type errors together" =
  check_files
    [ ( "a.stag"
      , {|let a : int = "a"
let b = (Int.add 1
let c = Int.add 1 true
let d = 1 $ 2
let e = Int.add b d
|}
      )
    ; ( "b.stag"
      , {|let x : string = A.c
|}
      )
    ];
  [%expect
    {|
    File "a.stag", line 1, characters 14-17:
    1 | let a : int = "a"
                      ^^^
    Error: This expression has type string but an expression was expected of type int
    File "a.stag", line 3, characters 0-3:
    3 | let c = Int.add 1 true
        ^^^
    Error: Expected `)` to close this `(` (opened at 2:8), found `let`
    File "a.stag", line 3, characters 18-22:
    3 | let c = Int.add 1 true
                          ^^^^
    Error: This expression has type bool but an expression was expected of type int
    File "a.stag", line 4, characters 10-11:
    4 | let d = 1 $ 2
                  ^
    Error: Illegal character '$'
    File "b.stag", line 1, characters 17-20:
    1 | let x : string = A.c
                         ^^^
    Error: This expression has type int but an expression was expected of type string
    |}]
;;

let%expect_test "a program with errors is not run" =
  check
    ~run:true
    {|let ran = print_endline "ran"
let x : int = "x"
|};
  [%expect
    {|
    File "a.stag", line 2, characters 14-17:
    2 | let x : int = "x"
                      ^^^
    Error: This expression has type string but an expression was expected of type int
    |}]
;;

let%expect_test "past 50 errors, only how many more" =
  let files =
    [ { Source_file.path = "a.stag"
      ; contents =
          String.concat
            (List.init 53 ~f:(fun i -> sprintf "let x%d : int = \"%d\"\n" i i))
      }
    ]
  in
  match Driver.check_files files with
  | Ok _ -> print_endline "checked"
  | Error errors ->
    let rendered =
      Diagnostic.list_to_string_hum
        (List.map errors ~f:Diagnostic.of_driver_error)
    in
    let lines = String.split_lines rendered in
    printf "%d errors, %d lines\n" (List.length errors) (List.length lines);
    List.iter (List.drop lines (List.length lines - 4)) ~f:print_endline;
    [%expect
      {|
      53 errors, 101 lines
      Error: This expression has type string but an expression was expected of type int
      File "a.stag", line 50, characters 16-20:
      Error: This expression has type string but an expression was expected of type int
      ... and 3 more errors
      |}]
;;
