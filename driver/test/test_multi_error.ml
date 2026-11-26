open! Import
open File_check

let%expect_test "several type errors in one file" =
  check {|let a : int = "a"
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
    [ "b.stag", {|let y : string = 1
let z = Int.add A.x 1
|}
    ; "a.stag", {|let x : int = true
let w = Int.add B.y 1
|}
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
  check {|let f (x : nope) : int = Int.add x 1
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
  check {|exception E
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
    ; "b.stag", {|let x : string = A.c
|}
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
  check ~run:true {|let ran = print_endline "ran"
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

let%expect_test "compile-time code never runs what failed to check" =
  check {|let bad _ = Int.add 1 "x"
let x = .~ (let _ = bad () in .< 1 >.)
|};
  check
    {|let bad _ = String.length 5
let x = .~ (print_int (bad ()); .< 1 >.)
|};
  check
    {|let bad _ = (print_endline "side effect"; Int.add 1 "x")
include .~ (let _ = bad () in Nil)
let y = 1
|};
  check
    {|let bad x = match x with | Some y -> Int.add y "a" | None -> 0
let x = .~ (print_int (bad (Some 3)); .< 1 >.)
|};
  check
    {|let indirect _ = Int.add 1 "x"
let bad _ = indirect ()
let x = .~ (let _ = bad () in .< 1 >.)
|};
  check {|let f x = x.nonexistent_field
let x = .~ (let _ = f 3 in .< 1 >.)
|};
  [%expect
    {|
    File "a.stag", line 1, characters 22-25:
    1 | let bad _ = Int.add 1 "x"
                              ^^^
    Error: This expression has type string but an expression was expected of type int
    File "a.stag", line 1, characters 26-27:
    1 | let bad _ = String.length 5
                                  ^
    Error: This expression has type int but an expression was expected of type string
    File "a.stag", line 1, characters 52-55:
    1 | let bad _ = (print_endline "side effect"; Int.add 1 "x")
                                                            ^^^
    Error: This expression has type string but an expression was expected of type int
    File "a.stag", line 1, characters 47-50:
    1 | let bad x = match x with | Some y -> Int.add y "a" | None -> 0
                                                       ^^^
    Error: This expression has type string but an expression was expected of type int
    File "a.stag", line 1, characters 27-30:
    1 | let indirect _ = Int.add 1 "x"
                                   ^^^
    Error: This expression has type string but an expression was expected of type int
    File "a.stag", line 1, characters 12-29:
    1 | let f x = x.nonexistent_field
                    ^^^^^^^^^^^^^^^^^
    Error: Unbound record field nonexistent_field
    |}]
;;

let%expect_test "a pattern that failed to check matches nothing at compile time"
  =
  check
    {|type t = Foo of nope
let classify v = match v with | Foo _ -> "foo" | _ -> "other"
let x = .~ (print_endline (classify 3); .< 1 >.)
|};
  [%expect
    {|
    File "a.stag", line 1, characters 16-20:
    1 | type t = Foo of nope
                        ^^^^
    Error: Unbound type nope
    |}]
;;

let%expect_test "a mistake in the first of several patterns is reported once" =
  check {|let n = match 3 with | Some x -> x | None -> 0
|};
  [%expect
    {|
    File "a.stag", line 1, characters 23-29:
    1 | let n = match 3 with | Some x -> x | None -> 0
                               ^^^^^^
    Error: This expression has type option['_a] but an expression was expected of type int
    |}]
;;

let%expect_test "a record missing a field still has its type" =
  check
    {|type point = { x : int; y : int }
let p = { x = 1 }
let z = p.z
let w : string = p
|};
  [%expect
    {|
    File "a.stag", line 2, characters 8-17:
    2 | let p = { x = 1 }
                ^^^^^^^^^
    Error: Some record fields are undefined: A.y
    File "a.stag", line 3, characters 10-11:
    3 | let z = p.z
                  ^
    Error: The record type A.point has no field z
    File "a.stag", line 4, characters 17-18:
    4 | let w : string = p
                         ^
    Error: This expression has type A.point but an expression was expected of type string
    |}]
;;

let%expect_test "errors of code checked by an abandoned splice attempt are kept"
  =
  check
    {|let c = b
include .~ (let _ = d in let _ = c in Nil)
let b = zzz
let d = Int.add 1 "s"
|};
  check
    {|let c = b
include .~ (let _ = d in Nil)
let d = Int.add 1 "s"
let b = 1
|};
  check
    {|let a = .~ (let _ = d in let _ = c in .< 1 >.)
let c = 2
let d = Int.add 1 "s"
|};
  [%expect
    {|
    File "a.stag", line 3, characters 8-11:
    3 | let b = zzz
                ^^^
    Error: Unbound value zzz
    File "a.stag", line 2, characters 0-42:
    2 | include .~ (let _ = d in let _ = c in Nil)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include, still being generated because its expression uses this code, may define it: move this code before the include or into another module
    File "a.stag", line 3, characters 8-15:
    3 | let d = Int.add 1 "s"
                ^^^^^^^
    Error: Int.add can't be resolved here: the include at a.stag:2:0-29 comes before this point and may define it, but it is still being generated (its expression uses this code). Move this code before the include or into another module
    File "a.stag", line 2, characters 0-29:
    2 | include .~ (let _ = d in Nil)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
    File "a.stag", line 3, characters 18-21:
    3 | let d = Int.add 1 "s"
                          ^^^
    Error: This expression has type string but an expression was expected of type int
    |}]
;;

let%expect_test "uses of a broken exception declaration are not errors" =
  check
    {|exception E of (int *
let f _ = raise (E 1)
let g x = match x with | E n -> n | _ -> 0
let b : int = "b"
|};
  [%expect
    {|
    File "a.stag", line 2, characters 0-3:
    2 | let f _ = raise (E 1)
        ^^^
    Error: Expected `)` to close this `(` (opened at 1:15), found `let`
    File "a.stag", line 4, characters 14-17:
    4 | let b : int = "b"
                      ^^^
    Error: This expression has type string but an expression was expected of type int
    |}];
  check
    {|exception E of
let f _ = raise (E 1)
let g x = match x with | E n -> n | _ -> 0
let b : int = "b"
|};
  [%expect
    {|
    File "a.stag", line 2, characters 0-3:
    2 | let f _ = raise (E 1)
        ^^^
    Error: Expected a type, found `let`
    File "a.stag", line 4, characters 14-17:
    4 | let b : int = "b"
                      ^^^
    Error: This expression has type string but an expression was expected of type int
    |}]
;;

let%expect_test "compile-time code never raises a broken exception" =
  check
    {|exception E of int *
let x = .~ (print_endline "ran"; raise (E 1))
include .~ (print_endline "ran"; raise (E 1))
let b : int = "b"
|};
  [%expect
    {|
    File "a.stag", line 2, characters 0-3:
    2 | let x = .~ (print_endline "ran"; raise (E 1))
        ^^^
    Error: Expected a type, found `let`
    File "a.stag", line 4, characters 14-17:
    4 | let b : int = "b"
                      ^^^
    Error: This expression has type string but an expression was expected of type int
    |}]
;;

let%expect_test "a mode error in code abandoned and checked anew is reported" =
  (* Checking [b] runs the include, whose code uses [c], still being checked:
     the include is abandoned, then checked again once [c] is. *)
  check
    {|let keep (f : int -> int) = Ptr.make f
let c = b
include .~ (let g = stack_ (fun u -> u) in let _ = keep g in let _ = c in Nil)
let b = zzz
|};
  [%expect
    {|
    File "a.stag", line 3, characters 51-57:
    3 | include .~ (let g = stack_ (fun u -> u) in let _ = keep g in let _ = c in Nil)
                                                           ^^^^^^
    Error: This is local, because it is allocated by [stack_] (at a.stag:3:20-39), then it is part of the value (at a.stag:3:20-39), then g is used (at a.stag:3:56-57), then it is passed to a function (at a.stag:3:51-57), but must be at most global, because Ptr.make holds it, then it is passed to a function (at a.stag:1:28-38), then f is used (at a.stag:1:37-38), then f is passed to the function (at a.stag:1:28-38)
    File "a.stag", line 4, characters 8-11:
    4 | let b = zzz
                ^^^
    Error: Unbound value zzz
    File "a.stag", line 3, characters 0-78:
    3 | include .~ (let g = stack_ (fun u -> u) in let _ = keep g in let _ = c in Nil)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include, still being generated because its expression uses this code, may define it: move this code before the include or into another module
    |}]
;;
