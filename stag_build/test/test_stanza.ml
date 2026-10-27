open! Core
open Stag_build

let parse contents =
  match Stanza.parse ~path:"dir/stag" contents with
  | Ok stanza -> print_s [%sexp (stanza : Stanza.t option)]
  | Error diagnostics ->
    List.iter diagnostics ~f:(fun diagnostic ->
      print_string
        (Stag.Diagnostic.to_string_hum
           ~contents:(fun _ -> Some contents)
           diagnostic))
;;

let%expect_test "stanzas" =
  parse "(library (name foo) (libraries bar baz))";
  [%expect
    {|
    (((kind Library) (name (foo @ dir/stag:1:15-18))
      (libraries ((bar @ dir/stag:1:31-34) (baz @ dir/stag:1:35-38)))
      (loc dir/stag:1:0-40)))
    |}];
  parse
    {|; a comment
(executable
 (name main) #;(ignored)
 (libraries foo))|};
  [%expect
    {|
    (((kind Executable) (name (main @ dir/stag:3:7-11))
      (libraries ((foo @ dir/stag:4:12-15))) (loc dir/stag:2:0-4:17)))
    |}];
  parse "(test (name foo_test2))";
  [%expect
    {|
    (((kind Test) (name (foo_test2 @ dir/stag:1:12-21)) (libraries ())
      (loc dir/stag:1:0-23)))
    |}];
  parse "(library (libraries) (name foo))";
  [%expect
    {|
    (((kind Library) (name (foo @ dir/stag:1:27-30)) (libraries ())
      (loc dir/stag:1:0-32)))
    |}]
;;

let%expect_test "no stanza" =
  parse "";
  parse "; just a comment\n";
  [%expect
    {|
    ()
    ()
    |}]
;;

let%expect_test "syntax errors" =
  parse "(library (name foo)";
  [%expect
    {|
    File "dir/stag", line 1, characters 19-19:
    1 | (library (name foo)
                           ^
    Error: Unclosed parentheses at end of input
    |}];
  parse "(library (name \"foo)";
  [%expect
    {|
    File "dir/stag", line 1, characters 20-20:
    1 | (library (name "foo)
                            ^
    Error: Unterminated quoted string
    |}]
;;

let%expect_test "invalid stanzas" =
  parse "library";
  [%expect
    {|
    File "dir/stag", line 1, characters 0-7:
    1 | library
        ^^^^^^^
    Error: Expected a stanza: (library ...), (executable ...) or (test ...)
    |}];
  parse "()";
  [%expect
    {|
    File "dir/stag", line 1, characters 0-2:
    1 | ()
        ^^
    Error: Expected a stanza: (library ...), (executable ...) or (test ...)
    |}];
  parse "(rule (name foo))";
  [%expect
    {|
    File "dir/stag", line 1, characters 1-5:
    1 | (rule (name foo))
         ^^^^
    Error: Unknown stanza rule: expected library, executable or test
    |}];
  parse "(library (name a))\n(library (name b))\n(test (name c))";
  [%expect
    {|
    File "dir/stag", line 2, characters 0-18:
    2 | (library (name b))
        ^^^^^^^^^^^^^^^^^^
    Error: A stag file has at most one stanza; put the others in their own directories
    File "dir/stag", line 3, characters 0-15:
    3 | (test (name c))
        ^^^^^^^^^^^^^^^
    Error: A stag file has at most one stanza; put the others in their own directories
    |}]
;;

let%expect_test "invalid fields" =
  parse "(library (libraries foo))";
  [%expect
    {|
    File "dir/stag", line 1, characters 0-25:
    1 | (library (libraries foo))
        ^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: The library stanza has no name: add (name ...)
    |}];
  parse "(executable (name foo) (modes byte) name () ((x)))";
  [%expect
    {|
    File "dir/stag", line 1, characters 24-29:
    1 | (executable (name foo) (modes byte) name () ((x)))
                                ^^^^^
    Error: Unknown field modes: expected name or libraries
    File "dir/stag", line 1, characters 36-40:
    1 | (executable (name foo) (modes byte) name () ((x)))
                                            ^^^^
    Error: Expected a field: (name ...) or (libraries ...)
    File "dir/stag", line 1, characters 41-43:
    1 | (executable (name foo) (modes byte) name () ((x)))
                                                 ^^
    Error: Expected a field: (name ...) or (libraries ...)
    File "dir/stag", line 1, characters 44-49:
    1 | (executable (name foo) (modes byte) name () ((x)))
                                                    ^^^^^
    Error: Expected a field: (name ...) or (libraries ...)
    |}];
  parse "(library (name foo) (name bar) (libraries) (libraries x))";
  [%expect
    {|
    File "dir/stag", line 1, characters 21-25:
    1 | (library (name foo) (name bar) (libraries) (libraries x))
                             ^^^^
    Error: The field name is given more than once
    File "dir/stag", line 1, characters 44-53:
    1 | (library (name foo) (name bar) (libraries) (libraries x))
                                                    ^^^^^^^^^
    Error: The field libraries is given more than once
    |}];
  parse "(library (name))";
  parse "(library (name a b))";
  parse "(library (name (a)))";
  [%expect
    {|
    File "dir/stag", line 1, characters 9-15:
    1 | (library (name))
                 ^^^^^^
    Error: The field name expects one name, eg. (name foo)
    File "dir/stag", line 1, characters 9-19:
    1 | (library (name a b))
                 ^^^^^^^^^^
    Error: The field name expects one name, eg. (name foo)
    File "dir/stag", line 1, characters 15-18:
    1 | (library (name (a)))
                       ^^^
    Error: Expected a library name
    |}]
;;

let%expect_test "invalid names" =
  parse "(test (name Foo))";
  parse "(library (name foo-bar) (libraries 1x (y) ok ok))";
  [%expect
    {|
    File "dir/stag", line 1, characters 12-15:
    1 | (test (name Foo))
                    ^^^
    Error: Invalid test name Foo: it should be a lowercase letter followed by letters, digits and _
    File "dir/stag", line 1, characters 15-22:
    1 | (library (name foo-bar) (libraries 1x (y) ok ok))
                       ^^^^^^^
    Error: Invalid library name foo-bar: it should be a lowercase letter followed by letters, digits and _
    File "dir/stag", line 1, characters 35-37:
    1 | (library (name foo-bar) (libraries 1x (y) ok ok))
                                           ^^
    Error: Invalid library name 1x: it should be a lowercase letter followed by letters, digits and _
    File "dir/stag", line 1, characters 38-41:
    1 | (library (name foo-bar) (libraries 1x (y) ok ok))
                                              ^^^
    Error: Expected a library name
    File "dir/stag", line 1, characters 45-47:
    1 | (library (name foo-bar) (libraries 1x (y) ok ok))
                                                     ^^
    Error: The library ok is listed more than once
    |}]
;;
