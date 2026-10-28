open! Import

let file = "t.stag"

let parse source =
  let program, errors = Parser.program_recovering ~file source in
  List.iter errors ~f:(fun error ->
    print_endline (Parser.Error.to_string error));
  print_endline "---";
  print_string (Printer.program program)
;;

let%expect_test "a missing [in]" =
  parse
    {|let f x =
  let y = add x 1
  mul y 2

let g = f 1
|};
  [%expect
    {|
    t.stag:5:0-3: expected `in` for the `let` at 2:2, found `let`
    ---
    let f x = <error>
    let g = f 1
    |}]
;;

let%expect_test "several errors, in order" =
  parse
    {|let a = (1
let b = 2
let c = ]
type t = A of
let d = 3
exception
let e = 4
|};
  [%expect
    {|
    t.stag:2:0-3: expected `)` to close this `(` (opened at 1:8), found `let`
    t.stag:3:8-9: expected an expression, found `]`
    t.stag:5:0-3: expected a type, found `let`
    t.stag:7:0-3: expected a constructor name, found `let`
    ---
    let a = <error>
    let b = 2
    let c = <error>

    type t

    let d = 3
    let e = 4
    |}]
;;

let%expect_test "unbalanced parentheses" =
  parse
    {|let a = (f (g 3)
let b = 1 2))
let c = 3
|};
  [%expect
    {|
    t.stag:2:0-3: expected `)` to close this `(` (opened at 1:8), found `let`
    t.stag:2:11-12: unmatched `)`
    ---
    let a = <error>
    let b = 1 2
    let c = 3
    |}]
;;

let%expect_test "a stray [end]" =
  parse
    {|let a = 1
end
let b = 2
end end
|};
  [%expect
    {|
    t.stag:2:0-3: unmatched `end`
    t.stag:4:0-3: unmatched `end`
    ---
    let a = 1
    let b = 2
    |}]
;;

let%expect_test "an unterminated string at the end of the file" =
  parse
    {|let a = (1 ]
let b = "abc
|};
  [%expect
    {|
    t.stag:1:11-12: expected `)` to close this `(` (opened at 1:8), found `]`
    t.stag:2:8-9: unterminated string literal
    ---
    let a = <error>
    let b = "abc\n"
    |}]
;;

let%expect_test "an unterminated comment at the end of the file" =
  parse
    {|let a = 1
let b = (2 (* 3
let c = 4
|};
  [%expect
    {|
    t.stag:2:11-13: unterminated comment
    ---
    let a = 1
    let b = <error>
    |}]
;;

let%expect_test "bad characters and tokens mid-file" =
  parse
    {|let a = 1 $ 2
let b = "a\qb"
let c = 12ab
let d = 'x
let e = 4 in
let f = 5
|};
  [%expect
    {|
    t.stag:1:10-11: illegal character '$'
    t.stag:2:10-12: illegal escape sequence \q
    t.stag:3:8-12: invalid integer literal 12ab
    t.stag:4:8-10: expected an expression, found `'x`
    t.stag:5:10-12: unexpected `in`: a toplevel `let` is a declaration, without `in`
    ---
    let a = 1 2
    let b = "a?b"
    let c = 0
    let d = <error>
    let e = 4
    let f = 5
    |}]
;;

let%expect_test "a syntax error just after a lexer error is not reported" =
  parse
    {|let a = $
let b = (1 ]
let c = 3 $ in
let d = {|abc
|};
  [%expect
    {|
    t.stag:1:8-9: illegal character '$'
    t.stag:2:11-12: expected `)` to close this `(` (opened at 2:8), found `]`
    t.stag:3:10-11: illegal character '$'
    t.stag:4:8-10: unterminated string literal
    ---
    let a = <error>
    let b = <error>
    let c = 3
    let d = "abc\n"
    |}]
;;

let%expect_test "errors in nested modules" =
  parse
    {|module M = struct
  let a = (1
  let b = 2
  module N = struct
    let c = f (
  end
  let d = 4
end
let e = 5
|};
  [%expect
    {|
    t.stag:3:2-5: expected `)` to close this `(` (opened at 2:10), found `let`
    t.stag:6:2-5: expected an expression, found `end`
    ---
    module M = struct
      let a = <error>
      let b = 2

      module N = struct
        let c = <error>
      end

      let d = 4
    end

    let e = 5
    |}]
;;

let%expect_test "an [end] at the indentation of the [struct] closes it" =
  parse
    {|module M = struct
  let a = (f (g 2
end
let b = 2
|};
  [%expect
    {|
    t.stag:3:0-3: expected `)` to close this `(` (opened at 2:13), found `end`
    ---
    module M = struct
      let a = <error>
    end

    let b = 2
    |}]
;;

let%expect_test "a missing [end] of a [struct]" =
  parse
    {|module M = struct
  let a = 1
  let b = f (
|};
  [%expect
    {|
    t.stag:4:0-0: expected an expression, found end of file
    ---
    module M = struct
      let a = 1
      let b = <error>
    end
    |}]
;;

let%expect_test "a missing [end] of a [struct], with no other error" =
  parse
    {|module M = struct
  let a = 1
|};
  [%expect
    {|
    t.stag:3:0-0: expected `end` to close this `struct` (opened at 1:11), found end of file
    ---
    module M = struct
      let a = 1
    end
    |}]
;;

let%expect_test "what of a broken declaration parsed is kept" =
  parse
    {|let f (x : int) : int = (x
let g (x : ) = 1
let h x : = 1
let%expect_test "t" = (1
let%bench "b" n in [1; 2] = (n
let%bench "c" n in [1; 2 = n
type u ['a] = A of
type v ['a = A
module A = (
module B = .~(f
include .~(f
include x
exception E of
let = 1
1 2
let rec x = 1
let ok = 1
|};
  [%expect
    {|
    t.stag:2:0-3: expected `)` to close this `(` (opened at 1:24), found `let`
    t.stag:2:11-12: expected a type, found `)`
    t.stag:3:10-11: expected a type, found `=`
    t.stag:5:0-9: expected `)` to close this `(` (opened at 4:22), found `let%bench`
    t.stag:6:0-9: expected `)` to close this `(` (opened at 5:28), found `let%bench`
    t.stag:6:25-26: expected `]` to close this `[` (opened at 6:19), found `=`
    t.stag:8:0-4: expected a type, found `type`
    t.stag:8:11-12: expected `]` to close this `[` (opened at 8:7), found `=`
    t.stag:9:11-12: expected `struct`, found `(`
    t.stag:11:0-7: expected `)` to close this `(` (opened at 10:13), found `include`
    t.stag:12:0-7: expected `)` to close this `(` (opened at 11:10), found `include`
    t.stag:12:8-9: expected a declaration splice `.~e` after `include`, found `x`
    t.stag:14:0-3: expected a type, found `let`
    t.stag:14:4-5: expected a name to bind, found `=`
    t.stag:16:4-7: lets are always recursive; remove `rec`
    ---
    let f (x : int) : int = <error>
    let g = <error>
    let h x = <error>

    let%expect_test "t" = <error>

    let%bench "b" n in [1; 2] = <error>

    let%bench "c" = <error>

    type u['a]
    type v

    module A = .~<error>
    module B = .~<error>

    include .~<error>
    include .~<error>

    exception E

    let ok = 1
    |}]
;;

let%expect_test
    "the errors and program of a file without errors are as without recovery"
  =
  List.iter (Test_printer.stag_files ()) ~f:(fun path ->
    let source = In_channel.read_all path in
    let recovered, errors = Parser.program_recovering ~file:path source in
    let strict = Parser.program ~file:path source |> Result.ok in
    if
      not
        (List.is_empty errors
         && Option.equal Syntax.Program.equal (Some recovered) strict)
    then print_endline path);
  [%expect {| |}]
;;

let%expect_test "a single error, with and without recovery" =
  List.iter
    [ "let x = 1 in x"
    ; "let x = (1"
    ; "let x = 1 )"
    ; "end"
    ; "let x = \"a"
    ; "let x = 1 $"
    ; "module M = struct let x = 1"
    ]
    ~f:(fun source ->
      let _, errors = Parser.program_recovering ~file source in
      let strict =
        match Parser.program ~file source with
        | Ok _ -> []
        | Error error -> [ error ]
      in
      print_s
        [%message
          source
            ~recovering:
              (List.map errors ~f:Parser.Error.to_string : string list)
            ~strict:(List.map strict ~f:Parser.Error.to_string : string list)]);
  [%expect
    {|
    ("let x = 1 in x"
     (recovering
      ("t.stag:1:10-12: unexpected `in`: a toplevel `let` is a declaration, without `in`"))
     (strict
      ("t.stag:1:10-12: unexpected `in`: a toplevel `let` is a declaration, without `in`")))
    ("let x = (1"
     (recovering
      ("t.stag:1:10-10: expected `)` to close this `(` (opened at 1:8), found end of file"))
     (strict
      ("t.stag:1:10-10: expected `)` to close this `(` (opened at 1:8), found end of file")))
    ("let x = 1 )" (recovering ("t.stag:1:10-11: unmatched `)`"))
     (strict
      ("t.stag:1:10-11: expected a declaration (`let`, `type`, `exception`, `module` or `include`), found `)`")))
    (end (recovering ("t.stag:1:0-3: unmatched `end`"))
     (strict
      ("t.stag:1:0-3: expected a declaration (`let`, `type`, `exception`, `module` or `include`), found `end`")))
    ("let x = \"a" (recovering ("t.stag:1:8-9: unterminated string literal"))
     (strict ("t.stag:1:8-9: unterminated string literal")))
    ("let x = 1 $" (recovering ("t.stag:1:10-11: illegal character '$'"))
     (strict ("t.stag:1:10-11: illegal character '$'")))
    ("module M = struct let x = 1"
     (recovering
      ("t.stag:1:27-27: expected `end` to close this `struct` (opened at 1:11), found end of file"))
     (strict
      ("t.stag:1:27-27: expected `end` to close this `struct` (opened at 1:11), found end of file")))
    |}]
;;
