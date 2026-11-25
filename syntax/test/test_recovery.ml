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
  parse {|let f x =
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

    type t = A

    let d = 3
    let e = 4
    |}]
;;

let%expect_test "unbalanced parentheses" =
  parse {|let a = (f (g 3)
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
  parse {|let a = 1
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
  parse {|let a = (1 ]
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
  parse {|let a = 1
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
    let a = <error>
    let b = "a?b"
    let c = 0
    let d = <error>
    let e = 4
    let f = 5
    |}]
;;

let%expect_test "a syntax error just after a lexer error is not reported" =
  parse {|let a = $
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
  parse {|module M = struct
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
  parse {|module M = struct
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
  parse {|module M = struct
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

let%expect_test "errors in inline expansions" =
  parse
    {|include%inline .~(gen 1)
  let x = (1
  let y = 2
end%inline
let z = 3
end%inline
module%inline M = .~e
  let w = ]
let v = 4
|};
  [%expect
    {|
    t.stag:3:2-5: expected `)` to close this `(` (opened at 2:10), found `let`
    t.stag:6:0-10: unmatched `end%inline`
    t.stag:8:10-11: expected an expression, found `]`
    t.stag:9:0-3: expected `end%inline` to close this `module%inline` (opened at 7:0), found `let`
    ---
    include%inline .~(gen 1)
      let x = <error>
      let y = 2
    end%inline

    let z = 3

    module%inline M = .~e
      let w = <error>
    end%inline

    let v = 4
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

    type u['a] = A
    type v

    module A = .~<error>
    module B = .~<error>

    include .~<error>
    include .~<error>

    exception E

    let ok = 1
    |}]
;;

let%expect_test "the errors and program of a file without errors are as \
                 without recovery"
  =
  List.iter (Test_printer.stag_files ()) ~f:(fun path ->
    let source = In_channel.read_all path in
    let recovered, errors = Parser.program_recovering ~file:path source in
    let strict = Parser.program ~file:path source |> Result.ok in
    if not
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
     (strict ("t.stag:1:0-3: unmatched `end`")))
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

let%expect_test "a source file" =
  let parse path contents =
    let toplevel, errors = Source_file.parse_recovering { path; contents } in
    List.iter errors ~f:(fun error ->
      print_s [%sexp (error : Source_file.Error.t)]);
    Option.iter toplevel ~f:(fun toplevel ->
      print_string (Printer.toplevel toplevel))
  in
  parse "dir/foo.stag" "let a = (1\nlet b = 2";
  [%expect
    {|
    (Parse
     (Syntax (loc dir/foo.stag:2:0-3)
      (message "expected `)` to close this `(` (opened at 1:8), found `let`")))
    module Foo = struct
      let a = <error>
      let b = 2
    end
    |}];
  parse "dir/1foo.stag" "let a = (1";
  [%expect
    {|
    (Invalid_file_name (path dir/1foo.stag)
     (message
      "1foo.stag is not a valid module file name: it should be a letter followed by letters, digits, _ and ', then .stag"))
    |}]
;;

let%expect_test "a broken type declaration keeps the members that parsed" =
  let kinds source =
    let program, errors = Parser.program_recovering ~file source in
    List.iter errors ~f:(fun error ->
      print_endline (Parser.Error.to_string error));
    List.iter program ~f:(fun (toplevel : Syntax.Toplevel.t) ->
      match toplevel.txt with
      | Type decl ->
        print_s
          [%sexp
            (decl.name.txt : string), (decl.kind : Syntax.Type_decl.Kind.t)]
      | _ -> ())
  in
  kinds
    {|type t = { a : int; b : }
type u = A | B of
type v = A of int * | C
type w = { a : int; b
type x = A | of
type y = { a : int; b }
type z['a] = { a : int; }
|};
  [%expect
    {|
    t.stag:1:24-25: expected a type, found `}`
    t.stag:3:0-4: expected a type, found `type`
    t.stag:3:20-21: expected a type, found `|`
    t.stag:5:0-4: expected `}` to close this `{` (opened at 4:9), found `type`
    t.stag:5:13-15: expected a constructor name, found `of`
    t.stag:6:22-23: expected `:`, found `}`
    (t
     (Malformed
      (Record
       (((a @ t.stag:1:11-12) ((Con (int @ t.stag:1:15-18) ()) @ t.stag:1:15-18))
        ((b @ t.stag:1:20-21) ((Tuple ()) @ t.stag:1:20-21))))))
    (u
     (Malformed (Variant (((A @ t.stag:2:9-10) ()) ((B @ t.stag:2:13-14) ())))))
    (v
     (Malformed
      (Variant
       (((A @ t.stag:3:9-10)
         (((Con (int @ t.stag:3:14-17) ()) @ t.stag:3:14-17)))))))
    (w
     (Malformed
      (Record
       (((a @ t.stag:4:11-12) ((Con (int @ t.stag:4:15-18) ()) @ t.stag:4:15-18))
        ((b @ t.stag:4:20-21) ((Tuple ()) @ t.stag:4:20-21))))))
    (x (Malformed (Variant (((A @ t.stag:5:9-10) ())))))
    (y
     (Malformed
      (Record
       (((a @ t.stag:6:11-12) ((Con (int @ t.stag:6:15-18) ()) @ t.stag:6:15-18))
        ((b @ t.stag:6:20-21) ((Tuple ()) @ t.stag:6:20-21))))))
    (z
     (Record
      (((a @ t.stag:7:15-16) ((Con (int @ t.stag:7:19-22) ()) @ t.stag:7:19-22)))))
    |}]
;;

let%expect_test "an unclosed bracket is reported at the next toplevel" =
  parse
    {|let a1 = (
let a2 = 1
let a3 = )
let b = f (.<x
type t = A
let c = .~ (
module M = struct
  let d = { x = 1;
  let e = .<
  module N = struct
    let f = (1,
  let g = 1
end
let h =
  let i = 1 in
  match i with
  | 1 -> 2
  | _ ->
let j = 4
|};
  [%expect
    {|
    t.stag:2:0-3: expected `)` to close this `(` (opened at 1:9), found `let`
    t.stag:3:9-10: expected an expression, found `)`
    t.stag:5:0-4: expected `>.` to close this `.<` (opened at 4:11), found `type`
    t.stag:7:0-6: expected `)` to close this `(` (opened at 6:11), found `module`
    t.stag:9:2-5: expected `}` to close this `{` (opened at 8:10), found `let`
    t.stag:10:2-8: expected `>.` to close this `.<` (opened at 9:10), found `module`
    t.stag:12:2-5: expected `)` to close this `(` (opened at 11:12), found `let`
    t.stag:19:0-3: expected an expression, found `let`
    ---
    let a1 = <error>
    let a2 = 1
    let a3 = <error>
    let b = <error>

    type t = A

    let c = <error>

    module M = struct
      let d = <error>
      let e = <error>

      module N = struct
        let f = <error>
      end

      let g = 1
    end

    let h = <error>
    let j = 4
    |}]
;;

let%expect_test "lexer errors that make a token don't hide what follows" =
  parse
    {|let a = "ok\q"
let b = Int.add a 1
let c = 99999999999999999999 )
let d = 1 (
let e = "\q
|};
  [%expect
    {|
    t.stag:1:11-13: illegal escape sequence \q
    t.stag:3:8-28: integer literal 99999999999999999999 exceeds the range of int
    t.stag:3:29-30: unmatched `)`
    t.stag:5:0-3: expected `)` to close this `(` (opened at 4:10), found `let`
    t.stag:5:8-9: unterminated string literal
    t.stag:5:9-11: illegal escape sequence \q
    ---
    let a = "ok?"
    let b = Int.add a 1
    let c = 0
    let d = <error>
    let e = "?\n"
    |}]
;;

let%expect_test "recovery takes linear time" =
  let steps n =
    let source =
      String.concat
        (List.init n ~f:(fun i ->
           sprintf "let y%d = (\nlet z%d = f (1, .<\n" i i))
    in
    let steps = ref 0 in
    let (_ : Syntax.Program.t * Parser.Error.t list) =
      Parser.program_recovering ~steps ~file source
    in
    !steps
  in
  let small = steps 1000 in
  let large = steps 2000 in
  printf "%.2f\n" (Float.of_int large /. Float.of_int small);
  [%expect {| 2.00 |}]
;;

let%expect_test "a broken exception declaration keeps the arguments that parsed"
  =
  let program, errors =
    Parser.program_recovering
      ~file
      {|exception A of (int *
exception B of
exception C of int * string *
exception D of int
|}
  in
  List.iter errors ~f:(fun error ->
    print_endline (Parser.Error.to_string error));
  List.iter program ~f:(fun (toplevel : Syntax.Toplevel.t) ->
    print_s [%sexp (toplevel.txt : Syntax.Toplevel.desc)]);
  [%expect
    {|
    t.stag:2:0-9: expected `)` to close this `(` (opened at 1:15), found `exception`
    t.stag:3:0-9: expected a type, found `exception`
    t.stag:4:0-9: expected a type, found `exception`
    (Malformed_exception (A @ t.stag:1:10-11) ())
    (Malformed_exception (B @ t.stag:2:10-11) ())
    (Malformed_exception (C @ t.stag:3:10-11)
     (((Con (int @ t.stag:3:15-18) ()) @ t.stag:3:15-18)
      ((Con (string @ t.stag:3:21-27) ()) @ t.stag:3:21-27)))
    (Exception (D @ t.stag:4:10-11)
     (((Con (int @ t.stag:4:15-18) ()) @ t.stag:4:15-18)))
    |}]
;;
