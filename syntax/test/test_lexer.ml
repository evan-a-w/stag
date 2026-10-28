open! Import

(* Each token with its location and the source text it spans, so offsets are
   checked too. *)
let lex src =
  match Lexer.tokenize ~file:"t.stag" src with
  | Error error ->
    print_s [%sexp (error : Lexer.Error.t)];
    print_endline (Lexer.Error.to_string error)
  | Ok tokens ->
    Array.iter tokens ~f:(fun { token; loc } ->
      let text =
        String.sub
          src
          ~pos:loc.start.offset
          ~len:(loc.stop.offset - loc.start.offset)
      in
      print_s [%sexp (token : Token.t), (loc : Loc.t), (text : string)])
;;

let%expect_test "empty" =
  lex "";
  [%expect {| (Eof t.stag:1:0-0 "") |}];
  lex "  \n\t ";
  [%expect {| (Eof t.stag:2:2-2 "") |}]
;;

let%expect_test "keywords and identifiers" =
  lex "let in match with type of module struct end true false";
  [%expect
    {|
    (Let t.stag:1:0-3 let)
    (In t.stag:1:4-6 in)
    (Match t.stag:1:7-12 match)
    (With t.stag:1:13-17 with)
    (Type t.stag:1:18-22 type)
    (Of t.stag:1:23-25 of)
    (Module t.stag:1:26-32 module)
    (Struct t.stag:1:33-39 struct)
    (End t.stag:1:40-43 end)
    (True t.stag:1:44-48 true)
    (False t.stag:1:49-54 false)
    (Eof t.stag:1:54-54 "")
    |}];
  lex "x _x x' x1_Y Foo F_o'o1 _ letx Let lets in_";
  [%expect
    {|
    ((Lident x) t.stag:1:0-1 x)
    ((Lident _x) t.stag:1:2-4 _x)
    ((Lident x') t.stag:1:5-7 x')
    ((Lident x1_Y) t.stag:1:8-12 x1_Y)
    ((Uident Foo) t.stag:1:13-16 Foo)
    ((Uident F_o'o1) t.stag:1:17-23 F_o'o1)
    (Underscore t.stag:1:24-25 _)
    ((Lident letx) t.stag:1:26-30 letx)
    ((Uident Let) t.stag:1:31-34 Let)
    ((Lident lets) t.stag:1:35-39 lets)
    ((Lident in_) t.stag:1:40-43 in_)
    (Eof t.stag:1:43-43 "")
    |}]
;;

let%expect_test "let%bench" =
  lex "let%bench let%expect_test";
  [%expect
    {|
    (Let_bench t.stag:1:0-9 let%bench)
    (Let_expect_test t.stag:1:10-25 let%expect_test)
    (Eof t.stag:1:25-25 "")
    |}];
  lex "let%benchx";
  [%expect
    {|
    ((kind (Illegal_character %)) (loc t.stag:1:3-4))
    t.stag:1:3-4: illegal character '%'
    |}]
;;

let%expect_test "punctuation" =
  lex "( ) [ ] { } , ; : = | -> * . .. _ \\ @ @@ .< >. .~";
  [%expect
    {|
    (Lparen t.stag:1:0-1 "(")
    (Rparen t.stag:1:2-3 ")")
    (Lbracket t.stag:1:4-5 [)
    (Rbracket t.stag:1:6-7 ])
    (Lbrace t.stag:1:8-9 {)
    (Rbrace t.stag:1:10-11 })
    (Comma t.stag:1:12-13 ,)
    (Semi t.stag:1:14-15 ";")
    (Colon t.stag:1:16-17 :)
    (Equal t.stag:1:18-19 =)
    (Bar t.stag:1:20-21 |)
    (Arrow t.stag:1:22-24 ->)
    (Star t.stag:1:25-26 *)
    (Dot t.stag:1:27-28 .)
    (Dotdot t.stag:1:29-31 ..)
    (Underscore t.stag:1:32-33 _)
    (Backslash t.stag:1:34-35 "\\")
    (At t.stag:1:36-37 @)
    (At_at t.stag:1:38-40 @@)
    (Quote_open t.stag:1:41-43 .<)
    (Quote_close t.stag:1:44-46 >.)
    (Splice t.stag:1:47-49 .~)
    (Eof t.stag:1:49-49 "")
    |}]
;;

let%expect_test "adjacent punctuation" =
  lex "(){}[]->->...<>..~.@@@";
  [%expect
    {|
    (Lparen t.stag:1:0-1 "(")
    (Rparen t.stag:1:1-2 ")")
    (Lbrace t.stag:1:2-3 {)
    (Rbrace t.stag:1:3-4 })
    (Lbracket t.stag:1:4-5 [)
    (Rbracket t.stag:1:5-6 ])
    (Arrow t.stag:1:6-8 ->)
    (Arrow t.stag:1:8-10 ->)
    (Dotdot t.stag:1:10-12 ..)
    (Quote_open t.stag:1:12-14 .<)
    (Quote_close t.stag:1:14-16 >.)
    (Splice t.stag:1:16-18 .~)
    (Dot t.stag:1:18-19 .)
    (At_at t.stag:1:19-21 @@)
    (At t.stag:1:21-22 @)
    (Eof t.stag:1:22-22 "")
    |}];
  lex "r.x M.N.x r.`x r.M.x";
  [%expect
    {|
    ((Lident r) t.stag:1:0-1 r)
    (Dot t.stag:1:1-2 .)
    ((Lident x) t.stag:1:2-3 x)
    ((Uident M) t.stag:1:4-5 M)
    (Dot t.stag:1:5-6 .)
    ((Uident N) t.stag:1:6-7 N)
    (Dot t.stag:1:7-8 .)
    ((Lident x) t.stag:1:8-9 x)
    ((Lident r) t.stag:1:10-11 r)
    (Dot t.stag:1:11-12 .)
    ((Label x) t.stag:1:12-14 `x)
    ((Lident r) t.stag:1:15-16 r)
    (Dot t.stag:1:16-17 .)
    ((Uident M) t.stag:1:17-18 M)
    (Dot t.stag:1:18-19 .)
    ((Lident x) t.stag:1:19-20 x)
    (Eof t.stag:1:20-20 "")
    |}];
  lex ".<.~f x>. .~e.{}";
  [%expect
    {|
    (Quote_open t.stag:1:0-2 .<)
    (Splice t.stag:1:2-4 .~)
    ((Lident f) t.stag:1:4-5 f)
    ((Lident x) t.stag:1:6-7 x)
    (Quote_close t.stag:1:7-9 >.)
    (Splice t.stag:1:10-12 .~)
    ((Lident e) t.stag:1:12-13 e)
    (Dot_lbrace t.stag:1:13-15 .{)
    (Rbrace t.stag:1:15-16 })
    (Eof t.stag:1:16-16 "")
    |}];
  lex "{..'r} {..} [] ()";
  [%expect
    {|
    (Lbrace t.stag:1:0-1 {)
    (Dotdot t.stag:1:1-3 ..)
    ((Tyvar r) t.stag:1:3-5 'r)
    (Rbrace t.stag:1:5-6 })
    (Lbrace t.stag:1:7-8 {)
    (Dotdot t.stag:1:8-10 ..)
    (Rbrace t.stag:1:10-11 })
    (Lbracket t.stag:1:12-13 [)
    (Rbracket t.stag:1:13-14 ])
    (Lparen t.stag:1:15-16 "(")
    (Rparen t.stag:1:16-17 ")")
    (Eof t.stag:1:17-17 "")
    |}]
;;

let%expect_test "labels" =
  lex "`A `x `foo_Bar1 `_x";
  [%expect
    {|
    ((Label A) t.stag:1:0-2 `A)
    ((Label x) t.stag:1:3-5 `x)
    ((Label foo_Bar1) t.stag:1:6-15 `foo_Bar1)
    ((Label _x) t.stag:1:16-19 `_x)
    (Eof t.stag:1:19-19 "")
    |}]
;;

let%expect_test "ints" =
  lex "1_000 1__0 1_ 4_611_686_018_427_387_903 _1";
  lex "1_000a";
  lex "4_611_686_018_427_387_904";
  lex "0 42 4611686018427387903 007";
  lex "4611686018427387904";
  [%expect
    {|
    ((Int 1000) t.stag:1:0-5 1_000)
    ((Int 10) t.stag:1:6-10 1__0)
    ((Int 1) t.stag:1:11-13 1_)
    ((Int 4611686018427387903) t.stag:1:14-39 4_611_686_018_427_387_903)
    ((Lident _1) t.stag:1:40-42 _1)
    (Eof t.stag:1:42-42 "")
    ((kind (Invalid_int_literal 1_000a)) (loc t.stag:1:0-6))
    t.stag:1:0-6: invalid integer literal 1_000a
    ((kind (Int_literal_out_of_range 4_611_686_018_427_387_904))
     (loc t.stag:1:0-25))
    t.stag:1:0-25: integer literal 4_611_686_018_427_387_904 exceeds the range of int
    ((Int 0) t.stag:1:0-1 0)
    ((Int 42) t.stag:1:2-4 42)
    ((Int 4611686018427387903) t.stag:1:5-24 4611686018427387903)
    ((Int 7) t.stag:1:25-28 007)
    (Eof t.stag:1:28-28 "")
    ((kind (Int_literal_out_of_range 4611686018427387904)) (loc t.stag:1:0-19))
    t.stag:1:0-19: integer literal 4611686018427387904 exceeds the range of int
    |}]
;;

let%expect_test "negative ints" =
  lex "-1 -1_000 -0 -007 -4611686018427387904 -4_611_686_018_427_387_903";
  lex "f -1 (-2) x-3 ->-4";
  lex "-4611686018427387905";
  lex "-4_611_686_018_427_387_905";
  lex "-12abc";
  lex "-";
  lex "- 1";
  lex "--1";
  lex "-x";
  lex "-_1";
  [%expect
    {|
    ((Int -1) t.stag:1:0-2 -1)
    ((Int -1000) t.stag:1:3-9 -1_000)
    ((Int 0) t.stag:1:10-12 -0)
    ((Int -7) t.stag:1:13-17 -007)
    ((Int -4611686018427387904) t.stag:1:18-38 -4611686018427387904)
    ((Int -4611686018427387903) t.stag:1:39-65 -4_611_686_018_427_387_903)
    (Eof t.stag:1:65-65 "")
    ((Lident f) t.stag:1:0-1 f)
    ((Int -1) t.stag:1:2-4 -1)
    (Lparen t.stag:1:5-6 "(")
    ((Int -2) t.stag:1:6-8 -2)
    (Rparen t.stag:1:8-9 ")")
    ((Lident x) t.stag:1:10-11 x)
    ((Int -3) t.stag:1:11-13 -3)
    (Arrow t.stag:1:14-16 ->)
    ((Int -4) t.stag:1:16-18 -4)
    (Eof t.stag:1:18-18 "")
    ((kind (Int_literal_out_of_range -4611686018427387905)) (loc t.stag:1:0-20))
    t.stag:1:0-20: integer literal -4611686018427387905 exceeds the range of int
    ((kind (Int_literal_out_of_range -4_611_686_018_427_387_905))
     (loc t.stag:1:0-26))
    t.stag:1:0-26: integer literal -4_611_686_018_427_387_905 exceeds the range of int
    ((kind (Invalid_int_literal -12abc)) (loc t.stag:1:0-6))
    t.stag:1:0-6: invalid integer literal -12abc
    ((kind (Illegal_character -)) (loc t.stag:1:0-1))
    t.stag:1:0-1: illegal character '-'
    ((kind (Illegal_character -)) (loc t.stag:1:0-1))
    t.stag:1:0-1: illegal character '-'
    ((kind (Illegal_character -)) (loc t.stag:1:0-1))
    t.stag:1:0-1: illegal character '-'
    ((kind (Illegal_character -)) (loc t.stag:1:0-1))
    t.stag:1:0-1: illegal character '-'
    ((kind (Illegal_character -)) (loc t.stag:1:0-1))
    t.stag:1:0-1: illegal character '-'
    |}]
;;

let%expect_test "chars and type variables" =
  lex
    "'a' 'Z' ' ' '\"' '_' '0' '\\'' '\\\\' '\\n' '\\t' '\\r' '\\b' '\\ ' \
     '\\065' '\\x41' '\\255'";
  [%expect
    {|
    ((Char a) t.stag:1:0-3 'a')
    ((Char Z) t.stag:1:4-7 'Z')
    ((Char " ") t.stag:1:8-11 "' '")
    ((Char "\"") t.stag:1:12-15 "'\"'")
    ((Char _) t.stag:1:16-19 '_')
    ((Char 0) t.stag:1:20-23 '0')
    ((Char ') t.stag:1:24-28 "'\\''")
    ((Char "\\") t.stag:1:29-33 "'\\\\'")
    ((Char "\n") t.stag:1:34-38 "'\\n'")
    ((Char "\t") t.stag:1:39-43 "'\\t'")
    ((Char "\r") t.stag:1:44-48 "'\\r'")
    ((Char "\b") t.stag:1:49-53 "'\\b'")
    ((Char " ") t.stag:1:54-58 "'\\ '")
    ((Char A) t.stag:1:59-65 "'\\065'")
    ((Char A) t.stag:1:66-72 "'\\x41'")
    ((Char "\255") t.stag:1:73-79 "'\\255'")
    (Eof t.stag:1:79-79 "")
    |}];
  lex "'a 'b1 'abc 'a' 'A 'a_b' 'r}";
  [%expect
    {|
    ((Tyvar a) t.stag:1:0-2 'a)
    ((Tyvar b1) t.stag:1:3-6 'b1)
    ((Tyvar abc) t.stag:1:7-11 'abc)
    ((Char a) t.stag:1:12-15 'a')
    ((Tyvar A) t.stag:1:16-18 'A)
    ((Tyvar a_b') t.stag:1:19-24 'a_b')
    ((Tyvar r) t.stag:1:25-27 'r)
    (Rbrace t.stag:1:27-28 })
    (Eof t.stag:1:28-28 "")
    |}];
  lex "'_a '_weak1 '__a";
  [%expect
    {|
    ((Weak_tyvar a) t.stag:1:0-3 '_a)
    ((Weak_tyvar weak1) t.stag:1:4-11 '_weak1)
    ((Weak_tyvar _a) t.stag:1:12-16 '__a)
    (Eof t.stag:1:16-16 "")
    |}];
  lex "'a -> 'b list['a]";
  [%expect
    {|
    ((Tyvar a) t.stag:1:0-2 'a)
    (Arrow t.stag:1:3-5 ->)
    ((Tyvar b) t.stag:1:6-8 'b)
    ((Lident list) t.stag:1:9-13 list)
    (Lbracket t.stag:1:13-14 [)
    ((Tyvar a) t.stag:1:14-16 'a)
    (Rbracket t.stag:1:16-17 ])
    (Eof t.stag:1:17-17 "")
    |}]
;;

let%expect_test "strings" =
  lex
    {|"" "abc" "a\nb\tc\\d\"e\'f" "\065\x41\x7a\xFF\000" "\ " "(* not a comment *)"|};
  [%expect
    {|
    ((String "") t.stag:1:0-2 "\"\"")
    ((String abc) t.stag:1:3-8 "\"abc\"")
    ((String  "a\
             \nb\tc\\d\"e'f") t.stag:1:9-27 "\"a\\nb\\tc\\\\d\\\"e\\'f\"")
    ((String "AAz\255\000") t.stag:1:28-50 "\"\\065\\x41\\x7a\\xFF\\000\"")
    ((String " ") t.stag:1:51-55 "\"\\ \"")
    ((String "(* not a comment *)") t.stag:1:56-77 "\"(* not a comment *)\"")
    (Eof t.stag:1:77-77 "")
    |}];
  lex "\"multi\nline\" x";
  [%expect
    {|
    ((String  "multi\
             \nline") t.stag:1:0-2:5  "\"multi\
                                     \nline\"")
    ((Lident x) t.stag:2:6-7 x)
    (Eof t.stag:2:7-7 "")
    |}]
;;

let%expect_test "line continuations in strings" =
  lex "\"a\\\n   \t b\" \"\\\n\" \"a\\\n\n b\"";
  [%expect
    {|
    ((String ab) t.stag:1:0-2:7  "\"a\\\
                                \n   \t b\"")
    ((String "") t.stag:2:8-3:1  "\"\\\
                                \n\"")
    ((String  "a\
             \n b") t.stag:3:2-5:3  "\"a\\\
                                   \n\
                                   \n b\"")
    (Eof t.stag:5:3-3 "")
    |}];
  lex "\"a\\\r\n  b\"";
  [%expect
    {|
    ((String ab) t.stag:1:0-2:4  "\"a\\\r\
                                \n  b\"")
    (Eof t.stag:2:4-4 "")
    |}];
  lex "\"a\\\r b\"";
  [%expect
    {|
    ((kind (Illegal_escape "\\\r")) (loc t.stag:1:2-4))
    t.stag:1:2-4: illegal escape sequence \
    |}];
  lex "'\\\n'";
  [%expect
    {|
    ((kind (Illegal_escape "\\")) (loc t.stag:1:1-2))
    t.stag:1:1-2: illegal escape sequence \
    |}];
  lex "(* \"\\\n\" *) x";
  [%expect
    {|
    ((Lident x) t.stag:2:5-6 x)
    (Eof t.stag:2:6-6 "")
    |}]
;;

let%expect_test "comments" =
  lex "a (* comment *) b";
  [%expect
    {|
    ((Lident a) t.stag:1:0-1 a)
    ((Lident b) t.stag:1:16-17 b)
    (Eof t.stag:1:17-17 "")
    |}];
  lex "a (* (* nested *) still *) b";
  [%expect
    {|
    ((Lident a) t.stag:1:0-1 a)
    ((Lident b) t.stag:1:27-28 b)
    (Eof t.stag:1:28-28 "")
    |}];
  lex "a (* \"*)\" '\"' '\\\"' ')' *) b";
  [%expect
    {|
    ((Lident a) t.stag:1:0-1 a)
    ((Lident b) t.stag:1:26-27 b)
    (Eof t.stag:1:27-27 "")
    |}];
  lex "(* it's *) x (* '\\'' *) y";
  [%expect
    {|
    ((Lident x) t.stag:1:11-12 x)
    ((Lident y) t.stag:1:24-25 y)
    (Eof t.stag:1:25-25 "")
    |}];
  lex "(**)(***)x(* *)";
  [%expect
    {|
    ((Lident x) t.stag:1:9-10 x)
    (Eof t.stag:1:15-15 "")
    |}];
  lex "a (* multi\nline\n(* nested\n *) *) b";
  [%expect
    {|
    ((Lident a) t.stag:1:0-1 a)
    ((Lident b) t.stag:4:7-8 b)
    (Eof t.stag:4:8-8 "")
    |}]
;;

let%expect_test "positions across lines" =
  lex "let f x =\n  match x with\n  | A -> 1\n  | B y -> \"s\"\n\r\nin f";
  [%expect
    {|
    (Let t.stag:1:0-3 let)
    ((Lident f) t.stag:1:4-5 f)
    ((Lident x) t.stag:1:6-7 x)
    (Equal t.stag:1:8-9 =)
    (Match t.stag:2:2-7 match)
    ((Lident x) t.stag:2:8-9 x)
    (With t.stag:2:10-14 with)
    (Bar t.stag:3:2-3 |)
    ((Uident A) t.stag:3:4-5 A)
    (Arrow t.stag:3:6-8 ->)
    ((Int 1) t.stag:3:9-10 1)
    (Bar t.stag:4:2-3 |)
    ((Uident B) t.stag:4:4-5 B)
    ((Lident y) t.stag:4:6-7 y)
    (Arrow t.stag:4:8-10 ->)
    ((String s) t.stag:4:11-14 "\"s\"")
    (In t.stag:6:0-2 in)
    ((Lident f) t.stag:6:3-4 f)
    (Eof t.stag:6:4-4 "")
    |}]
;;

let%expect_test "token to_string" =
  let tokens : Token.t list =
    [ Int 12
    ; Char 'a'
    ; Char '\n'
    ; Char '\''
    ; String "a\"b\n"
    ; Lident "x"
    ; Uident "M"
    ; Label "A"
    ; Tyvar "a"
    ; Weak_tyvar "a"
    ; Arrow
    ; Quote_open
    ; Quote_close
    ; Splice
    ; Backslash
    ; Eof
    ]
  in
  List.iter tokens ~f:(fun token -> print_endline (Token.to_string token));
  [%expect
    {|
    12
    'a'
    '\n'
    '\''
    "a\"b\n"
    x
    M
    `A
    'a
    '_a
    ->
    .<
    >.
    .~
    \
    end of file
    |}]
;;

let%expect_test "errors" =
  lex "a (* unterminated";
  [%expect
    {|
    ((kind Unterminated_comment) (loc t.stag:1:2-4))
    t.stag:1:2-4: unterminated comment
    |}];
  lex "a (* (* nested *) unterminated";
  [%expect
    {|
    ((kind Unterminated_comment) (loc t.stag:1:2-4))
    t.stag:1:2-4: unterminated comment
    |}];
  lex "(* \"unterminated string in comment *)";
  [%expect
    {|
    ((kind Unterminated_comment) (loc t.stag:1:0-2))
    t.stag:1:0-2: unterminated comment
    |}];
  lex "x\n  \"unterminated";
  [%expect
    {|
    ((kind Unterminated_string) (loc t.stag:2:2-3))
    t.stag:2:2-3: unterminated string literal
    |}];
  lex "\"abc\\";
  [%expect
    {|
    ((kind (Illegal_escape "\\")) (loc t.stag:1:4-5))
    t.stag:1:4-5: illegal escape sequence \
    |}];
  lex "\"\\q\"";
  [%expect
    {|
    ((kind (Illegal_escape "\\q")) (loc t.stag:1:1-3))
    t.stag:1:1-3: illegal escape sequence \q
    |}];
  lex "\"\\256\"";
  [%expect
    {|
    ((kind (Illegal_escape "\\256")) (loc t.stag:1:1-5))
    t.stag:1:1-5: illegal escape sequence \256
    |}];
  lex "\"\\12\"";
  [%expect
    {|
    ((kind (Illegal_escape "\\12")) (loc t.stag:1:1-4))
    t.stag:1:1-4: illegal escape sequence \12
    |}];
  lex "\"\\xg0\"";
  [%expect
    {|
    ((kind (Illegal_escape "\\x")) (loc t.stag:1:1-3))
    t.stag:1:1-3: illegal escape sequence \x
    |}];
  lex "\"\\x4\"";
  [%expect
    {|
    ((kind (Illegal_escape "\\x4")) (loc t.stag:1:1-4))
    t.stag:1:1-4: illegal escape sequence \x4
    |}];
  lex "\"a\\\nb\"";
  [%expect
    {|
    ((String ab) t.stag:1:0-2:2  "\"a\\\
                                \nb\"")
    (Eof t.stag:2:2-2 "")
    |}];
  lex "'\\q'";
  [%expect
    {|
    ((kind (Illegal_escape "\\q")) (loc t.stag:1:1-3))
    t.stag:1:1-3: illegal escape sequence \q
    |}];
  lex "'\\n";
  [%expect
    {|
    ((kind Unterminated_char) (loc t.stag:1:0-3))
    t.stag:1:0-3: unterminated character literal
    |}];
  lex "'\\nx'";
  [%expect
    {|
    ((kind Unterminated_char) (loc t.stag:1:0-3))
    t.stag:1:0-3: unterminated character literal
    |}];
  lex "''";
  [%expect
    {|
    ((kind (Illegal_character ')) (loc t.stag:1:0-1))
    t.stag:1:0-1: illegal character '\''
    |}];
  lex "'1";
  [%expect
    {|
    ((kind (Illegal_character ')) (loc t.stag:1:0-1))
    t.stag:1:0-1: illegal character '\''
    |}];
  lex "'\n'";
  [%expect
    {|
    ((kind (Illegal_character ')) (loc t.stag:1:0-1))
    t.stag:1:0-1: illegal character '\''
    |}];
  lex "'";
  [%expect
    {|
    ((kind (Illegal_character ')) (loc t.stag:1:0-1))
    t.stag:1:0-1: illegal character '\''
    |}];
  lex "'_ ";
  [%expect
    {|
    ((kind (Illegal_character ')) (loc t.stag:1:0-1))
    t.stag:1:0-1: illegal character '\''
    |}];
  lex "` x";
  [%expect
    {|
    ((kind (Illegal_character `)) (loc t.stag:1:0-1))
    t.stag:1:0-1: illegal character '`'
    |}];
  lex "`1";
  [%expect
    {|
    ((kind (Illegal_character `)) (loc t.stag:1:0-1))
    t.stag:1:0-1: illegal character '`'
    |}];
  lex "x - 3";
  [%expect
    {|
    ((kind (Illegal_character -)) (loc t.stag:1:2-3))
    t.stag:1:2-3: illegal character '-'
    |}];
  lex "a > b";
  [%expect
    {|
    ((Lident a) t.stag:1:0-1 a)
    (Greater t.stag:1:2-3 >)
    ((Lident b) t.stag:1:4-5 b)
    (Eof t.stag:1:5-5 "")
    |}];
  lex "a < b";
  [%expect
    {|
    ((Lident a) t.stag:1:0-1 a)
    (Less t.stag:1:2-3 <)
    ((Lident b) t.stag:1:4-5 b)
    (Eof t.stag:1:5-5 "")
    |}];
  lex "12abc";
  [%expect
    {|
    ((kind (Invalid_int_literal 12abc)) (loc t.stag:1:0-5))
    t.stag:1:0-5: invalid integer literal 12abc
    |}];
  lex "1_000";
  [%expect
    {|
    ((Int 1000) t.stag:1:0-5 1_000)
    (Eof t.stag:1:5-5 "")
    |}];
  lex "x\n  #";
  [%expect
    {|
    ((kind (Illegal_character #)) (loc t.stag:2:2-3))
    t.stag:2:2-3: illegal character '#'
    |}];
  lex "a + b";
  [%expect
    {|
    ((kind (Illegal_character +)) (loc t.stag:1:2-3))
    t.stag:1:2-3: illegal character '+'
    |}];
  lex "x \xff";
  [%expect
    {|
    ((kind (Illegal_character "\255")) (loc t.stag:1:2-3))
    t.stag:1:2-3: illegal character '\255'
    |}]
;;

let%expect_test "quoted strings and expect tokens" =
  lex {x|{|a "b" \n|} {id|x |} y|id} {||} let%expect_test [%expect {| 1 |}]|x};
  lex "{ x } {a b|} {A|x|A}";
  lex "letx%expect_test let%expect_testx [%expectx [%expect";
  lex "{|abc";
  lex "(* {| *) |} *) 1";
  [%expect
    {xxx|
    ((String "a \"b\" \\n") t.stag:1:0-12 "{|a \"b\" \\n|}")
    ((String "x |} y") t.stag:1:13-27 "{id|x |} y|id}")
    ((String "") t.stag:1:28-32 {||})
    (Let_expect_test t.stag:1:33-48 let%expect_test)
    (Expect_open t.stag:1:49-57 [%expect)
    ((String " 1 ") t.stag:1:58-65 "{| 1 |}")
    (Rbracket t.stag:1:65-66 ])
    (Eof t.stag:1:66-66 "")
    (Lbrace t.stag:1:0-1 {)
    ((Lident x) t.stag:1:2-3 x)
    (Rbrace t.stag:1:4-5 })
    (Lbrace t.stag:1:6-7 {)
    ((Lident a) t.stag:1:7-8 a)
    ((Lident b) t.stag:1:9-10 b)
    (Bar t.stag:1:10-11 |)
    (Rbrace t.stag:1:11-12 })
    (Lbrace t.stag:1:13-14 {)
    ((Uident A) t.stag:1:14-15 A)
    (Bar t.stag:1:15-16 |)
    ((Lident x) t.stag:1:16-17 x)
    (Bar t.stag:1:17-18 |)
    ((Uident A) t.stag:1:18-19 A)
    (Rbrace t.stag:1:19-20 })
    (Eof t.stag:1:20-20 "")
    ((kind (Illegal_character %)) (loc t.stag:1:4-5))
    t.stag:1:4-5: illegal character '%'
    ((kind Unterminated_string) (loc t.stag:1:0-2))
    t.stag:1:0-2: unterminated string literal
    ((Int 1) t.stag:1:15-16 1)
    (Eof t.stag:1:16-16 "")
    |xxx}]
;;

let%expect_test "a dot before a name starts a root path unless after an atom" =
  let tokens src =
    match Lexer.tokenize ~file:"t.stag" src with
    | Error error -> print_endline (Lexer.Error.to_string error)
    | Ok tokens ->
      Array.iter tokens ~f:(fun { token; loc = _ } ->
        match token with
        | Eof -> print_endline ""
        | Root -> print_string "ROOT "
        | Dot -> print_string "DOT "
        | token -> printf "%s " (Token.to_string token))
  in
  List.iter
    [ ".x f .M.x r.x M.x"
    ; "(e).x [a].x {}.x \"s\".x x'.x _.x 1.x"
    ; "(.x) [.x] {.x} f(.x) a,.x =.x ->.x"
    ; ">.x .~.x .<.x>. .{.x} ..x"
    ; "(* c *).x x(* c *).x x .x"
    ; ". x .1 .`a"
    ]
    ~f:tokens;
  [%expect
    {|
    ROOT x f ROOT M DOT x r DOT x M DOT x
    ( e ) DOT x [ a ] DOT x { } DOT x "s" DOT x x' DOT x _ DOT x 1 DOT x
    ( ROOT x ) [ ROOT x ] { ROOT x } f ( ROOT x ) a , ROOT x = ROOT x -> ROOT x
    >. x .~ ROOT x .< ROOT x >. .{ ROOT x } .. x
    ROOT x x ROOT x x ROOT x
    DOT x DOT 1 DOT `a
    |}]
;;
