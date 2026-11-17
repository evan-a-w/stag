open! Import

let%expect_test "semantic tokens up to a lexer error" =
  Client.with_server
    ~files:
      [ "a.stag", "let f x = x\nlet s = \"abc\nlet y = f 1\n"
      ; "b.stag", "let f x = x (* a\ncomment\n"
      ; "c.stag", "let f x = x\nlet c = 1 $ 2\n"
      ]
    (fun client ->
      Client.did_open client "a.stag";
      Client.did_open client "b.stag";
      Client.did_open client "c.stag";
      [%expect
        {|
         diagnostics a.stag (version 1):
           1:8-1:9 error: Unterminated string literal
         diagnostics b.stag (version 1):
           0:12-0:14 error: Unterminated comment
         diagnostics c.stag (version 1):
           1:10-1:11 error: Illegal character '$'
         |}];
      Client.print_semantic_tokens client "a.stag";
      [%expect
        {|
         0:0+3 Keyword "let"
         0:4+1 Variable "f"
         0:6+1 Variable "x"
         0:8+1 Operator "="
         0:10+1 Variable "x"
         1:0+3 Keyword "let"
         1:4+1 Variable "s"
         1:6+1 Operator "="
         |}];
      Client.print_semantic_tokens client "b.stag";
      [%expect
        {|
         0:0+3 Keyword "let"
         0:4+1 Variable "f"
         0:6+1 Variable "x"
         0:8+1 Operator "="
         0:10+1 Variable "x"
         |}];
      Client.print_semantic_tokens client "c.stag";
      [%expect
        {|
         0:0+3 Keyword "let"
         0:4+1 Variable "f"
         0:6+1 Variable "x"
         0:8+1 Operator "="
         0:10+1 Variable "x"
         1:0+3 Keyword "let"
         1:4+1 Variable "c"
         1:6+1 Operator "="
         1:8+1 Number "1"
         |}])
;;
