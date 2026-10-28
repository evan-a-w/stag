Each token and comment, with its byte offsets.

  $ cat > a.stag <<'EOF'
  > (* c "*)" *) let%expect_test "t" = let x = 'a' in {id|q|id}
  > type 'a t = `A of '_b | Some -1 [%expect]
  > EOF
  $ stag dump-tokens a.stag
  (comment 0 12)
  (expect 13 28)
  (string 29 32)
  (punct 33 34)
  (keyword 35 38)
  (lident 39 40)
  (punct 41 42)
  (char 43 46)
  (keyword 47 49)
  (quoted_string 50 59)
  (keyword 60 64)
  (tyvar 65 67)
  (lident 68 69)
  (punct 70 71)
  (label 72 74)
  (keyword 75 77)
  (weak_tyvar 78 81)
  (punct 82 83)
  (uident 84 88)
  (int 89 91)
  (expect 92 100)
  (punct 100 101)

Before an error, the tokens that lex.

  $ printf 'let x = "a\\q" 1 (* ' > b.stag
  $ stag dump-tokens b.stag
  (keyword 0 3)
  (lident 4 5)
  (punct 6 7)
  (error 10 12 "illegal escape sequence \\q")
  $ printf 'x (* ' > c.stag
  $ stag dump-tokens c.stag
  (lident 0 1)
  (error 2 4 "unterminated comment")
