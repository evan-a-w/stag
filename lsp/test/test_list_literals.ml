open! Import

let%expect_test "list literals: semantic tokens, hover and definitions" =
  Client.with_server
    ~files:
      [ ( "a.stag"
        , {|type t = Cons | Nil
let l = [Cons; Nil]
let f x = match x with [] -> 0 | [_; Cons] -> 1 | _ -> 2
|}
        )
      ]
    (fun client ->
      Client.did_open client "a.stag";
      [%expect {| |}];
      Client.print_semantic_tokens client "a.stag";
      [%expect
        {|
         0:0+4 Keyword "type"
         0:5+1 Type (declaration) "t"
         0:7+1 Operator "="
         0:9+4 Enum_member (declaration) "Cons"
         0:14+1 Operator "|"
         0:16+3 Enum_member (declaration) "Nil"
         1:0+3 Keyword "let"
         1:4+1 Variable (declaration) "l"
         1:6+1 Operator "="
         1:9+4 Enum_member "Cons"
         1:15+3 Enum_member "Nil"
         2:0+3 Keyword "let"
         2:4+1 Function (declaration) "f"
         2:6+1 Parameter (declaration) "x"
         2:8+1 Operator "="
         2:10+5 Keyword "match"
         2:16+1 Variable "x"
         2:18+4 Keyword "with"
         2:26+2 Operator "->"
         2:29+1 Number "0"
         2:31+1 Operator "|"
         2:37+4 Enum_member "Cons"
         2:43+2 Operator "->"
         2:46+1 Number "1"
         2:48+1 Operator "|"
         2:52+2 Operator "->"
         2:55+1 Number "2"
         |}];
      List.iter
        [ 1, 8; 1, 9; 1, 13; 1, 18; 2, 23; 2, 24; 2, 33; 2, 35 ]
        ~f:(fun (line, character) ->
          Client.print_hover client "a.stag" ~line ~character;
          Client.print_definition client "a.stag" ~line ~character);
      [%expect
        {|
         1:8 1:8-1:19 ```stag list[A.t] ```
         1:8 no definition
         1:9 1:9-1:13 ```stag A.Cons : A.t ```
         1:9 -> a.stag 0:9-0:13
         1:13 1:9-1:13 ```stag A.Cons : A.t ```
         1:13 -> a.stag 0:9-0:13
         1:18 1:18-1:19 ```stag list[A.t] ```
         1:18 no definition
         2:23 2:23-2:25 ```stag list[A.t] ```
         2:23 no definition
         2:24 2:23-2:25 ```stag list[A.t] ```
         2:24 no definition
         2:33 2:33-2:42 ```stag list[A.t] ```
         2:33 no definition
         2:35 2:34-2:35 ```stag A.t ```
         2:35 no definition
         |}])
;;
