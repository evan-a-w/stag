open! Import

let%expect_test "root paths: semantic tokens, hover and definitions" =
  Client.with_server
    ~files:
      [ ( "a.stag"
        , {|type t = int
module M = struct
  type t = string
  let x : .A.t = 1
  let y = .A.M.x
end
let z : t = M.y
|}
        )
      ]
    (fun client ->
       Client.did_open client "a.stag";
       [%expect {| |}];
       Client.print_semantic_tokens client "a.stag";
       [%expect {|
         0:0+4 Keyword "type"
         0:5+1 Type (declaration) "t"
         0:7+1 Operator "="
         0:9+3 Type "int"
         1:0+6 Keyword "module"
         1:7+1 Namespace (declaration) "M"
         1:9+1 Operator "="
         1:11+6 Keyword "struct"
         2:2+4 Keyword "type"
         2:7+1 Type (declaration) "t"
         2:9+1 Operator "="
         2:11+6 Type "string"
         3:2+3 Keyword "let"
         3:6+1 Variable (declaration) "x"
         3:10+1 Namespace "."
         3:11+1 Namespace "A"
         3:13+1 Type "t"
         3:15+1 Operator "="
         3:17+1 Number "1"
         4:2+3 Keyword "let"
         4:6+1 Variable (declaration) "y"
         4:8+1 Operator "="
         4:10+1 Namespace "."
         4:11+1 Namespace "A"
         4:13+1 Namespace "M"
         4:15+1 Variable "x"
         5:0+3 Keyword "end"
         6:0+3 Keyword "let"
         6:4+1 Variable (declaration) "z"
         6:8+1 Type "t"
         6:10+1 Operator "="
         6:12+1 Namespace "M"
         6:14+1 Variable "y"
         |}];
       List.iter [ 3, 11; 3, 12; 4, 13; 4, 14 ] ~f:(fun (line, character) ->
         Client.print_hover client "a.stag" ~line ~character;
         Client.print_definition client "a.stag" ~line ~character);
       [%expect {|
         3:11 3:10-3:14 ```stag type A.t ```
         3:11 -> a.stag 0:5-0:6
         3:12 3:10-3:14 ```stag type A.t ```
         3:12 -> a.stag 0:5-0:6
         4:13 4:10-4:16 ```stag A.M.x : int ```
         4:13 -> a.stag 3:6-3:7
         4:14 4:10-4:16 ```stag A.M.x : int ```
         4:14 -> a.stag 3:6-3:7
         |}])
;;
