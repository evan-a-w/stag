open! Import

let print_at client file positions =
  List.iter positions ~f:(fun (line, character) ->
    Client.print_hover client file ~line ~character;
    Client.print_definition client file ~line ~character)
;;

let%expect_test "an include's expression is indexed, not what it generates" =
  Client.with_server
    ~files:
      [ ( "a.stag"
        , {|let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
let x = 1
include .~(value "y" (Stag.Expr.var "x"))
let z = y
|}
        )
      ]
    (fun client ->
       Client.did_open client "a.stag";
       [%expect {| |}];
       print_at client "a.stag" [ 2, 0; 2, 12; 2, 18; 2, 30; 2, 37; 3, 4; 3, 8 ];
       [%expect
         {|
         2:0 no hover
         2:0 no definition
         2:12 2:11-2:16 ```stag A.value : string -> Stag.Expr.t -> list[Stag.Toplevel.t] ```
         2:12 -> a.stag 0:4-0:9
         2:18 2:17-2:20 ```stag string ```
         2:18 no definition
         2:30 2:22-2:35 ```stag prelude.Stag.Expr.var : string -> Stag.Expr.t ```
         2:30 no definition
         2:37 2:36-2:39 ```stag string ```
         2:37 no definition
         3:4 3:4-3:5 ```stag A.z : int ```
         3:4 -> a.stag 3:4-3:5
         3:8 3:8-3:9 ```stag A.y : int ```
         3:8 -> a.stag 2:0-2:41
         |}])
;;

let%expect_test "a spliced module" =
  Client.with_server
    ~files:
      [ ( "a.stag"
        , {|let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
let y = "written"
module M = .~(value "y" (Stag.Expr.int 1))
let z = M.y
let w = y
|}
        )
      ]
    (fun client ->
       Client.did_open client "a.stag";
       [%expect {| |}];
       print_at client "a.stag" [ 1, 4; 2, 7; 2, 15; 3, 4; 3, 10; 4, 8 ];
       [%expect
         {|
         1:4 1:4-1:5 ```stag A.y : string ```
         1:4 -> a.stag 1:4-1:5
         2:7 no hover
         2:7 no definition
         2:15 2:14-2:19 ```stag A.value : string -> Stag.Expr.t -> list[Stag.Toplevel.t] ```
         2:15 -> a.stag 0:4-0:9
         3:4 3:4-3:5 ```stag A.z : int ```
         3:4 -> a.stag 3:4-3:5
         3:10 3:8-3:11 ```stag A.M.y : int ```
         3:10 -> a.stag 2:0-2:42
         4:8 4:8-4:9 ```stag A.y : string ```
         4:8 -> a.stag 1:4-1:5
         |}])
;;

let%expect_test "a written declaration shadowed by an included one" =
  Client.with_server
    ~files:
      [ ( "a.stag"
        , {|let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
let y = "written"
module N = struct
  include .~(value "y" (Stag.Expr.int 1))
  let inner = y
end
let outer = y
let y2 = N.y
|}
        )
      ]
    (fun client ->
       Client.did_open client "a.stag";
       [%expect {| |}];
       print_at client "a.stag" [ 1, 4; 4, 14; 6, 12; 7, 11 ];
       [%expect
         {|
         1:4 1:4-1:5 ```stag A.y : string ```
         1:4 -> a.stag 1:4-1:5
         4:14 4:14-4:15 ```stag A.N.y : int ```
         4:14 -> a.stag 3:2-3:41
         6:12 6:12-6:13 ```stag A.y : string ```
         6:12 -> a.stag 1:4-1:5
         7:11 7:9-7:12 ```stag A.N.y : int ```
         7:11 -> a.stag 3:2-3:41
         |}])
;;

let%expect_test "semantic tokens of a large typechecked file take linear time" =
  let lines = 20_000 in
  let source =
    String.concat
      (List.init lines ~f:(fun i -> sprintf "let v%d = Int.add %d 1\n" i i))
  in
  Client.with_server
    ~files:[ "a.stag", source ]
    (fun client ->
       Client.did_open client "a.stag";
       Client.print_hover client "a.stag" ~line:0 ~character:12;
       let start = Time_ns.now () in
       let tokens =
         Client.request
           client
           "textDocument/semanticTokens/full"
           (Client.text_document client "a.stag")
       in
       let elapsed = Time_ns.diff (Time_ns.now ()) start in
       (match tokens with
        | Some json ->
          let tokens = Lsp_types.SemanticTokens.t_of_yojson json in
          printf "%d tokens\n" (Array.length tokens.data / 5)
        | None -> ());
       print_s
         [%sexp (Time_ns.Span.( < ) elapsed (Time_ns.Span.of_int_sec 5) : bool)];
       [%expect
         {|
         0:12 0:9-0:16 ```stag Int.add : int -> int -> int ```
         140000 tokens
         true
         |}])
;;

let%expect_test "inline expansions" =
  Client.with_server
    ~files:
      [ ( "a.stag"
        , {|let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
include%inline .~(value "x" (Stag.Expr.int 1))
  let x = 1
end%inline
module%inline M = .~(value "y" (Stag.Expr.var "x"))
  let y = x
end%inline
include%inline .~(value "s" (Stag.Expr.int 2))
  let s = 3
end%inline
let z = Int.add x M.y
let w = s
|}
        )
      ]
    (fun client ->
       Client.did_open client "a.stag";
       [%expect {| |}];
       print_at
         client
         "a.stag"
         [ 2, 6; 4, 14; 5, 6; 5, 10; 8, 6; 10, 16; 10, 20; 11, 8 ];
       [%expect
         {|
         2:6 2:6-2:7 ```stag A.x : int ```
         2:6 -> a.stag 2:6-2:7
         4:14 no hover
         4:14 no definition
         5:6 5:6-5:7 ```stag A.M.y : int ```
         5:6 -> a.stag 5:6-5:7
         5:10 5:10-5:11 ```stag A.x : int ```
         5:10 -> a.stag 2:6-2:7
         8:6 no hover
         8:6 no definition
         10:16 10:16-10:17 ```stag A.x : int ```
         10:16 -> a.stag 2:6-2:7
         10:20 10:18-10:21 ```stag A.M.y : int ```
         10:20 -> a.stag 5:6-5:7
         11:8 11:8-11:9 ```stag A.s : int ```
         11:8 -> a.stag 7:0-7:46
         |}];
       (match
          Client.request
            client
            "textDocument/documentSymbol"
            (Client.text_document client "a.stag")
        with
        | Some (`List symbols) ->
          let rec print indent (symbol : Lsp_types.DocumentSymbol.t) =
            printf
              "%s%s %d:%d-%d:%d\n"
              indent
              symbol.name
              symbol.selectionRange.start.line
              symbol.selectionRange.start.character
              symbol.selectionRange.end_.line
              symbol.selectionRange.end_.character;
            List.iter
              (Option.value symbol.children ~default:[])
              ~f:(print (indent ^ "  "))
          in
          List.iter symbols ~f:(fun json ->
            print "" (Lsp_types.DocumentSymbol.t_of_yojson json))
        | _ -> print_endline "unexpected");
       [%expect
         {|
         value 0:4-0:9
         x 2:6-2:7
         M 4:14-4:15
           y 5:6-5:7
         s 8:6-8:7
         z 10:4-10:5
         w 11:4-11:5
         |}];
       Client.print_semantic_tokens client "a.stag";
       [%expect
         {|
         0:0+3 Keyword "let"
         0:4+5 Function (declaration) "value"
         0:10+4 Parameter (declaration) "name"
         0:15+1 Parameter (declaration) "e"
         0:17+1 Operator "="
         0:19+4 Enum_member "Cons"
         0:25+4 Namespace "Stag"
         0:30+8 Namespace "Toplevel"
         0:39+4 Function "let_"
         0:44+4 Variable "name"
         0:49+3 Enum_member "Nil"
         0:53+1 Variable "e"
         0:56+3 Enum_member "Nil"
         1:0+14 Keyword "include%inline"
         1:15+2 Macro ".~"
         1:18+5 Function "value"
         1:24+3 String "\"x\""
         1:29+4 Namespace "Stag"
         1:34+4 Namespace "Expr"
         1:39+3 Function "int"
         1:43+1 Number "1"
         2:2+3 Keyword "let"
         2:6+1 Variable (declaration) "x"
         2:8+1 Operator "="
         2:10+1 Number "1"
         3:0+10 Keyword "end%inline"
         4:0+13 Keyword "module%inline"
         4:14+1 Namespace (declaration) "M"
         4:16+1 Operator "="
         4:18+2 Macro ".~"
         4:21+5 Function "value"
         4:27+3 String "\"y\""
         4:32+4 Namespace "Stag"
         4:37+4 Namespace "Expr"
         4:42+3 Function "var"
         4:46+3 String "\"x\""
         5:2+3 Keyword "let"
         5:6+1 Variable (declaration) "y"
         5:8+1 Operator "="
         5:10+1 Variable "x"
         6:0+10 Keyword "end%inline"
         7:0+14 Keyword "include%inline"
         7:15+2 Macro ".~"
         7:18+5 Function "value"
         7:24+3 String "\"s\""
         7:29+4 Namespace "Stag"
         7:34+4 Namespace "Expr"
         7:39+3 Function "int"
         7:43+1 Number "2"
         8:2+3 Keyword "let"
         8:6+1 Variable (declaration) "s"
         8:8+1 Operator "="
         8:10+1 Number "3"
         9:0+10 Keyword "end%inline"
         10:0+3 Keyword "let"
         10:4+1 Variable (declaration) "z"
         10:6+1 Operator "="
         10:8+3 Namespace "Int"
         10:12+3 Function "add"
         10:16+1 Variable "x"
         10:18+1 Namespace "M"
         10:20+1 Variable "y"
         11:0+3 Keyword "let"
         11:4+1 Variable (declaration) "w"
         11:6+1 Operator "="
         11:8+1 Variable "s"
         |}])
;;
