open! Import

let source =
  {stag|(* a ü comment *)
type point = {x : int; y : int}
type shape = Circle of int | Square of point
exception Bad of string

module Geometry = struct
  let area s =
    match s with
    | Circle r -> Int.mul r r
    | Square p -> Int.mul p.x p.y
end

let label = "é😊"
let code = .< Int.add 1 2 >.
let total = Geometry.area (Square {x = 2; y = 3})
let poly = {`a = 1}
let tagged = `Tag 1

let%expect_test "area" =
  print_int total;
  [%expect {| 6 |}]

let%bench "bench" = total
|stag}
;;

let hover ?(file = "a.stag") client ~line ~character =
  match
    Client.request
      client
      "textDocument/hover"
      (Client.text_document
         client
         file
         ~extra:[ "position", Client.position ~line ~character ])
  with
  | None | Some `Null -> print_endline "no hover"
  | Some json ->
    let hover = Lsp_types.Hover.t_of_yojson json in
    (match hover.contents with
     | `MarkupContent { value; _ } ->
       printf
         "%s%s\n"
         (match hover.range with
          | Some { start; end_ } ->
            sprintf
              "%d:%d-%d:%d "
              start.line
              start.character
              end_.line
              end_.character
          | None -> "")
         (String.substr_replace_all value ~pattern:"\n" ~with_:" ")
     | _ -> print_endline "unexpected hover")
;;

let definition client file ~line ~character =
  match
    Client.request
      client
      "textDocument/definition"
      (Client.text_document
         client
         file
         ~extra:[ "position", Client.position ~line ~character ])
  with
  | None | Some `Null -> print_endline "no definition"
  | Some (`List locations) ->
    List.iter locations ~f:(fun json ->
      let { Lsp_types.Location.uri; range = { start; end_ } } =
        Lsp_types.Location.t_of_yojson json
      in
      printf
        "-> %s %d:%d-%d:%d\n"
        (Client.relativize client (Lsp.Uri.to_string uri))
        start.line
        start.character
        end_.line
        end_.character)
  | Some json -> Client.print_json client json
;;

let%expect_test "diagnostics follow changes" =
  Client.with_server
    ~files:[ "a.stag", "let x = Int.add 1 \"é\"\n" ]
    (fun client ->
       Client.did_open client "a.stag";
       [%expect
         {|
      diagnostics a.stag (version 1):
        0:18-0:21 error: This expression has type string but an expression was expected of type int
      |}];
       Client.did_change client "a.stag" ~version:2 "let x = Int.add 1 2\n";
       [%expect {| diagnostics a.stag (version 2): none |}];
       Client.did_change
         client
         "a.stag"
         ~version:3
         "let x = (* 😊 *) Int.add 1 2 in\n";
       [%expect
         {|
      diagnostics a.stag (version 3):
        0:29-0:31 error: Unexpected `in`: a toplevel `let` is a declaration, without `in`
      |}];
       Client.notify
         client
         "textDocument/didClose"
         (Client.text_document client "a.stag");
       [%expect {| diagnostics a.stag: none |}])
;;

let%expect_test "changes are checked once, at their last version" =
  Client.with_server
    ~files:[ "a.stag", "let x = 1\n" ]
    (fun client ->
       Client.did_open client "a.stag";
       Client.did_change ~check:false client "a.stag" ~version:2 "let x = y\n";
       Client.did_change ~check:false client "a.stag" ~version:3 "let x = z\n";
       [%expect {| |}];
       Client.check_pending client;
       [%expect
         {|
      diagnostics a.stag (version 3):
        0:8-0:9 error: Unbound value z
      |}])
;;

let%expect_test "a looping splice is stopped by the limits" =
  Client.with_server
    ~limits:{ Server.default_limits with fuel = Some 100_000 }
    ~files:[ "a.stag", "let loop x = loop x\nlet y = .~(loop 0)\n" ]
    (fun client ->
       Client.did_open client "a.stag";
       [%expect
         {|
         diagnostics a.stag (version 1):
           1:8-1:18 error: This toplevel splice failed at compile time: Out of fuel: evaluation took more than 100000 steps
             related a.stag 0:13-0:17: The error was raised here
         |}])
;;

let%expect_test "a syntax splice: diagnostics and hover" =
  Client.with_server
    ~files:
      [ ( "a.stag"
        , "let one = .~(Stag.Expr.int 1)\n\
           let s = .~(Core.Sexp.sexp_of (type list[int])) [one]\n\
           let bad : string = .~(Stag.Expr.int 1)\n" )
      ]
    (fun client ->
       Client.did_open client "a.stag";
       [%expect
         {|
         diagnostics a.stag (version 1):
           2:19-2:38 error: In the expression this splice gave: This expression has type int but an expression was expected of type string
             Note: The splice gave the expression:
               1
         |}];
       List.iter
         [ 0, 4; 0, 10; 0, 20; 1, 13; 1, 48 ]
         ~f:(fun (line, character) ->
           printf "%d:%d " line character;
           hover client ~line ~character);
       [%expect
         {|
         0:4 0:4-0:7 ```stag A.one : int ```
         0:10 0:10-0:29 ```stag int ```
         0:20 0:13-0:26 ```stag prelude.Stag.Expr.int : int -> Stag.Expr.t ```
         1:13 1:11-1:28 ```stag prelude.Core.Sexp.sexp_of : Stag.Type.t[list[int]] -> Stag.Expr.t ```
         1:48 1:48-1:51 ```stag A.one : int ```
         |}])
;;

let%expect_test "formatting" =
  Client.with_server
    ~files:
      [ "a.stag", "let   x =\n  \"é\"   (* 😊 *)\n"; "b.stag", "let x = (\n" ]
    (fun client ->
       Client.did_open client "a.stag";
       Client.did_open client "b.stag";
       [%expect
         {|
        diagnostics b.stag (version 1):
          1:0-1:0 error: Expected an expression, found end of file
        |}];
       let format file =
         Option.iter
           (Client.request
              client
              "textDocument/formatting"
              (Client.text_document
                 client
                 file
                 ~extra:
                   [ ( "options"
                     , `Assoc [ "tabSize", `Int 2; "insertSpaces", `Bool true ]
                     )
                   ]))
           ~f:(Client.print_json client)
       in
       format "a.stag";
       [%expect
         {|
        [
          {
            "newText": "let x = \"é\" (* 😊 *)\n",
            "range": {
              "end": { "character": 0, "line": 2 },
              "start": { "character": 0, "line": 0 }
            }
          }
        ]
        |}];
       format "b.stag";
       [%expect {| [] |}])
;;

let%expect_test "semantic tokens" =
  Client.with_server
    ~files:[ "a.stag", source ]
    (fun client ->
       Client.did_open client "a.stag";
       [%expect {| |}];
       Client.print_semantic_tokens client "a.stag";
       [%expect
         {xxx|
      0:0+17 Comment "(* a \195\188 comment *)"
      1:0+4 Keyword "type"
      1:5+5 Type (declaration) "point"
      1:11+1 Operator "="
      1:14+1 Property (declaration) "x"
      1:18+3 Type "int"
      1:23+1 Property (declaration) "y"
      1:27+3 Type "int"
      2:0+4 Keyword "type"
      2:5+5 Type (declaration) "shape"
      2:11+1 Operator "="
      2:13+6 Enum_member (declaration) "Circle"
      2:20+2 Keyword "of"
      2:23+3 Type "int"
      2:27+1 Operator "|"
      2:29+6 Enum_member (declaration) "Square"
      2:36+2 Keyword "of"
      2:39+5 Type "point"
      3:0+9 Keyword "exception"
      3:10+3 Enum_member (declaration) "Bad"
      3:14+2 Keyword "of"
      3:17+6 Type "string"
      5:0+6 Keyword "module"
      5:7+8 Namespace (declaration) "Geometry"
      5:16+1 Operator "="
      5:18+6 Keyword "struct"
      6:2+3 Keyword "let"
      6:6+4 Function (declaration) "area"
      6:11+1 Parameter (declaration) "s"
      6:13+1 Operator "="
      7:4+5 Keyword "match"
      7:10+1 Variable "s"
      7:12+4 Keyword "with"
      8:4+1 Operator "|"
      8:6+6 Enum_member "Circle"
      8:13+1 Variable (declaration) "r"
      8:15+2 Operator "->"
      8:18+3 Namespace "Int"
      8:22+3 Function "mul"
      8:26+1 Variable "r"
      8:28+1 Variable "r"
      9:4+1 Operator "|"
      9:6+6 Enum_member "Square"
      9:13+1 Variable (declaration) "p"
      9:15+2 Operator "->"
      9:18+3 Namespace "Int"
      9:22+3 Function "mul"
      9:26+1 Variable "p"
      9:28+1 Property "x"
      9:30+1 Variable "p"
      9:32+1 Property "y"
      10:0+3 Keyword "end"
      12:0+3 Keyword "let"
      12:4+5 Variable (declaration) "label"
      12:10+1 Operator "="
      12:12+5 String "\"\195\169\240\159\152\138\""
      13:0+3 Keyword "let"
      13:4+4 Variable (declaration) "code"
      13:9+1 Operator "="
      13:11+2 Macro ".<"
      13:14+3 Namespace "Int"
      13:18+3 Function "add"
      13:22+1 Number "1"
      13:24+1 Number "2"
      13:26+2 Macro ">."
      14:0+3 Keyword "let"
      14:4+5 Variable (declaration) "total"
      14:10+1 Operator "="
      14:12+8 Namespace "Geometry"
      14:21+4 Function "area"
      14:27+6 Enum_member "Square"
      14:35+1 Property "x"
      14:37+1 Operator "="
      14:39+1 Number "2"
      14:42+1 Property "y"
      14:44+1 Operator "="
      14:46+1 Number "3"
      15:0+3 Keyword "let"
      15:4+4 Variable (declaration) "poly"
      15:9+1 Operator "="
      15:12+2 Property "`a"
      15:15+1 Operator "="
      15:17+1 Number "1"
      16:0+3 Keyword "let"
      16:4+6 Variable (declaration) "tagged"
      16:11+1 Operator "="
      16:13+4 Enum_member "`Tag"
      16:18+1 Number "1"
      18:0+15 Keyword "let%expect_test"
      18:16+6 String "\"area\""
      18:23+1 Operator "="
      19:2+9 Function "print_int"
      19:12+5 Variable "total"
      20:2+8 Keyword "[%expect"
      20:11+7 String "{| 6 |}"
      22:0+9 Keyword "let%bench"
      22:10+7 String "\"bench\""
      22:18+1 Operator "="
      22:20+5 Variable "total"
      |xxx}])
;;

let%expect_test "semantic tokens of a file that doesn't typecheck or parse" =
  Client.with_server
    ~files:
      [ "a.stag", "let f x = x\nlet y : t = f 1\n"; "b.stag", "let f x = (x\n" ]
    (fun client ->
       Client.did_open client "a.stag";
       Client.did_open client "b.stag";
       [%expect
         {|
        diagnostics a.stag (version 1):
          1:8-1:9 error: Unbound type t
        diagnostics b.stag (version 1):
          1:0-1:0 error: Expected `)` to close this `(` (opened at 1:10), found end of file
        |}];
       Client.print_semantic_tokens client "a.stag";
       [%expect
         {|
         0:0+3 Keyword "let"
         0:4+1 Function (declaration) "f"
         0:6+1 Parameter (declaration) "x"
         0:8+1 Operator "="
         0:10+1 Variable "x"
         1:0+3 Keyword "let"
         1:4+1 Variable (declaration) "y"
         1:8+1 Type "t"
         1:10+1 Operator "="
         1:12+1 Function "f"
         1:14+1 Number "1"
         |}];
       Client.print_semantic_tokens client "b.stag";
       [%expect
         {|
        0:0+3 Keyword "let"
        0:4+1 Variable "f"
        0:6+1 Variable "x"
        0:8+1 Operator "="
        0:11+1 Variable "x"
        |}])
;;

let%expect_test "hover and definition" =
  Client.with_server
    ~files:[ "a.stag", source ]
    (fun client ->
       Client.did_open client "a.stag";
       [%expect {| |}];
       List.iter
         [ 1, 6
         ; 1, 14
         ; 1, 18
         ; 2, 13
         ; 2, 39
         ; 3, 10
         ; 6, 7
         ; 6, 12
         ; 7, 11
         ; 8, 6
         ; 8, 21
         ; 9, 13
         ; 9, 31
         ; 14, 22
         ; 14, 31
         ; 14, 40
         ; 15, 7
         ; 13, 8
         ; 13, 20
         ]
         ~f:(fun (line, character) ->
           printf "%d:%d " line character;
           hover client ~line ~character);
       [%expect
         {|
      1:6 1:5-1:10 ```stag type A.point ```
      1:14 no hover
      1:18 1:18-1:21 ```stag type int ```
      2:13 no hover
      2:39 2:39-2:44 ```stag type A.point ```
      3:10 no hover
      6:7 6:6-6:10 ```stag A.Geometry.area : A.shape -> int ```
      6:12 6:11-6:12 ```stag s : A.shape ```
      7:11 7:10-7:11 ```stag s : A.shape ```
      8:6 8:6-8:12 ```stag A.Circle : A.shape ```
      8:21 8:18-8:25 ```stag Int.mul : int -> int -> int ```
      9:13 9:13-9:14 ```stag p : A.point ```
      9:31 9:30-9:31 ```stag p : A.point ```
      14:22 14:12-14:25 ```stag A.Geometry.area : A.shape -> int ```
      14:31 14:27-14:33 ```stag A.Square : A.shape ```
      14:40 14:39-14:40 ```stag int ```
      15:7 15:4-15:8 ```stag A.poly : {`a : int} ```
      13:8 13:4-13:8 ```stag A.code : <int>{..'a} ```
      13:20 13:14-13:21 ```stag Int.add : int -> int -> int ```
      |}];
       List.iter
         [ 8, 21
         ; 9, 32
         ; 9, 30
         ; 9, 13
         ; 14, 14
         ; 14, 35
         ; 8, 6
         ; 14, 22
         ; 14, 31
         ; 14, 40
         ; 2, 39
         ; 20, 13
         ]
         ~f:(fun (line, character) ->
           printf "%d:%d " line character;
           definition client "a.stag" ~line ~character);
       [%expect
         {|
      8:21 no definition
      9:32 -> a.stag 1:23-1:24
      9:30 -> a.stag 9:13-9:14
      9:13 -> a.stag 9:13-9:14
      14:14 -> a.stag 6:6-6:10
      14:35 -> a.stag 1:14-1:15
      8:6 -> a.stag 2:13-2:19
      14:22 -> a.stag 6:6-6:10
      14:31 -> a.stag 2:29-2:35
      14:40 no definition
      2:39 -> a.stag 1:5-1:10
      20:13 no definition
      |}])
;;

let%expect_test "hover and definition in a file with errors" =
  Client.with_server
    ~files:
      [ ( "a.stag"
        , "let double x = Int.mul 2 x\n\
           let bad : string = double (nope 1)\n\
           let f y = Int.add (double y) true\n\
           let g = in\n\
           let h = double 4\n" )
      ]
    (fun client ->
       Client.did_open client "a.stag";
       [%expect
         {|
         diagnostics a.stag (version 1):
           1:19-1:34 error: This expression has type int but an expression was expected of type string
           1:27-1:31 error: Unbound value nope
           2:29-2:33 error: This expression has type bool but an expression was expected of type int
           3:8-3:10 error: Expected an expression, found `in`
         |}];
       List.iter
         [ 1, 4; 1, 20; 2, 6; 2, 20; 2, 26; 4, 9 ]
         ~f:(fun (line, character) ->
           printf "%d:%d " line character;
           hover client ~line ~character);
       [%expect
         {|
         1:4 1:4-1:7 ```stag A.bad : string ```
         1:20 1:19-1:25 ```stag A.double : int -> int ```
         2:6 2:6-2:7 ```stag y : int ```
         2:20 2:19-2:25 ```stag A.double : int -> int ```
         2:26 2:26-2:27 ```stag y : int ```
         4:9 4:8-4:14 ```stag A.double : int -> int ```
         |}];
       List.iter
         [ 1, 20; 2, 20; 2, 27; 4, 9 ]
         ~f:(fun (line, character) ->
           printf "%d:%d " line character;
           definition client "a.stag" ~line ~character);
       [%expect
         {|
         1:20 -> a.stag 0:4-0:10
         2:20 -> a.stag 0:4-0:10
         2:27 -> a.stag 2:6-2:7
         4:9 -> a.stag 0:4-0:10
         |}])
;;

let%expect_test "mutable variables: hover and definition" =
  Client.with_server
    ~files:
      [ ( "a.stag"
        , "let mut count = 0\n\
           module M = struct let mut total = 1 end\n\
           let bump u = count <- Int.add count 1; M.total <- 2\n\
           let local u = let mut n = 3 in n <- Int.add n 1; n\n" )
      ]
    (fun client ->
       Client.did_open client "a.stag";
       [%expect {| |}];
       let positions = [ 0, 8; 2, 13; 2, 31; 2, 41; 3, 22; 3, 31; 3, 44 ] in
       List.iter positions ~f:(fun (line, character) ->
         printf "%d:%d " line character;
         hover client ~line ~character);
       [%expect
         {|
         0:8 0:8-0:13 ```stag A.count : int ```
         2:13 2:13-2:18 ```stag A.count : int ```
         2:31 2:30-2:35 ```stag A.count : int ```
         2:41 2:39-2:46 ```stag A.M.total : int ```
         3:22 3:22-3:23 ```stag n : int ```
         3:31 3:31-3:32 ```stag n : int ```
         3:44 3:44-3:45 ```stag n : int ```
         |}];
       List.iter positions ~f:(fun (line, character) ->
         printf "%d:%d " line character;
         definition client "a.stag" ~line ~character);
       [%expect
         {|
         0:8 -> a.stag 0:8-0:13
         2:13 -> a.stag 0:8-0:13
         2:31 -> a.stag 0:8-0:13
         2:41 -> a.stag 1:26-1:31
         3:22 -> a.stag 3:22-3:23
         3:31 -> a.stag 3:22-3:23
         3:44 -> a.stag 3:22-3:23
         |}])
;;

let%expect_test "hover on code that assigns a free variable" =
  Client.with_server
    ~files:[ "a.stag", "let incr = .<x <- Int.add x 1>.\n" ]
    (fun client ->
       Client.did_open client "a.stag";
       hover client ~line:0 ~character:4;
       [%expect {| 0:4-0:8 ```stag A.incr : <unit>{x : mut int; ..'a} ``` |}])
;;

let%expect_test "hover on the variables of a pattern that failed to check" =
  Client.with_server
    ~files:
      [ ( "a.stag"
        , "type t = Foo of nope\n\
           let f v = match v with | Foo z -> z\n\
           let g n = match n with | Some w -> Int.add w 1 | None -> 0\n\
           let h = g 3\n" )
      ]
    (fun client ->
       Client.did_open client "a.stag";
       [%expect
         {|
         diagnostics a.stag (version 1):
           0:16-0:20 error: Unbound type nope
           3:10-3:11 error: This expression has type int but an expression was expected of type option[int]
         |}];
       List.iter
         [ 1, 29; 1, 34; 2, 30; 2, 43 ]
         ~f:(fun (line, character) ->
           printf "%d:%d " line character;
           hover client ~line ~character);
       [%expect
         {|
         1:29 1:29-1:30 ```stag z : 'a ```
         1:34 1:34-1:35 ```stag z : 'a ```
         2:30 2:30-2:31 ```stag w : int ```
         2:43 2:43-2:44 ```stag w : int ```
         |}])
;;

let%expect_test "document symbols" =
  Client.with_server
    ~files:[ "a.stag", source ]
    (fun client ->
       Client.did_open client "a.stag";
       let rec print indent (symbol : Lsp_types.DocumentSymbol.t) =
         printf
           "%s%s %s%s %d:%d-%d:%d\n"
           indent
           (match symbol.kind with
            | Module -> "module"
            | Function -> "function"
            | Variable -> "variable"
            | Struct -> "struct"
            | Enum -> "enum"
            | Class -> "class"
            | EnumMember -> "enum-member"
            | Field -> "field"
            | Constructor -> "constructor"
            | Event -> "event"
            | _ -> "other")
           symbol.name
           (Option.value_map symbol.detail ~default:"" ~f:(sprintf " (%s)"))
           symbol.range.start.line
           symbol.range.start.character
           symbol.range.end_.line
           symbol.range.end_.character;
         List.iter
           (Option.value symbol.children ~default:[])
           ~f:(print (indent ^ "  "))
       in
       (match
          Client.request
            client
            "textDocument/documentSymbol"
            (Client.text_document client "a.stag")
        with
        | Some (`List symbols) ->
          List.iter symbols ~f:(fun json ->
            print "" (Lsp_types.DocumentSymbol.t_of_yojson json))
        | _ -> print_endline "unexpected");
       [%expect
         {|
      struct point 1:0-1:31
        field x 1:14-1:15
        field y 1:23-1:24
      enum shape 2:0-2:44
        enum-member Circle 2:13-2:19
        enum-member Square 2:29-2:35
      constructor Bad (exception) 3:0-3:23
      module Geometry 5:0-10:3
        function area 6:2-9:33
      variable label 12:0-12:17
      variable code 13:0-13:28
      variable total 14:0-14:49
      variable poly 15:0-15:19
      variable tagged 16:0-16:19
      event area (expect test) 18:0-20:19
      event bench (bench) 22:0-22:25
      |}])
;;

let%expect_test "lifecycle" =
  Client.with_server (fun client ->
    ignore (Client.request client "textDocument/unknown" (`Assoc []) : _ option);
    [%expect {| { "code": -32601, "message": "textDocument/unknown" } |}];
    Option.iter
      (Client.request client "shutdown" `Null)
      ~f:(Client.print_json client);
    ignore (Client.request client "textDocument/hover" (`Assoc []) : _ option);
    [%expect
      {|
      null
      {
        "code": -32602,
        "message": "lsp/src/types.ml.HoverParams.t_of_yojson: the following record elements were undefined: position textDocument"
      }
      |}];
    Client.notify client "exit" `Null;
    print_s [%sexp (Server.exit_code (Client.server client) : int option)];
    [%expect {| (0) |}])
;;

let%expect_test "a project: other files' errors, cross-file definitions" =
  Client.with_server
    ~files:
      [ "stag-project", ""
      ; "math/stag", "(library (name math))"
      ; "math/arith.stag", "let double x = Int.mul 2 x\n"
      ; "math/power.stag", "let quad x = Arith.double (Arith.double x)\n"
      ; "bin/stag", "(executable (name main) (libraries math))"
      ; "bin/main.stag", "let main = print_int (Math.Power.quad 3)\n"
      ]
    (fun client ->
       Client.did_open client "bin/main.stag";
       [%expect {| |}];
       definition client "bin/main.stag" ~line:0 ~character:34;
       hover ~file:"bin/main.stag" client ~line:0 ~character:34;
       [%expect
         {|
        -> math/power.stag 0:4-0:8
        0:22-0:37 ```stag Math.Power.quad : int -> int ```
        |}];
       Client.did_open client "math/arith.stag";
       [%expect {| |}];
       Client.did_change
         client
         "math/arith.stag"
         ~version:2
         "let double x = Int.mul \"2\" x\n";
       [%expect
         {|
        diagnostics math/arith.stag (version 2):
          0:23-0:26 error: This expression has type string but an expression was expected of type int
        |}];
       Client.did_change
         client
         "math/arith.stag"
         ~version:3
         "let double x = Int.mul 2 x\n";
       [%expect {| diagnostics math/arith.stag (version 3): none |}])
;;

let%expect_test "errors in unopened files of the project are published for them"
  =
  Client.with_server
    ~files:
      [ "stag-project", ""
      ; "lib/stag", "(library (name lib))"
      ; "lib/a.stag", "let x = B.y\n"
      ; "lib/b.stag", "let y : int = \"s\"\n"
      ]
    (fun client ->
       Client.did_open client "lib/a.stag";
       [%expect
         {|
        diagnostics lib/b.stag:
          0:14-0:17 error: This expression has type string but an expression was expected of type int
        |}];
       Client.did_change client "lib/a.stag" ~version:2 "let x = 1\n";
       [%expect {| |}])
;;

let%expect_test "documents without a file are checked standalone" =
  Client.with_server (fun client ->
    let open_untitled uri text =
      Client.notify
        client
        "textDocument/didOpen"
        (`Assoc
            [ ( "textDocument"
              , `Assoc
                  [ "uri", `String uri
                  ; "languageId", `String "stag"
                  ; "version", `Int 1
                  ; "text", `String text
                  ] )
            ])
    in
    open_untitled "untitled:Untitled-1" "let y = Int.add 1 \"s\"\n";
    open_untitled "untitled:Untitled-2" "let y = Int.add 1 2\n";
    [%expect
      {|
      diagnostics untitled:Untitled-1 (version 1):
        0:18-0:21 error: This expression has type string but an expression was expected of type int
      |}];
    Option.iter
      (Client.request
         client
         "textDocument/hover"
         (`Assoc
             [ "textDocument", `Assoc [ "uri", `String "untitled:Untitled-2" ]
             ; "position", Client.position ~line:0 ~character:5
             ]))
      ~f:(Client.print_json client);
    [%expect
      {|
      {
        "contents": {
          "kind": "markdown",
          "value": "```stag\nUntitled_2.y : int\n```"
        },
        "range": {
          "end": { "character": 5, "line": 0 },
          "start": { "character": 4, "line": 0 }
        }
      }
      |}])
;;

let calc =
  [ "stag-project", ""
  ; "math/stag", "(library (name math))"
  ; "math/arith.stag", "let square x = Int.mul x x\n"
  ; "math/power.stag", "let fourth x = Arith.square (Arith.square x)\n"
  ; "bin/stag", "(executable (name main) (libraries math))"
  ; "bin/main.stag", "let main = print_int (Math.Power.fourth 2)\n"
  ]
;;

let%expect_test "documents depending on a changed one are checked again" =
  Client.with_server ~files:calc (fun client ->
    Client.did_open client "bin/main.stag";
    Client.did_open client "math/power.stag";
    hover ~file:"bin/main.stag" client ~line:0 ~character:34;
    definition client "bin/main.stag" ~line:0 ~character:34;
    [%expect
      {|
      0:22-0:39 ```stag Math.Power.fourth : int -> int ```
      -> math/power.stag 0:4-0:10
      |}];
    Client.did_change
      ~check:false
      client
      "math/power.stag"
      ~version:2
      "\n\nlet fourth x = Arith.square x\n";
    Client.print_semantic_tokens client "bin/main.stag";
    [%expect
      {|
      0:0+3 Keyword "let"
      0:4+4 Variable (declaration) "main"
      0:9+1 Operator "="
      0:11+9 Variable "print_int"
      0:22+4 Namespace "Math"
      0:27+5 Namespace "Power"
      0:33+6 Variable "fourth"
      0:40+1 Number "2"
      |}];
    Client.check_pending client;
    [%expect {| |}];
    hover ~file:"bin/main.stag" client ~line:0 ~character:34;
    definition client "bin/main.stag" ~line:0 ~character:34;
    [%expect
      {|
      0:22-0:39 ```stag Math.Power.fourth : int -> int ```
      -> math/power.stag 2:4-2:10
      |}];
    Client.did_change
      client
      "math/power.stag"
      ~version:3
      "\n\nlet fourth x = \"s\"\n";
    [%expect
      {|
      diagnostics bin/main.stag (version 1):
        0:21-0:42 error: This expression has type string but an expression was expected of type int
      |}];
    Client.notify
      client
      "textDocument/didClose"
      (Client.text_document client "math/power.stag");
    [%expect {| diagnostics bin/main.stag (version 1): none |}])
;;

let%expect_test "documents not of files nor untitled are not checked" =
  Client.with_server (fun client ->
    let open_ uri text =
      Client.notify
        client
        "textDocument/didOpen"
        (`Assoc
            [ ( "textDocument"
              , `Assoc
                  [ "uri", `String uri
                  ; "languageId", `String "stag"
                  ; "version", `Int 1
                  ; "text", `String text
                  ] )
            ])
    in
    let hover uri =
      Option.iter
        (Client.request
           client
           "textDocument/hover"
           (`Assoc
               [ "textDocument", `Assoc [ "uri", `String uri ]
               ; "position", Client.position ~line:0 ~character:5
               ]))
        ~f:(Client.print_json client)
    in
    open_ "git:/x/a.stag?ref" "let y = Int.add 1 \"s\"\n";
    [%expect {| |}];
    hover "git:/x/a.stag?ref";
    [%expect {| null |}];
    Option.iter
      (Client.request
         client
         "textDocument/documentSymbol"
         (`Assoc
             [ "textDocument", `Assoc [ "uri", `String "git:/x/a.stag?ref" ] ]))
      ~f:(Client.print_json client);
    [%expect
      {|
      [
        {
          "children": [],
          "kind": 13,
          "name": "y",
          "range": {
            "end": { "character": 21, "line": 0 },
            "start": { "character": 0, "line": 0 }
          },
          "selectionRange": {
            "end": { "character": 5, "line": 0 },
            "start": { "character": 4, "line": 0 }
          }
        }
      ]
      |}];
    open_ "untitled:/x/same" "let y = Int.add 1 \"s\"\n";
    open_ "untitled:/y/same" "let y = 1\n";
    [%expect
      {|
      diagnostics untitled:/x/same (version 1):
        0:18-0:21 error: This expression has type string but an expression was expected of type int
      |}];
    hover "untitled:/x/same";
    hover "untitled:/y/same";
    [%expect
      {|
      {
        "contents": { "kind": "markdown", "value": "```stag\nSame.y : int\n```" },
        "range": {
          "end": { "character": 5, "line": 0 },
          "start": { "character": 4, "line": 0 }
        }
      }
      {
        "contents": { "kind": "markdown", "value": "```stag\nSame_2.y : int\n```" },
        "range": {
          "end": { "character": 5, "line": 0 },
          "start": { "character": 4, "line": 0 }
        }
      }
      |}])
;;

let%expect_test "files changed on disk, project errors, closing" =
  Client.with_server
    ~capabilities:
      (`Assoc
          [ ( "workspace"
            , `Assoc
                [ ( "didChangeWatchedFiles"
                  , `Assoc [ "dynamicRegistration", `Bool true ] )
                ] )
          ])
    ~files:
      [ "stag-project", ""
      ; "lib/stag", "(library (name lib))"
      ; "lib/a.stag", "let x = B.y\n"
      ; "lib/b.stag", "let y : int = \"s\"\n"
      ]
    (fun client ->
       [%expect
         {|
         request client/registerCapability
         {
           "registrations": [
             {
               "id": "stag-files",
               "method": "workspace/didChangeWatchedFiles",
               "registerOptions": {
                 "watchers": [
                   { "globPattern": "**/*.stag" },
                   { "globPattern": "**/stag" },
                   { "globPattern": "**/stag-project" }
                 ]
               }
             }
           ]
         }
         |}];
       Client.did_open client "lib/a.stag";
       [%expect
         {|
         diagnostics lib/b.stag:
           0:14-0:17 error: This expression has type string but an expression was expected of type int
         |}];
       let write file data =
         Out_channel.write_all (Client.path client file) ~data;
         Client.notify
           client
           "workspace/didChangeWatchedFiles"
           (`Assoc
               [ ( "changes"
                 , `List
                     [ `Assoc
                         [ "uri", `String (Client.uri client file)
                         ; "type", `Int 2
                         ]
                     ] )
               ])
       in
       write "lib/b.stag" "let y = 1\n";
       [%expect {| diagnostics lib/b.stag: none |}];
       write "lib/b.stag" "let y : int = \"s\"\n";
       [%expect
         {|
         diagnostics lib/b.stag:
           0:14-0:17 error: This expression has type string but an expression was expected of type int
         |}];
       write "lib/stag" "(library (nme lib))";
       [%expect
         {|
         diagnostics lib/stag:
           0:10-0:13 error: Unknown field nme: expected name or libraries
           0:0-0:19 error: The library stanza has no name: add (name ...)
         diagnostics lib/b.stag: none
         |}];
       write "lib/stag" "(library (name lib))";
       [%expect
         {|
         diagnostics lib/b.stag:
           0:14-0:17 error: This expression has type string but an expression was expected of type int
         diagnostics lib/stag: none
         |}];
       Client.notify
         client
         "textDocument/didClose"
         (Client.text_document client "lib/a.stag");
       [%expect {| diagnostics lib/b.stag: none |}])
;;
