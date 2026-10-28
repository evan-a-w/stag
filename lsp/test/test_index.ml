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
