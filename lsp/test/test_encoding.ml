open! Import

let source = "let s = (\"é😊\", Int.add 1 2)\n"

let show ?capabilities ~add () =
  Client.with_server
    ?capabilities
    ~files:[ "a.stag", source ]
    (fun client ->
       print_s [%sexp (Client.encoding client : Text.Encoding.t)];
       Client.did_open client "a.stag";
       Client.print_semantic_tokens client "a.stag";
       Option.iter
         (Client.request
            client
            "textDocument/hover"
            (Client.text_document
               client
               "a.stag"
               ~extra:[ "position", Client.position ~line:0 ~character:add ]))
         ~f:(Client.print_json client))
;;

let%expect_test "UTF-16 positions, by default" =
  show ~add:21 ();
  [%expect
    {|
    Utf_16
    0:0+3 Keyword "let"
    0:4+1 Variable (declaration) "s"
    0:6+1 Operator "="
    0:9+5 String "\"\195\169\240\159\152\138\""
    0:16+3 Namespace "Int"
    0:20+3 Function "add"
    0:24+1 Number "1"
    0:26+1 Number "2"
    {
      "contents": {
        "kind": "markdown",
        "value": "```stag\nInt.add : int -> int -> int\n```"
      },
      "range": {
        "end": { "character": 23, "line": 0 },
        "start": { "character": 16, "line": 0 }
      }
    }
    |}];
  show
    ~capabilities:
      (`Assoc
          [ ( "general"
            , `Assoc [ "positionEncodings", `List [ `String "utf-16" ] ] )
          ])
    ~add:21
    ();
  [%expect
    {|
    Utf_16
    0:0+3 Keyword "let"
    0:4+1 Variable (declaration) "s"
    0:6+1 Operator "="
    0:9+5 String "\"\195\169\240\159\152\138\""
    0:16+3 Namespace "Int"
    0:20+3 Function "add"
    0:24+1 Number "1"
    0:26+1 Number "2"
    {
      "contents": {
        "kind": "markdown",
        "value": "```stag\nInt.add : int -> int -> int\n```"
      },
      "range": {
        "end": { "character": 23, "line": 0 },
        "start": { "character": 16, "line": 0 }
      }
    }
    |}]
;;

let%expect_test "UTF-8 positions, if the client offers them" =
  show
    ~capabilities:
      (`Assoc
          [ ( "general"
            , `Assoc
                [ ( "positionEncodings"
                  , `List [ `String "utf-16"; `String "utf-8" ] )
                ] )
          ])
    ~add:24
    ();
  [%expect
    {|
    Utf_8
    0:0+3 Keyword "let"
    0:4+1 Variable (declaration) "s"
    0:6+1 Operator "="
    0:9+8 String "\"\195\169\240\159\152\138\""
    0:19+3 Namespace "Int"
    0:23+3 Function "add"
    0:27+1 Number "1"
    0:29+1 Number "2"
    {
      "contents": {
        "kind": "markdown",
        "value": "```stag\nInt.add : int -> int -> int\n```"
      },
      "range": {
        "end": { "character": 26, "line": 0 },
        "start": { "character": 19, "line": 0 }
      }
    }
    |}]
;;

let format client =
  match
    Client.request
      client
      "textDocument/formatting"
      (Client.text_document
         client
         "a.stag"
         ~extra:
           [ "options", `Assoc [ "tabSize", `Int 2; "insertSpaces", `Bool true ]
           ])
  with
  | Some (`List edits) ->
    List.iter edits ~f:(fun edit ->
      let { Lsp_types.TextEdit.newText; range = _ } =
        Lsp_types.TextEdit.t_of_yojson edit
      in
      print_endline (String.escaped newText))
  | json ->
    print_s [%sexp (Option.map json ~f:Yojson.Safe.to_string : string option)]
;;

let%expect_test "formatting keeps CRLF line endings" =
  List.iter
    [ "let x =   1\r\nlet y = \"a\r\nb\"\r\n"
    ; "let x =   1\nlet y = 2\n"
    ; "let x = 1\r\nlet y = 2\r\n"
    ]
    ~f:(fun contents ->
      Client.with_server
        ~files:[ "a.stag", contents ]
        (fun client ->
           Client.did_open client "a.stag";
           format client));
  [%expect
    {|
    let x = 1\r\nlet y = \"a\\r\\nb\"\r\n
    let x = 1\nlet y = 2\n
    |}]
;;
