open! Import

(* A stag checkout of the embedded prelude's sources, under [stag/]. *)
let checkout =
  ("stag/stag-stdlib", "")
  :: List.map
       (Prelude.Sources.embedded.prelude :: Prelude.Sources.embedded.core)
       ~f:(fun { path; contents } ->
         ( "stag/" ^ Option.value_exn (Stdlib_checkout.checkout_path path)
         , contents ))
;;

let stdlib_files = List.tl_exn checkout |> List.map ~f:fst

(* The line and character of the first [pattern] in [contents], plus [offset]. *)
let position ?(offset = 0) contents pattern =
  let index = String.substr_index_exn contents ~pattern + offset in
  let before = String.prefix contents index in
  let line = String.count before ~f:(Char.equal '\n') in
  let character =
    match String.rindex before '\n' with
    | Some newline -> index - newline - 1
    | None -> index
  in
  line, character
;;

let contents file = List.Assoc.find_exn checkout file ~equal:String.equal

let at ?offset client file pattern ~f =
  let line, character = position ?offset (contents file) pattern in
  f client file ~line ~character
;;

let%expect_test "the prelude's sources check cleanly, as the prelude" =
  Client.with_server ~files:checkout (fun client ->
    List.iter stdlib_files ~f:(fun file ->
      Client.did_open client file;
      printf "opened %s\n" file);
    [%expect
      {|
      opened stag/prelude/prelude.stag
      opened stag/core/bool.stag
      opened stag/core/char.stag
      opened stag/core/int.stag
      opened stag/core/list.stag
      opened stag/core/lru.stag
      opened stag/core/map.stag
      opened stag/core/option.stag
      opened stag/core/result.stag
      opened stag/core/sexp.stag
      opened stag/core/sexp_derive.stag
      opened stag/core/sexp_parser.stag
      opened stag/core/sexp_printer.stag
      opened stag/core/string.stag
      |}];
    let prelude = "stag/prelude/prelude.stag" in
    let list = "stag/core/list.stag" in
    at client prelude "Intrinsic.raise" ~f:Client.print_hover;
    at client prelude "Intrinsic.raise" ~offset:10 ~f:Client.print_hover;
    at client list "Int.succ" ~offset:4 ~f:Client.print_hover;
    at client list "Int.succ" ~offset:4 ~f:Client.print_definition;
    at client list "let length" ~offset:4 ~f:Client.print_hover;
    [%expect
      {|
      16:12 16:12-16:27 ```stag raise : exn -> 'a ```
      16:22 16:12-16:27 ```stag raise : exn -> 'a ```
      15:36 15:32-15:40 ```stag Core.Int.succ : int -> int ```
      15:36 -> stag/core/int.stag 14:4-14:8
      11:4 11:4-11:10 ```stag Core.List.length : list['a] -> int ```
      |}])
;;

let%expect_test "errors in the prelude's sources are located in them" =
  Client.with_server ~files:checkout (fun client ->
    let list = "stag/core/list.stag" in
    Client.did_open client list;
    Client.did_change
      client
      list
      ~version:2
      (String.substr_replace_first
         (contents list)
         ~pattern:"Int.succ"
         ~with_:"Int.succc");
    [%expect
      {|
      diagnostics stag/core/list.stag (version 2):
        15:32-15:41 error: Unbound value Int.succc
      |}];
    Client.did_change client list ~version:3 (contents list);
    [%expect {| diagnostics stag/core/list.stag (version 3): none |}];
    let int = "stag/core/int.stag" in
    Client.did_open client int;
    Client.did_change
      client
      int
      ~version:2
      (String.substr_replace_first
         (contents int)
         ~pattern:"let succ"
         ~with_:"let succc");
    [%expect
      {|
      diagnostics stag/core/list.stag (version 3):
        15:32-15:40 error: Unbound value Int.succ
        79:20-79:28 error: Unbound value Int.succ
        103:32-103:40 error: Unbound value Int.succ
        120:12-120:20 error: Unbound value Int.succ
        156:14-156:22 error: Unbound value Int.succ
      diagnostics stag/core/sexp.stag:
        22:28-22:36 error: Unbound value Int.succ
        23:33-23:41 error: Unbound value Int.succ
      diagnostics stag/core/sexp_parser.stag:
        46:31-46:39 error: Unbound value Int.succ
        85:18-85:26 error: Unbound value Int.succ
        169:25-169:33 error: Unbound value Int.succ
        196:15-196:23 error: Unbound value Int.succ
        220:25-220:33 error: Unbound value Int.succ
        228:25-228:33 error: Unbound value Int.succ
        229:26-229:34 error: Unbound value Int.succ
        230:27-230:35 error: Unbound value Int.succ
        252:31-252:39 error: Unbound value Int.succ
        262:24-262:32 error: Unbound value Int.succ
        263:30-263:38 error: Unbound value Int.succ
        276:54-276:62 error: Unbound value Int.succ
        277:24-277:32 error: Unbound value Int.succ
        278:27-278:35 error: Unbound value Int.succ
        290:15-290:23 error: Unbound value Int.succ
        314:30-314:38 error: Unbound value Int.succ
        321:32-321:40 error: Unbound value Int.succ
        322:33-322:41 error: Unbound value Int.succ
        330:15-330:23 error: Unbound value Int.succ
        337:31-337:39 error: Unbound value Int.succ
        338:38-338:46 error: Unbound value Int.succ
        342:38-342:46 error: Unbound value Int.succ
        342:54-342:62 error: Unbound value Int.succ
        354:30-354:38 error: Unbound value Int.succ
        356:18-356:26 error: Unbound value Int.succ
        361:45-361:53 error: Unbound value Int.succ
        365:40-365:48 error: Unbound value Int.succ
        366:29-366:37 error: Unbound value Int.succ
      diagnostics stag/core/sexp_printer.stag:
        46:41-46:49 error: Unbound value Int.succ
        58:22-58:30 error: Unbound value Int.succ
        62:28-62:36 error: Unbound value Int.succ
        154:9-154:17 error: Unbound value Int.succ
        170:63-170:71 error: Unbound value Int.succ
        170:75-170:83 error: Unbound value Int.succ
        181:68-181:76 error: Unbound value Int.succ
        186:77-186:85 error: Unbound value Int.succ
        191:68-191:76 error: Unbound value Int.succ
        205:71-205:79 error: Unbound value Int.succ
      diagnostics stag/core/string.stag:
        57:23-57:31 error: Unbound value Int.succ
        72:24-72:32 error: Unbound value Int.succ
        120:23-120:31 error: Unbound value Int.succ
        132:29-132:37 error: Unbound value Int.succ
        157:15-157:23 error: Unbound value Int.succ
        157:28-157:36 error: Unbound value Int.succ
        158:30-158:38 error: Unbound value Int.succ
        176:30-176:38 error: Unbound value Int.succ
        199:28-199:36 error: Unbound value Int.succ
        204:14-204:22 error: Unbound value Int.succ
        204:27-204:35 error: Unbound value Int.succ
        213:39-213:47 error: Unbound value Int.succ
      |}];
    Client.did_change client int ~version:3 (contents int);
    let prelude = "stag/prelude/prelude.stag" in
    Client.did_open client prelude;
    Client.did_change
      client
      prelude
      ~version:2
      ("let x = Prim.Nope.nope\n" ^ contents prelude);
    [%expect
      {|
      diagnostics stag/core/list.stag (version 3): none
      diagnostics stag/core/sexp.stag: none
      diagnostics stag/core/sexp_parser.stag: none
      diagnostics stag/core/sexp_printer.stag: none
      diagnostics stag/core/string.stag: none
      diagnostics stag/prelude/prelude.stag (version 2):
        0:8-0:22 error: Unbound value Prim.Nope.nope
      |}])
;;

let%expect_test "programs in a checkout: prims are the prelude's, definitions \
                 in its sources"
  =
  let a =
    "let x = Prim.String.length \"a\"\n\
     let y = Core.List.length (Core.List.singleton 1)\n"
  in
  Client.with_server
    ~files:(checkout @ [ "stag/tests/a.stag", a; "elsewhere/b.stag", a ])
    (fun client ->
      Client.did_open client "stag/tests/a.stag";
      Client.did_open client "elsewhere/b.stag";
      [%expect
        {|
         diagnostics stag/tests/a.stag (version 1):
           0:8-0:12 error: Unbound module Prim
         diagnostics elsewhere/b.stag (version 1):
           0:8-0:12 error: Unbound module Prim
         |}];
      let b =
        "let y = Core.List.length (Core.List.singleton 1)\nlet z = Some 1\n"
      in
      Client.did_change client "stag/tests/a.stag" ~version:2 b;
      Client.did_change client "elsewhere/b.stag" ~version:2 b;
      [%expect
        {|
         diagnostics stag/tests/a.stag (version 2): none
         diagnostics elsewhere/b.stag (version 2): none
         |}];
      let definitions file =
        let line, character = position b "length" in
        Client.print_definition client file ~line ~character;
        let line, character = position b "Some" in
        Client.print_definition client file ~line ~character
      in
      definitions "stag/tests/a.stag";
      definitions "elsewhere/b.stag";
      [%expect
        {|
        0:18 -> stag/core/list.stag 11:4-11:10
        1:8 -> stag/prelude/prelude.stag 8:25-8:29
        0:18 no definition
        1:8 no definition
        |}])
;;

let watched_change client file =
  Client.notify
    client
    "workspace/didChangeWatchedFiles"
    (`Assoc
      [ ( "changes"
        , `List
            [ `Assoc [ "uri", `String (Client.uri client file); "type", `Int 2 ]
            ] )
      ])
;;

let%expect_test "programs outside a checkout: definitions are in the server's \
                 checkout, if it matches the embedded prelude"
  =
  let b =
    "let y = Core.List.length (Core.List.singleton 1)\nlet z = Some 1\n"
  in
  let definitions client =
    let line, character = position b "length" in
    Client.print_definition client "elsewhere/b.stag" ~line ~character;
    let line, character = position b "Some" in
    Client.print_definition client "elsewhere/b.stag" ~line ~character
  in
  Client.with_server
    ~files:(checkout @ [ "elsewhere/b.stag", b ])
    ~executable:"stag/_build/default/bin/main.exe"
    (fun client ->
      Client.did_open client "elsewhere/b.stag";
      [%expect {| |}];
      definitions client;
      [%expect
        {|
        0:18 -> stag/core/list.stag 11:4-11:10
        1:8 -> stag/prelude/prelude.stag 8:25-8:29
        |}];
      let list = "stag/core/list.stag" in
      Client.did_open client list;
      Client.did_change client list ~version:2 ("\n" ^ contents list);
      [%expect {| |}];
      definitions client;
      [%expect
        {|
        0:18 no definition
        1:8 -> stag/prelude/prelude.stag 8:25-8:29
        |}];
      Client.did_change client list ~version:3 (contents list);
      [%expect {| |}];
      definitions client;
      [%expect
        {|
        0:18 -> stag/core/list.stag 11:4-11:10
        1:8 -> stag/prelude/prelude.stag 8:25-8:29
        |}]);
  Client.with_server
    ~files:
      (List.map checkout ~f:(fun (file, contents) ->
         ( file
         , if String.equal file "stag/core/list.stag"
           then "\n" ^ contents
           else contents ))
       @ [ "elsewhere/b.stag", b ])
    ~executable:"stag/bin/stag"
    (fun client ->
      Client.did_open client "elsewhere/b.stag";
      [%expect {| |}];
      definitions client;
      [%expect
        {|
        0:18 no definition
        1:8 -> stag/prelude/prelude.stag 8:25-8:29
        |}])
;;

let%expect_test "programs in a checkout see its sources, with unsaved edits, \
                 whatever they are"
  =
  let b =
    "let y = Core.List.length_plus_one Nil\nlet z = Core.List.length Nil\n"
  in
  let list = "stag/core/list.stag" in
  Client.with_server
    ~files:
      (checkout
       @ [ "stag/tests/b.stag", b; "stag/_build/default/tests/b.stag", b ])
    (fun client ->
      Client.did_open client "stag/tests/b.stag";
      Client.did_open client "stag/_build/default/tests/b.stag";
      [%expect
        {|
         diagnostics stag/tests/b.stag (version 1):
           0:8-0:33 error: Unbound value Core.List.length_plus_one
         diagnostics stag/_build/default/tests/b.stag (version 1):
           0:8-0:33 error: Unbound value Core.List.length_plus_one
         |}];
      Client.did_open client list;
      Client.did_change
        client
        list
        ~version:2
        ("let length_plus_one l = Int.succ (length l)\n" ^ contents list);
      [%expect {| diagnostics stag/tests/b.stag (version 1): none |}];
      let line, character = position b "length_plus_one" in
      Client.print_definition client "stag/tests/b.stag" ~line ~character;
      Client.print_hover client "stag/tests/b.stag" ~line ~character;
      let line, character = position b "length Nil" in
      Client.print_definition client "stag/tests/b.stag" ~line ~character;
      [%expect
        {|
         0:18 -> stag/core/list.stag 0:4-0:19
         0:18 0:8-0:33 ```stag prelude.Core.List.length_plus_one : list['_a] -> int ```
         1:18 -> stag/core/list.stag 12:4-12:10
         |}]);
  let c = "let y = Core.Extra.plus_one 1\n" in
  Client.with_server
    ~files:(checkout @ [ "stag/tests/c.stag", c ])
    (fun client ->
      Client.did_open client "stag/tests/c.stag";
      [%expect
        {|
         diagnostics stag/tests/c.stag (version 1):
           0:8-0:18 error: Unbound module Core.Extra
         |}];
      Out_channel.write_all
        (Client.path client "stag/core/extra.stag")
        ~data:"let plus_one = Int.succ\n";
      watched_change client "stag/core/extra.stag";
      [%expect {| diagnostics stag/tests/c.stag (version 1): none |}];
      let line, character = position c "plus_one" in
      Client.print_definition client "stag/tests/c.stag" ~line ~character;
      [%expect {| 0:19 -> stag/core/extra.stag 0:4-0:12 |}];
      Sys_unix.remove (Client.path client "stag/core/extra.stag");
      watched_change client "stag/core/extra.stag";
      [%expect
        {|
         diagnostics stag/tests/c.stag (version 1):
           0:8-0:18 error: Unbound module Core.Extra
         |}];
      Out_channel.write_all
        (Client.path client "stag/core/bad-name.stag")
        ~data:"let x = 1\n";
      watched_change client "stag/core/bad-name.stag";
      [%expect
        {|
         diagnostics stag/core/bad-name.stag:
           0:0-0:0 error: bad-name.stag is not a valid module file name: it should be a letter followed by letters, digits, _ and ', then .stag
         diagnostics stag/tests/c.stag (version 1):
           0:0-0:0 error: This file is not checked: its checkout's prelude has errors
         |}];
      Sys_unix.remove (Client.path client "stag/core/bad-name.stag");
      watched_change client "stag/core/bad-name.stag";
      Out_channel.write_all
        (Client.path client "stag/core/broken.stag")
        ~data:"let x =\n";
      watched_change client "stag/core/broken.stag";
      [%expect
        {|
         diagnostics stag/tests/c.stag (version 1):
           0:8-0:18 error: Unbound module Core.Extra
         diagnostics stag/core/bad-name.stag: none
         diagnostics stag/core/broken.stag:
           1:0-1:0 error: Expected an expression, found end of file
         diagnostics stag/tests/c.stag (version 1):
           0:0-0:0 error: This file is not checked: its checkout's prelude has errors
         |}];
      Out_channel.write_all
        (Client.path client "stag/core/broken.stag")
        ~data:"let x = Int.nope\nlet z = Extra.nope\n";
      watched_change client "stag/core/broken.stag";
      [%expect
        {|
         diagnostics stag/tests/c.stag (version 1):
           0:8-0:18 error: Unbound module Core.Extra
         diagnostics stag/core/broken.stag: none
         |}])
;;

let%expect_test "a symlinked checkout is its canonical path" =
  let b = "let y = Core.List.length Nil\n" in
  Client.with_server
    ~files:(checkout @ [ "elsewhere/b.stag", b ])
    ~symlinks:[ "link", "stag" ]
    ~executable:"link/bin/stag"
    (fun client ->
      Client.did_open client "elsewhere/b.stag";
      let line, character = position b "length" in
      Client.print_definition client "elsewhere/b.stag" ~line ~character;
      [%expect {| 0:18 -> stag/core/list.stag 11:4-11:10 |}];
      let list = "stag/core/list.stag" in
      Client.did_open client "link/core/list.stag";
      Client.did_change
        client
        "link/core/list.stag"
        ~version:2
        ("\n" ^ contents list);
      Client.print_definition client "elsewhere/b.stag" ~line ~character;
      [%expect {| 0:18 no definition |}];
      let line, character =
        position ("\n" ^ contents list) "let length" ~offset:4
      in
      Client.print_hover client "link/core/list.stag" ~line ~character;
      Client.print_definition client "link/core/list.stag" ~line ~character;
      [%expect
        {|
         12:4 12:4-12:10 ```stag Core.List.length : list['a] -> int ```
         12:4 -> link/core/list.stag 12:4-12:10
         |}])
;;

let%expect_test "two checkouts" =
  let program = "let x = Core.List.length_plus_one Nil\n" in
  let other =
    List.map checkout ~f:(fun (file, contents) ->
      ( "other/" ^ String.chop_prefix_exn file ~prefix:"stag/"
      , if String.equal file "stag/core/list.stag"
        then contents ^ "let length_plus_one l = Int.succ (length l)\n"
        else contents ))
  in
  Client.with_server
    ~files:
      (checkout
       @ other
       @ [ "stag/tests/a.stag", program; "other/tests/a.stag", program ])
    (fun client ->
      Client.did_open client "stag/tests/a.stag";
      Client.did_open client "other/tests/a.stag";
      Client.did_open client "other/core/list.stag";
      [%expect
        {|
         diagnostics stag/tests/a.stag (version 1):
           0:8-0:33 error: Unbound value Core.List.length_plus_one
         |}];
      let line, character = position program "length_plus_one" in
      Client.print_definition client "other/tests/a.stag" ~line ~character;
      [%expect {| 0:18 -> other/core/list.stag 384:4-384:19 |}])
;;

let%expect_test "definitions of the prelude's types, constructors and fields" =
  let a =
    "let f (x : option[int]) = x\n\
     let e : Core.Sexp.Parse_error.t = { message = \"\"; offset = 0; line = 0; \
     col = 0 }\n\
     let m = e.message\n\
     let t : Core.Sexp.t = Core.Sexp.Atom \"a\"\n"
  in
  Client.with_server
    ~files:(checkout @ [ "stag/tests/a.stag", a ])
    (fun client ->
      Client.did_open client "stag/tests/a.stag";
      [%expect {| |}];
      List.iter
        [ "option"; "Parse_error.t"; "message ="; ".message"; "Sexp.t"; "Atom" ]
        ~f:(fun pattern ->
          let offset =
            match pattern with
            | "Parse_error.t" | "Sexp.t" -> String.length pattern - 1
            | ".message" -> 1
            | _ -> 0
          in
          let line, character = position a pattern ~offset in
          Client.print_hover client "stag/tests/a.stag" ~line ~character;
          Client.print_definition client "stag/tests/a.stag" ~line ~character);
      [%expect
        {|
        0:11 0:11-0:17 ```stag type option ```
        0:11 -> stag/prelude/prelude.stag 8:5-8:11
        1:30 1:8-1:31 ```stag type Core.Sexp.Parse_error.t ```
        1:30 -> stag/core/sexp.stag 17:7-17:8
        1:36 1:36-1:43 ```stag prelude.Core.Sexp.Parse_error.message : string ```
        1:36 -> stag/core/sexp.stag 17:12-17:19
        2:10 2:10-2:17 ```stag prelude.Core.Sexp.Parse_error.message : string ```
        2:10 -> stag/core/sexp.stag 17:12-17:19
        3:18 3:8-3:19 ```stag type Core.Sexp.t ```
        3:18 -> stag/core/sexp.stag 3:5-3:6
        3:32 3:22-3:36 ```stag prelude.Core.Sexp.Atom : Core.Sexp.t ```
        3:32 -> stag/core/sexp.stag 3:9-3:13
        |}])
;;

let%expect_test "semantic tokens of a source of the prelude" =
  Client.with_server ~files:checkout (fun client ->
    Client.did_open client "stag/core/bool.stag";
    Client.print_semantic_tokens client "stag/core/bool.stag";
    [%expect
      {|
      0:0+3 Keyword "let"
      0:4+3 Function (declaration) "not"
      0:8+1 Operator "="
      0:10+4 Namespace "Prim"
      0:15+4 Namespace "Bool"
      0:20+3 Function "not"
      1:0+3 Keyword "let"
      1:4+5 Function (declaration) "equal"
      1:10+1 Operator "="
      1:12+4 Namespace "Prim"
      1:17+4 Namespace "Bool"
      1:22+5 Function "equal"
      3:0+3 Keyword "let"
      3:4+7 Function (declaration) "compare"
      3:12+1 Parameter (declaration) "a"
      3:14+1 Parameter (declaration) "b"
      3:16+1 Operator "="
      4:2+5 Keyword "match"
      4:9+1 Variable "a"
      4:12+1 Variable "b"
      4:15+4 Keyword "with"
      5:2+1 Operator "|"
      5:4+5 Keyword "false"
      5:11+4 Keyword "true"
      5:16+2 Operator "->"
      5:19+2 Number "-1"
      6:2+1 Operator "|"
      6:4+4 Keyword "true"
      6:10+5 Keyword "false"
      6:16+2 Operator "->"
      6:19+1 Number "1"
      7:2+1 Operator "|"
      7:6+2 Operator "->"
      7:9+1 Number "0"
      9:0+3 Keyword "let"
      9:4+9 Function (declaration) "to_string"
      9:14+1 Parameter (declaration) "b"
      9:16+1 Operator "="
      10:2+5 Keyword "match"
      10:8+1 Variable "b"
      10:10+4 Keyword "with"
      11:2+1 Operator "|"
      11:4+4 Keyword "true"
      11:9+2 Operator "->"
      11:12+6 String "\"true\""
      12:2+1 Operator "|"
      12:4+5 Keyword "false"
      12:10+2 Operator "->"
      12:13+7 String "\"false\""
      14:0+3 Keyword "let"
      14:4+9 Function (declaration) "of_string"
      14:14+1 Parameter (declaration) "s"
      14:16+1 Operator "="
      15:2+5 Keyword "match"
      15:8+1 Variable "s"
      15:10+4 Keyword "with"
      16:2+1 Operator "|"
      16:4+6 String "\"true\""
      16:11+2 Operator "->"
      16:14+4 Enum_member "Some"
      16:19+4 Keyword "true"
      17:2+1 Operator "|"
      17:4+7 String "\"false\""
      17:12+2 Operator "->"
      17:15+4 Enum_member "Some"
      17:20+5 Keyword "false"
      18:2+1 Operator "|"
      18:6+2 Operator "->"
      18:9+4 Enum_member "None"
      20:0+3 Keyword "let"
      20:4+13 Function (declaration) "of_string_exn"
      20:18+1 Parameter (declaration) "s"
      20:20+1 Operator "="
      21:2+5 Keyword "match"
      21:8+9 Function "of_string"
      21:18+1 Variable "s"
      21:20+4 Keyword "with"
      22:2+1 Operator "|"
      22:4+4 Enum_member "Some"
      22:9+1 Variable (declaration) "b"
      22:11+2 Operator "->"
      22:14+1 Variable "b"
      23:2+1 Operator "|"
      23:4+4 Enum_member "None"
      23:9+2 Operator "->"
      24:4+5 Function "raise"
      25:7+7 Enum_member "Failure"
      26:10+6 Namespace "String"
      26:17+6 Function "concat"
      26:24+2 String "\"\""
      26:28+37 String "\"Bool.of_string_exn: invalid bool \\\"\""
      26:67+1 Variable "s"
      26:70+4 String "\"\\\"\""
      |}])
;;

let%expect_test "stag, stag-build and the server agree on the prelude a \
                 program in a checkout sees, and a project's stanzas are \
                 inputs of its checks"
  =
  let main = "let x = Int.add only_in_checkout Lib.V.v\n" in
  Client.with_server
    ~files:
      (List.map checkout ~f:(fun (file, contents) ->
         ( file
         , if String.equal file "stag/prelude/prelude.stag"
           then contents ^ "let only_in_checkout = 1\n"
           else contents ))
       @ [ "stag/proj/stag-project", ""
         ; "stag/proj/app/stag", "(executable (name app))"
         ; "stag/proj/app/main.stag", main
         ; "stag/proj/lib/stag", "(library (name lib))"
         ; "stag/proj/lib/v.stag", "let v = 1\n"
         ; "elsewhere/b.stag", main
         ])
    (fun client ->
      List.iter
        [ "stag/proj/app/main.stag"; "elsewhere/b.stag" ]
        ~f:(fun file ->
          print_s
            [%sexp
              (file : string)
              , (Pipeline_config.Prelude_choice.of_paths
                   [ Client.path client file ]
                 |> function
                 | Pipeline_config.Prelude_choice.Embedded -> "embedded"
                 | Checkout _ -> "checkout"
                 : string)]);
      [%expect
        {|
        (stag/proj/app/main.stag checkout)
        (elsewhere/b.stag embedded)
        |}];
      let build () =
        match Project.load ~root:(Client.path client "stag/proj") () with
        | Error diagnostics -> print_s [%sexp (diagnostics : Diagnostic.t list)]
        | Ok project ->
          let target = Option.value_exn (Project.find project "app") in
          (match
             Program.check
               ~output:ignore
               ~limits:Limits.unlimited
               project
               target
             |> Result.map_error ~f:List.return
             |> Result.bind ~f:(fun (_, checked) ->
               Stag_build.Pipeline.Checked.to_result checked)
           with
           | Ok (_ : Stag_driver.Driver.t) -> print_endline "checks"
           | Error diagnostics ->
             List.iter diagnostics ~f:(fun diagnostic ->
               print_string
                 (Diagnostic.to_string_hum ~contents:(Fn.const None) diagnostic)))
      in
      build ();
      [%expect
        {|
        File "app/main.stag", line 1, characters 33-36:
        Error: Unbound module Lib
        |}];
      Client.did_open client "stag/proj/app/main.stag";
      Client.did_open client "elsewhere/b.stag";
      [%expect
        {|
        diagnostics stag/proj/app/main.stag (version 1):
          0:33-0:36 error: Unbound module Lib
        diagnostics elsewhere/b.stag (version 1):
          0:16-0:32 error: Unbound value only_in_checkout
          0:33-0:36 error: Unbound module Lib
        |}];
      let checked =
        Stag_lsp.Checker.check
          ~limits:Limits.unlimited
          ~contents:(Fn.const None)
          (Client.path client "stag/proj/app/main.stag")
      in
      List.iter checked.depends_on ~f:(fun path ->
        print_endline (Client.relativize client path));
      [%expect
        {|
        stag/proj/stag-project
        stag/proj/app/stag
        stag/proj/lib/stag
        stag/prelude/prelude.stag
        stag/core/bool.stag
        stag/core/char.stag
        stag/core/int.stag
        stag/core/list.stag
        stag/core/lru.stag
        stag/core/map.stag
        stag/core/option.stag
        stag/core/result.stag
        stag/core/sexp.stag
        stag/core/sexp_derive.stag
        stag/core/sexp_parser.stag
        stag/core/sexp_printer.stag
        stag/core/string.stag
        |}];
      let stanza = "(executable (name app) (libraries lib))" in
      Out_channel.write_all
        (Client.path client "stag/proj/app/stag")
        ~data:stanza;
      build ();
      [%expect {| checks |}])
;;
