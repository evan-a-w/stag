open! Import

(* A stag checkout of the embedded prelude's sources, under [stag/]. *)
let checkout =
  ("stag/stag-stdlib", "")
  :: List.map
       (Prelude.Sources.embedded.prelude :: Prelude.Sources.embedded.core)
       ~f:(fun { path; contents } ->
         "stag/" ^ Option.value_exn (Stdlib_checkout.checkout_path path), contents)
;;

let stdlib_files = List.tl_exn checkout |> List.map ~f:fst

(* The line and character of the first [pattern] in [contents], plus
   [offset]. *)
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
      opened stag/core/option.stag
      opened stag/core/result.stag
      opened stag/core/sexp.stag
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
      12:12 12:12-12:27 ```stag raise : exn -> 'a ```
      12:22 12:12-12:27 ```stag raise : exn -> 'a ```
      15:34 15:30-15:38 ```stag Core.Int.succ : int -> int ```
      15:34 -> stag/core/int.stag 14:4-14:8
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
        15:30-15:39 error: Unbound value Int.succc
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
        15:30-15:38 error: Unbound value Int.succ
      |}];
    Client.did_change client int ~version:3 (contents int);
    let prelude = "stag/prelude/prelude.stag" in
    Client.did_open client prelude;
    Client.did_change client prelude ~version:2 "let x = Prim.Nope.nope\n";
    [%expect
      {|
      diagnostics stag/core/list.stag (version 3): none
      diagnostics stag/prelude/prelude.stag (version 2):
        0:8-0:22 error: Unbound value Prim.Nope.nope
      |}])
;;

let%expect_test
    "programs in a checkout: prims are the prelude's, definitions in its \
     sources"
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
         1:8 -> stag/prelude/prelude.stag 4:25-4:29
         0:18 no definition
         1:8 no definition
         |}])
;;

let%expect_test "definitions are not in sources that differ from the prelude's" =
  let b =
    "let y = Core.List.length (Core.List.singleton 1)\nlet z = Some 1\n"
  in
  let edited = "stag/core/list.stag" in
  Client.with_server
    ~files:
      (List.map checkout ~f:(fun (file, contents) ->
         file, if String.equal file edited then "\n" ^ contents else contents)
       @ [ "stag/tests/b.stag", b ])
    (fun client ->
       Client.did_open client "stag/tests/b.stag";
       [%expect {| |}];
       let line, character = position b "length" in
       Client.print_definition client "stag/tests/b.stag" ~line ~character;
       let line, character = position b "Some" in
       Client.print_definition client "stag/tests/b.stag" ~line ~character;
       [%expect
         {|
         0:18 -> stag/core/list.stag 12:4-12:10
         1:8 -> stag/prelude/prelude.stag 4:25-4:29
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
      27:12+2 String "\"\""
      28:13+4 Enum_member "Cons"
      29:16+37 String "\"Bool.of_string_exn: invalid bool \\\"\""
      30:16+4 Enum_member "Cons"
      30:22+1 Variable "s"
      30:25+4 Enum_member "Cons"
      30:31+4 String "\"\\\"\""
      30:37+3 Enum_member "Nil"
      |}])
;;
