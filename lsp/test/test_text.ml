open! Import

let position line character = Lsp_types.Position.create ~line ~character

let show_position (position : Lsp_types.Position.t) =
  sprintf "%d:%d" position.line position.character
;;

let%expect_test "offsets and UTF-16 positions" =
  (* é is 2 bytes and 1 unit, 😊 4 bytes and 2 units. *)
  let text = Text.create "let s = \"é😊x\" (* ü *)\r\nnext\n" in
  List.iter [ 0; 9; 11; 15; 16; 17; 22; 26; 28; 34; 99 ] ~f:(fun offset ->
    let position = Text.position_of_offset text offset in
    printf
      "%2d -> %s -> %d\n"
      offset
      (show_position position)
      (Text.offset_of_position text position));
  [%expect
    {|
     0 -> 0:0 -> 0
     9 -> 0:9 -> 9
    11 -> 0:10 -> 11
    15 -> 0:12 -> 15
    16 -> 0:13 -> 16
    17 -> 0:14 -> 17
    22 -> 0:18 -> 21
    26 -> 0:22 -> 26
    28 -> 1:0 -> 28
    34 -> 2:0 -> 33
    99 -> 2:0 -> 33
    |}];
  (* Inside a sequence or pair, past a line's end, or past the end. *)
  List.iter
    [ position 0 10; position 0 11; position 0 99; position 1 2; position 7 0 ]
    ~f:(fun position ->
      printf
        "%s -> %d\n"
        (show_position position)
        (Text.offset_of_position text position));
  [%expect
    {|
    0:10 -> 11
    0:11 -> 11
    0:99 -> 26
    1:2 -> 30
    7:0 -> 33
    |}];
  printf "%d\n" (Text.utf16_length text ~pos:8 ~len:9);
  [%expect {| 6 |}];
  print_s [%sexp (Text.split_lines text ~start:16 ~stop:32 : (int * int) list)];
  [%expect {| ((16 26) (28 32)) |}]
;;

let%expect_test "invalid UTF-8 counts a unit per byte" =
  let text = Text.create "a\xffb" in
  printf
    "%s %d\n"
    (show_position (Text.position_of_offset text 2))
    (Text.offset_of_position text (position 0 2));
  [%expect {| 0:2 2 |}]
;;

let%expect_test "changes" =
  let change ?range text =
    Lsp_types.TextDocumentContentChangeEvent.create ?range ~text ()
  in
  let range (l1, c1) (l2, c2) =
    Lsp_types.Range.create ~start:(position l1 c1) ~end_:(position l2 c2)
  in
  let text = Text.create "let x = \"😊\"\nlet y = 2\n" in
  let show text = print_endline (String.escaped (Text.contents text)) in
  let text =
    Text.apply_change text (change ~range:(range (0, 11) (0, 11)) "é")
  in
  show text;
  let text = Text.apply_change text (change ~range:(range (0, 4) (1, 4)) "") in
  show text;
  let text = Text.apply_change text (change "whole") in
  show text;
  [%expect
    {|
    let x = \"\240\159\152\138\195\169\"\nlet y = 2\n
    let y = 2\n
    whole
    |}];
  Text.full_range (Text.create "a\n😊")
  |> Lsp_types.Range.yojson_of_t
  |> Yojson.Safe.to_string
  |> print_endline;
  [%expect
    {| {"end":{"character":2,"line":1},"start":{"character":0,"line":0}} |}]
;;
