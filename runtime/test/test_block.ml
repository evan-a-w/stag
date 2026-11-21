open! Import
open Layout_helpers

let show_block name block ~lengths =
  printf
    "%s: prefix at %d, elements at %d%s\n"
    name
    (Block.prefix_offset block)
    (Block.elements_offset block)
    (if Block.has_hidden_length block then ", hidden length" else "");
  List.iter lengths ~f:(fun length ->
    let header = Block.header block ~length Unmarked in
    let recovered =
      match Block.length block header with
      | n -> Int.to_string n
      | exception _ -> "raises"
    in
    print_s
      [%message
        "" (length : int) ~_:(header : Header.t) ~length:(recovered : string)])
;;

let fixed shape = Block.fixed (layout shape)

let variable prefix elt =
  Block.variable ~prefix:(layout prefix) ~elt:(layout elt)
;;

let%expect_test "fixed blocks" =
  show_block "mixed" (fixed (struct_ [ i64; ptr; i8 ])) ~lengths:[ 0 ];
  show_block "pointer-free" (fixed (struct_ [ i8; i8; i8 ])) ~lengths:[ 0 ];
  show_block "pointers only" (fixed (struct_ [ ptr; ptr ])) ~lengths:[ 0 ];
  show_block "empty" (fixed unit) ~lengths:[ 0 ];
  [%expect
    {|
    mixed: prefix at 0, elements at 16
    ((length 0) (Scanned (size_words 3) (scalar_words 2) (color Unmarked))
     (length 0))
    pointer-free: prefix at 0, elements at 3
    ((length 0) (Bytes (length 3) (color Unmarked)) (length 0))
    pointers only: prefix at 0, elements at 0
    ((length 0) (Scanned (size_words 2) (scalar_words 0) (color Unmarked))
     (length 0))
    empty: prefix at 0, elements at 0
    ((length 0) (Bytes (length 0) (color Unmarked)) (length 0))
    |}]
;;

let%expect_test "a fixed block has no elements" =
  let block = fixed (struct_ [ i64 ]) in
  print_s [%sexp (Block.element block : Layout.Element.t option)];
  Expect_test_helpers_core.require_does_raise (fun () ->
    ignore (Header.to_int64 (Block.header block ~length:1 Unmarked) : int64_u));
  [%expect {|
    ()
    ("Block.header: fixed" (length 1))
    |}]
;;

let%expect_test "strings: bytes, with an exact length" =
  show_block "string" (variable unit i8) ~lengths:[ 0; 1; 7; 8; 13 ];
  [%expect
    {|
    string: prefix at 0, elements at 0
    ((length 0) (Bytes (length 0) (color Unmarked)) (length 0))
    ((length 1) (Bytes (length 1) (color Unmarked)) (length 1))
    ((length 7) (Bytes (length 7) (color Unmarked)) (length 7))
    ((length 8) (Bytes (length 8) (color Unmarked)) (length 8))
    ((length 13) (Bytes (length 13) (color Unmarked)) (length 13))
    |}]
;;

let%expect_test "pointer-free elements after a pointer-free prefix" =
  show_block
    "i32s after an i8"
    (variable (struct_ [ i8 ]) i32)
    ~lengths:[ 0; 3 ];
  [%expect
    {|
    i32s after an i8: prefix at 0, elements at 4
    ((length 0) (Bytes (length 4) (color Unmarked)) (length 0))
    ((length 3) (Bytes (length 16) (color Unmarked)) (length 3))
    |}]
;;

let%expect_test "elements with pointers: from the pointer words" =
  show_block "ptrs" (variable unit ptr) ~lengths:[ 0; 1; 5 ];
  show_block
    "mixed after mixed"
    (variable (struct_ [ i64; ptr ]) (struct_ [ i32; ptr ]))
    ~lengths:[ 0; 1; 3 ];
  show_block
    "mixed after pointer-free"
    (variable (struct_ [ i8 ]) (struct_ [ ptr; ptr; i8 ]))
    ~lengths:[ 0; 2 ];
  [%expect
    {|
    ptrs: prefix at 0, elements at 0
    ((length 0) (Scanned (size_words 0) (scalar_words 0) (color Unmarked))
     (length 0))
    ((length 1) (Scanned (size_words 1) (scalar_words 0) (color Unmarked))
     (length 1))
    ((length 5) (Scanned (size_words 5) (scalar_words 0) (color Unmarked))
     (length 5))
    mixed after mixed: prefix at 0, elements at 8
    ((length 0) (Scanned (size_words 2) (scalar_words 1) (color Unmarked))
     (length 0))
    ((length 1) (Scanned (size_words 4) (scalar_words 2) (color Unmarked))
     (length 1))
    ((length 3) (Scanned (size_words 7) (scalar_words 3) (color Unmarked))
     (length 3))
    mixed after pointer-free: prefix at 0, elements at 1
    ((length 0) (Scanned (size_words 1) (scalar_words 1) (color Unmarked))
     (length 0))
    ((length 2) (Scanned (size_words 5) (scalar_words 1) (color Unmarked))
     (length 2))
    |}]
;;

let%expect_test "hidden length: pointer-free elements after pointers" =
  show_block "i8s after a ptr" (variable (struct_ [ ptr ]) i8) ~lengths:[ 0; 5 ];
  show_block
    "i64s after a ptr and an i32"
    (variable (struct_ [ ptr; i32 ]) i64)
    ~lengths:[ 0; 2 ];
  [%expect
    {|
    i8s after a ptr: prefix at 8, elements at 8, hidden length
    ((length 0) (Scanned (size_words 2) (scalar_words 1) (color Unmarked))
     (length raises))
    ((length 5) (Scanned (size_words 3) (scalar_words 2) (color Unmarked))
     (length raises))
    i64s after a ptr and an i32: prefix at 0, elements at 8
    ((length 0) (Scanned (size_words 2) (scalar_words 1) (color Unmarked))
     (length 0))
    ((length 2) (Scanned (size_words 4) (scalar_words 3) (color Unmarked))
     (length 2))
    |}]
;;

let%expect_test "hidden length: zero-size elements" =
  show_block "units" (variable unit unit) ~lengths:[ 0; 4 ];
  show_block
    "units after a ptr"
    (variable (struct_ [ ptr ]) unit)
    ~lengths:[ 3 ];
  [%expect
    {|
    units: prefix at 8, elements at 8, hidden length
    ((length 0) (Bytes (length 8) (color Unmarked)) (length raises))
    ((length 4) (Bytes (length 8) (color Unmarked)) (length raises))
    units after a ptr: prefix at 8, elements at 8, hidden length
    ((length 3) (Scanned (size_words 2) (scalar_words 1) (color Unmarked))
     (length raises))
    |}]
;;

let%expect_test "length of a header of another kind" =
  let strings = variable unit i8 in
  let ptrs = variable unit ptr in
  Expect_test_helpers_core.require_does_raise (fun () ->
    Block.length strings (Block.header ptrs ~length:1 Unmarked));
  Expect_test_helpers_core.require_does_raise (fun () ->
    Block.length ptrs (Header.free ~size_words:2));
  [%expect
    {|
    ("Block.length: wrong kind"
      (kind     Scanned)
      (expected Bytes))
    ("Block.length: wrong kind"
      (kind     Free)
      (expected Scanned))
    |}]
;;
