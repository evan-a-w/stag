open! Import

let hex t = sprintf "0x%016Lx" (Int64_u.to_int64 (Header.to_int64 t))

let show t =
  print_s
    [%message
      (hex t)
        ~_:(t : Header.t)
        ~kind:(Header.kind t : Header.Kind.t)
        ~size_words:(Header.size_words t : int)
        ~scalar_words:(Header.scalar_words t : int)]
;;

let%expect_test "every kind and colour" =
  List.iter Header.Color.all ~f:(fun color ->
    show (Header.scanned ~size_words:3 ~scalar_words:1 color);
    show (Header.bytes ~length:13 color));
  show (Header.free ~size_words:5);
  [%expect
    {|
    (0x0000000300000100
     (Scanned (size_words 3) (scalar_words 1) (color Unmarked)) (kind Scanned)
     (size_words 3) (scalar_words 1))
    (0x0000000000000d04 (Bytes (length 13) (color Unmarked)) (kind Bytes)
     (size_words 2) (scalar_words 2))
    (0x0000000300000101 (Scanned (size_words 3) (scalar_words 1) (color Marked))
     (kind Scanned) (size_words 3) (scalar_words 1))
    (0x0000000000000d05 (Bytes (length 13) (color Marked)) (kind Bytes)
     (size_words 2) (scalar_words 2))
    (0x0000000300000102
     (Scanned (size_words 3) (scalar_words 1) (color Immortal)) (kind Scanned)
     (size_words 3) (scalar_words 1))
    (0x0000000000000d06 (Bytes (length 13) (color Immortal)) (kind Bytes)
     (size_words 2) (scalar_words 2))
    (0x0000000500000008 (Free (size_words 5) (color Unmarked)) (kind Free)
     (size_words 5) (scalar_words 5))
    |}]
;;

let round_trip t =
  let back = Header.of_int64 (Header.to_int64 t) in
  if not (Header.equal t back) then print_endline "round trip changed it";
  show back
;;

(* Headers are unboxed, so lists hold thunks making them. *)
let%expect_test "extremes" =
  List.iter
    [ (fun () -> Header.scanned ~size_words:0 ~scalar_words:0 Unmarked)
    ; (fun () ->
        Header.scanned
          ~size_words:Header.max_size_words
          ~scalar_words:Header.max_scalar_words
          Immortal)
    ; (fun () ->
        Header.scanned ~size_words:Header.max_size_words ~scalar_words:0 Marked)
    ; (fun () ->
        Header.scanned
          ~size_words:Header.max_scalar_words
          ~scalar_words:Header.max_scalar_words
          Unmarked)
    ; (fun () -> Header.bytes ~length:0 Unmarked)
    ; (fun () -> Header.bytes ~length:1 Unmarked)
    ; (fun () -> Header.bytes ~length:8 Unmarked)
    ; (fun () -> Header.bytes ~length:9 Unmarked)
    ; (fun () -> Header.bytes ~length:Header.max_length Immortal)
    ; (fun () -> Header.free ~size_words:0)
    ; (fun () -> Header.free ~size_words:Header.max_size_words)
    ; (fun () -> Header.handle ~refs:0)
    ; (fun () -> Header.handle ~refs:1)
    ; (fun () -> Header.handle ~refs:(Header.max_size_words - 1))
    ]
    ~f:(fun make -> round_trip (make ()));
  [%expect
    {|
    (0x0000000000000000
     (Scanned (size_words 0) (scalar_words 0) (color Unmarked)) (kind Scanned)
     (size_words 0) (scalar_words 0))
    (0xffffffffffffff02
     (Scanned (size_words 4294967295) (scalar_words 16777215) (color Immortal))
     (kind Scanned) (size_words 4294967295) (scalar_words 16777215))
    (0xffffffff00000001
     (Scanned (size_words 4294967295) (scalar_words 0) (color Marked))
     (kind Scanned) (size_words 4294967295) (scalar_words 0))
    (0x00ffffffffffff00
     (Scanned (size_words 16777215) (scalar_words 16777215) (color Unmarked))
     (kind Scanned) (size_words 16777215) (scalar_words 16777215))
    (0x0000000000000004 (Bytes (length 0) (color Unmarked)) (kind Bytes)
     (size_words 0) (scalar_words 0))
    (0x0000000000000104 (Bytes (length 1) (color Unmarked)) (kind Bytes)
     (size_words 1) (scalar_words 1))
    (0x0000000000000804 (Bytes (length 8) (color Unmarked)) (kind Bytes)
     (size_words 1) (scalar_words 1))
    (0x0000000000000904 (Bytes (length 9) (color Unmarked)) (kind Bytes)
     (size_words 2) (scalar_words 2))
    (0xffffffffffffff06 (Bytes (length 72057594037927935) (color Immortal))
     (kind Bytes) (size_words 9007199254740992) (scalar_words 9007199254740992))
    (0x0000000000000008 (Free (size_words 0) (color Unmarked)) (kind Free)
     (size_words 0) (scalar_words 0))
    (0xffffffff00000008 (Free (size_words 4294967295) (color Unmarked))
     (kind Free) (size_words 4294967295) (scalar_words 4294967295))
    (0x000000010000010c (Handle (size_words 1) (color Unmarked)) (kind Handle)
     (size_words 1) (scalar_words 1))
    (0x000000020000010c (Handle (size_words 2) (color Unmarked)) (kind Handle)
     (size_words 2) (scalar_words 1))
    (0xffffffff0000010c (Handle (size_words 4294967295) (color Unmarked))
     (kind Handle) (size_words 4294967295) (scalar_words 1))
    |}]
;;

let%expect_test "out of range" =
  List.iter
    [ (fun () -> Header.scanned ~size_words:2 ~scalar_words:3 Unmarked)
    ; (fun () -> Header.scanned ~size_words:(-1) ~scalar_words:0 Unmarked)
    ; (fun () -> Header.scanned ~size_words:1 ~scalar_words:(-1) Unmarked)
    ; (fun () ->
        Header.scanned
          ~size_words:(Header.max_size_words + 1)
          ~scalar_words:0
          Unmarked)
    ; (fun () ->
        Header.scanned
          ~size_words:Header.max_size_words
          ~scalar_words:(Header.max_scalar_words + 1)
          Unmarked)
    ; (fun () -> Header.bytes ~length:(-1) Unmarked)
    ; (fun () -> Header.bytes ~length:(Header.max_length + 1) Unmarked)
    ; (fun () -> Header.free ~size_words:(-1))
    ; (fun () -> Header.free ~size_words:(Header.max_size_words + 1))
    ; (fun () -> Header.handle ~refs:(-1))
    ; (fun () -> Header.handle ~refs:Header.max_size_words)
    ]
    ~f:(fun make ->
      match make () with
      | t -> show t
      | exception exn -> print_s [%sexp (exn : exn)]);
  [%expect
    {|
    (Header.scanned (size_words 2) (scalar_words 3))
    (Header.scanned (size_words -1) (scalar_words 0))
    (Header.scanned (size_words 1) (scalar_words -1))
    (Header.scanned (size_words 4294967296) (scalar_words 0))
    (Header.scanned (size_words 4294967295) (scalar_words 16777216))
    (Header.bytes (length -1))
    (Header.bytes (length 72057594037927936))
    (Header.free (size_words -1))
    (Header.free (size_words 4294967296))
    (Header.handle (refs -1))
    (Header.handle (refs 4294967295))
    |}]
;;

let%expect_test "length" =
  let length t =
    print_s
      [%sexp (Or_error.try_with (fun () -> Header.length t) : int Or_error.t)]
  in
  length (Header.bytes ~length:13 Marked);
  length (Header.bytes ~length:0 Unmarked);
  length (Header.scanned ~size_words:2 ~scalar_words:1 Unmarked);
  length (Header.free ~size_words:2);
  [%expect
    {|
    (Ok 13)
    (Ok 0)
    (Error ("Header.length: not Bytes" (kind Scanned)))
    (Error ("Header.length: not Bytes" (kind Free)))
    |}]
;;

let%expect_test "unknown kind and colour" =
  let try_ name f =
    print_s [%message name ~_:(Or_error.try_with f : Sexp.t Or_error.t)]
  in
  (* Kind 4. *)
  let t = Header.of_int64 #0x10L in
  try_ "kind" (fun () -> [%sexp (Header.kind t : Header.Kind.t)]);
  try_ "color" (fun () -> [%sexp (Header.color t : Header.Color.t)]);
  (* Colour 3, of a [Scanned] header. *)
  let t = Header.of_int64 #0x0000000200000103L in
  try_ "kind" (fun () -> [%sexp (Header.kind t : Header.Kind.t)]);
  try_ "color" (fun () -> [%sexp (Header.color t : Header.Color.t)]);
  try_ "with_color" (fun () -> [%sexp (Header.with_color t Marked : Header.t)]);
  [%expect
    {|
    (kind (Error ("Header: unknown kind" (kind 4))))
    (color (Ok Unmarked))
    (kind (Ok Scanned))
    (color (Error ("Header: unknown colour" (color 3))))
    (with_color (Ok (Scanned (size_words 2) (scalar_words 1) (color Marked))))
    |}]
;;

let%expect_test "with_color keeps the rest" =
  List.iter
    [ (fun () ->
        Header.scanned
          ~size_words:Header.max_size_words
          ~scalar_words:Header.max_scalar_words
          Unmarked)
    ; (fun () -> Header.bytes ~length:Header.max_length Unmarked)
    ; (fun () -> Header.free ~size_words:Header.max_size_words)
    ]
    ~f:(fun make ->
      let t = make () in
      List.iter Header.Color.all ~f:(fun color ->
        let recolored = Header.with_color t color in
        print_s
          [%message
            (hex recolored)
              ~_:(recolored : Header.t)
              ~back:
                (Header.equal t (Header.with_color recolored Unmarked) : bool)]));
  [%expect
    {|
    (0xffffffffffffff00
     (Scanned (size_words 4294967295) (scalar_words 16777215) (color Unmarked))
     (back true))
    (0xffffffffffffff01
     (Scanned (size_words 4294967295) (scalar_words 16777215) (color Marked))
     (back true))
    (0xffffffffffffff02
     (Scanned (size_words 4294967295) (scalar_words 16777215) (color Immortal))
     (back true))
    (0xffffffffffffff04 (Bytes (length 72057594037927935) (color Unmarked))
     (back true))
    (0xffffffffffffff05 (Bytes (length 72057594037927935) (color Marked))
     (back true))
    (0xffffffffffffff06 (Bytes (length 72057594037927935) (color Immortal))
     (back true))
    (0xffffffff00000008 (Free (size_words 4294967295) (color Unmarked))
     (back true))
    (0xffffffff00000009 (Free (size_words 4294967295) (color Marked))
     (back true))
    (0xffffffff0000000a (Free (size_words 4294967295) (color Immortal))
     (back true))
    |}]
;;

let%expect_test "load and store at addr - 8" =
  let chunk = Memory.Chunk.create ~bytes:32 ~align:8 in
  let obj = Memory.Addr.add (Memory.Chunk.base chunk) 8 in
  Header.store obj (Header.scanned ~size_words:2 ~scalar_words:1 Unmarked);
  Memory.store_64 obj #42L;
  show (Header.load obj);
  print_s
    [%message
      ""
        ~word_0:
          (hex (Header.of_int64 (Memory.load_64 (Memory.Chunk.base chunk))))
        ~word_1:(Int64_u.to_int64 (Memory.load_64 obj) : int64)];
  Header.store obj (Header.with_color (Header.load obj) Marked);
  show (Header.load obj);
  Memory.Chunk.destroy chunk;
  [%expect
    {|
    (0x0000000200000100
     (Scanned (size_words 2) (scalar_words 1) (color Unmarked)) (kind Scanned)
     (size_words 2) (scalar_words 1))
    ((word_0 0x0000000200000100) (word_1 42))
    (0x0000000200000101 (Scanned (size_words 2) (scalar_words 1) (color Marked))
     (kind Scanned) (size_words 2) (scalar_words 1))
    |}]
;;
