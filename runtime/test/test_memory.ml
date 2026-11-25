open! Import
open Memory

let with_chunk ?(bytes = 64) ?(align = 8) f =
  let chunk = Chunk.create ~bytes ~align in
  Exn.protect ~f:(fun () -> f chunk) ~finally:(fun () -> Chunk.destroy chunk)
;;

let at chunk offset = Addr.add (Chunk.base chunk) offset

let print_bytes chunk ~pos ~len =
  List.init len ~f:(fun i ->
    sprintf "%02x" (Int8_u.to_int (load_8 (at chunk (pos + i))) land 0xff))
  |> String.concat ~sep:" "
  |> print_endline
;;

let%expect_test "chunks are zeroed and aligned" =
  List.iter [ 1; 8; 64; 4096 ] ~f:(fun align ->
    with_chunk ~bytes:32 ~align (fun chunk ->
      let zero =
        List.for_all (List.range 0 32) ~f:(fun i ->
          Int8_u.to_int (load_8 (at chunk i)) = 0)
      in
      print_s
        [%message
          (align : int)
            ~bytes:(Chunk.bytes chunk : int)
            ~aligned:(Addr.is_aligned (Chunk.base chunk) align : bool)
            (zero : bool)]));
  [%expect
    {|
    ((align 1) (bytes 32) (aligned true) (zero true))
    ((align 8) (bytes 32) (aligned true) (zero true))
    ((align 64) (bytes 32) (aligned true) (zero true))
    ((align 4096) (bytes 32) (aligned true) (zero true))
    |}]
;;

let%expect_test "invalid chunk arguments" =
  List.iter
    [ 0, 1; 0, 3; -1, 8; 8, 0 ]
    ~f:(fun (bytes, align) ->
      match Chunk.create ~bytes ~align with
      | chunk ->
        print_s [%message "ok" ~bytes:(Chunk.bytes chunk : int)];
        Chunk.destroy chunk
      | exception exn -> print_s [%sexp (exn : exn)]);
  [%expect
    {|
    (ok (bytes 0))
    (Memory.Chunk.create (bytes 0) (align 3))
    (Memory.Chunk.create (bytes -1) (align 8))
    (Memory.Chunk.create (bytes 8) (align 0))
    |}]
;;

let%expect_test "contains" =
  with_chunk ~bytes:16 (fun chunk ->
    List.iter [ -1; 0; 15; 16 ] ~f:(fun offset ->
      print_s
        [%message
          (offset : int)
            ~contains:(Chunk.contains chunk (at chunk offset) : bool)]));
  [%expect
    {|
    ((offset -1) (contains false))
    ((offset 0) (contains true))
    ((offset 15) (contains true))
    ((offset 16) (contains false))
    |}]
;;

let%expect_test "addresses" =
  with_chunk ~align:16 (fun chunk ->
    let base = Chunk.base chunk in
    let a = Addr.add base 5 in
    print_s
      [%message
        ""
          ~diff:(Addr.diff a base : int)
          ~back:(Addr.diff base a : int)
          ~align_up_8:(Addr.diff (Addr.align_up a 8) base : int)
          ~align_up_aligned:
            (Addr.diff (Addr.align_up (Addr.add base 8) 8) base : int)
          ~is_aligned_4:(Addr.is_aligned a 4 : bool)
          ~is_aligned_1:(Addr.is_aligned a 1 : bool)
          ~equal:(Addr.equal (Addr.add a (-5)) base : bool)
          ~compare:(Addr.compare base a : int)
          ~compare_back:(Addr.compare a base : int)]);
  [%expect
    {|
    ((diff 5) (back -5) (align_up_8 8) (align_up_aligned 8) (is_aligned_4 false)
     (is_aligned_1 true) (equal true) (compare -1) (compare_back 1))
    |}]
;;

let%expect_test "8 and 16 bits" =
  with_chunk (fun chunk ->
    List.iter [ 0; 1; 127; -128; -1 ] ~f:(fun n ->
      store_8 (at chunk 1) (Int8_u.of_int n);
      printf "%d -> %d: " n (Int8_u.to_int (load_8 (at chunk 1)));
      print_bytes chunk ~pos:0 ~len:3);
    List.iter [ 0; 0x1234; 32767; -32768; -1 ] ~f:(fun n ->
      store_16 (at chunk 3) (Int16_u.of_int n);
      printf "%d -> %d: " n (Int16_u.to_int (load_16 (at chunk 3)));
      print_bytes chunk ~pos:2 ~len:4));
  [%expect
    {|
    0 -> 0: 00 00 00
    1 -> 1: 00 01 00
    127 -> 127: 00 7f 00
    -128 -> -128: 00 80 00
    -1 -> -1: 00 ff 00
    0 -> 0: 00 00 00 00
    4660 -> 4660: 00 34 12 00
    32767 -> 32767: 00 ff 7f 00
    -32768 -> -32768: 00 00 80 00
    -1 -> -1: 00 ff ff 00
    |}]
;;

let%expect_test "32 and 64 bits, unaligned" =
  with_chunk (fun chunk ->
    List.iter
      [ 0l; 0x12345678l; Int32.max_value; Int32.min_value; -1l ]
      ~f:(fun n ->
        store_32 (at chunk 1) (Int32_u.of_int32 n);
        printf
          !"%{Int32} -> %{Int32}: "
          n
          (Int32_u.to_int32 (load_32 (at chunk 1)));
        print_bytes chunk ~pos:0 ~len:6);
    fill_zero (Chunk.base chunk) ~bytes:16;
    List.iter
      [ 0L; 0x0123456789abcdefL; Int64.max_value; Int64.min_value; -1L ]
      ~f:(fun n ->
        store_64 (at chunk 3) (Int64_u.of_int64 n);
        printf
          !"%{Int64} -> %{Int64}: "
          n
          (Int64_u.to_int64 (load_64 (at chunk 3)));
        print_bytes chunk ~pos:2 ~len:10));
  [%expect
    {|
    0 -> 0: 00 00 00 00 00 00
    305419896 -> 305419896: 00 78 56 34 12 00
    2147483647 -> 2147483647: 00 ff ff ff 7f 00
    -2147483648 -> -2147483648: 00 00 00 00 80 00
    -1 -> -1: 00 ff ff ff ff 00
    0 -> 0: 00 00 00 00 00 00 00 00 00 00
    81985529216486895 -> 81985529216486895: 00 ef cd ab 89 67 45 23 01 00
    9223372036854775807 -> 9223372036854775807: 00 ff ff ff ff ff ff ff 7f 00
    -9223372036854775808 -> -9223372036854775808: 00 00 00 00 00 00 00 00 80 00
    -1 -> -1: 00 ff ff ff ff ff ff ff ff 00
    |}]
;;

let%expect_test "floats" =
  with_chunk (fun chunk ->
    List.iter
      [ 0.; -0.; 1.5; -2.25; Float.infinity; Float.nan; 1e300 ]
      ~f:(fun x ->
        store_f64 (at chunk 8) (Float_u.of_float x);
        printf "%h -> %h: " x (Float_u.to_float (load_f64 (at chunk 8)));
        print_bytes chunk ~pos:8 ~len:8);
    List.iter [ 0.; 1.5; -2.25; Float.infinity; 1e30 ] ~f:(fun x ->
      store_f32 (at chunk 1) (Float32_u.of_float (Float_u.of_float x));
      printf
        "%h -> %h: "
        x
        (Float_u.to_float (Float32_u.to_float (load_f32 (at chunk 1))));
      print_bytes chunk ~pos:0 ~len:6));
  [%expect
    {|
    0x0p+0 -> 0x0p+0: 00 00 00 00 00 00 00 00
    -0x0p+0 -> -0x0p+0: 00 00 00 00 00 00 00 80
    0x1.8p+0 -> 0x1.8p+0: 00 00 00 00 00 00 f8 3f
    -0x1.2p+1 -> -0x1.2p+1: 00 00 00 00 00 00 02 c0
    infinity -> infinity: 00 00 00 00 00 00 f0 7f
    nan -> nan: 01 00 00 00 00 00 f8 7f
    0x1.7e43c8800759cp+996 -> 0x1.7e43c8800759cp+996: 9c 75 00 88 3c e4 37 7e
    0x0p+0 -> 0x0p+0: 00 00 00 00 00 00
    0x1.8p+0 -> 0x1.8p+0: 00 00 00 c0 3f 00
    -0x1.2p+1 -> -0x1.2p+1: 00 00 00 10 c0 00
    infinity -> infinity: 00 00 00 80 7f 00
    0x1.93e5939a08ceap+99 -> 0x1.93e594p+99: 00 ca f2 49 71 00
    |}]
;;

let%expect_test "addresses in memory" =
  with_chunk (fun chunk ->
    store_addr (at chunk 8) (at chunk 40);
    print_s
      [%message
        "" ~offset:(Addr.diff (load_addr (at chunk 8)) (Chunk.base chunk) : int)];
    store_addr (at chunk 17) (at chunk 3);
    print_s
      [%message
        ""
          ~unaligned:
            (Addr.diff (load_addr (at chunk 17)) (Chunk.base chunk) : int)]);
  [%expect {|
    (offset 40)
    (unaligned 3)
    |}]
;;

let fill chunk ~len =
  for i = 0 to len - 1 do
    store_8 (at chunk i) (Int8_u.of_int (i + 1))
  done
;;

let%expect_test "fill_zero" =
  with_chunk (fun chunk ->
    fill chunk ~len:12;
    fill_zero (at chunk 3) ~bytes:5;
    print_bytes chunk ~pos:0 ~len:12;
    fill_zero (at chunk 0) ~bytes:0;
    print_bytes chunk ~pos:0 ~len:12);
  [%expect
    {|
    01 02 03 00 00 00 00 00 09 0a 0b 0c
    01 02 03 00 00 00 00 00 09 0a 0b 0c
    |}]
;;

let%expect_test "copy" =
  with_chunk (fun chunk ->
    fill chunk ~len:12;
    copy ~src:(at chunk 0) ~dst:(at chunk 20) ~bytes:4;
    print_bytes chunk ~pos:20 ~len:4;
    copy ~src:(at chunk 0) ~dst:(at chunk 2) ~bytes:6;
    print_bytes chunk ~pos:0 ~len:12;
    fill chunk ~len:12;
    copy ~src:(at chunk 2) ~dst:(at chunk 0) ~bytes:6;
    print_bytes chunk ~pos:0 ~len:12);
  [%expect
    {|
    01 02 03 04
    01 02 01 02 03 04 05 06 09 0a 0b 0c
    03 04 05 06 07 08 07 08 09 0a 0b 0c
    |}]
;;

let%expect_test "chunks are separate" =
  with_chunk (fun a ->
    with_chunk (fun b ->
      store_64 (Chunk.base a) #1L;
      print_s
        [%message
          ""
            ~b_contains_a:(Chunk.contains b (Chunk.base a) : bool)
            ~b:(Int64_u.to_int64 (load_64 (Chunk.base b)) : int64)]));
  [%expect {| ((b_contains_a false) (b 0)) |}]
;;

let%expect_test "contains, far from the chunk" =
  with_chunk ~bytes:16 (fun chunk ->
    let base = Chunk.base chunk in
    let plus_2_62 a = Addr.add (Addr.add a Int.max_value) 1 in
    let minus_2_62 a = Addr.add a Int.min_value in
    List.iter
      [ ("base + 2^62", fun () -> plus_2_62 base)
      ; ("base + 2^63", fun () -> plus_2_62 (plus_2_62 base))
      ; ("base + 2^63 + 5", fun () -> Addr.add (plus_2_62 (plus_2_62 base)) 5)
      ; ("base + 2^63 + 2^62", fun () -> plus_2_62 (plus_2_62 (plus_2_62 base)))
      ; ( "base + 2^64 + 3, which wraps to base + 3"
        , fun () ->
            Addr.add (plus_2_62 (plus_2_62 (plus_2_62 (plus_2_62 base)))) 3 )
      ; ("base - 2^62", fun () -> minus_2_62 base)
      ; ("base - 2^63", fun () -> minus_2_62 (minus_2_62 base))
      ; ("base - 1", fun () -> Addr.add base (-1))
      ; ("base + 15", fun () -> Addr.add base 15)
      ; ("base + 16", fun () -> Addr.add base 16)
      ]
      ~f:(fun (name, addr) ->
        print_s
          [%message name ~contains:(Chunk.contains chunk (addr ()) : bool)]));
  [%expect
    {|
    ("base + 2^62" (contains false))
    ("base + 2^63" (contains false))
    ("base + 2^63 + 5" (contains false))
    ("base + 2^63 + 2^62" (contains false))
    ("base + 2^64 + 3, which wraps to base + 3" (contains true))
    ("base - 2^62" (contains false))
    ("base - 2^63" (contains false))
    ("base - 1" (contains false))
    ("base + 15" (contains true))
    ("base + 16" (contains false))
    |}]
;;

let%expect_test "destroy" =
  let chunk = Chunk.create ~bytes:16 ~align:8 in
  let base = Chunk.base chunk in
  Chunk.destroy chunk;
  print_s
    [%message
      ""
        ~bytes:(Chunk.bytes chunk : int)
        ~contains_base:(Chunk.contains chunk base : bool)];
  (match Chunk.destroy chunk with
   | () -> print_endline "destroyed twice"
   | exception exn -> print_s [%message "second destroy" (exn : exn)]);
  Gc.full_major ();
  print_endline "collected";
  [%expect
    {|
    ((bytes 0) (contains_base false))
    ("second destroy" (exn "Memory.Chunk.destroy: already destroyed"))
    collected
    |}]
;;

let%expect_test "a chunk nothing refers to stays alive until destroyed" =
  let live_before = Chunk.For_testing.live () in
  let base =
    (* The chunk itself is dropped: only an address into it is kept. *)
    let chunk = Chunk.create ~bytes:4096 ~align:8 in
    Chunk.base chunk
  in
  store_64 (Addr.add base 8) #0x1234_5678_9abc_def0L;
  for _ = 1 to 3 do
    Gc.full_major ()
  done;
  (* A fresh allocation would likely reuse freed memory, overwriting it. *)
  let others = List.init 64 ~f:(fun _ -> Chunk.create ~bytes:4096 ~align:8) in
  print_s
    [%message
      ""
        ~still_there:
          (sprintf "%Lx" (Int64_u.to_int64 (load_64 (Addr.add base 8))))
        ~live:(Chunk.For_testing.live () - live_before : int)];
  List.iter others ~f:Chunk.destroy;
  [%expect {| ((still_there 123456789abcdef0) (live 65)) |}]
;;

let%expect_test "alignment, by offset from an aligned base" =
  with_chunk ~bytes:8192 ~align:4096 (fun chunk ->
    let base = Chunk.base chunk in
    List.iter
      [ 0, 8; 1, 8; 7, 8; 8, 8; 9, 16; 1, 4096; 4095, 4096; 4096, 4096; 3, 1 ]
      ~f:(fun (offset, align) ->
        print_s
          [%message
            ""
              (offset : int)
              (align : int)
              ~align_up:
                (Addr.diff (Addr.align_up (at chunk offset) align) base : int)
              ~is_aligned:(Addr.is_aligned (at chunk offset) align : bool)]);
    print_s
      [%message
        ""
          ~is_aligned_64_128:(Addr.is_aligned (at chunk 64) 128 : bool)
          ~is_aligned_128_128:(Addr.is_aligned (at chunk 128) 128 : bool)
          ~compare_equal:(Addr.compare (at chunk 3) (Addr.add base 3) : int)]);
  [%expect
    {|
    ((offset 0) (align 8) (align_up 0) (is_aligned true))
    ((offset 1) (align 8) (align_up 8) (is_aligned false))
    ((offset 7) (align 8) (align_up 8) (is_aligned false))
    ((offset 8) (align 8) (align_up 8) (is_aligned true))
    ((offset 9) (align 16) (align_up 16) (is_aligned false))
    ((offset 1) (align 4096) (align_up 4096) (is_aligned false))
    ((offset 4095) (align 4096) (align_up 4096) (is_aligned false))
    ((offset 4096) (align 4096) (align_up 4096) (is_aligned true))
    ((offset 3) (align 1) (align_up 3) (is_aligned true))
    ((is_aligned_64_128 false) (is_aligned_128_128 true) (compare_equal 0))
    |}]
;;

let%expect_test "copy nothing" =
  with_chunk (fun chunk ->
    fill chunk ~len:8;
    copy ~src:(at chunk 0) ~dst:(at chunk 4) ~bytes:0;
    print_bytes chunk ~pos:0 ~len:8);
  [%expect {| 01 02 03 04 05 06 07 08 |}]
;;

let%expect_test "f32 edge cases" =
  with_chunk (fun chunk ->
    List.iter
      [ -0.; Float.nan; 0x1p-149; 0x1p-127; -0x1.fffffep+127 ]
      ~f:(fun x ->
        store_f32 (at chunk 1) (Float32_u.of_float (Float_u.of_float x));
        printf
          "%h -> %h: "
          x
          (Float_u.to_float (Float32_u.to_float (load_f32 (at chunk 1))));
        print_bytes chunk ~pos:1 ~len:4));
  [%expect
    {|
    -0x0p+0 -> -0x0p+0: 00 00 00 80
    nan -> nan: 00 00 c0 7f
    0x1p-149 -> 0x1p-149: 01 00 00 00
    0x1p-127 -> 0x1p-127: 00 00 40 00
    -0x1.fffffep+127 -> -0x1.fffffep+127: ff ff 7f ff
    |}]
;;

let%expect_test "large chunks are zeroed" =
  let bytes = 1 lsl 20 in
  let dirty = Chunk.create ~bytes ~align:4096 in
  for i = 0 to (bytes / 8) - 1 do
    store_64 (Addr.add (Chunk.base dirty) (i * 8)) (Int64_u.of_int64 (-1L))
  done;
  Chunk.destroy dirty;
  with_chunk ~bytes ~align:4096 (fun chunk ->
    let nonzero = ref 0 in
    for i = 0 to (bytes / 8) - 1 do
      if not (Int64_u.equal (load_64 (at chunk (i * 8))) #0L) then incr nonzero
    done;
    print_s [%message "" ~nonzero:(!nonzero : int)]);
  [%expect {| (nonzero 0) |}]
;;

let%expect_test "null and words" =
  with_chunk (fun chunk ->
    let base = Chunk.base chunk in
    print_s
      [%message
        ""
          ~null_is_null:(Addr.is_null (Addr.null ()) : bool)
          ~base_is_null:(Addr.is_null base : bool)
          ~null_plus_8_is_null:(Addr.is_null (Addr.add (Addr.null ()) 8) : bool)
          ~null_word:(Nativeint_u.to_int (Addr.to_word (Addr.null ())) : int)
          ~round_trip:
            (Addr.equal (Addr.of_word (Addr.to_word base)) base : bool)
          ~word_of_base_plus_8:
            (Nativeint_u.to_int (Addr.to_word (Addr.add base 8))
             - Nativeint_u.to_int (Addr.to_word base)
             : int)
          ~of_word_4096_minus_null:
            (Addr.diff (Addr.of_word #4096n) (Addr.null ()) : int)
          ~null_below_base:(Addr.compare (Addr.null ()) base < 0 : bool)]);
  [%expect
    {|
    ((null_is_null true) (base_is_null false) (null_plus_8_is_null false)
     (null_word 0) (round_trip true) (word_of_base_plus_8 8)
     (of_word_4096_minus_null 4096) (null_below_base true))
    |}]
;;

let%expect_test "words below the bound are not pointers, as unsigned" =
  List.iter [ 0; 4095; 4096; -1 ] ~f:(fun word ->
    let word = Nativeint_u.of_int word in
    print_s
      [%message
        ""
          ~word:(Nativeint_u.to_nativeint word : nativeint)
          ~non_pointer:(Addr.is_non_pointer (Addr.of_word word) : bool)]);
  [%expect
    {|
    ((word 0) (non_pointer true))
    ((word 4095) (non_pointer true))
    ((word 4096) (non_pointer false))
    ((word -1) (non_pointer false))
    |}]
;;
