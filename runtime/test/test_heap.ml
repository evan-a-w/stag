open! Import
module Addr = Memory.Addr

(* 256-byte pages, 4 to an arena: a 24-byte descriptor, then slots of 16, 24 and
   40 bytes. *)
let small =
  { Heap.Config.page_bytes = 256
  ; arena_bytes = 1024
  ; size_classes = [ 1; 2; 4 ]
  }
;;

let scanned ?(color = Header.Color.Unmarked) ?(scalar = 0) words =
  Header.scanned ~size_words:words ~scalar_words:scalar color
;;

let set_color block color =
  Header.store block (Header.with_color (Header.load block) color)
;;

let where heap addr =
  if Addr.is_null addr
  then [%sexp "null"]
  else (
    match Heap.For_testing.locate heap addr with
    | Some (region, offset) -> [%sexp (region : int), (offset : int)]
    | None -> [%sexp "outside"])
;;

(* [n] blocks, the [i]th of [header i]. *)
let alloc_n heap n ~header : Addr.t array =
  let blocks = Array.create ~len:n (Addr.null ()) in
  for i = 0 to n - 1 do
    blocks.(i) <- Heap.alloc heap (header i)
  done;
  blocks
;;

(* In order, unlike [List.init]. *)
let sequentially n ~f =
  List.rev (List.fold (List.range 0 n) ~init:[] ~f:(fun acc i -> f i :: acc))
;;

let mark_if blocks ~f =
  for i = 0 to Array.length blocks - 1 do
    if f i then set_color blocks.(i) Marked
  done
;;

let%expect_test "invalid configs" =
  let show config =
    print_s
      [%sexp
        (Or_error.try_with (fun () -> ignore (Heap.create config : Heap.t))
         : unit Or_error.t)]
  in
  show { small with page_bytes = 255 };
  show { small with page_bytes = 32 };
  show { small with size_classes = [] };
  show { small with size_classes = [ 0; 1 ] };
  show { small with size_classes = [ 2; 2 ] };
  show { small with size_classes = [ 1; 29 ] };
  (* The largest that fits: 24 + (28 + 1) * 8 = 256. *)
  show { small with size_classes = [ 1; 28 ] };
  [%expect
    {|
    (Error
     ("Heap.create: invalid config"
      (config ((page_bytes 255) (arena_bytes 1024) (size_classes (1 2 4))))))
    (Error
     ("Heap.create: invalid config"
      (config ((page_bytes 32) (arena_bytes 1024) (size_classes (1 2 4))))))
    (Error
     ("Heap.create: invalid config"
      (config ((page_bytes 256) (arena_bytes 1024) (size_classes ())))))
    (Error
     ("Heap.create: invalid config"
      (config ((page_bytes 256) (arena_bytes 1024) (size_classes (0 1))))))
    (Error
     ("Heap.create: invalid config"
      (config ((page_bytes 256) (arena_bytes 1024) (size_classes (2 2))))))
    (Error
     ("Heap.create: invalid config"
      (config ((page_bytes 256) (arena_bytes 1024) (size_classes (1 29))))))
    (Ok ())
    |}]
;;

let%expect_test "allocating from each class" =
  let heap = Heap.create small in
  let blocks =
    List.map [ 1; 2; 3; 4; 1 ] ~f:(fun words ->
      let block = Heap.alloc heap (scanned words) in
      words, where heap block)
  in
  print_s [%sexp (blocks : (int * Sexp.t) list)];
  print_s [%sexp (heap : Heap.t)];
  print_s
    [%message
      ""
        ~regions:(Heap.For_testing.regions heap : int)
        ~pages:(Heap.For_testing.pages heap : int)];
  [%expect
    {|
    ((1 (0 32)) (2 (1 32)) (3 (2 32)) (4 (2 72)) (1 (0 48)))
    ((Page
      ((index 0) (words 1)
       (slots
        ((32 (Scanned (size_words 1) (scalar_words 0) (color Unmarked)))
         (48 (Scanned (size_words 1) (scalar_words 0) (color Unmarked)))
         (64 (Free (size_words 1) (color Unmarked)))
         (80 (Free (size_words 1) (color Unmarked)))
         (96 (Free (size_words 1) (color Unmarked)))
         (112 (Free (size_words 1) (color Unmarked)))
         (128 (Free (size_words 1) (color Unmarked)))
         (144 (Free (size_words 1) (color Unmarked)))
         (160 (Free (size_words 1) (color Unmarked)))
         (176 (Free (size_words 1) (color Unmarked)))
         (192 (Free (size_words 1) (color Unmarked)))
         (208 (Free (size_words 1) (color Unmarked)))
         (224 (Free (size_words 1) (color Unmarked)))
         (240 (Free (size_words 1) (color Unmarked)))))))
     (Page
      ((index 1) (words 2)
       (slots
        ((32 (Scanned (size_words 2) (scalar_words 0) (color Unmarked)))
         (56 (Free (size_words 2) (color Unmarked)))
         (80 (Free (size_words 2) (color Unmarked)))
         (104 (Free (size_words 2) (color Unmarked)))
         (128 (Free (size_words 2) (color Unmarked)))
         (152 (Free (size_words 2) (color Unmarked)))
         (176 (Free (size_words 2) (color Unmarked)))
         (200 (Free (size_words 2) (color Unmarked)))
         (224 (Free (size_words 2) (color Unmarked)))))))
     (Page
      ((index 2) (words 4)
       (slots
        ((32 (Scanned (size_words 3) (scalar_words 0) (color Unmarked)))
         (72 (Scanned (size_words 4) (scalar_words 0) (color Unmarked)))
         (112 (Free (size_words 4) (color Unmarked)))
         (152 (Free (size_words 4) (color Unmarked)))
         (192 (Free (size_words 4) (color Unmarked))))))))
    ((regions 3) (pages 3))
    |}]
;;

let%expect_test "a page fills, then another is made" =
  let heap = Heap.create small in
  (* (256 - 24) / 40 = 5 slots of 4 words. *)
  let blocks =
    sequentially 7 ~f:(fun _ -> where heap (Heap.alloc heap (scanned 4)))
  in
  print_s [%sexp (blocks : Sexp.t list)];
  [%expect {| ((0 32) (0 72) (0 112) (0 152) (0 192) (1 32) (1 72)) |}]
;;

let%expect_test "blocks are zeroed, their headers written" =
  let heap = Heap.create small in
  let block = Heap.alloc heap (scanned ~scalar:1 2) in
  Memory.store_64 block (Int64_u.of_int (-1));
  Memory.store_64 (Addr.add block 8) (Int64_u.of_int (-1));
  ignore (Heap.sweep heap : Heap.Sweep_stats.t);
  let again = Heap.alloc heap (Header.bytes ~length:13 Unmarked) in
  print_s
    [%message
      ""
        ~same:(Addr.equal block again : bool)
        ~header:(Header.load again : Header.t)
        ~words:
          (List.init 2 ~f:(fun i ->
             Int64_u.to_int64 (Memory.load_64 (Addr.add again (i * 8))))
           : int64 list)];
  [%expect
    {| ((same true) (header (Bytes (length 13) (color Unmarked))) (words (0 0))) |}]
;;

let%expect_test "zero-size blocks are shared immortal atoms" =
  let heap = Heap.create small in
  let a = Heap.alloc heap (scanned 0) in
  let b = Heap.alloc heap (scanned ~color:Immortal 0) in
  let s = Heap.alloc heap (Header.bytes ~length:0 Unmarked) in
  let s' = Heap.alloc heap (Header.bytes ~length:0 Unmarked) in
  print_s
    [%message
      ""
        ~same_scanned:(Addr.equal a b : bool)
        ~same_bytes:(Addr.equal s s' : bool)
        ~scanned_is_bytes:(Addr.equal a s : bool)
        ~scanned:(Header.load a : Header.t)
        ~bytes:(Header.load s : Header.t)
        ~where:(where heap a : Sexp.t)
        ~block_start:(where heap (Heap.block_start heap a) : Sexp.t)
        ~regions:(Heap.For_testing.regions heap : int)];
  print_s [%sexp (Heap.sweep heap : Heap.Sweep_stats.t)];
  print_s [%sexp (Header.load a : Header.t)];
  [%expect
    {|
    ((same_scanned true) (same_bytes true) (scanned_is_bytes false)
     (scanned (Scanned (size_words 0) (scalar_words 0) (color Immortal)))
     (bytes (Bytes (length 0) (color Immortal))) (where outside)
     (block_start null) (regions 0))
    ((freed_blocks 0) (freed_words 0) (kept_blocks 0) (kept_words 0)
     (kept_footprint_words 0))
    (Scanned (size_words 0) (scalar_words 0) (color Immortal))
    |}]
;;

let%expect_test "allocating a free header raises" =
  let heap = Heap.create small in
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         ignore (Heap.alloc heap (Header.free ~size_words:1) : Addr.t))
       : unit Or_error.t)];
  [%expect
    {|
    (Error
     ("Heap.alloc: a Free header"
      (header (Free (size_words 1) (color Unmarked)))))
    |}]
;;

(* For every byte from the slot's header to past its end: the block it is in, as
   runs of equal results. *)
let print_block_starts heap ~from ~bytes =
  let results =
    List.init bytes ~f:(fun i ->
      Sexp.to_string (where heap (Heap.block_start heap (Addr.add from i))))
  in
  let runs = List.group results ~break:String.( <> ) in
  let _, lines =
    List.fold runs ~init:(0, []) ~f:(fun (start, lines) run ->
      let stop = start + List.length run in
      stop, sprintf "[%d, %d): %s" start stop (List.hd_exn run) :: lines)
  in
  List.iter (List.rev lines) ~f:print_endline
;;

let%expect_test "interior pointers into small blocks" =
  let heap = Heap.create small in
  (* A 3-word block in a 4-word slot, between two full ones, then a free slot. *)
  let a = Heap.alloc heap (scanned 4) in
  let b = Heap.alloc heap (scanned 3) in
  let c = Heap.alloc heap (Header.bytes ~length:25 Unmarked) in
  print_s [%sexp ([ where heap a; where heap b; where heap c ] : Sexp.t list)];
  (* From the page's base: descriptor, then each slot's header and words. *)
  print_block_starts heap ~from:(Addr.add a (-32)) ~bytes:256;
  [%expect
    {|
    ((0 32) (0 72) (0 112))
    [0, 32): null
    [32, 64): (0 32)
    [64, 72): null
    [72, 96): (0 72)
    [96, 112): null
    [112, 144): (0 112)
    [144, 256): null
    |}]
;;

let%expect_test "free slots, descriptors and foreign addresses" =
  let heap = Heap.create small in
  let a = Heap.alloc heap (scanned 1) in
  let b = Heap.alloc heap (scanned 1) in
  set_color a Marked;
  ignore (Heap.sweep heap : Heap.Sweep_stats.t);
  let other = Memory.Chunk.create ~bytes:256 ~align:256 in
  let show name addr =
    print_s
      [%message name ~block:(where heap (Heap.block_start heap addr) : Sexp.t)]
  in
  show "a" a;
  show "b (freed)" b;
  show "descriptor" (Addr.add a (-32));
  show "a's header" (Addr.add a (-8));
  show "null" (Addr.null ());
  show "4095" (Addr.of_word #4095n);
  show "another chunk" (Memory.Chunk.base other);
  show "2^63" (Addr.of_word (Nativeint_u.shift_left #1n 63));
  show "past the last slot" (Addr.add a (-32 + 255));
  show "the next page" (Addr.add a (-32 + 256));
  Memory.Chunk.destroy other;
  [%expect
    {|
    (a (block (0 32)))
    ("b (freed)" (block null))
    (descriptor (block null))
    ("a's header" (block null))
    (null (block null))
    (4095 (block null))
    ("another chunk" (block null))
    (2^63 (block null))
    ("past the last slot" (block null))
    ("the next page" (block null))
    |}]
;;

let%expect_test "large blocks" =
  let heap = Heap.create small in
  (* 24 + 8 + 10 * 8 = 112 bytes: one page. 24 + 8 + 100 * 8 = 832: four. *)
  let one = Heap.alloc heap (scanned ~scalar:3 10) in
  let four = Heap.alloc heap (Header.bytes ~length:800 Unmarked) in
  print_s [%sexp (heap : Heap.t)];
  print_s [%message "" ~pages:(Heap.For_testing.pages heap : int)];
  print_block_starts heap ~from:(Addr.add one (-32)) ~bytes:256;
  print_block_starts heap ~from:(Addr.add four (-32)) ~bytes:1024;
  [%expect
    {|
    ((Large
      ((index 0) (pages 1)
       (slots ((32 (Scanned (size_words 10) (scalar_words 3) (color Unmarked)))))))
     (Large
      ((index 1) (pages 4) (slots ((32 (Bytes (length 800) (color Unmarked))))))))
    (pages 5)
    [0, 32): null
    [32, 112): (0 32)
    [112, 256): null
    [0, 32): null
    [32, 832): (1 32)
    [832, 1024): null
    |}]
;;

let%expect_test "sweep" =
  let heap = Heap.create small in
  let blocks = alloc_n heap 6 ~header:(fun i -> scanned ((i % 4) + 1)) in
  let large = Heap.alloc heap (scanned 50) in
  let large' = Heap.alloc heap (scanned 60) in
  mark_if blocks ~f:(fun i -> i % 2 = 0);
  set_color large Marked;
  print_s [%sexp (Heap.sweep heap : Heap.Sweep_stats.t)];
  print_s [%sexp (heap : Heap.t)];
  print_s
    [%message
      ""
        ~regions:(Heap.For_testing.regions heap : int)
        ~pages:(Heap.For_testing.pages heap : int)
        ~large_freed:(where heap (Heap.block_start heap large') : Sexp.t)];
  (* Nothing marked now: everything goes. *)
  print_s [%sexp (Heap.sweep heap : Heap.Sweep_stats.t)];
  print_s
    [%message
      ""
        ~regions:(Heap.For_testing.regions heap : int)
        ~pages:(Heap.For_testing.pages heap : int)];
  [%expect
    {|
    ((freed_blocks 4) (freed_words 68) (kept_blocks 4) (kept_words 55)
     (kept_footprint_words 73))
    ((Page
      ((index 0) (words 1)
       (slots
        ((32 (Scanned (size_words 1) (scalar_words 0) (color Unmarked)))
         (48 (Scanned (size_words 1) (scalar_words 0) (color Unmarked)))
         (64 (Free (size_words 1) (color Unmarked)))
         (80 (Free (size_words 1) (color Unmarked)))
         (96 (Free (size_words 1) (color Unmarked)))
         (112 (Free (size_words 1) (color Unmarked)))
         (128 (Free (size_words 1) (color Unmarked)))
         (144 (Free (size_words 1) (color Unmarked)))
         (160 (Free (size_words 1) (color Unmarked)))
         (176 (Free (size_words 1) (color Unmarked)))
         (192 (Free (size_words 1) (color Unmarked)))
         (208 (Free (size_words 1) (color Unmarked)))
         (224 (Free (size_words 1) (color Unmarked)))
         (240 (Free (size_words 1) (color Unmarked)))))))
     (Page
      ((index 2) (words 4)
       (slots
        ((32 (Scanned (size_words 3) (scalar_words 0) (color Unmarked)))
         (72 (Free (size_words 4) (color Unmarked)))
         (112 (Free (size_words 4) (color Unmarked)))
         (152 (Free (size_words 4) (color Unmarked)))
         (192 (Free (size_words 4) (color Unmarked)))))))
     (Large
      ((index 3) (pages 2)
       (slots ((32 (Scanned (size_words 50) (scalar_words 0) (color Unmarked))))))))
    ((regions 3) (pages 4) (large_freed null))
    ((freed_blocks 4) (freed_words 55) (kept_blocks 0) (kept_words 0)
     (kept_footprint_words 0))
    ((regions 0) (pages 0))
    |}]
;;

let%expect_test "freed slots are reused in address order" =
  let heap = Heap.create small in
  let blocks = alloc_n heap 12 ~header:(fun _ -> scanned 2) in
  (* Keep every third. *)
  mark_if blocks ~f:(fun i -> i % 3 = 0);
  ignore (Heap.sweep heap : Heap.Sweep_stats.t);
  let again =
    sequentially 10 ~f:(fun _ -> where heap (Heap.alloc heap (scanned 2)))
  in
  print_s [%sexp (again : Sexp.t list)];
  [%expect
    {| ((0 56) (0 80) (0 128) (0 152) (0 200) (0 224) (1 56) (1 80) (1 104) (1 128)) |}]
;;

let%expect_test "immortal blocks are kept as they are" =
  let heap = Heap.create small in
  let block = Heap.alloc heap (scanned ~color:Immortal 1) in
  let large = Heap.alloc heap (scanned ~color:Immortal 40) in
  print_s [%sexp (Heap.sweep heap : Heap.Sweep_stats.t)];
  print_s
    [%message
      ""
        ~block:(Header.load block : Header.t)
        ~large:(Header.load large : Header.t)];
  [%expect
    {|
    ((freed_blocks 0) (freed_words 0) (kept_blocks 2) (kept_words 41)
     (kept_footprint_words 66))
    ((block (Scanned (size_words 1) (scalar_words 0) (color Immortal)))
     (large (Scanned (size_words 40) (scalar_words 0) (color Immortal))))
    |}]
;;

let%expect_test "iter" =
  let heap = Heap.create small in
  let blocks = alloc_n heap 5 ~header:(fun i -> scanned ((i % 3) + 1)) in
  let _large = Heap.alloc heap (scanned 30) in
  mark_if blocks ~f:(fun i -> i <> 1);
  ignore (Heap.sweep heap : Heap.Sweep_stats.t);
  let seen = ref [] in
  Heap.iter heap ~f:(fun block -> seen := where heap block :: !seen);
  print_s [%sexp (List.rev !seen : Sexp.t list)];
  [%expect {| ((0 32) (0 48) (1 56) (2 32)) |}]
;;

let%expect_test "the default config" =
  let heap = Heap.create Heap.Config.default in
  print_s [%sexp (Heap.Config.default : Heap.Config.t)];
  let largest_small = Heap.alloc heap (scanned 1024) in
  let smallest_large = Heap.alloc heap (scanned 1025) in
  let odd = Heap.alloc heap (scanned 9) in
  print_s
    [%message
      ""
        ~largest_small:(where heap largest_small : Sexp.t)
        ~smallest_large:(where heap smallest_large : Sexp.t)
        ~odd:(where heap odd : Sexp.t)
        ~regions:(Heap.For_testing.regions heap : int)
        ~pages:(Heap.For_testing.pages heap : int)
        ~interior:
          (where
             heap
             (Heap.block_start heap (Addr.add smallest_large (1024 * 8)))
           : Sexp.t)];
  [%expect
    {|
    ((page_bytes 65536) (arena_bytes 4194304)
     (size_classes
      (1 2 3 4 5 6 7 8 10 12 14 16 20 24 28 32 40 48 56 64 80 96 112 128 160 192
       224 256 320 384 448 512 640 768 896 1024)))
    ((largest_small (0 32)) (smallest_large (1 32)) (odd (2 32)) (regions 3)
     (pages 3) (interior (1 32)))
    |}]
;;

let%expect_test "a large block spanning many pages" =
  let heap = Heap.create Heap.Config.default in
  (* 1 MiB of fields: 17 pages with the descriptor and header. *)
  let words = 1 lsl 17 in
  let block = Heap.alloc heap (scanned words) in
  let resolves offset =
    where heap (Heap.block_start heap (Addr.add block offset))
  in
  print_s
    [%message
      ""
        ~pages:(Heap.For_testing.pages heap : int)
        ~first:(resolves 0 : Sexp.t)
        ~page_boundaries:
          (List.for_all (List.range 1 17) ~f:(fun page ->
             Sexp.equal (resolves ((page * 65536) - 32)) (resolves 0))
           : bool)
        ~last:(resolves ((words * 8) - 1) : Sexp.t)
        ~past:(resolves (words * 8) : Sexp.t)];
  print_s [%sexp (Heap.sweep heap : Heap.Sweep_stats.t)];
  print_s
    [%message
      "" ~pages:(Heap.For_testing.pages heap : int) ~after:(resolves 0 : Sexp.t)];
  [%expect
    {|
    ((pages 17) (first (0 32)) (page_boundaries true) (last (0 32)) (past null))
    ((freed_blocks 1) (freed_words 131072) (kept_blocks 0) (kept_words 0)
     (kept_footprint_words 0))
    ((pages 0) (after null))
    |}]
;;

let%expect_test "large blocks freed and allocated again" =
  let heap = Heap.create small in
  let rounds =
    sequentially 4 ~f:(fun round ->
      let block = Heap.alloc heap (scanned (20 + (round * 30))) in
      let pages = Heap.For_testing.pages heap in
      let found = where heap (Heap.block_start heap block) in
      ignore (Heap.sweep heap : Heap.Sweep_stats.t);
      [%message
        ""
          (round : int)
          (pages : int)
          (found : Sexp.t)
          ~freed:(where heap (Heap.block_start heap block) : Sexp.t)
          ~pages_after:(Heap.For_testing.pages heap : int)])
  in
  List.iter rounds ~f:print_s;
  [%expect
    {|
    ((round 0) (pages 1) (found (0 32)) (freed null) (pages_after 0))
    ((round 1) (pages 2) (found (1 32)) (freed null) (pages_after 0))
    ((round 2) (pages 3) (found (2 32)) (freed null) (pages_after 0))
    ((round 3) (pages 4) (found (3 32)) (freed null) (pages_after 0))
    |}]
;;

let%expect_test "a marked large block survives sweep" =
  let heap = Heap.create small in
  let block = Heap.alloc heap (scanned 60) in
  set_color block Marked;
  print_s [%sexp (Heap.sweep heap : Heap.Sweep_stats.t)];
  print_s
    [%message
      ""
        ~header:(Header.load block : Header.t)
        ~start:(where heap (Heap.block_start heap block) : Sexp.t)
        ~interior:
          (where heap (Heap.block_start heap (Addr.add block 300)) : Sexp.t)];
  print_s [%sexp (Heap.sweep heap : Heap.Sweep_stats.t)];
  [%expect
    {|
    ((freed_blocks 0) (freed_words 0) (kept_blocks 1) (kept_words 60)
     (kept_footprint_words 64))
    ((header (Scanned (size_words 60) (scalar_words 0) (color Unmarked)))
     (start (0 32)) (interior (0 32)))
    ((freed_blocks 1) (freed_words 60) (kept_blocks 0) (kept_words 0)
     (kept_footprint_words 0))
    |}]
;;

let%expect_test "a class whose last slot ends at the page end" =
  (* A 24-byte descriptor, then one slot of 29 * 8 = 232 bytes: 256 exactly. *)
  let heap =
    Heap.create
      { Heap.Config.page_bytes = 256
      ; arena_bytes = 1024
      ; size_classes = [ 28 ]
      }
  in
  let block = Heap.alloc heap (scanned 28) in
  let resolves offset =
    where heap (Heap.block_start heap (Addr.add block offset))
  in
  print_s
    [%message
      ""
        ~block:(where heap block : Sexp.t)
        ~last_byte:(resolves ((28 * 8) - 1) : Sexp.t)
        ~next_page:(resolves (28 * 8) : Sexp.t)];
  [%expect {| ((block (0 32)) (last_byte (0 32)) (next_page null)) |}]
;;

let%expect_test "sweeping bytes blocks counts their words" =
  let heap = Heap.create small in
  let kept = Heap.alloc heap (Header.bytes ~length:9 Unmarked) in
  let _freed = Heap.alloc heap (Header.bytes ~length:1 Unmarked) in
  let _freed_large = Heap.alloc heap (Header.bytes ~length:100 Unmarked) in
  set_color kept Marked;
  print_s [%sexp (Heap.sweep heap : Heap.Sweep_stats.t)];
  [%expect
    {|
    ((freed_blocks 2) (freed_words 14) (kept_blocks 1) (kept_words 2)
     (kept_footprint_words 3))
    |}]
;;

let%expect_test "destroy" =
  let heap = Heap.create small in
  let block = Heap.alloc heap (scanned 2) in
  let _large = Heap.alloc heap (scanned 60) in
  Heap.destroy heap;
  print_s
    [%message
      ""
        ~regions:(Heap.For_testing.regions heap : int)
        ~pages:(Heap.For_testing.pages heap : int)
        ~block:(where heap (Heap.block_start heap block) : Sexp.t)
        ~again:
          (Or_error.try_with (fun () -> Heap.destroy heap) : unit Or_error.t)
        ~alloc:
          (Or_error.try_with (fun () ->
             ignore (Heap.alloc heap (scanned 1) : Addr.t))
           : unit Or_error.t)];
  [%expect
    {|
    ((regions 0) (pages 0) (block null)
     (again (Error ("Heap: destroyed" (name destroy))))
     (alloc (Error ("Heap: destroyed" (name alloc)))))
    |}]
;;

let%expect_test "arenas: first fit, page runs, own chunks, release" =
  let heap = Heap.create small in
  let show () =
    print_s
      [%message
        ""
          ~arenas:(Heap.For_testing.arenas heap : int)
          ~free_pages:(Heap.For_testing.free_pages heap : int list)]
  in
  let page addr =
    [%sexp (Heap.For_testing.arena_page heap addr : (int * int) option)]
  in
  let one = Heap.alloc heap (scanned 1) in
  let two_pages = Heap.alloc heap (scanned 40) in
  let one_page = Heap.alloc heap (scanned 20) in
  print_s
    [%message
      ""
        ~one:(page one : Sexp.t)
        ~two_pages:(page two_pages : Sexp.t)
        ~one_page:(page one_page : Sexp.t)];
  show ();
  [%expect
    {|
    ((one ((0 0))) (two_pages ((0 1))) (one_page ((0 3))))
    ((arenas 1) (free_pages (0)))
    |}];
  (* A run of every page of an arena: a new one. *)
  let whole = Heap.alloc heap (scanned 120) in
  (* Bigger than an arena: a chunk of its own. *)
  let huge = Heap.alloc heap (scanned 200) in
  print_s
    [%message
      ""
        ~whole:(page whole : Sexp.t)
        ~huge:(page huge : Sexp.t)
        ~huge_resolves:
          (Addr.equal (Heap.block_start heap (Addr.add huge 1592)) huge : bool)];
  show ();
  [%expect
    {|
    ((whole ((1 0))) (huge ()) (huge_resolves true))
    ((arenas 2) (free_pages (0 0)))
    |}];
  set_color one Marked;
  set_color one_page Marked;
  ignore (Heap.sweep heap : Heap.Sweep_stats.t);
  (* The emptied second arena is given back; the first keeps its free run. *)
  show ();
  [%expect {| ((arenas 1) (free_pages (2))) |}];
  (* Freed pages are taken again, first fit, by another class and a run. *)
  let two = Heap.alloc heap (scanned 2) in
  let again = Heap.alloc heap (scanned 20) in
  print_s [%message "" ~two:(page two : Sexp.t) ~again:(page again : Sexp.t)];
  show ();
  [%expect
    {|
    ((two ((0 1))) (again ((0 2))))
    ((arenas 1) (free_pages (0)))
    |}];
  (* Pages of slots left with no block go back too: here, every page. *)
  ignore (Heap.sweep heap : Heap.Sweep_stats.t);
  show ();
  let four = Heap.alloc heap (scanned 4) in
  print_s
    [%message
      ""
        ~four:(page four : Sexp.t)
        ~at_one_s_old_address:(Addr.equal four one : bool)];
  [%expect
    {|
    ((arenas 1) (free_pages (4)))
    ((four ((0 0))) (at_one_s_old_address true))
    |}]
;;

let%expect_test "alloc rejects marked headers" =
  let heap = Heap.create small in
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         ignore (Heap.alloc heap (scanned ~color:Marked 1) : Addr.t))
       : unit Or_error.t)];
  [%expect
    {|
    (Error
     ("Heap.alloc: a Marked header"
      (header (Scanned (size_words 1) (scalar_words 0) (color Marked)))))
    |}]
;;

let%expect_test "footprint_words" =
  let heap = Heap.create small in
  List.iter [ 0; 1; 2; 3; 4; 5; 28; 29; 124; 200 ] ~f:(fun words ->
    print_s
      [%message
        ""
          (words : int)
          ~footprint:(Heap.footprint_words heap (scanned words) : int)]);
  print_s
    [%message
      ""
        ~bytes_13:
          (Heap.footprint_words heap (Header.bytes ~length:13 Unmarked) : int)];
  [%expect
    {|
    ((words 0) (footprint 0))
    ((words 1) (footprint 2))
    ((words 2) (footprint 3))
    ((words 3) (footprint 5))
    ((words 4) (footprint 5))
    ((words 5) (footprint 32))
    ((words 28) (footprint 32))
    ((words 29) (footprint 64))
    ((words 124) (footprint 128))
    ((words 200) (footprint 224))
    (bytes_13 3)
    |}]
;;
