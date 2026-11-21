open! Import
module Addr = Memory.Addr

let scanned ~size ~scalars =
  Header.scanned ~size_words:size ~scalar_words:scalars Unmarked
;;

let word block i = Addr.add block (i * 8)

let%expect_test "regions nest" =
  let stack = Data_stack.create () in
  let outer = Data_stack.enter stack in
  let a = Data_stack.alloc stack (scanned ~size:2 ~scalars:1) in
  let inner = Data_stack.enter stack in
  let _ = Data_stack.alloc stack (Header.bytes ~length:3 Marked) in
  let _ = Data_stack.alloc stack (scanned ~size:1 ~scalars:0) in
  print_s [%message "" (outer : Data_stack.Mark.t) (inner : Data_stack.Mark.t)];
  print_s [%sexp (stack : Data_stack.t)];
  [%expect
    {|
    ((outer ((segment 0) (offset 0))) (inner ((segment 0) (offset 24))))
    ((0 8 (Scanned (size_words 2) (scalar_words 1) (color Unmarked)))
     (0 32 (Bytes (length 3) (color Unmarked)))
     (0 48 (Scanned (size_words 1) (scalar_words 0) (color Unmarked))))
    |}];
  Data_stack.exit stack inner;
  print_s [%sexp (stack : Data_stack.t)];
  [%expect
    {| ((0 8 (Scanned (size_words 2) (scalar_words 1) (color Unmarked)))) |}];
  (* Reused, and zeroed. *)
  Memory.store_64 (word a 1) #99L;
  Memory.store_64 (Addr.add a 24) #99L;
  let b = Data_stack.alloc stack (scanned ~size:2 ~scalars:0) in
  print_s
    [%message
      ""
        ~reused:(Addr.diff b a : int)
        ~zeroed:(Int64_u.to_int (Memory.load_64 b) : int)];
  [%expect {| ((reused 24) (zeroed 0)) |}];
  Data_stack.exit stack outer;
  print_s [%sexp (stack : Data_stack.t)];
  [%expect {| () |}];
  Data_stack.destroy stack
;;

let%expect_test "zero-size blocks are the static ones" =
  let stack = Data_stack.create () in
  print_s
    [%message
      ""
        ~same:
          ([ Addr.equal
               (Data_stack.alloc stack (scanned ~size:0 ~scalars:0))
               (Static_data.empty ())
           ; Addr.equal
               (Data_stack.alloc stack (Header.bytes ~length:0 Unmarked))
               (Static_data.empty_bytes ())
           ]
           : bool list)
        ~blocks:(stack : Data_stack.t)];
  [%expect {| ((same (true true)) (blocks ())) |}];
  Data_stack.destroy stack
;;

let%expect_test "segments: blocks never move, large ones get their own" =
  let stack = Data_stack.create ~segment_bytes:64 () in
  let mark = Data_stack.enter stack in
  (* 3 words each: two to a segment of 64 bytes. *)
  let blocks = Array.create ~len:5 (Addr.null ()) in
  for i = 0 to 4 do
    blocks.(i) <- Data_stack.alloc stack (scanned ~size:3 ~scalars:3);
    Memory.store_64 blocks.(i) (Int64_u.of_int i)
  done;
  let large = Data_stack.alloc stack (scanned ~size:20 ~scalars:20) in
  let after = Data_stack.alloc stack (scanned ~size:1 ~scalars:1) in
  print_s [%sexp (stack : Data_stack.t)];
  print_s
    [%message
      ""
        ~still:
          (List.init 5 ~f:(fun i -> Int64_u.to_int (Memory.load_64 blocks.(i)))
           : int list)
        ~segments:(Data_stack.segments stack : int)
        ~made:(Data_stack.For_testing.segments_made stack : int)
        ~contains:
          ([ Data_stack.contains stack large
           ; Data_stack.contains stack after
           ; Data_stack.contains stack (Addr.null ())
           ; Data_stack.contains stack (Static_data.empty ())
           ]
           : bool list)];
  [%expect
    {|
    ((0 8 (Scanned (size_words 3) (scalar_words 3) (color Unmarked)))
     (0 40 (Scanned (size_words 3) (scalar_words 3) (color Unmarked)))
     (1 8 (Scanned (size_words 3) (scalar_words 3) (color Unmarked)))
     (1 40 (Scanned (size_words 3) (scalar_words 3) (color Unmarked)))
     (2 8 (Scanned (size_words 3) (scalar_words 3) (color Unmarked)))
     (3 8 (Scanned (size_words 20) (scalar_words 20) (color Unmarked)))
     (4 8 (Scanned (size_words 1) (scalar_words 1) (color Unmarked))))
    ((still (0 1 2 3 4)) (segments 5) (made 5)
     (contains (true true false false)))
    |}];
  (* Back to the start: the segments are kept, and reused; one too small for a
     large block is replaced. *)
  Data_stack.exit stack mark;
  let _ = Data_stack.alloc stack (scanned ~size:3 ~scalars:3) in
  let _ = Data_stack.alloc stack (scanned ~size:12 ~scalars:12) in
  print_s
    [%message
      ""
        ~blocks:(stack : Data_stack.t)
        ~segments:(Data_stack.segments stack : int)
        ~made:(Data_stack.For_testing.segments_made stack : int)];
  [%expect
    {|
    ((blocks
      ((0 8 (Scanned (size_words 3) (scalar_words 3) (color Unmarked)))
       (1 8 (Scanned (size_words 12) (scalar_words 12) (color Unmarked)))))
     (segments 2) (made 5))
    |}];
  Data_stack.destroy stack
;;

let%expect_test "marks in earlier segments" =
  let stack = Data_stack.create ~segment_bytes:64 () in
  let _ = Data_stack.alloc stack (scanned ~size:3 ~scalars:3) in
  let in_first = Data_stack.enter stack in
  let _ = Data_stack.alloc stack (scanned ~size:3 ~scalars:3) in
  let _ = Data_stack.alloc stack (scanned ~size:3 ~scalars:3) in
  let in_second = Data_stack.enter stack in
  let try_exit mark =
    print_s
      [%sexp
        (Or_error.try_with (fun () -> Data_stack.exit stack mark)
         : unit Or_error.t)]
  in
  try_exit in_first;
  print_s [%message "" ~blocks:(stack : Data_stack.t)];
  (* Exited: a later mark is no longer live. *)
  try_exit in_second;
  [%expect
    {|
    (Ok ())
    (blocks ((0 8 (Scanned (size_words 3) (scalar_words 3) (color Unmarked)))))
    (Error ("Data_stack.exit: not a live mark" (mark ((segment 1) (offset 32)))))
    |}];
  Data_stack.destroy stack;
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         ignore (Data_stack.alloc stack (scanned ~size:1 ~scalars:1)))
       : unit Or_error.t)];
  print_s
    [%sexp
      (Or_error.try_with (fun () -> Data_stack.exit stack in_first)
       : unit Or_error.t)];
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         ignore (Data_stack.alloc stack (Header.free ~size_words:1)))
       : unit Or_error.t)];
  [%expect
    {|
    (Error "Data_stack: destroyed")
    (Error ("Data_stack.exit: not a live mark" (mark ((segment 0) (offset 32)))))
    (Error
     ("Data_stack.alloc: a Free header"
      (header (Free (size_words 1) (color Unmarked)))))
    |}]
;;

let heap_config =
  { Heap.Config.page_bytes = 256
  ; arena_bytes = 4096
  ; size_classes = [ 1; 2; 4 ]
  }
;;

let manual = { Stag_gc.Config.space_overhead = 100; min_words = Int.max_value }

let%expect_test "the GC: data-stack blocks root heap blocks until their region \
                 exits"
  =
  let heap = Heap.create heap_config in
  let roots = Roots.create () in
  let stack = Data_stack.create () in
  let gc = Stag_gc.create manual heap roots stack in
  let collect () =
    print_s [%sexp (Stag_gc.collect gc : Stag_gc.Stats.t)];
    print_s [%sexp (Stag_gc.check_invariants gc : unit Or_error.t)]
  in
  let mark = Data_stack.enter stack in
  (* [local] is [scalar; ptr; ptr]: its scalar looks like a pointer to [a], and
     is not followed. *)
  let local = Data_stack.alloc stack (scanned ~size:3 ~scalars:1) in
  let other_local = Data_stack.alloc stack (scanned ~size:1 ~scalars:0) in
  let a = Stag_gc.alloc gc (scanned ~size:1 ~scalars:1) in
  let b = Stag_gc.alloc gc (scanned ~size:1 ~scalars:0) in
  let c = Stag_gc.alloc gc (scanned ~size:1 ~scalars:1) in
  Memory.store_addr local a;
  Memory.store_addr (word local 1) b;
  Memory.store_addr (word local 2) other_local;
  Memory.store_addr other_local c;
  (* [b] points to [c] too. *)
  Memory.store_addr b c;
  (* Bytes are not scanned either. *)
  let bytes = Data_stack.alloc stack (Header.bytes ~length:8 Unmarked) in
  Memory.store_addr bytes a;
  collect ();
  [%expect
    {|
    ((marked_blocks 2) (marked_words 2) (freed_blocks 1) (freed_words 1))
    (Ok ())
    |}];
  Data_stack.exit stack mark;
  collect ();
  [%expect
    {|
    ((marked_blocks 0) (marked_words 0) (freed_blocks 2) (freed_words 2))
    (Ok ())
    |}];
  Stag_gc.destroy gc;
  Data_stack.destroy stack;
  Roots.destroy roots;
  Heap.destroy heap
;;

let%expect_test "check_invariants: nothing but the data stack points into it" =
  let heap = Heap.create heap_config in
  let roots = Roots.create () in
  let stack = Data_stack.create () in
  let static = Static_data.create () in
  let gc = Stag_gc.create manual heap roots stack in
  let frame = Roots.push_frame roots ~slots:1 in
  let local = Data_stack.alloc stack (scanned ~size:1 ~scalars:0) in
  let block = Stag_gc.alloc gc (scanned ~size:1 ~scalars:0) in
  let static_block = Static_data.alloc static (scanned ~size:1 ~scalars:0) in
  Memory.store_addr frame block;
  (* A root may point into the data stack. *)
  let frame2 = Roots.push_frame roots ~slots:1 in
  Memory.store_addr frame2 local;
  let check () =
    print_s
      [%sexp (Stag_gc.check_invariants ~static:[ static ] gc : unit Or_error.t)]
  in
  check ();
  Memory.store_addr block local;
  Memory.store_addr static_block local;
  (* A data-stack block pointing to a freed slot. *)
  let local2 = Data_stack.alloc stack (scanned ~size:1 ~scalars:0) in
  Memory.store_addr local2 (Addr.add block 8);
  check ();
  [%expect
    {|
    (Ok ())
    (Error
     (Gc.check_invariants
      (errors
       (("a pointer into the data stack" (from (block (block 0) (word 0))))
        ("a pointer into the data stack"
         (from (static (static 0) (block 0) (word 0))))
        ("a pointer into the heap but no block"
         (from ("data stack" (block 1) (word 0))))))))
    |}];
  Stag_gc.destroy gc;
  Static_data.destroy static;
  Data_stack.destroy stack;
  Roots.destroy roots;
  Heap.destroy heap
;;
