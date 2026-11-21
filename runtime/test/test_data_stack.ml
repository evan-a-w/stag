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
    ((outer ((depth 1) (generation 1))) (inner ((depth 2) (generation 1))))
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
    (Error
     ("Data_stack.exit: not a live mark" (mark ((depth 2) (generation 1)))))
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
    (Error
     ("Data_stack.exit: not a live mark" (mark ((depth 1) (generation 1)))))
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
  let roots = Shadow_stack.create () in
  let stack = Data_stack.create () in
  let gc =
    Stag_gc.create
      manual
      ~heap
      ~roots:[ Shadow_stack.root_source roots ]
      ~data_stack:stack
      ~static:(Static_data.create ())
  in
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
  Shadow_stack.destroy roots;
  Heap.destroy heap
;;

let%expect_test "check_invariants: nothing but the data stack points into it" =
  let heap = Heap.create heap_config in
  let roots = Shadow_stack.create () in
  let stack = Data_stack.create () in
  let static = Static_data.create () in
  let gc =
    Stag_gc.create
      manual
      ~heap
      ~roots:[ Shadow_stack.root_source roots ]
      ~data_stack:stack
      ~static
  in
  let frame = Shadow_stack.push_frame roots ~slots:1 in
  let local = Data_stack.alloc stack (scanned ~size:1 ~scalars:0) in
  let block = Stag_gc.alloc gc (scanned ~size:1 ~scalars:0) in
  let static_block = Static_data.alloc static (scanned ~size:1 ~scalars:0) in
  Memory.store_addr frame block;
  (* A root may point into the data stack. *)
  let frame2 = Shadow_stack.push_frame roots ~slots:1 in
  Memory.store_addr frame2 local;
  let check () =
    print_s [%sexp (Stag_gc.check_invariants gc : unit Or_error.t)]
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
        ("a pointer into the data stack" (from (static (block 0) (word 0))))
        ("a pointer into the heap but no block"
         (from ("data stack" (block 1) (word 0))))))))
    |}];
  Stag_gc.destroy gc;
  Static_data.destroy static;
  Data_stack.destroy stack;
  Shadow_stack.destroy roots;
  Heap.destroy heap
;;

let%expect_test "stale and out-of-order marks raise" =
  let stack = Data_stack.create ~segment_bytes:64 () in
  let try_exit name mark =
    print_s
      [%message
        name
          ~_:
            (Or_error.try_with (fun () -> Data_stack.exit stack mark)
             : unit Or_error.t)]
  in
  (* The review's case: [m1] is stale once [m0] exits, though a later block
     covers where it was. *)
  let m0 = Data_stack.enter stack in
  let _ = Data_stack.alloc stack (scanned ~size:2 ~scalars:2) in
  let m1 = Data_stack.enter stack in
  try_exit "m0" m0;
  let _ = Data_stack.alloc stack (scanned ~size:4 ~scalars:4) in
  try_exit "stale m1" m1;
  (* In an earlier segment: entered in segment 0, exited after growing. *)
  let a = Data_stack.enter stack in
  let _ = Data_stack.alloc stack (scanned ~size:3 ~scalars:3) in
  let b = Data_stack.enter stack in
  let _ = Data_stack.alloc stack (scanned ~size:3 ~scalars:3) in
  let _ = Data_stack.alloc stack (scanned ~size:3 ~scalars:3) in
  try_exit "a" a;
  let _ = Data_stack.enter stack in
  try_exit "stale b, at the same depth as a new region" b;
  (* Exiting an outer region exits those in it. *)
  let outer = Data_stack.enter stack in
  let inner = Data_stack.enter stack in
  try_exit "outer" outer;
  try_exit "inner, exited with it" inner;
  print_s [%message "" ~blocks:(stack : Data_stack.t)];
  Data_stack.destroy stack;
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         ignore (Data_stack.enter stack : Data_stack.Mark.t))
       : unit Or_error.t)];
  [%expect
    {|
    (m0 (Ok ()))
    ("stale m1"
     (Error
      ("Data_stack.exit: not a live mark" (mark ((depth 2) (generation 1))))))
    (a (Ok ()))
    ("stale b, at the same depth as a new region"
     (Error
      ("Data_stack.exit: not a live mark" (mark ((depth 2) (generation 2))))))
    (outer (Ok ()))
    ("inner, exited with it"
     (Error
      ("Data_stack.exit: not a live mark" (mark ((depth 3) (generation 1))))))
    (blocks ((0 8 (Scanned (size_words 4) (scalar_words 4) (color Unmarked)))))
    (Error "Data_stack.enter: destroyed")
    |}]
;;

let%expect_test "live_contains" =
  let stack = Data_stack.create ~segment_bytes:64 () in
  let mark = Data_stack.enter stack in
  let a = Data_stack.alloc stack (scanned ~size:2 ~scalars:2) in
  let b = Data_stack.alloc stack (scanned ~size:5 ~scalars:5) in
  let show () =
    print_s
      [%message
        ""
          ~a:(Data_stack.live_contains stack a : bool)
          ~a_header:(Data_stack.live_contains stack (Addr.add a (-8)) : bool)
          ~b:(Data_stack.live_contains stack b : bool)
          ~past_b:(Data_stack.live_contains stack (word b 5) : bool)
          ~contains_b:(Data_stack.contains stack b : bool)]
  in
  show ();
  Data_stack.exit stack mark;
  show ();
  Data_stack.destroy stack;
  [%expect
    {|
    ((a true) (a_header true) (b true) (past_b false) (contains_b true))
    ((a false) (a_header false) (b false) (past_b false) (contains_b true))
    |}]
;;

let%expect_test "check_invariants: dangling pointers into the data stack, and \
                 a broken walk"
  =
  let heap = Heap.create heap_config in
  let roots = Shadow_stack.create () in
  let globals = Globals.create () in
  let stack = Data_stack.create ~segment_bytes:128 () in
  let static = Static_data.create () in
  let gc =
    Stag_gc.create
      manual
      ~heap
      ~roots:[ Shadow_stack.root_source roots; Globals.root_source globals ]
      ~data_stack:stack
      ~static
  in
  let global =
    Static_data.add_global static globals (Layout.of_shape Ptr |> ok_exn)
  in
  let frame = Shadow_stack.push_frame roots ~slots:1 in
  let check () =
    print_s [%sexp (Stag_gc.check_invariants gc : unit Or_error.t)]
  in
  let mark = Data_stack.enter stack in
  let old = Data_stack.alloc stack (scanned ~size:1 ~scalars:0) in
  let inner = Data_stack.enter stack in
  let young = Data_stack.alloc stack (scanned ~size:1 ~scalars:0) in
  Memory.store_addr frame young;
  Memory.store_addr old young;
  check ();
  (* After [young]'s region exits, both dangle, and a global may never point
     into the data stack. *)
  Data_stack.exit stack inner;
  Memory.store_addr (Static_data.Global.ptrs global) old;
  check ();
  Memory.store_addr frame (Addr.null ());
  Memory.store_addr old (Addr.null ());
  Memory.store_addr (Static_data.Global.ptrs global) (Addr.null ());
  check ();
  (* A header the walk can't step over: overwritten by a program bug. *)
  Header.store old (Header.scanned ~size_words:100 ~scalar_words:100 Unmarked);
  check ();
  Header.store old (Header.free ~size_words:1);
  check ();
  Header.store old (Header.scanned ~size_words:1 ~scalar_words:0 Marked);
  check ();
  Header.store old (scanned ~size:1 ~scalars:0);
  Data_stack.exit stack mark;
  [%expect
    {|
    (Ok ())
    (Error
     (Gc.check_invariants
      (errors
       (("a pointer into the data stack past its top"
         (from (root (source "shadow stack") (index 0))))
        ("a pointer into the data stack"
         (from (root (source globals) (index 0))))
        ("a pointer into the data stack past its top"
         (from ("data stack" (block 0) (word 0))))))))
    (Ok ())
    (Error
     (Gc.check_invariants
      (errors
       (("the data stack's blocks overrun the end of a segment" (segment 0))))))
    (Error
     (Gc.check_invariants
      (errors (("a free data-stack block" (from ("data stack" (block 0))))))))
    (Error
     (Gc.check_invariants
      (errors
       (("a data-stack block not unmarked" (from ("data stack" (block 0))))))))
    |}];
  Static_data.remove_global globals global;
  Stag_gc.destroy gc;
  Static_data.destroy static;
  Data_stack.destroy stack;
  Shadow_stack.destroy roots;
  Heap.destroy heap
;;

let%expect_test "the GC ignores stale blocks in a reused segment" =
  let heap = Heap.create heap_config in
  let roots = Shadow_stack.create () in
  let stack = Data_stack.create ~segment_bytes:64 () in
  let gc =
    Stag_gc.create
      manual
      ~heap
      ~roots:[ Shadow_stack.root_source roots ]
      ~data_stack:stack
      ~static:(Static_data.create ())
  in
  let mark = Data_stack.enter stack in
  (* Fill two segments with blocks pointing to heap blocks. *)
  for _ = 1 to 6 do
    let local = Data_stack.alloc stack (scanned ~size:2 ~scalars:0) in
    Memory.store_addr local (Stag_gc.alloc gc (scanned ~size:1 ~scalars:1));
    Memory.store_addr
      (word local 1)
      (Stag_gc.alloc gc (scanned ~size:1 ~scalars:1))
  done;
  let segments = Data_stack.segments stack in
  Data_stack.exit stack mark;
  (* Regrow over the same segments, with smaller blocks: the old ones' words are
     still there, past each block, and must not be read. *)
  let mark = Data_stack.enter stack in
  for _ = 1 to 5 do
    ignore (Data_stack.alloc stack (scanned ~size:1 ~scalars:1) : Addr.t)
  done;
  print_s
    [%message
      ""
        ~segments_before:(segments : int)
        ~segments:(Data_stack.segments stack : int)
        ~stats:(Stag_gc.collect gc : Stag_gc.Stats.t)
        ~invariants:(Stag_gc.check_invariants gc : unit Or_error.t)];
  Data_stack.exit stack mark;
  [%expect
    {|
    ((segments_before 3) (segments 2)
     (stats
      ((marked_blocks 0) (marked_words 0) (freed_blocks 12) (freed_words 12)))
     (invariants (Ok ())))
    |}];
  Stag_gc.destroy gc;
  Data_stack.destroy stack;
  Shadow_stack.destroy roots;
  Heap.destroy heap
;;
