open! Import
open Layout_helpers
module Addr = Memory.Addr

let heap_config =
  { Heap.Config.page_bytes = 256
  ; arena_bytes = 4096
  ; size_classes = [ 1; 2; 4 ]
  }
;;

let manual = { Stag_gc.Config.space_overhead = 100; min_words = Int.max_value }
let word block i = Addr.add block (i * 8)
let small n = Addr.of_word (Nativeint_u.of_int n)

let scanned ~size ~scalars =
  Header.scanned ~size_words:size ~scalar_words:scalars
;;

let%expect_test "immortal blocks, bump allocated" =
  let static = Static_data.create () in
  let a = Static_data.alloc static (scanned ~size:2 ~scalars:1 Unmarked) in
  let b = Static_data.alloc static (Header.bytes ~length:5 Marked) in
  let c = Static_data.alloc static (scanned ~size:1 ~scalars:0 Immortal) in
  Memory.store_64 a #7L;
  Memory.store_addr (word a 1) c;
  print_s [%sexp (static : Static_data.t)];
  print_s
    [%message
      ""
        ~a_to_c:(Addr.equal (Memory.load_addr (word a 1)) c : bool)
        ~contains:
          ([ Static_data.contains static a
           ; Static_data.contains static (Addr.add a (-8))
           ; Static_data.contains static (word c 1)
           ; Static_data.contains static b
           ]
           : bool list)];
  [%expect
    {|
    (((bytes 65536) (used 56)
      (blocks
       ((8 (Scanned (size_words 2) (scalar_words 1) (color Immortal)))
        (32 (Bytes (length 5) (color Immortal)))
        (48 (Scanned (size_words 1) (scalar_words 0) (color Immortal)))))))
    ((a_to_c true) (contains (true true false true)))
    |}];
  let count = ref 0 in
  Static_data.iter static ~f:(fun _ -> incr count);
  print_s [%message "" ~blocks:(!count : int)];
  [%expect {| (blocks 3) |}];
  Static_data.destroy static
;;

let%expect_test "blocks larger than a segment get one of their own" =
  let static = Static_data.create () in
  let _ = Static_data.alloc static (scanned ~size:1 ~scalars:1 Unmarked) in
  let _ = Static_data.alloc static (Header.bytes ~length:100_000 Unmarked) in
  let _ = Static_data.alloc static (scanned ~size:1 ~scalars:1 Unmarked) in
  print_s [%sexp (static : Static_data.t)];
  [%expect
    {|
    (((bytes 65536) (used 16)
      (blocks ((8 (Scanned (size_words 1) (scalar_words 1) (color Immortal))))))
     ((bytes 100008) (used 100008)
      (blocks ((8 (Bytes (length 100000) (color Immortal))))))
     ((bytes 65536) (used 16)
      (blocks ((8 (Scanned (size_words 1) (scalar_words 1) (color Immortal)))))))
    |}];
  Static_data.destroy static
;;

let%expect_test "zero-size blocks are shared, by heaps too" =
  let static = Static_data.create () in
  let heap = Heap.create heap_config in
  let scanned_empty =
    Static_data.alloc static (scanned ~size:0 ~scalars:0 Unmarked)
  in
  let bytes_empty =
    Static_data.alloc static (Header.bytes ~length:0 Unmarked)
  in
  print_s
    [%message
      ""
        ~same:
          ([ Addr.equal scanned_empty (Static_data.empty ())
           ; Addr.equal bytes_empty (Static_data.empty_bytes ())
           ; Addr.equal
               (Heap.alloc heap (scanned ~size:0 ~scalars:0 Unmarked))
               (Static_data.empty ())
           ; Addr.equal
               (Heap.alloc heap (Header.bytes ~length:0 Unmarked))
               (Static_data.empty_bytes ())
           ]
           : bool list)
        ~scanned_header:(Header.load (Static_data.empty ()) : Header.t)
        ~bytes_header:(Header.load (Static_data.empty_bytes ()) : Header.t)
        ~in_static:(Static_data.contains static (Static_data.empty ()) : bool)
        ~block_start:
          (Addr.is_null (Heap.block_start heap (Static_data.empty ())) : bool)];
  [%expect
    {|
    ((same (true true true true))
     (scanned_header (Scanned (size_words 0) (scalar_words 0) (color Immortal)))
     (bytes_header (Bytes (length 0) (color Immortal))) (in_static false)
     (block_start true))
    |}];
  Heap.destroy heap;
  Static_data.destroy static
;;

let%expect_test "errors" =
  let static = Static_data.create () in
  let try_ f = print_s [%sexp (Or_error.try_with f : unit Or_error.t)] in
  try_ (fun () -> ignore (Static_data.alloc static (Header.free ~size_words:1)));
  Static_data.destroy static;
  try_ (fun () ->
    ignore (Static_data.alloc static (scanned ~size:1 ~scalars:1 Unmarked)));
  try_ (fun () -> Static_data.destroy static);
  [%expect
    {|
    (Error
     ("Static_data.alloc: a Free header"
      (header (Free (size_words 1) (color Unmarked)))))
    (Error ("Static_data: destroyed" (name alloc)))
    (Error ("Static_data: destroyed" (name destroy)))
    |}]
;;

let%expect_test "globals: scalars, then pointer slots that are roots" =
  let static = Static_data.create () in
  let roots = Roots.create () in
  (* An i8, an i64 and two pointers: 16 bytes of scalars, then two slots. *)
  let global =
    Static_data.add_global
      static
      roots
      (layout (struct_ [ scalar I8; ptr; scalar I64; ptr ]))
  in
  let nothing = Static_data.add_global static roots (layout (struct_ [])) in
  let only_scalars =
    Static_data.add_global static roots (layout (scalar I32))
  in
  let roots_slots () =
    let slots = ref [] in
    Roots.iter_slots roots ~f:(fun slot ->
      slots := Addr.diff slot (Static_data.Global.block global) :: !slots);
    List.rev !slots |> List.sort ~compare:Int.compare
  in
  print_s
    [%message
      ""
        ~ptrs_from_scalars:
          (Addr.diff
             (Static_data.Global.ptrs global)
             (Static_data.Global.scalars global)
           : int)
        ~header:(Header.load (Static_data.Global.block global) : Header.t)
        ~root_slots:(roots_slots () : int list)
        ~empty_global_is_empty_bytes:
          (Addr.equal
             (Static_data.Global.block nothing)
             (Static_data.empty_bytes ())
           : bool)
        ~only_scalars_header:
          (Header.load (Static_data.Global.block only_scalars) : Header.t)];
  Static_data.remove_global roots global;
  print_s [%message "removed" ~root_slots:(roots_slots () : int list)];
  print_s
    [%sexp
      (Or_error.try_with (fun () -> Static_data.remove_global roots global)
       : unit Or_error.t)];
  [%expect
    {|
    ((ptrs_from_scalars 16) (header (Bytes (length 32) (color Immortal)))
     (root_slots (16 24)) (empty_global_is_empty_bytes true)
     (only_scalars_header (Bytes (length 8) (color Immortal))))
    (removed (root_slots ()))
    (Error "Static_data.remove_global: removed")
    |}];
  Roots.destroy roots;
  Static_data.destroy static
;;

let%expect_test "a global keeps what its slots point to alive, until removed" =
  let static = Static_data.create () in
  let heap = Heap.create heap_config in
  let roots = Roots.create () in
  let gc = Stag_gc.create manual heap roots (Data_stack.create ()) in
  let global =
    Static_data.add_global static roots (layout (struct_ [ ptr; scalar I64 ]))
  in
  let cell = Stag_gc.alloc gc (scanned ~size:2 ~scalars:1 Unmarked) in
  let other = Stag_gc.alloc gc (scanned ~size:1 ~scalars:1 Unmarked) in
  Memory.store_addr (word cell 1) other;
  Memory.store_addr (Static_data.Global.ptrs global) cell;
  Memory.store_64 (Static_data.Global.scalars global) #42L;
  let collect () =
    let stats = Stag_gc.collect gc in
    print_s [%sexp (stats : Stag_gc.Stats.t)];
    Stag_gc.check_invariants ~static:[ static ] gc |> ok_exn
  in
  collect ();
  [%expect
    {| ((marked_blocks 2) (marked_words 3) (freed_blocks 0) (freed_words 0)) |}];
  Static_data.remove_global roots global;
  collect ();
  [%expect
    {| ((marked_blocks 0) (marked_words 0) (freed_blocks 2) (freed_words 3)) |}];
  Stag_gc.destroy gc;
  Roots.destroy roots;
  Heap.destroy heap;
  Static_data.destroy static
;;

let%expect_test "check_invariants: static data must not point into the heap" =
  let static = Static_data.create () in
  let heap = Heap.create heap_config in
  let roots = Roots.create () in
  let gc = Stag_gc.create manual heap roots (Data_stack.create ()) in
  let frame = Roots.push_frame roots ~slots:1 in
  let cell = Stag_gc.alloc gc (scanned ~size:1 ~scalars:1 Unmarked) in
  Memory.store_addr frame cell;
  let a = Static_data.alloc static (scanned ~size:3 ~scalars:0 Unmarked) in
  let b = Static_data.alloc static (scanned ~size:1 ~scalars:0 Unmarked) in
  (* Static data, a niche value, foreign memory: fine. *)
  Memory.store_addr a b;
  Memory.store_addr (word a 1) (small 3);
  Memory.store_addr (word a 2) (Static_data.empty ());
  print_s
    [%sexp (Stag_gc.check_invariants ~static:[ static ] gc : unit Or_error.t)];
  [%expect {| (Ok ()) |}];
  Memory.store_addr (word a 1) cell;
  (* Even as a scalar of a [Bytes] block it is not a pointer. *)
  let bytes = Static_data.alloc static (Header.bytes ~length:8 Unmarked) in
  Memory.store_addr bytes cell;
  print_s
    [%sexp (Stag_gc.check_invariants ~static:[ static ] gc : unit Or_error.t)];
  [%expect
    {|
    (Error
     (Gc.check_invariants
      (errors
       (("static data points into the heap" (static 0) (block 0) (word 1))))))
    |}];
  Stag_gc.destroy gc;
  Roots.destroy roots;
  Heap.destroy heap;
  Static_data.destroy static
;;
