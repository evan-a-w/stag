open! Import
module Addr = Memory.Addr

let key = Type_equal.Id.create ~name:"name" String.sexp_of_t
let other_key = Type_equal.Id.create ~name:"other name" String.sexp_of_t

let%expect_test "handle reachability and slot reuse" =
  let heap =
    Heap.create { page_bytes = 256; arena_bytes = 4096; size_classes = [ 1 ] }
  in
  let roots = Roots.create () in
  let data_stack = Data_stack.create () in
  let static = Static_data.create () in
  let gc =
    Stag_gc.create Stag_gc.Config.default ~heap ~roots ~data_stack ~static
  in
  let root = Roots.push_frame roots ~slots:1 in
  let parent =
    Stag_gc.alloc gc (Header.scanned ~size_words:1 ~scalar_words:0 Unmarked)
  in
  Memory.store_addr root parent;
  let kept = Stag_gc.alloc_handle gc key "kept" in
  let dead = Stag_gc.alloc_handle gc key "dead" in
  let old_id = Int64_u.to_int (Memory.load_64 dead) in
  Memory.store_addr parent kept;
  let collect () =
    let stats = Stag_gc.collect gc in
    Stag_gc.check_invariants gc |> ok_exn;
    print_s
      [%message
        ""
          ~marked:(stats.marked_blocks : int)
          ~freed:(stats.freed_blocks : int)
          ~handles:(Heap.For_testing.handles heap : int)]
  in
  collect ();
  print_s
    [%message
      ""
        ~kept:(Stag_gc.get_handle gc kept key : string option)
        ~dead:(Stag_gc.get_handle gc dead key : string option)
        ~wrong_key:(Stag_gc.get_handle gc kept other_key : string option)
        ~interior:(Stag_gc.get_handle gc (Addr.add kept 1) key : string option)];
  let fresh = Stag_gc.alloc_handle gc key "fresh" in
  print_s
    [%message
      ""
        ~reused:(Addr.equal dead fresh : bool)
        ~new_id:(Int64_u.to_int (Memory.load_64 fresh) > old_id : bool)
        ~fresh:(Stag_gc.get_handle gc fresh key : string option)];
  Roots.pop_frame roots;
  collect ();
  let _ = Stag_gc.alloc_handle gc key "released at heap destruction" in
  Stag_gc.destroy gc;
  print_s [%sexp (Heap.For_testing.handles heap : int)];
  Roots.destroy roots;
  Data_stack.destroy data_stack;
  Static_data.destroy static;
  Heap.destroy heap;
  print_s [%sexp (Heap.For_testing.handles heap : int)];
  [%expect
    {|
    ((marked 2) (freed 1) (handles 1))
    ((kept (kept)) (dead ()) (wrong_key ()) (interior ()))
    ((reused true) (new_id true) (fresh (fresh)))
    ((marked 0) (freed 3) (handles 0))
    1
    0
    |}]
;;

let%expect_test "handle allocation may collect" =
  let heap =
    Heap.create { page_bytes = 256; arena_bytes = 4096; size_classes = [ 1 ] }
  in
  let roots = Roots.create () in
  let data_stack = Data_stack.create () in
  let static = Static_data.create () in
  let gc =
    Stag_gc.create
      { space_overhead = 0; min_words = 2 }
      ~heap
      ~roots
      ~data_stack
      ~static
  in
  let root = Roots.push_frame roots ~slots:1 in
  let first = Stag_gc.alloc_handle gc key "first" in
  Memory.store_addr root first;
  let second = Stag_gc.alloc_handle gc key "second" in
  print_s
    [%message
      ""
        ~collections:(Stag_gc.collections gc : int)
        ~first:(Stag_gc.get_handle gc first key : string option)
        ~second:(Stag_gc.get_handle gc second key : string option)];
  Roots.pop_frame roots;
  Stag_gc.destroy gc;
  Roots.destroy roots;
  Data_stack.destroy data_stack;
  Static_data.destroy static;
  Heap.destroy heap;
  [%expect {|
    ((collections 1) (first (first)) (second (second)))
    |}]
;;
