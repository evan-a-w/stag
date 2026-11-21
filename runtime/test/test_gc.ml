open! Import
module Addr = Memory.Addr

(* 256-byte pages, slots of 1, 2, 4 and 8 words; larger blocks are large. *)
let heap_config =
  { Heap.Config.page_bytes = 256
  ; arena_bytes = 4096
  ; size_classes = [ 1; 2; 4; 8 ]
  }
;;

(* Collections only when asked for. *)
let manual = { Stag_gc.Config.space_overhead = 100; min_words = Int.max_value }

module Env = struct
  type t =
    { heap : Heap.t
    ; roots : Shadow_stack.t
    ; globals : Globals.t
    ; gc : Stag_gc.t
    }

  let create ?(config = manual) () =
    let heap = Heap.create heap_config in
    let roots = Shadow_stack.create () in
    let globals = Globals.create () in
    { heap
    ; roots
    ; globals
    ; gc =
        Stag_gc.create
          config
          ~heap
          ~roots:[ Shadow_stack.root_source roots; Globals.root_source globals ]
          ~data_stack:(Data_stack.create ())
          ~static:(Static_data.create ())
    }
  ;;
end

open Env

let check env = Stag_gc.check_invariants env.gc |> ok_exn

(* Collects, printing the stats, checking the invariants after. *)
let collect env =
  let stats = Stag_gc.collect env.gc in
  print_s [%sexp (stats : Stag_gc.Stats.t)];
  check env
;;

(* A block of [scalars] scalar words, then [ptrs] pointer words. *)
let alloc ?(scalars = 0) env ~ptrs =
  Stag_gc.alloc
    env.gc
    (Header.scanned ~size_words:(scalars + ptrs) ~scalar_words:scalars Unmarked)
;;

let word block i = Addr.add block (i * 8)
let set block i value = Memory.store_addr (word block i) value
let set_int block i n = Memory.store_64 (word block i) (Int64_u.of_int n)
let get_int block i = Int64_u.to_int (Memory.load_64 (word block i))
let small n = Addr.of_word (Nativeint_u.of_int n)

let where env addr =
  if Addr.is_null addr
  then [%sexp "null"]
  else (
    match Heap.For_testing.locate env.heap addr with
    | Some (region, offset) -> [%sexp (region : int), (offset : int)]
    | None -> [%sexp "outside"])
;;

(* The allocated blocks, by where they are. *)
let blocks env =
  let blocks = ref [] in
  Heap.iter env.heap ~f:(fun block -> blocks := where env block :: !blocks);
  List.rev !blocks
;;

let print_blocks env = print_s [%sexp (blocks env : Sexp.t list)]

(* A list of [n] cells of [head; tail], [first] first, rooted nowhere. *)
let list env ~n ~first =
  let roots = Shadow_stack.push_frame env.roots ~slots:1 in
  for i = n - 1 downto 0 do
    let cell = alloc env ~scalars:1 ~ptrs:1 in
    set_int cell 0 (first + i);
    set cell 1 (Memory.load_addr roots);
    Memory.store_addr roots cell
  done;
  let head = Memory.load_addr roots in
  Shadow_stack.pop_frame env.roots;
  head
;;

let rec list_values cell =
  if Addr.is_null cell
  then []
  else get_int cell 0 :: list_values (Memory.load_addr (word cell 1))
;;

let%expect_test "lists: the rooted one is kept" =
  let env = Env.create () in
  let root = Shadow_stack.push_frame env.roots ~slots:1 in
  Memory.store_addr root (list env ~n:4 ~first:10);
  let (_ : Addr.t) = list env ~n:3 ~first:100 in
  print_blocks env;
  collect env;
  print_blocks env;
  print_s [%sexp (list_values (Memory.load_addr root) : int list)];
  (* Dropping the root frees it all. *)
  Memory.store_addr root (Addr.null ());
  collect env;
  print_blocks env;
  [%expect
    {|
    ((0 32) (0 56) (0 80) (0 104) (0 128) (0 152) (0 176))
    ((marked_blocks 4) (marked_words 8) (freed_blocks 3) (freed_words 6))
    ((0 32) (0 56) (0 80) (0 104))
    (10 11 12 13)
    ((marked_blocks 0) (marked_words 0) (freed_blocks 4) (freed_words 8))
    ()
    |}]
;;

let%expect_test "trees and cycles" =
  let env = Env.create () in
  let roots = Shadow_stack.push_frame env.roots ~slots:2 in
  (* A tree of depth 3, each node [left; right]. *)
  let rec tree depth =
    if depth = 0
    then Addr.null ()
    else (
      let node = alloc env ~ptrs:2 in
      (* [node] must be rooted across the allocations below. *)
      let frame = Shadow_stack.push_frame env.roots ~slots:1 in
      Memory.store_addr frame node;
      set node 0 (tree (depth - 1));
      set node 1 (tree (depth - 1));
      Shadow_stack.pop_frame env.roots;
      node)
  in
  Memory.store_addr roots (tree 3);
  (* A cycle of three, rooted, and another not. *)
  let cycle () =
    let a = alloc env ~ptrs:1 in
    let b = alloc env ~ptrs:1 in
    let c = alloc env ~ptrs:1 in
    set a 0 b;
    set b 0 c;
    set c 0 a;
    a
  in
  Memory.store_addr (word roots 1) (cycle ());
  let (_ : Addr.t) = cycle () in
  collect env;
  (* Unrooting the tree's left half. *)
  set (Memory.load_addr roots) 0 (Addr.null ());
  collect env;
  Memory.store_addr (word roots 1) (Addr.null ());
  collect env;
  [%expect
    {|
    ((marked_blocks 10) (marked_words 17) (freed_blocks 3) (freed_words 3))
    ((marked_blocks 7) (marked_words 11) (freed_blocks 3) (freed_words 6))
    ((marked_blocks 4) (marked_words 8) (freed_blocks 3) (freed_words 3))
    |}]
;;

let%expect_test "interior pointers keep their block" =
  let env = Env.create () in
  let roots = Shadow_stack.push_frame env.roots ~slots:3 in
  let small_block = alloc env ~ptrs:4 in
  let large = alloc env ~ptrs:40 in
  let target = alloc env ~ptrs:0 ~scalars:1 in
  (* Into its last word, and into a large block's middle, which points to
     [target]'s only word. *)
  Memory.store_addr roots (word small_block 3);
  Memory.store_addr (word roots 1) (word large 20);
  set large 39 (word target 0);
  let (_ : Addr.t) = alloc env ~ptrs:4 in
  collect env;
  print_blocks env;
  [%expect
    {|
    ((marked_blocks 3) (marked_words 45) (freed_blocks 1) (freed_words 4))
    ((0 32) (1 32) (2 32))
    |}]
;;

let%expect_test "words that aren't pointers to blocks are left alone" =
  let env = Env.create () in
  let foreign = Memory.Chunk.create ~bytes:64 ~align:8 in
  let atom = alloc env ~ptrs:0 in
  let block = alloc env ~ptrs:5 in
  let roots = Shadow_stack.push_frame env.roots ~slots:1 in
  Memory.store_addr roots block;
  (* A niche value, a foreign pointer, a zero-size atom, the largest small
     value. *)
  set block 0 (small 3);
  set block 1 (Memory.Chunk.base foreign);
  set block 2 atom;
  set block 3 (small 4095);
  collect env;
  print_blocks env;
  (* Pointers into the heap but to no block are skipped, but break the
     invariant. *)
  let freed = alloc env ~ptrs:1 in
  set block 4 (Addr.add block (-8));
  print_s [%sexp (Stag_gc.check_invariants env.gc : unit Or_error.t)];
  set block 4 freed;
  print_s [%sexp (Stag_gc.collect env.gc : Stag_gc.Stats.t)];
  print_s [%sexp (Stag_gc.check_invariants env.gc : unit Or_error.t)];
  [%expect
    {|
    ((marked_blocks 1) (marked_words 5) (freed_blocks 0) (freed_words 0))
    ((0 32))
    (Error
     (Gc.check_invariants
      (errors
       (("a pointer into the heap but no block"
         (from (block (block 0) (word 4))))))))
    ((marked_blocks 2) (marked_words 6) (freed_blocks 0) (freed_words 0))
    (Ok ())
    |}]
;;

let%expect_test "scalars are not traced" =
  let env = Env.create () in
  let block = alloc env ~scalars:2 ~ptrs:1 in
  let other = alloc env ~ptrs:0 ~scalars:1 in
  let roots = Shadow_stack.push_frame env.roots ~slots:1 in
  Memory.store_addr roots block;
  (* [other]'s address, but in a scalar word. *)
  set block 0 other;
  set block 1 (word other 0);
  collect env;
  print_blocks env;
  [%expect
    {|
    ((marked_blocks 1) (marked_words 3) (freed_blocks 1) (freed_words 1))
    ((0 32))
    |}]
;;

let%expect_test "bytes blocks are marked, not scanned" =
  let env = Env.create () in
  let string = Stag_gc.alloc env.gc (Header.bytes ~length:20 Unmarked) in
  let other = alloc env ~ptrs:0 ~scalars:1 in
  (* Bytes that happen to be a pointer. *)
  set string 0 other;
  let roots = Shadow_stack.push_frame env.roots ~slots:1 in
  Memory.store_addr roots string;
  collect env;
  print_blocks env;
  [%expect
    {|
    ((marked_blocks 1) (marked_words 3) (freed_blocks 1) (freed_words 1))
    ((0 32))
    |}]
;;

let%expect_test "globals are roots" =
  let env = Env.create () in
  let static = Memory.Chunk.create ~bytes:16 ~align:8 in
  let global = Memory.Chunk.base static in
  Globals.add env.globals global;
  Memory.store_addr global (list env ~n:2 ~first:1);
  let (_ : Addr.t) = alloc env ~ptrs:1 in
  collect env;
  print_s [%sexp (list_values (Memory.load_addr global) : int list)];
  Globals.remove env.globals global;
  collect env;
  [%expect
    {|
    ((marked_blocks 2) (marked_words 4) (freed_blocks 1) (freed_words 1))
    (1 2)
    ((marked_blocks 0) (marked_words 0) (freed_blocks 2) (freed_words 4))
    |}]
;;

let%expect_test "immortal blocks are not traced" =
  let env = Env.create () in
  let immortal =
    Stag_gc.alloc env.gc (Header.scanned ~size_words:2 ~scalar_words:0 Immortal)
  in
  let immortal' =
    Stag_gc.alloc env.gc (Header.scanned ~size_words:1 ~scalar_words:0 Immortal)
  in
  set immortal 0 immortal';
  check env;
  let mortal = alloc env ~ptrs:0 ~scalars:1 in
  set immortal 1 mortal;
  print_s [%sexp (Stag_gc.check_invariants env.gc : unit Or_error.t)];
  (* Unrooted, so freed, even though [immortal] points to it. *)
  print_s [%sexp (Stag_gc.collect env.gc : Stag_gc.Stats.t)];
  set immortal 1 (Addr.null ());
  check env;
  print_blocks env;
  [%expect
    {|
    (Error
     (Gc.check_invariants
      (errors
       (("an immortal block points to a mortal one"
         (from (block (block 0) (word 1))))))))
    ((marked_blocks 0) (marked_words 0) (freed_blocks 1) (freed_words 1))
    ((0 32) (1 32))
    |}]
;;

let%expect_test "a wide block grows the mark stack" =
  let env = Env.create () in
  let roots = Shadow_stack.push_frame env.roots ~slots:1 in
  let wide = alloc env ~ptrs:3000 in
  Memory.store_addr roots wide;
  for i = 0 to 2999 do
    let leaf = alloc env ~ptrs:1 in
    set wide i leaf
  done;
  let before = Stag_gc.For_testing.mark_stack_capacity env.gc in
  collect env;
  print_s
    [%message
      ""
        (before : int)
        ~after:(Stag_gc.For_testing.mark_stack_capacity env.gc : int)];
  [%expect
    {|
    ((marked_blocks 3001) (marked_words 6000) (freed_blocks 0) (freed_words 0))
    ((before 1024) (after 4096))
    |}]
;;

let%expect_test "a deep chain" =
  let env = Env.create () in
  let head = list env ~n:100_000 ~first:0 in
  let roots = Shadow_stack.push_frame env.roots ~slots:1 in
  Memory.store_addr roots head;
  collect env;
  let values = list_values (Memory.load_addr roots) in
  print_s
    [%message
      ""
        ~length:(List.length values : int)
        ~in_order:(List.equal Int.equal values (List.range 0 100_000) : bool)
        ~mark_stack_capacity:
          (Stag_gc.For_testing.mark_stack_capacity env.gc : int)];
  [%expect
    {|
    ((marked_blocks 100000) (marked_words 200000) (freed_blocks 0)
     (freed_words 0))
    ((length 100000) (in_order true) (mark_stack_capacity 1024))
    |}]
;;

let%expect_test "the policy" =
  let config = { Stag_gc.Config.space_overhead = 100; min_words = 100 } in
  let env = Env.create ~config () in
  let roots = Shadow_stack.push_frame env.roots ~slots:1 in
  let allocate_garbage n =
    for _ = 1 to n do
      ignore (alloc env ~ptrs:1 : Addr.t)
    done
  in
  (* Nothing live: a collection every [min_words]. *)
  allocate_garbage 1000;
  print_s [%message "" ~collections:(Stag_gc.collections env.gc : int)];
  (* 1000 words live: every 1000 words. *)
  Memory.store_addr roots (list env ~n:500 ~first:0);
  let collections = Stag_gc.collections env.gc in
  allocate_garbage 5000;
  print_s
    [%message
      ""
        ~collections:(Stag_gc.collections env.gc - collections : int)
        ~live:(List.length (list_values (Memory.load_addr roots)) : int)];
  check env;
  [%expect {|
    (collections 19)
    ((collections 7) (live 500))
    |}]
;;

let%expect_test "collecting allocates the same however much is live" =
  let allocated_by_collect ~n =
    let env = Env.create () in
    let roots = Shadow_stack.push_frame env.roots ~slots:1 in
    Memory.store_addr roots (list env ~n ~first:0);
    let (_ : Addr.t) = list env ~n ~first:0 in
    (* Once to settle the free lists' pages. *)
    ignore (Stag_gc.collect env.gc : Stag_gc.Stats.t);
    let before = Gc.minor_words () in
    ignore (Stag_gc.collect env.gc : Stag_gc.Stats.t);
    Gc.minor_words () - before
  in
  print_s
    [%message
      ""
        ~small:(allocated_by_collect ~n:10 : int)
        ~large:(allocated_by_collect ~n:100_000 : int)];
  [%expect {| ((small 11) (large 11)) |}]
;;

let%expect_test "destroy" =
  let heap = Heap.create Heap.Config.default in
  let roots = Shadow_stack.create () in
  let gc =
    Stag_gc.create
      Stag_gc.Config.default
      ~heap
      ~roots:[ Shadow_stack.root_source roots ]
      ~data_stack:(Data_stack.create ())
      ~static:(Static_data.create ())
  in
  Stag_gc.destroy gc;
  let try_ f = print_s [%sexp (Or_error.try_with f : unit Or_error.t)] in
  try_ (fun () -> ignore (Stag_gc.collect gc : Stag_gc.Stats.t));
  try_ (fun () ->
    ignore
      (Stag_gc.alloc gc (Header.scanned ~size_words:1 ~scalar_words:0 Unmarked)
       : Memory.Addr.t));
  try_ (fun () -> Stag_gc.destroy gc);
  Shadow_stack.destroy roots;
  Heap.destroy heap;
  [%expect
    {|
    (Error "Gc: destroyed")
    (Error "Gc: destroyed")
    (Error "Gc: destroyed")
    |}]
;;

let%expect_test "a mark that raises leaves no block marked" =
  (* The mark stack fails to grow with 1024 children of [wide] marked but not
     scanned: the next collection must still trace their leaves. *)
  let env = Env.create () in
  let roots = Shadow_stack.push_frame env.roots ~slots:1 in
  let wide = alloc env ~ptrs:2000 in
  Memory.store_addr roots wide;
  for i = 0 to 1999 do
    let child = alloc env ~ptrs:1 in
    set child 0 (alloc env ~ptrs:0 ~scalars:1);
    set wide i child
  done;
  Stag_gc.For_testing.fail_next_grow env.gc;
  Expect_test_helpers_core.require_does_raise (fun () ->
    ignore (Stag_gc.collect env.gc : Stag_gc.Stats.t));
  print_s [%sexp (Stag_gc.check_invariants env.gc : unit Or_error.t)];
  collect env;
  [%expect
    {|
    "Gc: injected mark stack failure"
    (Ok ())
    ((marked_blocks 4001) (marked_words 6000) (freed_blocks 0) (freed_words 0))
    |}]
;;
