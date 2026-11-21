open! Import
module Addr = Memory.Addr

let heap_config =
  { Heap.Config.page_bytes = 256; arena_bytes = 4096; size_classes = [ 1; 2 ] }
;;

let manual = { Stag_gc.Config.space_overhead = 100; min_words = Int.max_value }

let%expect_test "a user-defined root source keeps exactly what it roots alive" =
  let heap = Heap.create heap_config in
  let table = Root_table.create () in
  let data_stack = Data_stack.create () in
  let static = Static_data.create () in
  let source = Root_table.root_source table in
  let gc =
    Stag_gc.create
      manual
      ~heap
      ~roots:[ source; Shadow_stack.root_source (Shadow_stack.create ()) ]
      ~data_stack
      ~static
  in
  (* Ten root words in C memory, in the table. Each block is [id; next]. *)
  let words = Memory.Chunk.create ~bytes:80 ~align:8 in
  let root i = Addr.add (Memory.Chunk.base words) (i * 8) in
  for i = 0 to 9 do
    Memory.store_addr (root i) (Addr.null ());
    Root_table.add table (root i)
  done;
  let alloc ~id ~next =
    let block =
      Stag_gc.alloc gc (Header.scanned ~size_words:2 ~scalar_words:1 Unmarked)
    in
    Memory.store_64 block (Int64_u.of_int id);
    Memory.store_addr (Addr.add block 8) next;
    block
  in
  let ids () =
    let ids = ref [] in
    Heap.iter heap ~f:(fun block ->
      ids := Int64_u.to_int (Memory.load_64 block) :: !ids);
    List.sort ~compare !ids
  in
  (* 1 -> 2 -> 3 and 4 rooted (4 twice, and by a root pointing inside it), 5 ->
     6 and 7 not; a small value in a root word is left alone. *)
  Memory.store_addr
    (root 0)
    (alloc ~id:1 ~next:(alloc ~id:2 ~next:(alloc ~id:3 ~next:(Addr.null ()))));
  let four = alloc ~id:4 ~next:(Addr.null ()) in
  Memory.store_addr (root 3) four;
  Memory.store_addr (root 4) four;
  Memory.store_addr (root 5) (Addr.add four 8);
  Memory.store_addr (root 6) (Addr.of_word #42n);
  ignore (alloc ~id:5 ~next:(alloc ~id:6 ~next:(Addr.null ())) : Addr.t);
  ignore (alloc ~id:7 ~next:(Addr.null ()) : Addr.t);
  let collect () =
    let stats = Stag_gc.collect gc in
    print_s
      [%message
        ""
          (stats : Stag_gc.Stats.t)
          ~live:(ids () : int list)
          ~invariants:(Stag_gc.check_invariants gc : unit Or_error.t)]
  in
  print_s
    [%message
      ""
        ~name:(Root_source.name source : string)
        ~holds_locals:(Root_source.holds_locals source : bool)
        ~all:(ids () : int list)];
  collect ();
  (* Dropping a root word from the table, not its value, frees what it held. *)
  Root_table.clear table;
  Root_table.add table (root 3);
  collect ();
  Root_table.clear table;
  collect ();
  [%expect
    {|
    ((name table) (holds_locals false) (all (1 2 3 4 5 6 7)))
    ((stats
      ((marked_blocks 4) (marked_words 8) (freed_blocks 3) (freed_words 6)))
     (live (1 2 3 4)) (invariants (Ok ())))
    ((stats
      ((marked_blocks 1) (marked_words 2) (freed_blocks 3) (freed_words 6)))
     (live (4)) (invariants (Ok ())))
    ((stats
      ((marked_blocks 0) (marked_words 0) (freed_blocks 1) (freed_words 2)))
     (live ()) (invariants (Ok ())))
    |}];
  Memory.Chunk.destroy words;
  Stag_gc.destroy gc;
  Heap.destroy heap
;;

let%expect_test "check_invariants checks a user-defined source's root words" =
  let heap = Heap.create heap_config in
  let table = Root_table.create () in
  let data_stack = Data_stack.create () in
  let gc =
    Stag_gc.create
      manual
      ~heap
      ~roots:[ Root_table.root_source table ]
      ~data_stack
      ~static:(Static_data.create ())
  in
  let words = Memory.Chunk.create ~bytes:24 ~align:8 in
  let root i = Addr.add (Memory.Chunk.base words) (i * 8) in
  for i = 0 to 2 do
    Memory.store_addr (root i) (Addr.null ());
    Root_table.add table (root i)
  done;
  let alloc () =
    Stag_gc.alloc gc (Header.scanned ~size_words:1 ~scalar_words:1 Unmarked)
  in
  let kept = alloc () in
  let block = alloc () in
  let local =
    Data_stack.alloc
      data_stack
      (Header.scanned ~size_words:1 ~scalar_words:1 Unmarked)
  in
  Memory.store_addr (root 0) kept;
  Memory.store_addr (root 2) block;
  print_s [%sexp (Stag_gc.check_invariants gc : unit Or_error.t)];
  (* Not [holds_locals]: it must not point into the data stack. And a freed slot
     of a page still in use is no block. *)
  Memory.store_addr (root 2) (Addr.null ());
  ignore (Stag_gc.collect gc : Stag_gc.Stats.t);
  Memory.store_addr (root 2) block;
  Memory.store_addr (root 1) local;
  print_s [%sexp (Stag_gc.check_invariants gc : unit Or_error.t)];
  [%expect
    {|
    (Ok ())
    (Error
     (Gc.check_invariants
      (errors
       (("a pointer into the data stack" (from (root (source table) (index 1))))
        ("a pointer into the heap but no block"
         (from (root (source table) (index 2))))))))
    |}];
  Memory.Chunk.destroy words;
  Stag_gc.destroy gc;
  Data_stack.destroy data_stack;
  Heap.destroy heap
;;
