open! Import
open Memory

(* Compiling these checks the [zero_alloc] claims of [Memory] and [Header]
   across modules. *)

let[@zero_alloc] sum_words base ~words =
  let mutable acc = #0L in
  for i = 0 to words - 1 do
    acc <- Int64_u.add acc (load_64 (Addr.add base (i * 8)))
  done;
  acc
;;

let[@zero_alloc] round_trip chunk addr =
  if Chunk.contains chunk addr
  then (
    store_8 addr (load_8 addr);
    store_16 addr (load_16 addr);
    store_32 addr (load_32 addr);
    store_64 addr (load_64 addr);
    store_f32 addr (load_f32 addr);
    store_f64 addr (load_f64 addr);
    store_addr addr (load_addr addr);
    copy ~src:addr ~dst:addr ~bytes:8;
    fill_zero addr ~bytes:0)
;;

let[@zero_alloc] mark block =
  let header = Header.load block in
  Header.store block (Header.with_color header Marked);
  Header.size_words header - Header.scalar_words header
;;

let%expect_test "no allocation at run time" =
  let chunk = Chunk.create ~bytes:64 ~align:8 in
  let block = Addr.add (Chunk.base chunk) 8 in
  Header.store block (Header.scanned ~size_words:3 ~scalar_words:1 Unmarked);
  store_64 (Addr.add block 8) #5L;
  let before = Gc.minor_words () in
  let pointers = mark block in
  round_trip chunk block;
  let sum = sum_words block ~words:3 in
  let allocated = Gc.minor_words () - before in
  print_s
    [%message
      ""
        (pointers : int)
        ~sum:(Int64_u.to_int64 sum : int64)
        ~header:(Header.load block : Header.t)
        (allocated : int)];
  Chunk.destroy chunk;
  [%expect
    {|
    ((pointers 2) (sum 5)
     (header (Scanned (size_words 3) (scalar_words 1) (color Marked)))
     (allocated 0))
    |}]
;;

(* As a GC would, resolving every word of a block and counting those that
   resolve to it. *)
let[@zero_alloc] resolve_words heap block ~words =
  let mutable found = 0 in
  for i = 0 to words - 1 do
    let start = Heap.block_start heap (Addr.add block (i * 8)) in
    if Addr.equal start block then found <- found + 1
  done;
  found
;;

let[@zero_alloc] page_owned table addr =
  not (Addr.is_null (Page_table.find table addr))
;;

let%expect_test "no allocation in lookups" =
  let heap =
    Heap.create
      { Heap.Config.page_bytes = 4096
      ; arena_bytes = 65536
      ; size_classes = [ 4 ]
      }
  in
  let block =
    Heap.alloc heap (Header.scanned ~size_words:4 ~scalar_words:0 Unmarked)
  in
  let large =
    Heap.alloc heap (Header.scanned ~size_words:1000 ~scalar_words:0 Unmarked)
  in
  let table = Page_table.create ~page_bytes:4096 in
  Page_table.add table ~page:(Addr.null ()) ~owner:(Addr.of_word #8192n);
  let before = Gc.minor_words () in
  let small_found = resolve_words heap block ~words:4 in
  let large_found = resolve_words heap large ~words:1000 in
  let owned = page_owned table (Addr.of_word #100n) in
  let not_owned = page_owned table (Addr.of_word #5000n) in
  let allocated = Gc.minor_words () - before in
  print_s
    [%message
      ""
        (small_found : int)
        (large_found : int)
        (owned : bool)
        (not_owned : bool)
        (allocated : int)];
  [%expect
    {|
    ((small_found 4) (large_found 1000) (owned true) (not_owned false)
     (allocated 0))
    |}]
;;

let[@zero_alloc] walk_roots roots =
  let mutable count = 0 in
  let mutable frame = Shadow_stack.latest_frame roots in
  while not (Addr.is_null frame) do
    for i = 0 to Shadow_stack.Frame.slots frame - 1 do
      if not (Addr.is_null (load_addr (Shadow_stack.Frame.slot frame i)))
      then count <- count + 1
    done;
    frame <- Shadow_stack.Frame.next frame
  done;
  count
;;

let[@zero_alloc] push_and_pop roots ~frames =
  for i = 1 to frames do
    let slots = i land 3 in
    let slot = Shadow_stack.push_frame roots ~slots in
    if Int.(slots > 0) then store_addr slot slot
  done;
  let count = walk_roots roots in
  for _ = 1 to frames do
    Shadow_stack.pop_frame roots
  done;
  count
;;

let%expect_test "no allocation in pushing, popping and walking frames" =
  let roots = Shadow_stack.create () in
  (* Once the segments are there, nothing allocates. *)
  let (_ : int) = push_and_pop roots ~frames:10_000 in
  let before = Gc.minor_words () in
  let count = push_and_pop roots ~frames:10_000 in
  let allocated = Gc.minor_words () - before in
  print_s [%message "" (count : int) (allocated : int)];
  [%expect {| ((count 7500) (allocated 0)) |}]
;;

let[@zero_alloc] regions stack ~blocks =
  let outer = Data_stack.enter stack in
  for i = 1 to blocks do
    let inner = Data_stack.enter stack in
    let block =
      Data_stack.alloc
        stack
        (Header.scanned ~size_words:(1 + (i land 7)) ~scalar_words:1 Unmarked)
    in
    store_addr block block;
    if Int.(i land 1 = 0) then Data_stack.exit stack inner
  done;
  let mutable count = 0 in
  for segment = 0 to Data_stack.segments stack - 1 do
    let end_ = Data_stack.segment_end stack segment in
    let mutable block = Data_stack.first_block stack segment in
    while Int.(Addr.compare (Addr.add block (-8)) end_ < 0) do
      count <- count + 1;
      block <- Data_stack.next_block block
    done
  done;
  Data_stack.exit stack outer;
  count
;;

let%expect_test "no allocation in data-stack regions" =
  let stack = Data_stack.create ~segment_bytes:1024 () in
  let (_ : int) = regions stack ~blocks:10_000 in
  let before = Gc.minor_words () in
  let count = regions stack ~blocks:10_000 in
  let allocated = Gc.minor_words () - before in
  print_s
    [%message
      ""
        (count : int)
        (allocated : int)
        ~segments_made:(Data_stack.For_testing.segments_made stack : int)];
  Data_stack.destroy stack;
  [%expect {| ((count 5000) (allocated 0) (segments_made 250)) |}]
;;

let[@zero_alloc] push_wide roots ~frames =
  for _ = 1 to frames do
    let slot = Shadow_stack.push_frame roots ~slots:100 in
    store_addr (Addr.add slot (99 * 8)) slot
  done;
  let count = walk_roots roots in
  for _ = 1 to frames do
    Shadow_stack.pop_frame roots
  done;
  count
;;

let%expect_test "no allocation in pushing frames of 100 slots, past the first \
                 maps"
  =
  let roots = Shadow_stack.create () in
  let (_ : int) = push_wide roots ~frames:100 in
  let before = Gc.minor_words () in
  let count = push_wide roots ~frames:100 in
  let allocated = Gc.minor_words () - before in
  print_s [%message "" (count : int) (allocated : int)];
  Shadow_stack.destroy roots;
  [%expect {| ((count 100) (allocated 0)) |}]
;;

(* Allocation on the OCaml heap, minor or major. *)
let ocaml_words () =
  Gc.minor_words () + Gc.major_words () - Gc.promoted_words ()
;;

(* [Root_source.iter]'s call through the source is only assumed zero-alloc, so
   these measure that the common paths through it really don't allocate. *)

let[@zero_alloc] count_roots source =
  let count = stack_ (ref 0) in
  Root_source.iter source ~f:(fun [@zero_alloc] root ->
    if not (Addr.is_null (load_addr root)) then count := !count + 1);
  !count
;;

let[@zero_alloc] count_all sources =
  let mutable count = 0 in
  for i = 0 to Array.length sources - 1 do
    count <- count + count_roots sources.(i)
  done;
  count
;;

let%expect_test "no allocation in iterating root sources" =
  let stack = Shadow_stack.create () in
  let globals = Globals.create () in
  let table = Root_table.create () in
  let chunk = Chunk.create ~bytes:(200 * 8) ~align:8 in
  let word i = Addr.add (Chunk.base chunk) (i * 8) in
  for i = 0 to 199 do
    store_addr (word i) (if i % 3 = 0 then Addr.null () else word i);
    if i < 100
    then Globals.add globals (word i)
    else Root_table.add table (word i)
  done;
  for i = 1 to 1000 do
    let slot = Shadow_stack.push_frame stack ~slots:(i % 4) in
    if i % 4 > 0 then store_addr slot slot
  done;
  let sources =
    [| Shadow_stack.root_source stack
     ; Globals.root_source globals
     ; Root_table.root_source table
    |]
  in
  let counts = Array.map sources ~f:(fun source -> count_roots source) in
  let before = ocaml_words () in
  let total = count_all sources in
  let allocated = ocaml_words () - before in
  print_s [%message "" (counts : int array) (total : int) (allocated : int)];
  Chunk.destroy chunk;
  Shadow_stack.destroy stack;
  [%expect {|
    ((counts (750 66 67)) (total 883) (allocated 0))
    |}]
;;

module Populated = struct
  type t =
    { gc : Stag_gc.t
    ; heap : Heap.t
    ; stack : Shadow_stack.t
    ; data_stack : Data_stack.t
    ; chunk : Chunk.t
    }

  let manual =
    { Stag_gc.Config.space_overhead = 100; min_words = Int.max_value }
  ;;

  (* Lists rooted by a shadow stack, globals, a user-defined table and the data
     stack, with as much garbage. *)
  let create () =
    let heap = Heap.create Heap.Config.default in
    let stack = Shadow_stack.create () in
    let globals = Globals.create () in
    let table = Root_table.create () in
    let data_stack = Data_stack.create () in
    let gc =
      Stag_gc.create
        manual
        ~heap
        ~roots:
          [ Shadow_stack.root_source stack
          ; Globals.root_source globals
          ; Root_table.root_source table
          ]
        ~data_stack
        ~static:(Static_data.create ())
    in
    let chunk = Chunk.create ~bytes:(20 * 8) ~align:8 in
    let cell next =
      let cell =
        Stag_gc.alloc gc (Header.scanned ~size_words:2 ~scalar_words:1 Unmarked)
      in
      store_addr (Addr.add cell 8) next;
      cell
    in
    (* Each list is built in a root word, so it is rooted while it grows. *)
    let list root ~n =
      store_addr root (Addr.null ());
      for _ = 1 to n do
        store_addr root (cell (load_addr root))
      done
    in
    for i = 0 to 9 do
      let slot = Shadow_stack.push_frame stack ~slots:1 in
      list slot ~n:100;
      list (Addr.add (Chunk.base chunk) (i * 8)) ~n:100;
      Globals.add globals (Addr.add (Chunk.base chunk) (i * 8));
      list (Addr.add (Chunk.base chunk) ((10 + i) * 8)) ~n:100;
      Root_table.add table (Addr.add (Chunk.base chunk) ((10 + i) * 8));
      let local =
        Data_stack.alloc
          data_stack
          (Header.scanned ~size_words:1 ~scalar_words:0 Unmarked)
      in
      list local ~n:100;
      let garbage = Shadow_stack.push_frame stack ~slots:1 in
      list garbage ~n:400;
      Shadow_stack.pop_frame stack
    done;
    { gc; heap; stack; data_stack; chunk }
  ;;

  let destroy t =
    Stag_gc.destroy t.gc;
    Data_stack.destroy t.data_stack;
    Shadow_stack.destroy t.stack;
    Heap.destroy t.heap;
    Chunk.destroy t.chunk
  ;;
end

let%expect_test "no allocation in marking from several sources" =
  let t = Populated.create () in
  let first = Stag_gc.collect t.gc in
  let before = ocaml_words () in
  Stag_gc.For_testing.mark t.gc;
  let allocated_by_mark = ocaml_words () - before in
  (* The sweep after it keeps what it marked. *)
  let sweep = Heap.sweep t.heap in
  let before = ocaml_words () in
  let second = Stag_gc.collect t.gc in
  let allocated_by_collect = ocaml_words () - before in
  print_s
    [%message
      ""
        (first : Stag_gc.Stats.t)
        (allocated_by_mark : int)
        ~freed_by_sweep:(sweep.freed_blocks : int)
        (second : Stag_gc.Stats.t)
        (allocated_by_collect : int)
        ~invariants:(Stag_gc.check_invariants t.gc : unit Or_error.t)];
  Populated.destroy t;
  [%expect
    {|
    ((first
      ((marked_blocks 4000) (marked_words 8000) (freed_blocks 4000)
       (freed_words 8000)))
     (allocated_by_mark 0) (freed_by_sweep 0)
     (second
      ((marked_blocks 4000) (marked_words 8000) (freed_blocks 0) (freed_words 0)))
     (allocated_by_collect 11) (invariants (Ok ())))
    |}]
;;

let alloc_cells gc ~n =
  for _ = 1 to n do
    ignore
      (Stag_gc.alloc gc (Header.scanned ~size_words:2 ~scalar_words:1 Unmarked)
       : Addr.t)
  done
;;

let%expect_test "no allocation in Gc.alloc's fast path" =
  let t = Populated.create () in
  ignore (Stag_gc.collect t.gc : Stag_gc.Stats.t);
  (* A list of 20000 cells with every other one cut out, so the collection
     leaves 10000 free slots in pages still in use. *)
  let root = Shadow_stack.push_frame t.stack ~slots:1 in
  store_addr root (Addr.null ());
  for _ = 1 to 20_000 do
    let cell =
      Stag_gc.alloc t.gc (Header.scanned ~size_words:2 ~scalar_words:1 Unmarked)
    in
    store_addr (Addr.add cell 8) (load_addr root);
    store_addr root cell
  done;
  let mutable cell = load_addr root in
  while not (Addr.is_null cell) do
    let skipped = load_addr (Addr.add cell 8) in
    let next =
      if Addr.is_null skipped then skipped else load_addr (Addr.add skipped 8)
    in
    store_addr (Addr.add cell 8) next;
    cell <- next
  done;
  let freed = (Stag_gc.collect t.gc).freed_blocks in
  let before = ocaml_words () in
  alloc_cells t.gc ~n:10_000;
  let allocated = ocaml_words () - before in
  print_s
    [%message
      ""
        (freed : int)
        (allocated : int)
        ~collections:(Stag_gc.collections t.gc : int)
        ~invariants:(Stag_gc.check_invariants t.gc : unit Or_error.t)];
  Shadow_stack.pop_frame t.stack;
  Populated.destroy t;
  [%expect
    {| ((freed 10000) (allocated 0) (collections 2) (invariants (Ok ()))) |}]
;;
