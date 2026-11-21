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
    Heap.create { Heap.Config.page_bytes = 4096; size_classes = [ 4 ] }
  in
  let block =
    Heap.alloc heap (Header.scanned ~size_words:4 ~scalar_words:0 Unmarked)
  in
  let large =
    Heap.alloc heap (Header.scanned ~size_words:1000 ~scalar_words:0 Unmarked)
  in
  let table = Page_table.create ~page_bytes:4096 in
  Page_table.add table ~page:(Addr.null ()) ~owner:block;
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
