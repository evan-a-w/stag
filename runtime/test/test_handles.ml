open! Import
module Addr = Memory.Addr

let key = Type_equal.Id.create ~name:"name" String.sexp_of_t
let other_key = Type_equal.Id.create ~name:"other name" String.sexp_of_t

let addr_key =
  Type_equal.Id.create ~name:"addresses" (fun (_ : Addr.t array) ->
    Sexp.Atom "<addresses>")
;;

let create_gc ?(config = Stag_gc.Config.default) ?(classes = [ 1 ]) () =
  let heap =
    Heap.create { page_bytes = 256; arena_bytes = 4096; size_classes = classes }
  in
  let roots = Roots.create () in
  let data_stack = Data_stack.create () in
  let static = Static_data.create () in
  let gc = Stag_gc.create config ~heap ~roots ~data_stack ~static in
  gc, heap, roots, data_stack, static
;;

let destroy gc roots data_stack static heap =
  Stag_gc.destroy gc;
  Roots.destroy roots;
  Data_stack.destroy data_stack;
  Static_data.destroy static;
  Heap.destroy heap
;;

let word block = Int64_u.to_int (Memory.load_64 block)
let store_word block n = Memory.store_64 block (Int64_u.of_int n)

let live heap block =
  (not (Addr.is_null block)) && Addr.equal (Heap.block_start heap block) block
;;

let%expect_test "handle reachability and slot reuse" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:1 in
  let parent =
    Stag_gc.alloc gc (Header.scanned ~size_words:1 ~scalar_words:0 Unmarked)
  in
  Memory.store_addr root parent;
  let kept = Stag_gc.alloc_handle gc key "kept" ~refs:[||] in
  let dead = Stag_gc.alloc_handle gc key "dead" ~refs:[||] in
  let old_id = word dead in
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
  let fresh = Stag_gc.alloc_handle gc key "fresh" ~refs:[||] in
  print_s
    [%message
      ""
        ~reused:(Addr.equal dead fresh : bool)
        ~new_id:(word fresh > old_id : bool)
        ~fresh:(Stag_gc.get_handle gc fresh key : string option)];
  Roots.pop_frame roots;
  collect ();
  let _ =
    Stag_gc.alloc_handle gc key "released at heap destrucion" ~refs:[||]
  in
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
  let gc, heap, roots, data_stack, static =
    create_gc ~config:{ space_overhead = 0; min_words = 2 } ()
  in
  let root = Roots.push_frame roots ~slots:1 in
  let first = Stag_gc.alloc_handle gc key "first" ~refs:[||] in
  Memory.store_addr root first;
  let second = Stag_gc.alloc_handle gc key "second" ~refs:[||] in
  print_s
    [%message
      ""
        ~collections:(Stag_gc.collections gc : int)
        ~first:(Stag_gc.get_handle gc first key : string option)
        ~second:(Stag_gc.get_handle gc second key : string option)];
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect {|
    ((collections 1) (first (first)) (second (second)))
    |}]
;;

let%expect_test "refs keep their blocks alive, and die with the handle" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:1 in
  let capture =
    Stag_gc.alloc gc (Header.scanned ~size_words:2 ~scalar_words:1 Unmarked)
  in
  store_word capture 42;
  let handle =
    Stag_gc.alloc_handle gc addr_key [| capture |] ~refs:[| capture |]
  in
  print_s
    [%message
      ""
        ~size_words:(Header.size_words (Header.load handle) : int)
        ~scalar_words:(Header.scalar_words (Header.load handle) : int)];
  (* The handle is the capture's only root. *)
  let collect () =
    let stats = Stag_gc.collect gc in
    Stag_gc.check_invariants gc |> ok_exn;
    print_s
      [%message
        ""
          ~marked:(stats.marked_blocks : int)
          ~capture:(live heap capture : bool)
          ~word:(word capture : int)
          ~has_handle:
            (Option.is_some (Stag_gc.get_handle gc handle addr_key) : bool)]
  in
  Memory.store_addr root handle;
  collect ();
  Roots.pop_frame roots;
  collect ();
  destroy gc roots data_stack static heap;
  [%expect
    {|
    ((size_words 2) (scalar_words 1))
    ((marked 2) (capture true) (word 42) (has_handle true))
    ((marked 0) (capture false) (word 42) (has_handle false))
    |}]
;;

let%expect_test "the OCaml payload is not traced" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:1 in
  let capture =
    Stag_gc.alloc gc (Header.scanned ~size_words:1 ~scalar_words:0 Unmarked)
  in
  (* The payload refers to [capture], but nothing traces it. *)
  let handle = Stag_gc.alloc_handle gc addr_key [| capture |] ~refs:[||] in
  Memory.store_addr root handle;
  let stats = Stag_gc.collect gc in
  Stag_gc.check_invariants gc |> ok_exn;
  print_s
    [%message
      ""
        ~marked:(stats.marked_blocks : int)
        ~capture:(live heap capture : bool)
        ~payload:(Option.is_some (Stag_gc.get_handle gc handle addr_key) : bool)];
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect {|
    ((marked 1) (capture false) (payload true))
    |}]
;;

let%expect_test "alloc_handle roots its refs across the collection it runs" =
  let gc, heap, roots, data_stack, static =
    create_gc ~config:{ space_overhead = 0; min_words = 1 } ()
  in
  (* Live, but for the refs passed to [alloc_handle]: collected were they not
     rooted across it. *)
  let capture =
    Stag_gc.alloc gc (Header.scanned ~size_words:1 ~scalar_words:0 Unmarked)
  in
  store_word capture 7;
  let handle =
    Stag_gc.alloc_handle gc addr_key [| capture |] ~refs:[| capture |]
  in
  let root = Roots.push_frame roots ~slots:1 in
  Memory.store_addr root handle;
  print_s
    [%message
      ""
        ~collections:(Stag_gc.collections gc : int)
        ~capture:(live heap capture : bool)
        ~word:(word capture : int)];
  let stats = Stag_gc.collect gc in
  Stag_gc.check_invariants gc |> ok_exn;
  print_s [%sexp (stats.marked_blocks : int), (live heap capture : bool)];
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect {|
    ((collections 2) (capture true) (word 7))
    (2 true)
    |}]
;;

let%expect_test "many refs, in a large block" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:1 in
  let refs = Array.create ~len:300 (Addr.null ()) in
  for i = 0 to Array.length refs - 1 do
    refs.(i)
    <- Stag_gc.alloc gc (Header.scanned ~size_words:1 ~scalar_words:0 Unmarked)
  done;
  let handle = Stag_gc.alloc_handle gc addr_key refs ~refs in
  Memory.store_addr root handle;
  let stats = Stag_gc.collect gc in
  Stag_gc.check_invariants gc |> ok_exn;
  print_s
    [%message
      ""
        ~size_words:(Header.size_words (Header.load handle) : int)
        ~marked:(stats.marked_blocks : int)
        ~marked_words:(stats.marked_words : int)];
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect {| ((size_words 301) (marked 301) (marked_words 601)) |}]
;;

let%expect_test "metadata consistency" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:2 in
  let first = Stag_gc.alloc_handle gc key "first" ~refs:[||] in
  let second = Stag_gc.alloc_handle gc key "second" ~refs:[||] in
  Memory.store_addr root first;
  Memory.store_addr (Addr.add root 8) second;
  let report () =
    match Stag_gc.check_invariants gc with
    | Ok () -> print_endline "ok"
    | Error error -> print_s [%sexp (error : Error.t)]
  in
  report ();
  (* [first] claims [second]'s ID. *)
  store_word first (word second);
  report ();
  Stag_gc.collect gc |> ignore;
  report ();
  (* A block that claims to be a handle, with no metadata. *)
  let orphan =
    Stag_gc.alloc gc (Header.scanned ~size_words:1 ~scalar_words:0 Unmarked)
  in
  Header.store orphan (Header.handle ~refs:0);
  store_word orphan 99;
  print_s [%sexp (Header.load orphan : Header.t)];
  report ();
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect
    {|
    ok
    (Gc.check_invariants
     (errors
      (("handle metadata"
        (error_ ("a handle block whose metadata is another's" (id 1))))
       ("handle metadata"
        (error_
         ("metadata stored under another block's ID" (id 0) (stored_id 1)))))))
    (Gc.check_invariants
     (errors
      (("handle metadata"
        (error_ ("a handle block whose metadata is another's" (id 1))))
       ("handle metadata"
        (error_
         ("metadata stored under another block's ID" (id 0) (stored_id 1)))))))
    (Handle (size_words 1) (color Unmarked))
    (Gc.check_invariants
     (errors
      (("handle metadata"
        (error_ ("a handle block whose metadata is another's" (id 1))))
       ("handle metadata" (error_ ("a handle block with no metadata" (id 99))))
       ("handle metadata"
        (error_
         ("handle blocks and metadata entries of different counts" (blocks 3)
          (entries 2))))
       ("handle metadata"
        (error_
         ("metadata stored under another block's ID" (id 0) (stored_id 1)))))))
    |}]
;;

let%expect_test "a corrupt ID does not remove another block's metadata" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:1 in
  let doomed = Stag_gc.alloc_handle gc key "doomed" ~refs:[||] in
  let kept = Stag_gc.alloc_handle gc key "kept" ~refs:[||] in
  Memory.store_addr root kept;
  (* [doomed] claims [kept]'s ID, and is swept. *)
  store_word doomed (word kept);
  let stats = Stag_gc.collect gc in
  print_s
    [%message
      ""
        ~freed:(stats.freed_blocks : int)
        ~handles:(Heap.For_testing.handles heap : int)
        ~kept:(Stag_gc.get_handle gc kept key : string option)];
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect {| ((freed 1) (handles 2) (kept (kept))) |}]
;;
