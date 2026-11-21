open! Import
module Addr = Memory.Addr

let name_kind =
  Handle_kind.create ~name:"name" String.sexp_of_t ~refs:(fun _ -> [||])
;;

let other_kind =
  Handle_kind.create ~name:"other name" String.sexp_of_t ~refs:(fun _ -> [||])
;;

(* Captured Stag blocks, such as a closure's. *)
let captures_kind =
  Handle_kind.create
    ~name:"captures"
    (fun (_ : Addr.t array) -> Sexp.Atom "<captures>")
    ~refs:Fn.id
;;

(* Code naming other code, as a staged code object naming another. *)
module Code = struct
  type t =
    { name : string
    ; calls : Handle_ref.t list
    }
  [@@deriving sexp_of]

  let kind =
    Handle_kind.create ~name:"code" sexp_of_t ~refs:(fun t ->
      let calls = Array.of_list t.calls in
      let refs = Array.create ~len:(Array.length calls) (Addr.null ()) in
      for i = 0 to Array.length calls - 1 do
        refs.(i) <- Handle_ref.addr calls.(i)
      done;
      refs)
  ;;

  (* Breaks [Handle_kind]'s rules: its calls are not kept alive. *)
  let kind_without_refs =
    Handle_kind.create ~name:"code without refs" sexp_of_t ~refs:(fun _ -> [||])
  ;;
end

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

let report gc =
  match Stag_gc.check_invariants gc with
  | Ok () -> print_endline "ok"
  | Error error -> print_s [%sexp (error : Error.t)]
;;

let scanned gc =
  Stag_gc.alloc gc (Header.scanned ~size_words:1 ~scalar_words:0 Unmarked)
;;

let%expect_test "handle reachability and slot reuse" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:1 in
  let parent = scanned gc in
  Memory.store_addr root parent;
  let kept = Stag_gc.alloc_handle gc name_kind "kept" in
  let dead = Stag_gc.alloc_handle gc name_kind "dead" in
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
        ~kept:(Stag_gc.get_handle gc name_kind kept : string option)
        ~dead:(Stag_gc.get_handle gc name_kind dead : string option)
        ~other_kind:(Stag_gc.get_handle gc other_kind kept : string option)
        ~interior:
          (Stag_gc.get_handle gc name_kind (Addr.add kept 1) : string option)];
  let fresh = Stag_gc.alloc_handle gc name_kind "fresh" in
  print_s
    [%message
      ""
        ~reused:(Addr.equal dead fresh : bool)
        ~new_id:(word fresh > old_id : bool)
        ~fresh:(Stag_gc.get_handle gc name_kind fresh : string option)];
  Roots.pop_frame roots;
  collect ();
  let _ = Stag_gc.alloc_handle gc name_kind "released at heap destrucion" in
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
    ((kept (kept)) (dead ()) (other_kind ()) (interior ()))
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
  let first = Stag_gc.alloc_handle gc name_kind "first" in
  Memory.store_addr root first;
  let second = Stag_gc.alloc_handle gc name_kind "second" in
  print_s
    [%message
      ""
        ~collections:(Stag_gc.collections gc : int)
        ~first:(Stag_gc.get_handle gc name_kind first : string option)
        ~second:(Stag_gc.get_handle gc name_kind second : string option)];
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect {| ((collections 1) (first (first)) (second (second))) |}]
;;

let%expect_test "refs keep their blocks alive, and die with the handle" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:1 in
  let capture =
    Stag_gc.alloc gc (Header.scanned ~size_words:2 ~scalar_words:1 Unmarked)
  in
  store_word capture 42;
  let handle = Stag_gc.alloc_handle gc captures_kind [| capture |] in
  print_s
    [%message
      ""
        ~size_words:(Header.size_words (Header.load handle) : int)
        ~scalar_words:(Header.scalar_words (Header.load handle) : int)
        ~ref_is_capture:
          (Addr.equal (Memory.load_addr (Addr.add handle 8)) capture : bool)];
  (* The handle is the capture's only root. *)
  let collect () =
    let stats = Stag_gc.collect gc in
    Stag_gc.check_invariants gc |> ok_exn;
    print_s
      [%message
        ""
          ~marked:(stats.marked_blocks : int)
          ~freed:(stats.freed_blocks : int)
          ~capture:(live heap capture : bool)
          ~handle:
            (Option.is_some (Stag_gc.get_handle gc captures_kind handle) : bool)]
  in
  Memory.store_addr root handle;
  collect ();
  Roots.pop_frame roots;
  collect ();
  destroy gc roots data_stack static heap;
  [%expect
    {|
    ((size_words 2) (scalar_words 1) (ref_is_capture true))
    ((marked 2) (freed 0) (capture true) (handle true))
    ((marked 0) (freed 2) (capture false) (handle false))
    |}]
;;

let%expect_test "alloc_handle roots refs only its payload holds across the \
                 collection it runs"
  =
  let gc, heap, roots, data_stack, static =
    create_gc ~config:{ space_overhead = 0; min_words = 1 } ()
  in
  (* Nothing but the payload refers to [capture]: it would be collected were it
     not rooted across [alloc_handle]. *)
  let capture = scanned gc in
  store_word capture 7;
  let handle = Stag_gc.alloc_handle gc captures_kind [| capture |] in
  let root = Roots.push_frame roots ~slots:1 in
  Memory.store_addr root handle;
  print_s
    [%message
      ""
        ~collections:(Stag_gc.collections gc : int)
        ~capture:(live heap capture : bool)
        ~word:(word capture : int)];
  let stats = Stag_gc.collect gc in
  report gc;
  print_s [%sexp (stats.marked_blocks : int), (live heap capture : bool)];
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect
    {|
    ((collections 2) (capture true) (word 7))
    ok
    (2 true)
    |}]
;;

let%expect_test "many refs, in a large block" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:1 in
  let refs = Array.create ~len:300 (Addr.null ()) in
  for i = 0 to Array.length refs - 1 do
    refs.(i) <- scanned gc
  done;
  let handle = Stag_gc.alloc_handle gc captures_kind refs in
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

let%expect_test "a handle named only by another's payload lives as long as it" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:1 in
  let callee =
    Stag_gc.alloc_handle gc Code.kind { name = "callee"; calls = [] }
  in
  let callee_ref = Stag_gc.handle_ref gc callee in
  let caller =
    Stag_gc.alloc_handle
      gc
      Code.kind
      { name = "caller"; calls = [ callee_ref ] }
  in
  Memory.store_addr root caller;
  let show () =
    let stats = Stag_gc.collect gc in
    report gc;
    print_s
      [%message
        ""
          ~freed:(stats.freed_blocks : int)
          ~caller:(Stag_gc.get_handle gc Code.kind caller : Code.t option)
          ~callee:
            (Stag_gc.get_handle_checked gc Code.kind callee_ref : Code.t option)]
  in
  show ();
  Memory.store_addr root (Addr.null ());
  show ();
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect
    {|
    ok
    ((freed 0) (caller (((name caller) (calls (((id 0)))))))
     (callee (((name callee) (calls ())))))
    ok
    ((freed 2) (caller ()) (callee ()))
    |}]
;;

(* A payload naming a handle its kind doesn't count as a ref (which a correct
   kind can't do): once the handle is swept, its slot goes to another handle. A
   bare address then finds the other's payload, but a [Handle_ref.t] nothing. *)
let%expect_test "a stale handle reference" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:1 in
  let callee =
    Stag_gc.alloc_handle gc Code.kind { name = "callee"; calls = [] }
  in
  let callee_ref = Stag_gc.handle_ref gc callee in
  let caller =
    Stag_gc.alloc_handle
      gc
      Code.kind_without_refs
      { name = "caller"; calls = [ callee_ref ] }
  in
  Memory.store_addr root caller;
  let stats = Stag_gc.collect gc in
  report gc;
  let other =
    Stag_gc.alloc_handle gc Code.kind { name = "other"; calls = [] }
  in
  print_s
    [%message
      ""
        ~freed:(stats.freed_blocks : int)
        ~reused:(Addr.equal other callee : bool)
        ~by_address:(Stag_gc.get_handle gc Code.kind callee : Code.t option)
        ~checked:
          (Stag_gc.get_handle_checked gc Code.kind callee_ref : Code.t option)
        ~other:
          (Stag_gc.get_handle_checked gc Code.kind (Stag_gc.handle_ref gc other)
           : Code.t option)];
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect
    {|
    ok
    ((freed 1) (reused true) (by_address (((name other) (calls ()))))
     (checked ()) (other (((name other) (calls ())))))
    |}]
;;

let%expect_test "get_handle_checked of a handle with a corrupt ID" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:1 in
  let handle = Stag_gc.alloc_handle gc name_kind "handle" in
  Memory.store_addr root handle;
  let handle_ref = Stag_gc.handle_ref gc handle in
  let get () =
    print_s
      [%sexp
        (Stag_gc.get_handle_checked gc name_kind handle_ref : string option)]
  in
  get ();
  store_word handle 99;
  get ();
  store_word handle (Handle_ref.id handle_ref);
  get ();
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect {|
    (handle)
    ()
    (handle)
    |}]
;;

let%expect_test "handle_ref of anything but a handle raises" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let block = scanned gc in
  let handle = Stag_gc.alloc_handle gc name_kind "handle" in
  let raises addr =
    Expect_test_helpers_core.require_does_raise (fun () ->
      Stag_gc.handle_ref gc addr)
  in
  raises block;
  raises (Addr.add handle 8);
  raises (Addr.null ());
  print_s [%sexp (Stag_gc.handle_ref gc handle : Handle_ref.t)];
  destroy gc roots data_stack static heap;
  [%expect
    {|
    "Heap.handle_ref: not a handle"
    "Heap.handle_ref: not a handle"
    "Heap.handle_ref: not a handle"
    ((id 0))
    |}]
;;

let%expect_test "alloc_handle rejects a ref into the heap but no block" =
  let gc, heap, roots, data_stack, static = create_gc ~classes:[ 2 ] () in
  let block = scanned gc in
  let root = Roots.push_frame roots ~slots:1 in
  Memory.store_addr root block;
  let swept = scanned gc in
  ignore (Stag_gc.collect gc : Stag_gc.Stats.t);
  let alloc_raises payload =
    Expect_test_helpers_core.require_does_raise (fun () ->
      ignore
        (Addr.is_null (Stag_gc.alloc_handle gc captures_kind payload) : bool))
  in
  alloc_raises [| swept |];
  alloc_raises [| block; Addr.add block (-8) |];
  print_s [%message "" ~handles:(Heap.For_testing.handles heap : int)];
  report gc;
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect
    {|
    ("Heap.alloc_handle: a ref to no block"
      (kind  captures)
      (index 0))
    ("Heap.alloc_handle: a ref to no block"
      (kind  captures)
      (index 1))
    (handles 0)
    ok
    |}]
;;

let%expect_test "check_invariants reports refs a handle lacks" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:4 in
  let slot i = Addr.add root (i * 8) in
  let first = scanned gc in
  let second = scanned gc in
  let payload = [| first; second |] in
  let handle = Stag_gc.alloc_handle gc captures_kind payload in
  Memory.store_addr (slot 0) handle;
  report gc;
  (* A ref word overwritten, as through a typed block accessor. *)
  let other = scanned gc in
  Memory.store_addr (slot 1) other;
  Memory.store_addr (Addr.add handle 16) other;
  report gc;
  Memory.store_addr (Addr.add handle 16) second;
  report gc;
  (* The payload's Stag pointers changed after creation: [Handle_kind]'s rules
     ask for a Stag block among the refs to be mutated instead. *)
  payload.(0) <- other;
  report gc;
  payload.(0) <- first;
  (* A ref word to no block. *)
  Memory.store_addr (Addr.add handle 8) (Addr.add first (-8));
  report gc;
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect
    {|
    ok
    (Gc.check_invariants
     (errors
      (("handle metadata"
        (error_
         ("a handle without refs of its payload" (id 0) (kind captures)
          (missing (1))))))))
    ok
    (Gc.check_invariants
     (errors
      (("handle metadata"
        (error_
         ("a handle without refs of its payload" (id 0) (kind captures)
          (missing (0))))))))
    (Gc.check_invariants
     (errors
      (("a pointer into the heap but no block" (from (block (block 3) (word 1))))
       ("handle metadata"
        (error_
         ("a handle without refs of its payload" (id 0) (kind captures)
          (missing (0))))))))
    |}]
;;

let%expect_test "metadata consistency" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:2 in
  let first = Stag_gc.alloc_handle gc name_kind "first" in
  let second = Stag_gc.alloc_handle gc name_kind "second" in
  Memory.store_addr root first;
  Memory.store_addr (Addr.add root 8) second;
  report gc;
  (* [first] claims [second]'s ID. *)
  store_word first (word second);
  report gc;
  Stag_gc.collect gc |> ignore;
  report gc;
  (* A block that claims to be a handle, with no metadata. *)
  let orphan = scanned gc in
  Header.store orphan (Header.handle ~refs:0);
  store_word orphan 99;
  print_s [%sexp (Header.load orphan : Header.t)];
  report gc;
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

let%expect_test "a corrupt ID does not remove another block's metadata, but \
                 leaks its own"
  =
  let gc, heap, roots, data_stack, static = create_gc () in
  let root = Roots.push_frame roots ~slots:1 in
  let doomed = Stag_gc.alloc_handle gc name_kind "doomed" in
  let kept = Stag_gc.alloc_handle gc name_kind "kept" in
  Memory.store_addr root kept;
  (* [doomed] claims [kept]'s ID, and is swept. Its own entry, under the ID it
     had, is never removed. *)
  store_word doomed (word kept);
  let stats = Stag_gc.collect gc in
  print_s
    [%message
      ""
        ~freed:(stats.freed_blocks : int)
        ~handles:(Heap.For_testing.handles heap : int)
        ~kept:(Stag_gc.get_handle gc name_kind kept : string option)];
  report gc;
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect
    {|
    ((freed 1) (handles 2) (kept (kept)))
    (Gc.check_invariants
     (errors
      (("handle metadata"
        (error_
         ("handle blocks and metadata entries of different counts" (blocks 1)
          (entries 2))))
       ("handle metadata" (error_ ("metadata for no live block" (id 0)))))))
    |}]
;;

let%expect_test "alloc_handle rejects a ref into the data stack" =
  let gc, heap, roots, data_stack, static = create_gc () in
  let alloc_raises payload =
    Expect_test_helpers_core.require_does_raise (fun () ->
      ignore
        (Addr.is_null (Stag_gc.alloc_handle gc captures_kind payload) : bool))
  in
  let mark = Data_stack.enter data_stack in
  let local =
    Data_stack.alloc
      data_stack
      (Header.scanned ~size_words:1 ~scalar_words:0 Unmarked)
  in
  alloc_raises [| local |];
  Data_stack.exit data_stack mark;
  alloc_raises [| Static_data.empty (); local |];
  print_s [%message "" ~handles:(Heap.For_testing.handles heap : int)];
  report gc;
  destroy gc roots data_stack static heap;
  [%expect
    {|
    ("Gc.alloc_handle: a ref into the data stack"
     (kind  captures)
     (index 0))
    ("Gc.alloc_handle: a ref into the data stack"
     (kind  captures)
     (index 1))
    (handles 0)
    ok
    |}]
;;

let%expect_test "alloc_handle failing leaves no handle" =
  let gc, heap, roots, data_stack, static = create_gc ~classes:[ 1; 2 ] () in
  let root = Roots.push_frame roots ~slots:2 in
  let capture = scanned gc in
  Memory.store_addr root capture;
  let handle = Stag_gc.alloc_handle gc name_kind "first" in
  Memory.store_addr (Addr.add root 8) handle;
  let alloc_raises () =
    Expect_test_helpers_core.require_does_raise (fun () ->
      ignore
        (Addr.is_null (Stag_gc.alloc_handle gc captures_kind [| capture |])
         : bool))
  in
  let show () =
    report gc;
    print_s
      [%message
        ""
          ~handles:(Heap.For_testing.handles heap : int)
          ~first:(Stag_gc.get_handle gc name_kind handle : string option)]
  in
  (* Registering it raises, as [first] has its ID. *)
  Heap.For_testing.set_next_handle_id heap 0;
  alloc_raises ();
  show ();
  Heap.iter heap ~f:(fun block ->
    print_s [%sexp (Header.load block : Header.t)]);
  Heap.For_testing.set_next_handle_id heap Int.max_value;
  alloc_raises ();
  show ();
  let stats = Stag_gc.collect gc in
  print_s [%message "" ~freed:(stats.freed_blocks : int)];
  show ();
  Roots.pop_frame roots;
  destroy gc roots data_stack static heap;
  [%expect
    {|
    ("Hashtbl.add_exn got key already present" 0)
    ok
    ((handles 1) (first (first)))
    (Scanned (size_words 1) (scalar_words 0) (color Unmarked))
    (Handle (size_words 1) (color Unmarked))
    (Scanned (size_words 2) (scalar_words 1) (color Unmarked))
    "Heap.alloc_handle: ID exhausted"
    ok
    ((handles 1) (first (first)))
    (freed 1)
    ok
    ((handles 1) (first (first)))
    |}]
;;
