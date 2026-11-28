open! Import
module Addr = Memory.Addr

type t =
  { heap : Heap.t
  ; data_stack : Data_stack.t
  ; shadow_stack : Shadow_stack.t
  ; scalar_stack : Scalar_stack.t
  ; static_data : Static_data.t
  ; globals : Globals.t
  ; root_table : Root_table.t
  ; gc : Gc.t
  ; exn_table : Runtime_objects.Exn_table.t
  ; partial_table : Runtime_objects.Partial_table.t
  ; static_strings : Nativeint.t String.Table.t
  ; mutable allocated_globals : Static_data.Global.t list
  ; mutable destroyed : bool
  }

let create
  ?(heap_config = Heap.Config.default)
  ?(gc_config = Gc.Config.default)
  ()
  =
  let heap = Heap.create heap_config in
  let data_stack = Data_stack.create () in
  let shadow_stack = Shadow_stack.create () in
  let scalar_stack = Scalar_stack.create () in
  let static_data = Static_data.create () in
  let globals = Globals.create () in
  let root_table = Root_table.create () in
  let gc =
    Gc.create
      gc_config
      ~heap
      ~roots:
        [ Shadow_stack.root_source shadow_stack
        ; Globals.root_source globals
        ; Root_table.root_source root_table
        ]
      ~data_stack
      ~static:static_data
  in
  { heap
  ; data_stack
  ; shadow_stack
  ; scalar_stack
  ; static_data
  ; globals
  ; root_table
  ; gc
  ; exn_table = Runtime_objects.Exn_table.create ()
  ; partial_table = Runtime_objects.Partial_table.create ()
  ; static_strings = String.Table.create ()
  ; allocated_globals = []
  ; destroyed = false
  }
;;

let destroy t =
  if t.destroyed then raise_s [%message "Runtime_state.destroy: destroyed"];
  t.destroyed <- true;
  Root_table.drain_deferred t.root_table;
  List.iter t.allocated_globals ~f:(Static_data.remove_global t.globals);
  t.allocated_globals <- [];
  Hashtbl.clear t.static_strings;
  Gc.destroy t.gc;
  Root_table.destroy t.root_table;
  Scalar_stack.destroy t.scalar_stack;
  Shadow_stack.destroy t.shadow_stack;
  Data_stack.destroy t.data_stack;
  Static_data.destroy t.static_data;
  Heap.destroy t.heap
;;

let heap t = t.heap
let data_stack t = t.data_stack
let shadow_stack t = t.shadow_stack
let scalar_stack t = t.scalar_stack
let static_data t = t.static_data
let globals t = t.globals
let root_table t = t.root_table
let gc t = t.gc
let exn_table t = t.exn_table
let partial_table t = t.partial_table
let check_invariants t = Gc.check_invariants t.gc

let collect t =
  Root_table.drain_deferred t.root_table;
  Gc.collect t.gc
;;

let fixed_header layout = Block.header (Block.fixed layout) ~length:0 Unmarked

let alloc_heap_block t layout =
  Root_table.drain_deferred t.root_table;
  Gc.alloc t.gc (fixed_header layout)
;;

let alloc_region_block t layout =
  Data_stack.alloc t.data_stack (fixed_header layout)
;;

let alloc_with_header
  t
  ~(placement : Compiled.Placement.t)
  ~in_inner_region
  header
  =
  match placement with
  | Heap ->
    Root_table.drain_deferred t.root_table;
    Gc.alloc t.gc header
  | Region -> Data_stack.alloc t.data_stack header
  | Caller_region ->
    if in_inner_region
    then (
      Root_table.drain_deferred t.root_table;
      Gc.alloc t.gc header)
    else Data_stack.alloc t.data_stack header
;;

let alloc_block t ~placement ~in_inner_region layout =
  alloc_with_header t ~placement ~in_inner_region (fixed_header layout)
;;

let alloc_closure_block t ~placement ~in_inner_region ~fn_id ~env_layout =
  let env_scalar_words =
    Layout_mem.round_up_word (Layout.scalar_bytes env_layout) / 8
  in
  let scalar_words = 1 + env_scalar_words in
  let ptr_words = Layout.ptr_words env_layout in
  let header =
    Header.scanned ~size_words:(scalar_words + ptr_words) ~scalar_words Unmarked
  in
  let addr = alloc_with_header t ~placement ~in_inner_region header in
  Memory.store_64 addr (Int64_u.of_int (Compiled.Fn_id.to_int fn_id));
  addr
;;

let alloc_partial_block
  t
  ~placement
  ~in_inner_region
  (desc : Runtime_objects.Partial_desc.t)
  =
  let payload_scalar_words =
    Frame_layout.scalar_bytes desc.payload_layout / 8
  in
  let scalar_words = 1 + payload_scalar_words in
  let ptr_words = Frame_layout.ptr_words desc.payload_layout in
  let header =
    Header.scanned ~size_words:(scalar_words + ptr_words) ~scalar_words Unmarked
  in
  let addr = alloc_with_header t ~placement ~in_inner_region header in
  Memory.store_64
    addr
    (Int64_u.of_int (Runtime_objects.Partial_desc.tag_word desc));
  addr
;;

let alloc_exn_block t ~exn_id ~payload_layout =
  let payload_scalar_words =
    Layout_mem.round_up_word (Layout.scalar_bytes payload_layout) / 8
  in
  let scalar_words = 1 + payload_scalar_words in
  let ptr_words = Layout.ptr_words payload_layout in
  let header =
    Header.scanned ~size_words:(scalar_words + ptr_words) ~scalar_words Unmarked
  in
  Root_table.drain_deferred t.root_table;
  let addr = Gc.alloc t.gc header in
  Memory.store_64 addr (Int64_u.of_int exn_id);
  addr
;;

let write_string_bytes s ~dst =
  Layout_mem.write_bytes (Bytes.unsafe_of_string_promise_no_mutation s) ~dst
;;

let alloc_string t s =
  let len = String.length s in
  if len = 0
  then Static_data.empty_bytes ()
  else (
    Root_table.drain_deferred t.root_table;
    let addr = Gc.alloc t.gc (Header.bytes ~length:len Unmarked) in
    write_string_bytes s ~dst:addr;
    addr)
;;

let alloc_static_string t s =
  let len = String.length s in
  if len = 0
  then Static_data.empty_bytes ()
  else (
    match Hashtbl.find t.static_strings s with
    | Some word -> Addr.of_word (Nativeint_u.of_nativeint word)
    | None ->
      let addr =
        Static_data.alloc t.static_data (Header.bytes ~length:len Immortal)
      in
      write_string_bytes s ~dst:addr;
      Hashtbl.add_exn
        t.static_strings
        ~key:s
        ~data:(Nativeint_u.to_nativeint (Addr.to_word addr));
      addr)
;;

let read_string addr =
  let len = Header.length (Header.load addr) in
  if len = 0
  then ""
  else
    Bytes.unsafe_to_string
      ~no_mutation_while_string_reachable:(Layout_mem.read_bytes addr ~len)
;;

let alloc_handle t kind payload =
  Root_table.drain_deferred t.root_table;
  Gc.alloc_handle t.gc kind payload
;;

let get_handle t kind addr = Gc.get_handle t.gc kind addr

let alloc_global t layout =
  let g = Static_data.add_global t.static_data t.globals layout in
  t.allocated_globals <- g :: t.allocated_globals;
  g
;;

let register_root t ptrs = Root_table.register t.root_table ptrs
let unregister_root t id = Root_table.unregister t.root_table id
let defer_unregister_root t id = Root_table.defer_unregister t.root_table id

let with_temp_value t layout ~f =
  let mark = Scalar_stack.enter t.scalar_stack in
  let scalars =
    Scalar_stack.alloc t.scalar_stack (Layout.scalar_bytes layout)
  in
  let ptr_words = Layout.ptr_words layout in
  let ptrs =
    if ptr_words > 0
    then Shadow_stack.push_frame t.shadow_stack ~slots:ptr_words
    else Addr.null ()
  in
  Exn.protect
    ~f:(fun () -> f ~scalars ~ptrs)
    ~finally:(fun () ->
      if ptr_words > 0 then Shadow_stack.pop_frame t.shadow_stack;
      Scalar_stack.exit t.scalar_stack mark)
;;

let with_snapshot t (snapshot : Runtime_objects.Value_snapshot.t) ~f =
  with_temp_value t snapshot.layout ~f:(fun ~scalars ~ptrs ->
    Runtime_objects.Value_snapshot.write_to_memory
      snapshot
      ~dst_s:scalars
      ~dst_p:ptrs;
    f ~scalars ~ptrs)
;;
