open! Import
module Addr = Memory.Addr
module Chunk = Memory.Chunk

module Config = struct
  type t =
    { space_overhead : int
    ; min_words : int
    }
  [@@deriving sexp_of]

  let default = { space_overhead = 120; min_words = 32768 }
end

module Stats = struct
  type t =
    { marked_blocks : int
    ; marked_words : int
    ; freed_blocks : int
    ; freed_words : int
    }
  [@@deriving sexp_of]
end

let initial_mark_stack_blocks = 1024
let[@cold] fail sexp = raise (Error.to_exn (Error.create_s sexp))

type t =
  { config : Config.t
  ; heap : Heap.t
  ; roots : Roots.t
  ; data_stack : Data_stack.t
  ; static : Static_data.t
  ; mutable live_words : int (** After the last collection. *)
  ; mutable allocated_words : int (** Since the last collection. *)
  ; mutable collections : int
  ; mutable stack : Chunk.t (** The mark stack: blocks to scan. *)
  ; mutable stack_base : Addr.t
  ; mutable stack_top : Addr.t
  ; mutable stack_limit : Addr.t
  ; mutable marked_blocks : int
  ; mutable marked_words : int
  ; mutable destroyed : bool
  }

let set_stack t chunk =
  t.stack <- chunk;
  t.stack_base <- Chunk.base chunk;
  t.stack_limit <- Addr.add (Chunk.base chunk) (Chunk.bytes chunk)
;;

let create config ~heap ~roots ~data_stack ~static =
  let stack = Chunk.create ~bytes:(initial_mark_stack_blocks * 8) ~align:8 in
  { config
  ; heap
  ; roots
  ; data_stack
  ; static
  ; live_words = 0
  ; allocated_words = 0
  ; collections = 0
  ; stack
  ; stack_base = Chunk.base stack
  ; stack_top = Chunk.base stack
  ; stack_limit = Addr.add (Chunk.base stack) (Chunk.bytes stack)
  ; marked_blocks = 0
  ; marked_words = 0
  ; destroyed = false
  }
;;

(* Doubles the mark stack. It allocates, but only when the stack is deeper than
   it has been, so marking is assumed zero-alloc. *)
let[@cold] [@zero_alloc assume] grow_stack t =
  let used = Addr.diff t.stack_top t.stack_base in
  let chunk = Chunk.create ~bytes:(2 * Chunk.bytes t.stack) ~align:8 in
  Memory.copy ~src:t.stack_base ~dst:(Chunk.base chunk) ~bytes:used;
  Chunk.destroy t.stack;
  set_stack t chunk;
  t.stack_top <- Addr.add t.stack_base used
;;

let[@inline] [@zero_alloc] is_small word =
  Int.(Addr.compare word (Addr.of_word #4096n) < 0)
;;

(* Marks and pushes the block [word] points into, if it should be. *)
let[@inline] [@zero_alloc] visit t word =
  if not (is_small word)
  then (
    let block = Heap.block_start t.heap word in
    if not (Addr.is_null block)
    then (
      let header = Header.load block in
      match Header.color header with
      | Marked | Immortal -> ()
      | Unmarked ->
        Header.store block (Header.with_color header Marked);
        t.marked_blocks <- t.marked_blocks + 1;
        t.marked_words <- t.marked_words + Header.size_words header;
        (* Scanned blocks' pointer words, and the traced words of a handle (all
           but its metadata ID). *)
        if Int.(Header.scalar_words header < Header.size_words header)
        then (
          if Int.(Addr.diff t.stack_limit t.stack_top < 8) then grow_stack t;
          Memory.store_addr t.stack_top block;
          t.stack_top <- Addr.add t.stack_top 8)))
;;

let[@zero_alloc] mark t =
  (* Left mid-stack if a previous mark raised. *)
  t.stack_top <- t.stack_base;
  t.marked_blocks <- 0;
  t.marked_words <- 0;
  let mutable frame = Roots.latest_frame t.roots in
  while not (Addr.is_null frame) do
    for i = 0 to Roots.Frame.slots frame - 1 do
      visit t (Memory.load_addr (Roots.Frame.slot frame i))
    done;
    frame <- Roots.Frame.next frame
  done;
  for i = 0 to Roots.globals t.roots - 1 do
    visit t (Memory.load_addr (Roots.global t.roots i))
  done;
  for segment = 0 to Data_stack.segments t.data_stack - 1 do
    let end_ = Data_stack.segment_end t.data_stack segment in
    let mutable block = Data_stack.first_block t.data_stack segment in
    while Int.(Addr.compare (Addr.add block (-8)) end_ < 0) do
      let header = Header.load block in
      (match Header.kind header with
       | Scanned ->
         for i = Header.scalar_words header to Header.size_words header - 1 do
           visit t (Memory.load_addr (Addr.add block (i * 8)))
         done
       | Bytes | Free | Handle -> ());
      block <- Data_stack.next_block block
    done
  done;
  while not (Addr.equal t.stack_top t.stack_base) do
    t.stack_top <- Addr.add t.stack_top (-8);
    let block = Memory.load_addr t.stack_top in
    let header = Header.load block in
    for i = Header.scalar_words header to Header.size_words header - 1 do
      visit t (Memory.load_addr (Addr.add block (i * 8)))
    done
  done
;;

let check_live t = if t.destroyed then fail [%message "Gc: destroyed"]

let collect t =
  check_live t;
  mark t;
  let sweep = Heap.sweep t.heap in
  t.live_words <- sweep.kept_footprint_words;
  t.allocated_words <- 0;
  t.collections <- t.collections + 1;
  { Stats.marked_blocks = t.marked_blocks
  ; marked_words = t.marked_words
  ; freed_blocks = sweep.freed_blocks
  ; freed_words = sweep.freed_words
  }
;;

let should_collect t words =
  t.allocated_words + words
  > Int.max t.config.min_words (t.live_words * t.config.space_overhead / 100)
;;

let before_alloc t header =
  check_live t;
  let words = Heap.footprint_words t.heap header in
  if should_collect t words then ignore (collect t : Stats.t);
  words
;;

let alloc t header =
  (match Header.kind header with
   | Handle -> fail [%message "Gc.alloc: use alloc_handle"]
   | Scanned | Bytes | Free -> ());
  let words = before_alloc t header in
  let block = Heap.alloc t.heap header in
  t.allocated_words <- t.allocated_words + words;
  block
;;

(* The refs may be held only by the payload, which the collector doesn't see:
   root them across the collection that may follow. *)
let alloc_handle t kind value =
  check_live t;
  let refs = Handle_kind.refs kind value in
  let refs_count = Array.length refs in
  for index = 0 to refs_count - 1 do
    let ref_ = refs.(index) in
    if (not (is_small ref_)) && Data_stack.contains t.data_stack ref_
    then
      fail
        [%message
          "Gc.alloc_handle: a ref into the data stack"
            ~kind:(Handle_kind.name kind : string)
            (index : int)]
  done;
  let words = Heap.footprint_words t.heap (Header.handle ~refs:refs_count) in
  let mark = Data_stack.enter t.data_stack in
  let result = Array.create ~len:1 (Addr.null ()) in
  Exn.protect
    ~f:(fun () ->
      if refs_count > 0
      then (
        let scratch =
          Data_stack.alloc
            t.data_stack
            (Header.scanned ~size_words:refs_count ~scalar_words:0 Unmarked)
        in
        for index = 0 to refs_count - 1 do
          Memory.store_addr (Addr.add scratch (index * 8)) refs.(index)
        done);
      if should_collect t words then ignore (collect t : Stats.t);
      let block = Heap.alloc_handle t.heap kind value in
      t.allocated_words <- t.allocated_words + words;
      result.(0) <- block)
    ~finally:(fun () -> Data_stack.exit t.data_stack mark);
  result.(0)
;;

let get_handle t kind block =
  check_live t;
  Heap.get_handle t.heap kind block
;;

let handle_ref t block =
  check_live t;
  Heap.handle_ref t.heap block
;;

let get_handle_checked t kind handle_ref =
  check_live t;
  Heap.get_handle_checked t.heap kind handle_ref
;;

let collections t = t.collections

let destroy t =
  check_live t;
  Chunk.destroy t.stack;
  t.destroyed <- true
;;

let check_invariants t =
  let errors = ref [] in
  let error message = errors := message :: !errors in
  let check_word ~from ~immortal word =
    if (not (is_small word)) && Heap.owns t.heap word
    then (
      let block = Heap.block_start t.heap word in
      if Addr.is_null block
      then
        error [%message "a pointer into the heap but no block" (from : Sexp.t)]
      else if immortal
      then (
        match Header.color (Header.load block) with
        | Immortal -> ()
        | Unmarked | Marked ->
          error
            [%message
              "an immortal block points to a mortal one" (from : Sexp.t)]))
  in
  let in_data_stack word =
    (not (is_small word)) && Data_stack.contains t.data_stack word
  in
  let check_not_data_stack ~from word =
    if in_data_stack word
    then error [%message "a pointer into the data stack" (from : Sexp.t)]
  in
  (* Of frames and data-stack blocks, which may point to live data-stack blocks. *)
  let check_local ~from word =
    check_word ~from ~immortal:false word;
    if in_data_stack word && not (Data_stack.live_contains t.data_stack word)
    then
      error
        [%message "a pointer into the data stack past its top" (from : Sexp.t)]
  in
  let frame_index = ref 0 in
  let mutable frame = Roots.latest_frame t.roots in
  while not (Addr.is_null frame) do
    for i = 0 to Roots.Frame.slots frame - 1 do
      check_local
        ~from:[%message "frame" ~frame:(!frame_index : int) ~slot:(i : int)]
        (Memory.load_addr (Roots.Frame.slot frame i))
    done;
    incr frame_index;
    frame <- Roots.Frame.next frame
  done;
  for i = 0 to Roots.globals t.roots - 1 do
    let word = Memory.load_addr (Roots.global t.roots i) in
    let from = [%message "global" ~index:(i : int)] in
    check_word ~from ~immortal:false word;
    check_not_data_stack ~from word
  done;
  let block_index = ref 0 in
  Heap.iter t.heap ~f:(fun block ->
    let header = Header.load block in
    let immortal =
      match Header.color header with
      | Marked ->
        error [%message "a marked block" ~block:(!block_index : int)];
        false
      | Unmarked -> false
      | Immortal -> true
    in
    (match Header.kind header with
     | Scanned | Handle ->
       for i = Header.scalar_words header to Header.size_words header - 1 do
         let word = Memory.load_addr (Addr.add block (i * 8)) in
         let from =
           [%message "block" ~block:(!block_index : int) ~word:(i : int)]
         in
         check_word ~from ~immortal word;
         check_not_data_stack ~from word
       done
     | Bytes | Free -> ());
    incr block_index);
  List.iter (Heap.check_handles t.heap) ~f:(fun error_ ->
    error [%message "handle metadata" (error_ : Sexp.t)]);
  let block_index = ref 0 in
  Static_data.iter t.static ~f:(fun block ->
    let header = Header.load block in
    (match Header.kind header with
     | Scanned ->
       for i = Header.scalar_words header to Header.size_words header - 1 do
         let word = Memory.load_addr (Addr.add block (i * 8)) in
         let from =
           [%message "static" ~block:(!block_index : int) ~word:(i : int)]
         in
         if (not (is_small word)) && Heap.owns t.heap word
         then
           error [%message "static data points into the heap" (from : Sexp.t)];
         check_not_data_stack ~from word
       done
     | Bytes | Free | Handle -> ());
    incr block_index);
  let block_index = ref 0 in
  for segment = 0 to Data_stack.segments t.data_stack - 1 do
    let end_ = Data_stack.segment_end t.data_stack segment in
    let mutable block = Data_stack.first_block t.data_stack segment in
    let mutable walking = true in
    while walking && Int.(Addr.compare (Addr.add block (-8)) end_ < 0) do
      let from = [%message "data stack" ~block:(!block_index : int)] in
      match
        Header.kind (Header.load block), Header.color (Header.load block)
      with
      | exception exn ->
        error [%message "a data-stack header" (from : Sexp.t) (exn : exn)];
        walking <- false
      | Free, _ ->
        error [%message "a free data-stack block" (from : Sexp.t)];
        walking <- false
      | Handle, _ ->
        error [%message "a data-stack handle block" (from : Sexp.t)];
        walking <- false
      | ((Scanned | Bytes) as kind), color ->
        (match color with
         | Unmarked -> ()
         | Marked | Immortal ->
           error [%message "a data-stack block not unmarked" (from : Sexp.t)]);
        let header = Header.load block in
        (match kind with
         | Scanned ->
           for i = Header.scalar_words header to Header.size_words header - 1 do
             check_local
               ~from:
                 [%message
                   "data stack" ~block:(!block_index : int) ~word:(i : int)]
               (Memory.load_addr (Addr.add block (i * 8)))
           done
         | Bytes | Free | Handle -> ());
        incr block_index;
        block <- Data_stack.next_block block
    done;
    if walking && not (Addr.equal (Addr.add block (-8)) end_)
    then
      error
        [%message
          "the data stack's blocks overrun the end of a segment" (segment : int)]
  done;
  match List.rev !errors with
  | [] -> Ok ()
  | errors ->
    Or_error.error_s [%message "Gc.check_invariants" (errors : Sexp.t list)]
;;

module For_testing = struct
  let mark_stack_capacity t = Chunk.bytes t.stack / 8
end
