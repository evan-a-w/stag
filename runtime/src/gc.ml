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

let create config heap roots =
  let stack = Chunk.create ~bytes:(initial_mark_stack_blocks * 8) ~align:8 in
  { config
  ; heap
  ; roots
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
        (match Header.kind header with
         | Scanned ->
           if Int.(Header.scalar_words header < Header.size_words header)
           then (
             if Int.(Addr.diff t.stack_limit t.stack_top < 8) then grow_stack t;
             Memory.store_addr t.stack_top block;
             t.stack_top <- Addr.add t.stack_top 8)
         | Bytes | Free -> ())))
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

let alloc t header =
  check_live t;
  let words = Heap.footprint_words t.heap header in
  let threshold =
    Int.max t.config.min_words (t.live_words * t.config.space_overhead / 100)
  in
  if t.allocated_words + words > threshold then ignore (collect t : Stats.t);
  let block = Heap.alloc t.heap header in
  t.allocated_words <- t.allocated_words + words;
  block
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
  let index = ref 0 in
  Roots.iter_slots t.roots ~f:(fun slot ->
    check_word
      ~from:[%message "root" ~index:(!index : int)]
      ~immortal:false
      (Memory.load_addr slot);
    incr index);
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
     | Scanned ->
       for i = Header.scalar_words header to Header.size_words header - 1 do
         check_word
           ~from:[%message "block" ~block:(!block_index : int) ~word:(i : int)]
           ~immortal
           (Memory.load_addr (Addr.add block (i * 8)))
       done
     | Bytes | Free -> ());
    incr block_index);
  match List.rev !errors with
  | [] -> Ok ()
  | errors ->
    Or_error.error_s [%message "Gc.check_invariants" (errors : Sexp.t list)]
;;

module For_testing = struct
  let mark_stack_capacity t = Chunk.bytes t.stack / 8
end
