open! Import
module Addr = Memory.Addr
module Chunk = Memory.Chunk

let[@cold] fail sexp = raise (Error.to_exn (Error.create_s sexp))

module Frame_map = struct
  (* LLVM's [{ int32_t NumRoots; int32_t NumMeta; void *Meta[] }], with no
     metadata. *)
  let bytes = 8
  let[@inline] [@zero_alloc] num_roots map = Int32_u.to_int (Memory.load_32 map)

  let[@inline] [@zero_alloc] num_meta map =
    Int32_u.to_int (Memory.load_32 (Addr.add map 4))
  ;;

  let store map ~num_roots =
    Memory.store_32 map (Int32_u.of_int num_roots);
    Memory.store_32 (Addr.add map 4) #0l
  ;;
end

module Frame = struct
  let header_bytes = 16
  let[@inline] [@zero_alloc] next base = Memory.load_addr base
  let[@inline] [@zero_alloc] map base = Memory.load_addr (Addr.add base 8)
  let[@inline] [@zero_alloc] slots base = Frame_map.num_roots (map base)
  let[@inline] [@zero_alloc] slot base i = Addr.add base (header_bytes + (i * 8))
  let[@inline] [@zero_alloc] bytes ~slots = header_bytes + (slots * 8)
end

let default_segment_bytes = 64 * 1024
let max_slots = 0x7fff_ffff

let grow_addrs a ~len =
  let b = Array.create ~len (Addr.null ()) in
  for i = 0 to Array.length a - 1 do
    b.(i) <- a.(i)
  done;
  b
;;

type t =
  { head_chunk : Chunk.t
  ; head : Addr.t (** The word holding the latest frame's base, or null. *)
  ; mutable own : Addr.t array
  (** The frames pushed here, by depth: [depth] of them. *)
  ; mutable depth : int
  ; mutable top : Addr.t (** Where the next frame goes in the segment. *)
  ; mutable base : Addr.t (** The start of the segment. *)
  ; mutable limit : Addr.t (** The end of the segment. *)
  ; mutable bases : Addr.t array
  ; mutable limits : Addr.t array
  ; mutable tops : Addr.t array
  (** Of the segments before the current one, [top] when it was left. *)
  ; mutable segment : int (** The segment [top] is in. *)
  ; mutable segments : Chunk.t array
  (** Segments [0, segment_count) have been used, by depth. *)
  ; mutable segment_count : int
  ; segment_bytes : int
  ; mutable globals : Addr.t array
  ; mutable global_count : int
  ; global_index : int Int.Table.t (** By address, as a word. *)
  ; mutable maps : Addr.t array
  (** The frame map of each slot count, or null: frames of a count share one. *)
  ; big_maps : int Int.Table.t (** Maps' addresses as words. *)
  ; mutable map_chunks : Chunk.t list
  ; mutable map_top : Addr.t
  ; mutable map_limit : Addr.t
  ; mutable destroyed : bool
  }

let enter_segment t i =
  let chunk = t.segments.(i) in
  t.segment <- i;
  t.base <- Chunk.base chunk;
  t.bases.(i) <- Chunk.base chunk;
  t.limits.(i) <- Addr.add (Chunk.base chunk) (Chunk.bytes chunk);
  t.top <- Chunk.base chunk;
  t.limit <- Addr.add (Chunk.base chunk) (Chunk.bytes chunk)
;;

let create_with ~segment_bytes =
  let first = Chunk.create ~bytes:segment_bytes ~align:8 in
  let head_chunk = Chunk.create ~bytes:8 ~align:8 in
  let t =
    { head_chunk
    ; head = Chunk.base head_chunk
    ; own = Array.create ~len:16 (Addr.null ())
    ; depth = 0
    ; top = Chunk.base first
    ; base = Chunk.base first
    ; limit = Chunk.base first
    ; bases = Array.create ~len:1 (Chunk.base first)
    ; limits = Array.create ~len:1 (Chunk.base first)
    ; tops = Array.create ~len:1 (Chunk.base first)
    ; segment = 0
    ; segments = [| first |]
    ; segment_count = 1
    ; segment_bytes
    ; globals = Array.create ~len:4 (Addr.null ())
    ; global_count = 0
    ; global_index = Int.Table.create ()
    ; maps = Array.create ~len:16 (Addr.null ())
    ; big_maps = Int.Table.create ()
    ; map_chunks = []
    ; map_top = Addr.null ()
    ; map_limit = Addr.null ()
    ; destroyed = false
    }
  in
  Memory.store_addr t.head (Addr.null ());
  enter_segment t 0;
  t
;;

let create () = create_with ~segment_bytes:default_segment_bytes
let[@zero_alloc] chain_head t = t.head

(* Moves to the next segment, making it if it isn't there or is too small. It
   allocates, but only when the stack is deeper than it has been, so
   [push_frame] is assumed zero-alloc. *)
let[@cold] [@zero_alloc assume] grow t ~bytes =
  t.tops.(t.segment) <- t.top;
  let next = t.segment + 1 in
  if next < t.segment_count && Chunk.bytes t.segments.(next) >= bytes
  then enter_segment t next
  else (
    let chunk = Chunk.create ~bytes:(Int.max t.segment_bytes bytes) ~align:8 in
    if next < t.segment_count
    then (
      (* Too small, and unused: no frame is in it. *)
      Chunk.destroy t.segments.(next);
      t.segments.(next) <- chunk)
    else (
      if next = Array.length t.segments
      then
        t.segments
        <- Array.init (2 * next) ~f:(fun i ->
             if i < next then t.segments.(i) else chunk);
      if next = Array.length t.bases
      then (
        t.bases <- grow_addrs t.bases ~len:(2 * next);
        t.limits <- grow_addrs t.limits ~len:(2 * next);
        t.tops <- grow_addrs t.tops ~len:(2 * next));
      t.segments.(next) <- chunk;
      t.segment_count <- next + 1);
    enter_segment t next)
;;

(* Allocates, but only when the stack is deeper than it has been. *)
let[@cold] [@zero_alloc assume] grow_own t =
  t.own <- grow_addrs t.own ~len:(2 * Array.length t.own)
;;

let map_chunk_bytes = 4096

(* Slot counts from this one have their maps in [big_maps]. *)
let max_direct_map = 1024

(* Allocates, but once per slot count, so [push_frame] is assumed zero-alloc. *)
let[@cold] [@zero_alloc assume] make_map t ~slots =
  match Hashtbl.find t.big_maps slots with
  | Some map -> Addr.of_word (Nativeint_u.of_int map)
  | None ->
    if Int.(Addr.diff t.map_limit t.map_top < Frame_map.bytes)
       || Addr.is_null t.map_top
    then (
      let chunk = Chunk.create ~bytes:map_chunk_bytes ~align:8 in
      t.map_chunks <- chunk :: t.map_chunks;
      t.map_top <- Chunk.base chunk;
      t.map_limit <- Addr.add (Chunk.base chunk) map_chunk_bytes);
    let map = t.map_top in
    Frame_map.store map ~num_roots:slots;
    t.map_top <- Addr.add map Frame_map.bytes;
    if slots >= max_direct_map
    then
      Hashtbl.set
        t.big_maps
        ~key:slots
        ~data:(Nativeint_u.to_int (Addr.to_word map))
    else (
      if slots >= Array.length t.maps
      then (
        let maps =
          Array.create
            ~len:
              (Int.min
                 max_direct_map
                 (Int.max (slots + 1) (2 * Array.length t.maps)))
            (Addr.null ())
        in
        for i = 0 to Array.length t.maps - 1 do
          maps.(i) <- t.maps.(i)
        done;
        t.maps <- maps);
      t.maps.(slots) <- map);
    map
;;

let[@inline] [@zero_alloc] map_for t ~slots =
  if Int.(slots < Array.length t.maps) && not (Addr.is_null t.maps.(slots))
  then t.maps.(slots)
  else make_map t ~slots
;;

let[@zero_alloc] push_frame t ~slots =
  if t.destroyed then fail [%message "Roots.push_frame: destroyed"];
  if Int.(slots < 0 || slots > max_slots)
  then fail [%message "Roots.push_frame" (slots : int)];
  let bytes = Frame.bytes ~slots in
  if Int.(Addr.diff t.limit t.top < bytes) then grow t ~bytes;
  if Int.(t.depth = Array.length t.own) then grow_own t;
  let frame = t.top in
  Memory.store_addr frame (Memory.load_addr t.head);
  Memory.store_addr (Addr.add frame 8) (map_for t ~slots);
  Memory.fill_zero (Frame.slot frame 0) ~bytes:(slots * 8);
  Memory.store_addr t.head frame;
  t.own.(t.depth) <- frame;
  t.depth <- t.depth + 1;
  t.top <- Addr.add frame bytes;
  Frame.slot frame 0
;;

let[@zero_alloc] pop_frame t =
  if Int.(t.depth = 0) then fail [%message "Roots.pop_frame: no frame"];
  let frame = t.own.(t.depth - 1) in
  if not (Addr.equal (Memory.load_addr t.head) frame)
  then fail [%message "Roots.pop_frame: a foreign frame is the latest"];
  Memory.store_addr t.head (Frame.next frame);
  t.depth <- t.depth - 1;
  t.own.(t.depth) <- Addr.null ();
  if Int.(t.segment > 0) && Addr.equal frame t.base
  then (
    (* The segment is empty: back to the previous one, as it was left. *)
    t.segment <- t.segment - 1;
    t.base <- t.bases.(t.segment);
    t.limit <- t.limits.(t.segment);
    t.top <- t.tops.(t.segment))
  else t.top <- frame
;;

let depth t = t.depth

let destroy t =
  if t.destroyed then fail [%message "Roots.destroy: destroyed"];
  for i = 0 to t.segment_count - 1 do
    Chunk.destroy t.segments.(i)
  done;
  List.iter t.map_chunks ~f:Chunk.destroy;
  t.map_chunks <- [];
  t.maps <- [||];
  Chunk.destroy t.head_chunk;
  t.depth <- 0;
  t.global_count <- 0;
  Hashtbl.clear t.global_index;
  t.destroyed <- true
;;

let[@zero_alloc] latest_frame t =
  if t.destroyed then Addr.null () else Memory.load_addr t.head
;;

let[@zero_alloc] globals t = t.global_count

let[@zero_alloc] global t i =
  if Int.(i < 0 || i >= t.global_count)
  then fail [%message "Roots.global" (i : int)];
  t.globals.(i)
;;

let key addr = Nativeint_u.to_int (Addr.to_word addr)
let is_global t addr = Hashtbl.mem t.global_index (key addr)

let add_global t addr =
  if t.destroyed then fail [%message "Roots.add_global: destroyed"];
  if is_global t addr then fail [%message "Roots.add_global: already added"];
  if t.global_count = Array.length t.globals
  then t.globals <- grow_addrs t.globals ~len:(2 * t.global_count);
  t.globals.(t.global_count) <- addr;
  Hashtbl.set t.global_index ~key:(key addr) ~data:t.global_count;
  t.global_count <- t.global_count + 1
;;

let remove_global t addr =
  match Hashtbl.find t.global_index (key addr) with
  | None -> fail [%message "Roots.remove_global: not added"]
  | Some i ->
    Hashtbl.remove t.global_index (key addr);
    t.global_count <- t.global_count - 1;
    if i < t.global_count
    then (
      let last = t.globals.(t.global_count) in
      t.globals.(i) <- last;
      Hashtbl.set t.global_index ~key:(key last) ~data:i);
    t.globals.(t.global_count) <- Addr.null ()
;;

let iter_slots t ~(f : _ @ local) =
  let mutable frame = latest_frame t in
  while not (Addr.is_null frame) do
    for i = 0 to Frame.slots frame - 1 do
      f (Frame.slot frame i)
    done;
    frame <- Frame.next frame
  done;
  for i = 0 to t.global_count - 1 do
    f t.globals.(i)
  done
;;

module For_testing = struct
  let create = create_with
  let segments t = t.segment_count
  let segment t = t.segment
end

module Frames_source = struct
  type nonrec t = t

  let name = "shadow stack"
  let holds_locals = true

  let[@zero_alloc] iter t ~(f : _ @ local) =
    let mutable frame = latest_frame t in
    while not (Addr.is_null frame) do
      for i = 0 to Frame.slots frame - 1 do
        (f [@zero_alloc assume]) (Frame.slot frame i)
      done;
      frame <- Frame.next frame
    done
  ;;
end

module Globals_source = struct
  type nonrec t = t

  let name = "globals"
  let holds_locals = false

  let[@zero_alloc] iter t ~(f : _ @ local) =
    for i = 0 to t.global_count - 1 do
      (f [@zero_alloc assume]) t.globals.(i)
    done
  ;;
end

let root_sources t =
  [ Root_source.create (module Frames_source) t
  ; Root_source.create (module Globals_source) t
  ]
;;
