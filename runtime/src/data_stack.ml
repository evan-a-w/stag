open! Import
module Addr = Memory.Addr
module Chunk = Memory.Chunk

let[@cold] fail sexp = raise (Error.to_exn (Error.create_s sexp))
let default_segment_bytes = 64 * 1024

(* A position is [segment lsl offset_bits lor offset], [offset] the bytes used
   in [segment]: segments' addresses alone are ambiguous, as one may start where
   another ends. *)
let offset_bits = 40

module Position = struct
  let[@inline] [@zero_alloc] create ~segment ~offset =
    (segment lsl offset_bits) lor offset
  ;;

  let[@inline] [@zero_alloc] segment t = t lsr offset_bits
  let[@inline] [@zero_alloc] offset t = t land ((1 lsl offset_bits) - 1)
end

(* A mark is [generation lsl depth_bits lor depth]: the region's depth, from 1,
   and how many regions have been entered at that depth, so a mark of a region
   already exited never matches a live one. *)
let depth_bits = 24
let generation_mask = (1 lsl (Int.num_bits - 1 - depth_bits)) - 1

module Mark = struct
  type t = int

  let[@inline] [@zero_alloc] equal a b = Int.(a = b)

  let[@inline] [@zero_alloc] create ~depth ~generation =
    (generation lsl depth_bits) lor depth
  ;;

  let[@inline] [@zero_alloc] depth t = t land ((1 lsl depth_bits) - 1)
  let[@inline] [@zero_alloc] generation t = t lsr depth_bits

  let sexp_of_t t =
    [%sexp { depth = (depth t : int); generation = (generation t : int) }]
  ;;
end

type t =
  { mutable top : Addr.t (** Where the next block's header goes. *)
  ; mutable limit : Addr.t (** The end of the current segment. *)
  ; mutable segment : int (** The current segment. *)
  ; mutable bases : Addr.t array
  ; mutable limits : Addr.t array (** The ends of the segments. *)
  ; mutable ends : Addr.t array
  (** Of the segments before the current one, the end of their blocks. *)
  ; mutable chunks : Chunk.t array (** Segments [0, made), made in order. *)
  ; mutable made : int
  ; segment_bytes : int
  ; mutable depth : int (** Regions entered and not exited. *)
  ; mutable positions : int array
  (** Where each live region starts, by depth. *)
  ; mutable generations : int array (** Of each depth, its latest region's. *)
  ; mutable destroyed : bool
  }

let[@inline] [@zero_alloc] enter_segment t i =
  t.segment <- i;
  t.top <- t.bases.(i);
  t.limit <- t.limits.(i)
;;

let create ?(segment_bytes = default_segment_bytes) () =
  if segment_bytes < 16 || segment_bytes >= 1 lsl offset_bits
  then raise_s [%message "Data_stack.create" (segment_bytes : int)];
  let first = Chunk.create ~bytes:segment_bytes ~align:8 in
  let t =
    { top = Chunk.base first
    ; limit = Chunk.base first
    ; segment = 0
    ; bases = Array.create ~len:4 (Chunk.base first)
    ; limits = Array.create ~len:4 (Addr.add (Chunk.base first) segment_bytes)
    ; ends = Array.create ~len:4 (Chunk.base first)
    ; chunks = [| first |]
    ; made = 1
    ; segment_bytes
    ; depth = 0
    ; positions = Array.create ~len:16 0
    ; generations = Array.create ~len:16 0
    ; destroyed = false
    }
  in
  enter_segment t 0;
  t
;;

let destroy t =
  if t.destroyed then fail [%message "Data_stack.destroy: destroyed"];
  for i = 0 to t.made - 1 do
    Chunk.destroy t.chunks.(i)
  done;
  t.made <- 0;
  t.depth <- 0;
  t.destroyed <- true;
  (* So [alloc] can't fit anything, and goes to [grow], which raises. *)
  t.top <- Addr.null ();
  t.limit <- Addr.null ()
;;

(* Allocates, but only when regions nest deeper than they have. *)
let[@cold] [@zero_alloc assume] grow_regions t =
  let grown a =
    let b = Array.create ~len:(2 * Array.length a) 0 in
    Array.blit ~src:a ~src_pos:0 ~dst:b ~dst_pos:0 ~len:(Array.length a);
    b
  in
  t.positions <- grown t.positions;
  t.generations <- grown t.generations
;;

let[@zero_alloc] enter t =
  if t.destroyed then fail [%message "Data_stack.enter: destroyed"];
  if Int.(t.depth + 1 >= 1 lsl depth_bits)
  then fail [%message "Data_stack.enter: too deep"];
  if Int.(t.depth = Array.length t.positions) then grow_regions t;
  let generation = (t.generations.(t.depth) + 1) land generation_mask in
  t.generations.(t.depth) <- generation;
  t.positions.(t.depth)
  <- Position.create
       ~segment:t.segment
       ~offset:(Addr.diff t.top t.bases.(t.segment));
  t.depth <- t.depth + 1;
  Mark.create ~depth:t.depth ~generation
;;

let[@zero_alloc] exit t mark =
  let depth = Mark.depth mark in
  if t.destroyed
     || Int.(depth < 1)
     || Int.(depth > t.depth)
     || Int.(t.generations.(depth - 1) <> Mark.generation mark)
  then fail [%message "Data_stack.exit: not a live mark" (mark : Mark.t)];
  let position = t.positions.(depth - 1) in
  let segment = Position.segment position in
  enter_segment t segment;
  t.top <- Addr.add t.bases.(segment) (Position.offset position);
  t.depth <- depth - 1
;;

let grow_array a ~len ~fill =
  let b = Array.create ~len (fill ()) in
  for i = 0 to Array.length a - 1 do
    b.(i) <- a.(i)
  done;
  b
;;

(* Moves to the next segment, making it if it isn't there or is too small. It
   allocates, but only when the stack is deeper than it has been, so [alloc] is
   assumed zero-alloc. *)
let[@cold] [@zero_alloc assume] grow t ~bytes =
  if t.destroyed then fail [%message "Data_stack: destroyed"];
  t.ends.(t.segment) <- t.top;
  let next = t.segment + 1 in
  if next < t.made && Chunk.bytes t.chunks.(next) >= bytes
  then enter_segment t next
  else (
    let chunk = Chunk.create ~bytes:(Int.max t.segment_bytes bytes) ~align:8 in
    if next < t.made
    then (
      (* Too small, and unused: no live block is in it. Later segments are
         unused too, and kept. *)
      Chunk.destroy t.chunks.(next);
      t.chunks.(next) <- chunk)
    else (
      if next = Array.length t.chunks
      then
        t.chunks
        <- Array.init (2 * next) ~f:(fun i ->
             if i < next then t.chunks.(i) else chunk);
      if next = Array.length t.bases
      then (
        t.bases <- grow_array t.bases ~len:(2 * next) ~fill:Addr.null;
        t.limits <- grow_array t.limits ~len:(2 * next) ~fill:Addr.null;
        t.ends <- grow_array t.ends ~len:(2 * next) ~fill:Addr.null);
      t.chunks.(next) <- chunk;
      t.made <- next + 1);
    t.bases.(next) <- Chunk.base chunk;
    t.limits.(next) <- Addr.add (Chunk.base chunk) (Chunk.bytes chunk);
    enter_segment t next)
;;

let[@zero_alloc] alloc t header =
  (match Header.kind header with
   | Free ->
     fail [%message "Data_stack.alloc: a Free header" (header : Header.t)]
   | Handle ->
     fail [%message "Data_stack.alloc: a Handle header" (header : Header.t)]
   | Scanned | Bytes -> ());
  let words = Header.size_words header in
  if Int.(words = 0)
  then (
    match Header.kind header with
    | Bytes -> Static_data.empty_bytes ()
    | Scanned | Free | Handle -> Static_data.empty ())
  else (
    let bytes = (words + 1) * 8 in
    if t.destroyed || Int.(Addr.diff t.limit t.top < bytes) then grow t ~bytes;
    let block = Addr.add t.top 8 in
    t.top <- Addr.add t.top bytes;
    Header.store block (Header.with_color header Unmarked);
    Memory.fill_zero block ~bytes:(words * 8);
    block)
;;

let[@zero_alloc] contains t addr =
  let mutable found = false in
  let mutable i = 0 in
  while (not found) && Int.(i < t.made) do
    if Int.(Addr.compare addr t.bases.(i) >= 0)
       && Int.(Addr.compare addr t.limits.(i) < 0)
    then found <- true;
    i <- i + 1
  done;
  found
;;

let[@zero_alloc] live_contains t addr =
  let mutable found = false in
  if not t.destroyed
  then (
    let mutable i = 0 in
    while (not found) && Int.(i <= t.segment) do
      let end_ = if Int.(i = t.segment) then t.top else t.ends.(i) in
      if Int.(Addr.compare addr t.bases.(i) >= 0)
         && Int.(Addr.compare addr end_ < 0)
      then found <- true;
      i <- i + 1
    done);
  found
;;

let[@inline] [@zero_alloc] segments t = if t.destroyed then 0 else t.segment + 1
let[@inline] [@zero_alloc] first_block t i = Addr.add t.bases.(i) 8

let[@inline] [@zero_alloc] segment_end t i =
  if Int.(i = t.segment) then t.top else t.ends.(i)
;;

let[@inline] [@zero_alloc] next_block block =
  Addr.add block ((Header.size_words (Header.load block) + 1) * 8)
;;

let iter t ~(f : _ @ local) =
  for i = 0 to segments t - 1 do
    let end_ = segment_end t i in
    let mutable block = first_block t i in
    while Int.(Addr.compare (Addr.add block (-8)) end_ < 0) do
      f block;
      block <- next_block block
    done
  done
;;

let sexp_of_t t =
  let blocks = ref [] in
  iter t ~f:(fun block ->
    let rec segment i =
      if Addr.compare block t.bases.(i) >= 0
         && Addr.compare block (Addr.add t.bases.(i) (Chunk.bytes t.chunks.(i)))
            < 0
      then i
      else segment (i + 1)
    in
    let i = segment 0 in
    blocks
    := [%sexp
         (i : int)
         , (Addr.diff block t.bases.(i) : int)
         , (Header.load block : Header.t)]
       :: !blocks);
  [%sexp (List.rev !blocks : Sexp.t list)]
;;

module For_testing = struct
  let segments_made t = t.made
end
