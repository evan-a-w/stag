open! Import
module Addr = Memory.Addr
module Chunk = Memory.Chunk

let[@cold] fail sexp = raise (Error.to_exn (Error.create_s sexp))
let default_segment_bytes = 64 * 1024
let offset_bits = 40
let depth_bits = 24
let generation_mask = (1 lsl (Int.num_bits - 1 - depth_bits)) - 1

module Position = struct
  let[@inline] [@zero_alloc] create ~segment ~offset =
    (segment lsl offset_bits) lor offset
  ;;

  let[@inline] [@zero_alloc] segment t = t lsr offset_bits
  let[@inline] [@zero_alloc] offset t = t land ((1 lsl offset_bits) - 1)
end

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
  { mutable top : Addr.t
  ; mutable limit : Addr.t
  ; mutable segment : int
  ; mutable bases : Addr.t array
  ; mutable limits : Addr.t array
  ; mutable chunks : Chunk.t array
  ; mutable made : int
  ; segment_bytes : int
  ; mutable depth : int
  ; mutable positions : int array
  ; mutable generations : int array
  ; mutable destroyed : bool
  }

let[@inline] [@zero_alloc] enter_segment t i =
  t.segment <- i;
  t.top <- t.bases.(i);
  t.limit <- t.limits.(i)
;;

let create ?(segment_bytes = default_segment_bytes) () =
  if segment_bytes < 8 || segment_bytes >= 1 lsl offset_bits
  then raise_s [%message "Scalar_stack.create" (segment_bytes : int)];
  let first = Chunk.create ~bytes:segment_bytes ~align:8 in
  let base = Chunk.base first in
  let limit = Addr.add base segment_bytes in
  let t =
    { top = base
    ; limit
    ; segment = 0
    ; bases = Array.create ~len:4 base
    ; limits = Array.create ~len:4 limit
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
  if t.destroyed then fail [%message "Scalar_stack.destroy: destroyed"];
  for i = 0 to t.made - 1 do
    Chunk.destroy t.chunks.(i)
  done;
  t.made <- 0;
  t.depth <- 0;
  t.destroyed <- true;
  t.top <- Addr.null ();
  t.limit <- Addr.null ()
;;

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
  if t.destroyed then fail [%message "Scalar_stack.enter: destroyed"];
  if Int.(t.depth + 1 >= 1 lsl depth_bits)
  then fail [%message "Scalar_stack.enter: too deep"];
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
  then fail [%message "Scalar_stack.exit: not a live mark" (mark : Mark.t)];
  let position = t.positions.(depth - 1) in
  let segment = Position.segment position in
  enter_segment t segment;
  t.top <- Addr.add t.bases.(segment) (Position.offset position);
  t.depth <- depth - 1
;;

let grow_addr_array a ~len ~fill =
  let b = Array.create ~len fill in
  for i = 0 to Array.length a - 1 do
    b.(i) <- a.(i)
  done;
  b
;;

let[@cold] [@zero_alloc assume] advance_segment t aligned_bytes =
  if t.destroyed then fail [%message "Scalar_stack.alloc: destroyed"];
  let next = t.segment + 1 in
  if next < t.made
  then
    if Chunk.bytes t.chunks.(next) >= aligned_bytes
    then enter_segment t next
    else (
      let bytes = Int.max t.segment_bytes aligned_bytes in
      Chunk.destroy t.chunks.(next);
      let chunk = Chunk.create ~bytes ~align:8 in
      let base = Chunk.base chunk in
      t.chunks.(next) <- chunk;
      t.bases.(next) <- base;
      t.limits.(next) <- Addr.add base bytes;
      enter_segment t next)
  else (
    let bytes = Int.max t.segment_bytes aligned_bytes in
    let chunk = Chunk.create ~bytes ~align:8 in
    let base = Chunk.base chunk in
    let limit = Addr.add base bytes in
    if next = Array.length t.bases
    then (
      let new_len = 2 * Array.length t.bases in
      t.bases <- grow_addr_array t.bases ~len:new_len ~fill:(Addr.null ());
      t.limits <- grow_addr_array t.limits ~len:new_len ~fill:(Addr.null ()));
    if next = Array.length t.chunks
    then (
      let new_chunks = Array.create ~len:(2 * Array.length t.chunks) chunk in
      Array.blit
        ~src:t.chunks
        ~src_pos:0
        ~dst:new_chunks
        ~dst_pos:0
        ~len:(Array.length t.chunks);
      t.chunks <- new_chunks);
    t.chunks.(next) <- chunk;
    t.bases.(next) <- base;
    t.limits.(next) <- limit;
    t.made <- next + 1;
    enter_segment t next)
;;

let[@zero_alloc] alloc t bytes =
  if Int.(bytes < 0) then fail [%message "Scalar_stack.alloc" (bytes : int)];
  if t.destroyed then fail [%message "Scalar_stack.alloc: destroyed"];
  if Int.(bytes = 0)
  then Addr.null ()
  else (
    let aligned_bytes = (bytes + 7) land lnot 7 in
    if Int.(Addr.diff t.limit t.top < aligned_bytes)
    then advance_segment t aligned_bytes;
    let addr = t.top in
    t.top <- Addr.add addr aligned_bytes;
    Memory.fill_zero addr ~bytes:aligned_bytes;
    addr)
;;
