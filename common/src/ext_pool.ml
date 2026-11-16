open! Import
include Ext_pool_intf.Definitions

(* A slot's generation is odd while it is live, so a handle (carrying the
   generation it was allocated at) is live exactly when the generations match. *)
let index_bits = 32
let generation_mask = (1 lsl 30) - 1

[%%template
[@@@kind_set.define
  pointer_free
  = ( immediate
    , float64
    , bits64
    , bits32
    , word
    , immediate & immediate
    , immediate & immediate & immediate
    , float64 & float64
    , bits64 & bits64 )]

module%template Make (E : Element [@kind k]) = struct
  type elt = E.t

  module Handle = struct
    type 'brand t = int

    let index t = t land ((1 lsl index_bits) - 1)
    let generation t = t lsr index_bits
    let create ~index ~generation = index lor (generation lsl index_bits)
    let equal = Int.equal
    let compare = Int.compare
    let hash = Int.hash
    let sexp_of_t t = [%sexp (index t : int), (generation t : int)]
  end

  type 'brand t =
    { mutable slab : E.t array
    ; mutable generations : int array
    ; mutable used : int
        (* Slots [0, used) have been allocated at least once. *)
    ; mutable free : int array
    ; mutable free_count : int
    ; mutable length : int
    }

  type packed = T : 'brand t -> packed [@@unboxed]

  let create () =
    T
      { slab = [||]
      ; generations = [||]
      ; used = 0
      ; free = [||]
      ; free_count = 0
      ; length = 0
      }
  ;;

  let with_ ~(f : 'brand. 'brand t -> 'a) =
    let (T t) = create () in
    f t
  ;;

  (* Unboxed arrays need an element to be created with, so the slab grows on
     [alloc], from the element being allocated. *)
  let grow t (x : E.t) =
    let length = Int.max 16 (2 * Array.length t.slab) in
    let slab = Array.create ~len:length x in
    for i = 0 to t.used - 1 do
      Array.unsafe_set slab i (Array.unsafe_get t.slab i)
    done;
    t.slab <- slab;
    let generations = Array.create ~len:length 0 in
    Array.blit
      ~src:t.generations
      ~src_pos:0
      ~dst:generations
      ~dst_pos:0
      ~len:t.used;
    t.generations <- generations
  ;;

  let alloc t x =
    let index =
      if t.free_count > 0
      then (
        t.free_count <- t.free_count - 1;
        t.free.(t.free_count))
      else (
        if t.used = Array.length t.slab then grow t x;
        t.used <- t.used + 1;
        t.used - 1)
    in
    let generation = (t.generations.(index) + 1) land generation_mask in
    t.generations.(index) <- generation;
    Array.unsafe_set t.slab index x;
    t.length <- t.length + 1;
    Handle.create ~index ~generation
  ;;

  let is_live t handle =
    let index = Handle.index handle in
    index < t.used && t.generations.(index) = Handle.generation handle
  ;;

  let[@cold] stale handle =
    raise_s [%message "Ext_pool: stale handle" (handle : Handle.t)]
  ;;

  let[@inline] check t handle = if not (is_live t handle) then stale handle

  let get t handle =
    check t handle;
    Array.unsafe_get t.slab (Handle.index handle)
  ;;

  let set t handle x =
    check t handle;
    Array.unsafe_set t.slab (Handle.index handle) x
  ;;

  let push_free t index =
    if t.free_count = Array.length t.free
    then (
      let free = Array.create ~len:(Int.max 16 (2 * t.free_count)) 0 in
      Array.blit ~src:t.free ~src_pos:0 ~dst:free ~dst_pos:0 ~len:t.free_count;
      t.free <- free);
    t.free.(t.free_count) <- index;
    t.free_count <- t.free_count + 1
  ;;

  let free t handle =
    check t handle;
    let index = Handle.index handle in
    t.generations.(index) <- (t.generations.(index) + 1) land generation_mask;
    t.length <- t.length - 1;
    push_free t index
  ;;

  let clear t =
    for index = 0 to t.used - 1 do
      let generation = t.generations.(index) in
      if generation land 1 = 1
      then t.generations.(index) <- (generation + 1) land generation_mask
    done;
    t.used <- 0;
    t.free_count <- 0;
    t.length <- 0
  ;;

  let length t = t.length

  let iter t ~f =
    for index = 0 to t.used - 1 do
      let generation = t.generations.(index) in
      if generation land 1 = 1
      then f (Handle.create ~index ~generation) (Array.unsafe_get t.slab index)
    done
  ;;
end
[@@kind k = pointer_free]]
