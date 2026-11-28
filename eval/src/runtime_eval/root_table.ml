open! Import
module Addr = Memory.Addr
module Chunk = Memory.Chunk

let[@cold] fail sexp = raise (Error.to_exn (Error.create_s sexp))

module Id = struct
  type t = int [@@deriving equal, sexp_of]
end

type t =
  { mutable slots_chunk : Chunk.t
  ; mutable slots_base : Addr.t
  ; mutable capacity : int
  ; mutable used : int
  ; mutable slot_owner_id : int array
  ; mutable slot_owner_pos : int array
  ; entries : int array Int.Table.t
  ; mutable next_id : int
  ; deferred : int list Atomic.t
  ; mutable destroyed : bool
  }

let initial_capacity = 64

let create () =
  let slots_chunk = Chunk.create ~bytes:(initial_capacity * 8) ~align:8 in
  { slots_chunk
  ; slots_base = Chunk.base slots_chunk
  ; capacity = initial_capacity
  ; used = 0
  ; slot_owner_id = Array.create ~len:initial_capacity 0
  ; slot_owner_pos = Array.create ~len:initial_capacity 0
  ; entries = Int.Table.create ()
  ; next_id = 0
  ; deferred = Atomic.make []
  ; destroyed = false
  }
;;

let check_live t = if t.destroyed then fail [%message "Root_table: destroyed"]

let ensure_capacity t needed =
  if needed > t.capacity
  then (
    let new_capacity = ref t.capacity in
    while !new_capacity < needed do
      new_capacity := !new_capacity * 2
    done;
    let cap = !new_capacity in
    let new_chunk = Chunk.create ~bytes:(cap * 8) ~align:8 in
    let new_base = Chunk.base new_chunk in
    if t.used > 0
    then Memory.copy ~src:t.slots_base ~dst:new_base ~bytes:(t.used * 8);
    Chunk.destroy t.slots_chunk;
    let grow_ints a =
      let b = Array.create ~len:cap 0 in
      Array.blit ~src:a ~src_pos:0 ~dst:b ~dst_pos:0 ~len:t.used;
      b
    in
    t.slots_chunk <- new_chunk;
    t.slots_base <- new_base;
    t.capacity <- cap;
    t.slot_owner_id <- grow_ints t.slot_owner_id;
    t.slot_owner_pos <- grow_ints t.slot_owner_pos)
;;

let register t ptrs =
  check_live t;
  let id = t.next_id in
  t.next_id <- id + 1;
  let len = Array.length ptrs in
  ensure_capacity t (t.used + len);
  let indices = Array.create ~len 0 in
  for i = 0 to len - 1 do
    let s = t.used in
    t.used <- s + 1;
    Memory.store_addr (Addr.add t.slots_base (s * 8)) ptrs.(i);
    t.slot_owner_id.(s) <- id;
    t.slot_owner_pos.(s) <- i;
    indices.(i) <- s
  done;
  Hashtbl.add_exn t.entries ~key:id ~data:indices;
  id
;;

let unregister t id =
  if not t.destroyed
  then (
    match Hashtbl.find_and_remove t.entries id with
    | None -> ()
    | Some indices ->
      for i = Array.length indices - 1 downto 0 do
        let s = indices.(i) in
        let last = t.used - 1 in
        t.used <- last;
        if s < last
        then (
          let word = Memory.load_addr (Addr.add t.slots_base (last * 8)) in
          Memory.store_addr (Addr.add t.slots_base (s * 8)) word;
          let moved_id = t.slot_owner_id.(last) in
          let moved_pos = t.slot_owner_pos.(last) in
          t.slot_owner_id.(s) <- moved_id;
          t.slot_owner_pos.(s) <- moved_pos;
          if moved_id = id
          then indices.(moved_pos) <- s
          else (Hashtbl.find_exn t.entries moved_id).(moved_pos) <- s);
        Memory.store_addr (Addr.add t.slots_base (last * 8)) (Addr.null ())
      done)
;;

let rec defer_unregister t id =
  let old = Atomic.get t.deferred in
  if not (Atomic.compare_and_set t.deferred old (id :: old))
  then defer_unregister t id
;;

let drain_deferred t =
  if not t.destroyed
  then (
    match Atomic.exchange t.deferred [] with
    | [] -> ()
    | ids -> List.iter ids ~f:(unregister t))
;;

let destroy t =
  check_live t;
  ignore (Atomic.exchange t.deferred [] : int list);
  Hashtbl.clear t.entries;
  Chunk.destroy t.slots_chunk;
  t.slots_base <- Addr.null ();
  t.used <- 0;
  t.capacity <- 0;
  t.destroyed <- true
;;

let live_count t = Hashtbl.length t.entries

module Source = struct
  type nonrec t = t

  let name = "root_table"
  let holds_locals = false

  let[@zero_alloc] iter t ~(f : _ @ local) =
    for i = 0 to t.used - 1 do
      (f [@zero_alloc assume]) (Addr.add t.slots_base (i * 8))
    done
  ;;
end

let root_source t = Root_source.create (module Source) t
