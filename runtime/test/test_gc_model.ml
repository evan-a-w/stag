open! Import
module Addr = Memory.Addr

(* Random allocations, stores, root changes and collections, checked against a
   model of reachability: after each collection, the allocated blocks must be
   exactly the ones reachable from the roots, with their words unchanged. *)

let heap_config =
  { Heap.Config.page_bytes = 4096
  ; arena_bytes = 65536
  ; size_classes = [ 1; 2; 3; 4; 6; 8; 16; 32 ]
  }
;;

let to_int addr = Nativeint_u.to_int (Addr.to_word addr)
let of_int n = Addr.of_word (Nativeint_u.of_int n)

(* What a word holds. *)
module Value = struct
  type t =
    | Null
    | Small of int (** Below 4096. *)
    | Foreign of int (** Into memory outside the heap. *)
    | Atom of int (** A zero-size block's address. *)
    | Ref of
        { id : int
        ; word : int (** Which of its words it points to. *)
        }
    | Scalar of int (** In a scalar word: maybe a block's address. *)
end

module Block = struct
  type t =
    { addr : int
    ; scalars : int
    ; words : Value.t array
    }
end

type model =
  { blocks : Block.t Int.Table.t
  ; root_values : Value.t array
  ; mutable next_id : int
  }

let encode model (value : Value.t) =
  match value with
  | Null -> 0
  | Small n | Foreign n | Atom n | Scalar n -> n
  | Ref { id; word } -> (Hashtbl.find_exn model.blocks id).addr + (word * 8)
;;

(* Drops the blocks not reachable from the roots. *)
let prune model =
  let reachable = Int.Hash_set.create () in
  let rec visit (value : Value.t) =
    match value with
    | Ref { id; word = _ } ->
      if not (Hash_set.mem reachable id)
      then (
        Hash_set.add reachable id;
        let block = Hashtbl.find_exn model.blocks id in
        Array.iteri block.words ~f:(fun i value ->
          if i >= block.scalars then visit value))
    | Null | Small _ | Foreign _ | Atom _ | Scalar _ -> ()
  in
  Array.iter model.root_values ~f:visit;
  Hashtbl.filter_keys_inplace model.blocks ~f:(Hash_set.mem reachable)
;;

let%expect_test "random mutation against a model" =
  let heap = Heap.create heap_config in
  let roots = Roots.create () in
  let gc =
    Stag_gc.create
      { Stag_gc.Config.space_overhead = 50; min_words = 500 }
      heap
      roots
  in
  let root_count = 6 in
  let frame = Roots.push_frame roots ~slots:(root_count - 1) in
  let static = Memory.Chunk.create ~bytes:8 ~align:8 in
  Roots.add_global roots (Memory.Chunk.base static);
  let root_slot i =
    if i = root_count - 1
    then Memory.Chunk.base static
    else Addr.add frame (i * 8)
  in
  let foreign = Memory.Chunk.create ~bytes:64 ~align:8 in
  let model =
    { blocks = Int.Table.create ()
    ; root_values = Array.create ~len:root_count Value.Null
    ; next_id = 0
    }
  in
  let random = Random.State.make [| 7 |] in
  let atoms = ref [] in
  let int bound = Random.State.int random bound in
  let random_block () =
    if Hashtbl.is_empty model.blocks
    then None
    else (
      let ids = Hashtbl.keys model.blocks in
      Some (List.nth_exn ids (int (List.length ids))))
  in
  let random_pointer () : Value.t =
    match int 10, random_block () with
    | (0 | 1 | 2 | 3 | 4 | 5), Some id ->
      let block = Hashtbl.find_exn model.blocks id in
      let size = Array.length block.words in
      Ref { id; word = (if int 2 = 0 then 0 else int size) }
    | 6, _ -> Small (1 + int 4095)
    | 7, _ -> Foreign (to_int (Memory.Chunk.base foreign) + (8 * int 8))
    | 8, _ when not (List.is_empty !atoms) ->
      Atom (List.nth_exn !atoms (int (List.length !atoms)))
    | _ -> Null
  in
  let random_scalar () : Value.t =
    match random_block () with
    | Some id when int 2 = 0 -> Scalar (encode model (Ref { id; word = 0 }))
    | _ -> Scalar (int 1_000_000)
  in
  let collections = ref 0 in
  let checks = ref 0 in
  let compare ?fresh () =
    incr checks;
    Stag_gc.check_invariants gc |> ok_exn;
    let allocated = ref Int.Set.empty in
    Heap.iter heap ~f:(fun block ->
      allocated := Set.add !allocated (to_int block));
    let expected =
      Hashtbl.data model.blocks
      |> List.map ~f:(fun (block : Block.t) -> block.addr)
      |> Int.Set.of_list
    in
    (* A block allocated after the collection, not yet in the model. *)
    let expected = Option.fold fresh ~init:expected ~f:Set.add in
    if not (Set.equal !allocated expected)
    then
      raise_s
        [%message
          "blocks differ"
            ~extra:(Set.length (Set.diff !allocated expected) : int)
            ~missing:(Set.length (Set.diff expected !allocated) : int)];
    Hashtbl.iter model.blocks ~f:(fun block ->
      Array.iteri block.words ~f:(fun i value ->
        let actual =
          Int64_u.to_int (Memory.load_64 (of_int (block.addr + (i * 8))))
        in
        if actual <> encode model value
        then raise_s [%message "a word changed" (i : int)]))
  in
  let sync_collections ?fresh () =
    if Stag_gc.collections gc > !collections
    then (
      collections := Stag_gc.collections gc;
      prune model;
      compare ?fresh ())
  in
  let alloc () =
    let words = if int 20 = 0 then 33 + int 60 else int 9 in
    let scalars = if words = 0 then 0 else int (words + 1) in
    let addr =
      Stag_gc.alloc
        gc
        (Header.scanned ~size_words:words ~scalar_words:scalars Unmarked)
    in
    (* An atom isn't in the heap. *)
    sync_collections ?fresh:(if words = 0 then None else Some (to_int addr)) ();
    if words = 0
    then atoms := to_int addr :: !atoms
    else (
      let id = model.next_id in
      model.next_id <- id + 1;
      let block =
        { Block.addr = to_int addr
        ; scalars
        ; words = Array.create ~len:words Value.Null
        }
      in
      Hashtbl.add_exn model.blocks ~key:id ~data:block;
      for i = 0 to scalars - 1 do
        let value = random_scalar () in
        block.words.(i) <- value;
        Memory.store_64
          (Addr.add addr (i * 8))
          (Int64_u.of_int (encode model value))
      done;
      (* Rooted at once. *)
      let root = int root_count in
      model.root_values.(root) <- Ref { id; word = 0 };
      Memory.store_addr (root_slot root) addr)
  in
  let store () =
    match random_block () with
    | None -> ()
    | Some id ->
      let block = Hashtbl.find_exn model.blocks id in
      let size = Array.length block.words in
      if size > block.scalars
      then (
        let i = block.scalars + int (size - block.scalars) in
        let value = random_pointer () in
        block.words.(i) <- value;
        Memory.store_addr
          (of_int (block.addr + (i * 8)))
          (of_int (encode model value)))
  in
  let set_root () =
    let root = int root_count in
    let value = random_pointer () in
    model.root_values.(root) <- value;
    Memory.store_addr (root_slot root) (of_int (encode model value))
  in
  for _ = 1 to 20_000 do
    match int 100 with
    | n when n < 40 -> alloc ()
    | n when n < 80 -> store ()
    | n when n < 98 -> set_root ()
    | _ ->
      ignore (Stag_gc.collect gc : Stag_gc.Stats.t);
      sync_collections ()
  done;
  print_s
    [%message
      ""
        ~collections:(!collections : int)
        ~checks:(!checks : int)
        ~blocks_at_end:(Hashtbl.length model.blocks : int)
        ~allocated:(model.next_id : int)];
  [%expect
    {| ((collections 428) (checks 428) (blocks_at_end 13) (allocated 7238)) |}]
;;
