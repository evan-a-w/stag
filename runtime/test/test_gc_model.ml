open! Import
module Addr = Memory.Addr

(* Random allocations, stores, frame pushes and pops, root changes and
   collections, checked against a model of reachability: after each collection,
   the allocated blocks must be exactly the immortal ones and the ones reachable
   from the roots, with their words unchanged. *)

(* 16 pages to an arena: blocks of over 8188 words get a chunk of their own. *)
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
    | Edge of
        { id : int
        ; offset : int
        (** Its header word ([-8]), or just past its words: in the heap but in
            no block while the block is alive, so not followed. *)
        }
    | Scalar of int (** In a scalar word: maybe a block's address. *)
    | Local of int
    (** A data-stack block's address, in a data-stack word: never followed, and
        maybe freed. *)
end

(* A data-stack block: its pointer words are roots. *)
module Local_block = struct
  type t =
    { addr : int
    ; scalars : int
    ; words : Value.t array
    }
end

module Block = struct
  type t =
    { addr : int
    ; scalars : int (** All its words, for [Bytes]. *)
    ; words : Value.t array
    ; immortal : bool
    }
end

module Frame = struct
  type t =
    { slots : int (** The address of its first slot. *)
    ; values : Value.t array
    }
end

module Model = struct
  type t =
    { blocks : Block.t Int.Table.t
    ; mutable frames : Frame.t list
    (** Latest first, as pushed. The last is never popped. *)
    ; global_slot : int
    ; mutable global : Value.t
    ; mutable next_id : int
    ; mutable regions : Local_block.t list list
    (** Of the data stack, latest first, each its blocks latest first. *)
    }

  let encode t (value : Value.t) =
    match value with
    | Null -> 0
    | Small n | Foreign n | Atom n | Scalar n | Local n -> n
    | Ref { id; word } -> (Hashtbl.find_exn t.blocks id).addr + (word * 8)
    | Edge { id; offset } -> (Hashtbl.find_exn t.blocks id).addr + offset
  ;;

  let local_blocks t = List.concat t.regions

  (* Every root slot's address, and how to set its value in the model: the
     global, then the frames' slots, latest first, then the pointer words of the
     data-stack blocks. *)
  let root_slots t =
    ((t.global_slot, fun value -> t.global <- value)
     :: List.concat_map t.frames ~f:(fun (frame : Frame.t) ->
       List.init (Array.length frame.values) ~f:(fun i ->
         frame.slots + (i * 8), fun value -> frame.values.(i) <- value)))
    @ List.concat_map (local_blocks t) ~f:(fun (block : Local_block.t) ->
      List.init
        (Array.length block.words - block.scalars)
        ~f:(fun j ->
          let i = block.scalars + j in
          block.addr + (i * 8), fun value -> block.words.(i) <- value))
  ;;

  let root_values t =
    (t.global
     :: List.concat_map t.frames ~f:(fun (frame : Frame.t) ->
       Array.to_list frame.values))
    @ List.concat_map (local_blocks t) ~f:(fun (block : Local_block.t) ->
      Array.to_list block.words |> Fn.flip List.drop block.scalars)
  ;;

  (* Drops the blocks neither immortal nor reachable from the roots. *)
  let prune t =
    let reachable = Int.Hash_set.create () in
    let rec visit (value : Value.t) =
      match value with
      | Ref { id; word = _ } ->
        if not (Hash_set.mem reachable id)
        then (
          Hash_set.add reachable id;
          let block = Hashtbl.find_exn t.blocks id in
          Array.iteri block.words ~f:(fun i value ->
            if i >= block.scalars then visit value))
      | Null | Small _ | Foreign _ | Atom _ | Edge _ | Scalar _ | Local _ -> ()
    in
    List.iter (root_values t) ~f:visit;
    Hashtbl.filteri_inplace t.blocks ~f:(fun ~key:id ~data:(block : Block.t) ->
      block.immortal || Hash_set.mem reachable id)
  ;;

  (* Sets every word holding an edge to [f] of its value. *)
  let rewrite_edges t ~f =
    let rewrite (value : Value.t) =
      match value with
      | Edge _ -> Some (f value)
      | Null | Small _ | Foreign _ | Atom _ | Ref _ | Scalar _ | Local _ -> None
    in
    Hashtbl.iter t.blocks ~f:(fun (block : Block.t) ->
      Array.iteri block.words ~f:(fun i value ->
        Option.iter (rewrite value) ~f:(fun word ->
          Memory.store_addr (of_int (block.addr + (i * 8))) (of_int word))));
    List.iter2_exn (root_slots t) (root_values t) ~f:(fun (slot, _) value ->
      Option.iter (rewrite value) ~f:(fun word ->
        Memory.store_addr (of_int slot) (of_int word)))
  ;;

  (* An edge of a freed block may come to be within a block allocated in its
     place: the program drops it, as it would a dangling pointer. *)
  let drop_dangling_edges t =
    let dangling (value : Value.t) =
      match value with
      | Edge { id; offset = _ } -> not (Hashtbl.mem t.blocks id)
      | Null | Small _ | Foreign _ | Atom _ | Ref _ | Scalar _ | Local _ ->
        false
    in
    Hashtbl.iter t.blocks ~f:(fun (block : Block.t) ->
      Array.iteri block.words ~f:(fun i value ->
        if dangling value
        then (
          block.words.(i) <- Null;
          Memory.store_addr (of_int (block.addr + (i * 8))) (Addr.null ()))));
    List.iter2_exn (root_slots t) (root_values t) ~f:(fun (slot, set) value ->
      if dangling value
      then (
        set Value.Null;
        Memory.store_addr (of_int slot) (Addr.null ())))
  ;;
end

let%expect_test "random mutation against a model" =
  let heap = Heap.create heap_config in
  let roots = Roots.create () in
  let data_stack = Data_stack.create ~segment_bytes:512 () in
  let gc =
    Stag_gc.create
      { Stag_gc.Config.space_overhead = 50; min_words = 500 }
      heap
      roots
      data_stack
  in
  let static = Memory.Chunk.create ~bytes:8 ~align:8 in
  Roots.add_global roots (Memory.Chunk.base static);
  let foreign = Memory.Chunk.create ~bytes:64 ~align:8 in
  let model =
    { Model.blocks = Int.Table.create ()
    ; frames = []
    ; global_slot = to_int (Memory.Chunk.base static)
    ; global = Null
    ; next_id = 0
    ; regions = []
    }
  in
  let marks = ref [] in
  let random = Random.State.make [| 7 |] in
  let int bound = Random.State.int random bound in
  let push_frame slots =
    let first = Roots.push_frame roots ~slots in
    model.frames
    <- { Frame.slots = to_int first
       ; values = Array.create ~len:slots Value.Null
       }
       :: model.frames
  in
  push_frame 5;
  let atoms = ref [] in
  let random_block ?(immortal_only = false) () =
    let ids =
      Hashtbl.filter model.blocks ~f:(fun (block : Block.t) ->
        block.immortal || not immortal_only)
      |> Hashtbl.keys
    in
    if List.is_empty ids
    then None
    else Some (List.nth_exn ids (int (List.length ids)))
  in
  (* Immortal blocks may only point to immortal ones. *)
  let random_pointer ~immortal : Value.t =
    match int 12, random_block ~immortal_only:immortal () with
    | (0 | 1 | 2 | 3 | 4 | 5), Some id ->
      let block = Hashtbl.find_exn model.blocks id in
      let size = Array.length block.words in
      Ref { id; word = (if int 2 = 0 then 0 else int size) }
    | 6, Some id ->
      let block = Hashtbl.find_exn model.blocks id in
      Edge
        { id
        ; offset = (if int 2 = 0 then -8 else Array.length block.words * 8)
        }
    | 7, _ -> Small (1 + int 4095)
    | 8, _ -> Foreign (to_int (Memory.Chunk.base foreign) + (8 * int 8))
    | 9, _ when not (List.is_empty !atoms) ->
      Atom (List.nth_exn !atoms (int (List.length !atoms)))
    | _ -> Null
  in
  let random_scalar () : Value.t =
    match random_block () with
    | Some id when int 2 = 0 ->
      Scalar (Model.encode model (Ref { id; word = 0 }))
    | _ -> Scalar (int 1_000_000)
  in
  let collections = ref 0 in
  let checks = ref 0 in
  let compare ?fresh () =
    incr checks;
    (* Edges are against the invariants, but not against marking: they are there
       for collections, and nulled for the check. *)
    Model.rewrite_edges model ~f:(fun _ -> 0);
    Stag_gc.check_invariants gc |> ok_exn;
    Model.rewrite_edges model ~f:(Model.encode model);
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
    let locals = ref [] in
    Data_stack.iter data_stack ~f:(fun block ->
      locals := to_int block :: !locals);
    let expected_locals =
      List.map (Model.local_blocks model) ~f:(fun (block : Local_block.t) ->
        block.addr)
    in
    if not ([%equal: int list] !locals expected_locals)
    then raise_s [%message "data-stack blocks differ"];
    List.iter (Model.local_blocks model) ~f:(fun block ->
      Array.iteri block.words ~f:(fun i value ->
        if Int64_u.to_int (Memory.load_64 (of_int (block.addr + (i * 8))))
           <> Model.encode model value
        then raise_s [%message "a data-stack word changed" (i : int)]));
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
        if actual <> Model.encode model value
        then raise_s [%message "a word changed" (i : int)]))
  in
  let sync_collections ?fresh () =
    if Stag_gc.collections gc > !collections
    then (
      collections := Stag_gc.collections gc;
      Model.prune model;
      Model.drop_dangling_edges model;
      compare ?fresh ())
  in
  let set_root (slot, set) value =
    set value;
    Memory.store_addr (of_int slot) (of_int (Model.encode model value))
  in
  let random_root () =
    let slots = Model.root_slots model in
    List.nth_exn slots (int (List.length slots))
  in
  let large_blocks = ref 0 in
  let alloc ?words ?scalars ?root () =
    let words =
      match words with
      | Some words -> words
      | None ->
        (* Large: over pages, and over a whole arena. *)
        if int 200 = 0
        then (
          incr large_blocks;
          List.nth_exn [ 600; 3000; 9000 ] (int 3))
        else if int 20 = 0
        then 33 + int 60
        else int 9
    in
    let immortal = words > 0 && words < 100 && int 30 = 0 in
    let bytes = words > 0 && (not immortal) && int 5 = 0 in
    let header =
      if bytes
      then Header.bytes ~length:((words * 8) - int 8) Unmarked
      else
        Header.scanned
          ~size_words:words
          ~scalar_words:
            (match scalars with
             | Some scalars -> scalars
             | None -> if words = 0 then 0 else int (words + 1))
          (if immortal then Immortal else Unmarked)
    in
    let addr = Stag_gc.alloc gc header in
    (* An atom isn't in the heap. *)
    sync_collections ?fresh:(if words = 0 then None else Some (to_int addr)) ();
    if words = 0
    then (
      atoms := to_int addr :: !atoms;
      None)
    else (
      let id = model.next_id in
      model.next_id <- id + 1;
      let block =
        { Block.addr = to_int addr
        ; scalars = Header.scalar_words header
        ; words = Array.create ~len:words Value.Null
        ; immortal
        }
      in
      Hashtbl.add_exn model.blocks ~key:id ~data:block;
      for i = 0 to block.scalars - 1 do
        let value = random_scalar () in
        block.words.(i) <- value;
        Memory.store_64
          (Addr.add addr (i * 8))
          (Int64_u.of_int (Model.encode model value))
      done;
      (* Rooted at once. *)
      set_root
        (match root with
         | Some root -> root
         | None -> random_root ())
        (Ref { id; word = 0 });
      Some id)
  in
  let store_at id i value =
    let block = Hashtbl.find_exn model.blocks id in
    block.words.(i) <- value;
    Memory.store_addr
      (of_int (block.addr + (i * 8)))
      (of_int (Model.encode model value))
  in
  let store () =
    match random_block () with
    | None -> ()
    | Some id ->
      let block = Hashtbl.find_exn model.blocks id in
      let size = Array.length block.words in
      if size > block.scalars
      then
        store_at
          id
          (block.scalars + int (size - block.scalars))
          (random_pointer ~immortal:block.immortal)
  in
  (* A block of many pointers, each to a fresh block with a pointer, rooted in a
     frame of its own: marking it pushes them all, growing the mark stack. *)
  let wide () =
    push_frame 1;
    (* The global, then the new frame's slot. *)
    let root = List.nth_exn (Model.root_slots model) 1 in
    let words = 1500 in
    let wide =
      let addr =
        Stag_gc.alloc
          gc
          (Header.scanned ~size_words:words ~scalar_words:0 Unmarked)
      in
      sync_collections ~fresh:(to_int addr) ();
      let id = model.next_id in
      model.next_id <- id + 1;
      Hashtbl.add_exn
        model.blocks
        ~key:id
        ~data:
          { Block.addr = to_int addr
          ; scalars = 0
          ; words = Array.create ~len:words Value.Null
          ; immortal = false
          };
      set_root root (Ref { id; word = 0 });
      id
    in
    for i = 0 to words - 1 do
      Option.iter
        (alloc
           ~words:1
           ~scalars:0
           ~root:(List.hd_exn (Model.root_slots model))
           ())
        ~f:(fun id -> store_at wide i (Ref { id; word = 0 }))
    done;
    ignore (Stag_gc.collect gc : Stag_gc.Stats.t);
    sync_collections ()
  in
  (* Data-stack regions, whose blocks root heap blocks, and point to each other. *)
  let local_blocks = ref 0 in
  let enter () =
    marks := Data_stack.enter data_stack :: !marks;
    model.regions <- [] :: model.regions
  in
  let exit () =
    match !marks with
    | [] -> ()
    | mark :: rest ->
      Data_stack.exit data_stack mark;
      marks := rest;
      model.regions <- List.tl_exn model.regions
  in
  let local_alloc () =
    if List.is_empty model.regions then enter ();
    incr local_blocks;
    (* Some too large for a segment of 512 bytes. *)
    let words = if int 30 = 0 then 70 + int 40 else 1 + int 8 in
    let bytes = int 6 = 0 in
    let header =
      if bytes
      then Header.bytes ~length:((words * 8) - int 8) Unmarked
      else
        Header.scanned
          ~size_words:words
          ~scalar_words:(int (words + 1))
          Unmarked
    in
    let addr = Data_stack.alloc data_stack header in
    let block =
      { Local_block.addr = to_int addr
      ; scalars = Header.scalar_words header
      ; words = Array.create ~len:words Value.Null
      }
    in
    for i = 0 to block.scalars - 1 do
      let value = random_scalar () in
      block.words.(i) <- value;
      Memory.store_64
        (Addr.add addr (i * 8))
        (Int64_u.of_int (Model.encode model value))
    done;
    model.regions
    <- (block :: List.hd_exn model.regions) :: List.tl_exn model.regions
  in
  let local_store () =
    match Model.local_blocks model with
    | [] -> ()
    | blocks ->
      let block = List.nth_exn blocks (int (List.length blocks)) in
      let size = Array.length block.words in
      if size > block.scalars
      then (
        let i = block.scalars + int (size - block.scalars) in
        let value : Value.t =
          if int 3 = 0
          then Local (List.nth_exn blocks (int (List.length blocks))).addr
          else random_pointer ~immortal:false
        in
        block.words.(i) <- value;
        Memory.store_addr
          (of_int (block.addr + (i * 8)))
          (of_int (Model.encode model value)))
  in
  let pops = ref 0 in
  for step = 1 to 20_000 do
    if step % 7_000 = 0 then wide ();
    match int 100 with
    | n when n < 36 -> ignore (alloc () : int option)
    | n when n < 66 -> store ()
    | n when n < 69 -> local_alloc ()
    | n when n < 72 -> local_store ()
    | 72 -> enter ()
    | 73 | 74 -> exit ()
    | n when n < 90 ->
      set_root (random_root ()) (random_pointer ~immortal:false)
    | n when n < 94 -> push_frame (int 5)
    | n when n < 98 ->
      if List.length model.frames > 1
      then (
        incr pops;
        Roots.pop_frame roots;
        model.frames <- List.tl_exn model.frames)
    | _ ->
      ignore (Stag_gc.collect gc : Stag_gc.Stats.t);
      sync_collections ()
  done;
  let count f = Hashtbl.count model.blocks ~f in
  print_s
    [%message
      ""
        ~collections:(!collections : int)
        ~checks:(!checks : int)
        ~blocks_at_end:(Hashtbl.length model.blocks : int)
        ~immortal_at_end:(count (fun block -> block.immortal) : int)
        ~allocated:(model.next_id : int)
        ~large_blocks:(!large_blocks : int)
        ~pops:(!pops : int)
        ~local_blocks:(!local_blocks : int)
        ~local_blocks_at_end:(List.length (Model.local_blocks model) : int)
        ~regions_at_end:(List.length model.regions : int)
        ~data_stack_segments:
          (Data_stack.For_testing.segments_made data_stack : int)
        ~depth_at_end:(Roots.depth roots : int)
        ~mark_stack_capacity:(Stag_gc.For_testing.mark_stack_capacity gc : int)];
  (* Kept reachable until here, or their memory would be freed. *)
  Memory.Chunk.destroy static;
  Memory.Chunk.destroy foreign;
  [%expect
    {|
    ((collections 495) (checks 495) (blocks_at_end 322) (immortal_at_end 293)
     (allocated 9461) (large_blocks 41) (pops 753) (local_blocks 625)
     (local_blocks_at_end 1) (regions_at_end 1) (data_stack_segments 5)
     (depth_at_end 22) (mark_stack_capacity 2048))
    |}]
;;
