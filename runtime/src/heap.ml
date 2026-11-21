open! Import
module Addr = Memory.Addr
module Chunk = Memory.Chunk

module Config = struct
  type t =
    { page_bytes : int
    ; arena_bytes : int
    ; size_classes : int list
    }
  [@@deriving sexp_of]

  let default =
    { page_bytes = 64 * 1024
    ; arena_bytes = 4 * 1024 * 1024
    ; size_classes =
        List.range 1 9
        @ [ 10; 12; 14; 16; 20; 24; 28; 32; 40; 48; 56; 64 ]
        @ [ 80; 96; 112; 128; 160; 192; 224; 256 ]
        @ [ 320; 384; 448; 512; 640; 768; 896; 1024 ]
    }
  ;;

  let max_page_bytes = 1 lsl 20
  let max_arena_bytes = 1 lsl 30
end

module Sweep_stats = struct
  type t =
    { freed_blocks : int
    ; freed_words : int
    ; kept_blocks : int
    ; kept_words : int
    ; kept_footprint_words : int
    }
  [@@deriving sexp_of]
end

let[@cold] fail sexp = raise (Error.to_exn (Error.create_s sexp))
let[@inline] [@zero_alloc] load_int addr = Int64_u.to_int (Memory.load_64 addr)

let[@inline] [@zero_alloc] store_int addr n =
  Memory.store_64 addr (Int64_u.of_int n)
;;

(* At the base of each region: the bytes of each slot (its header and block),
   the reciprocal of that, and the number of slots. The first slot's header
   follows.

   A slot's index is [offset / slot_bytes] for the [offset] of an address from
   the first slot, computed as [(offset * magic) lsr shift] with
   [shift = 2 * log2 page_bytes] and [magic = ceil (2^shift / slot_bytes)].
   Writing [magic * slot_bytes = 2^shift + e] with [0 <= e < slot_bytes],
   [offset * magic / 2^shift = offset / slot_bytes + offset * e / (slot_bytes * 2^shift)],
   whose floor is [offset / slot_bytes]'s as long as [offset * e < 2^shift],
   which holds as [offset] and [e] are below [page_bytes]. The product is below
   [page_bytes * 2^shift <= 2^60] for pages of at most 1 MiB. A large block's
   region has one slot and a [magic] of 0, so any offset is in slot 0. *)
module Descriptor = struct
  let bytes = 24

  let store base ~slot_bytes ~magic ~slots =
    store_int base slot_bytes;
    store_int (Addr.add base 8) magic;
    store_int (Addr.add base 16) slots
  ;;

  let[@inline] [@zero_alloc] slot_bytes base = load_int base
  let[@inline] [@zero_alloc] magic base = load_int (Addr.add base 8)
  let[@inline] [@zero_alloc] slots base = load_int (Addr.add base 16)
end

(* A chunk of [arena_bytes], aligned to [page_bytes], its pages handed out to
   regions. *)
module Arena = struct
  type t =
    { chunk : Chunk.t
    ; free : bool array (** By page. *)
    ; mutable free_pages : int
    ; mutable hint : int (** No page before it is free. *)
    }

  let create ~arena_bytes ~page_bytes =
    let pages = arena_bytes / page_bytes in
    { chunk = Chunk.create ~bytes:arena_bytes ~align:page_bytes
    ; free = Array.create ~len:pages true
    ; free_pages = pages
    ; hint = 0
    }
  ;;

  let is_empty t = t.free_pages = Array.length t.free

  (* The first of the first run of [pages] free pages, which are taken, or -1. *)
  let take t ~pages =
    if pages > t.free_pages
    then -1
    else (
      let len = Array.length t.free in
      let found = ref (-1) in
      let run = ref 0 in
      let page = ref t.hint in
      while !found < 0 && !page < len do
        if t.free.(!page)
        then (
          incr run;
          if !run = pages then found := !page - pages + 1)
        else run := 0;
        incr page
      done;
      if !found >= 0
      then (
        Array.fill t.free ~pos:!found ~len:pages false;
        t.free_pages <- t.free_pages - pages;
        if !found = t.hint then t.hint <- !found + pages);
      !found)
  ;;

  let release t ~first ~pages =
    Array.fill t.free ~pos:first ~len:pages true;
    t.free_pages <- t.free_pages + pages;
    t.hint <- Int.min t.hint first
  ;;
end

module Region = struct
  module Kind = struct
    type t =
      | Page of { class_index : int }
      | Large
  end

  module Backing = struct
    type t =
      | Arena of
          { arena : Arena.t
          ; first : int
          }
      | Chunk of Chunk.t
  end

  type t =
    { index : int
    ; backing : Backing.t
    ; pages : int
    ; kind : Kind.t
    ; slot_bytes : int
    ; slots : int
    ; mutable removed : bool (** Its pages given back. *)
    }

  let base t ~page_bytes =
    match t.backing with
    | Arena { arena; first } ->
      Addr.add (Chunk.base arena.chunk) (first * page_bytes)
    | Chunk chunk -> Chunk.base chunk
  ;;

  (* The [i]th slot's block. *)
  let block t i ~page_bytes =
    Addr.add (base t ~page_bytes) (Descriptor.bytes + (i * t.slot_bytes) + 8)
  ;;
end

module Handle = struct
  module Payload = struct
    type t = T : 'a Type_equal.Id.t * 'a -> t
  end

  type t =
    { block : Addr.t
    ; payload : Payload.t
    }

  let id block = Int64_u.to_int (Memory.load_64 block)
end

type t =
  { config : Config.t
  ; shift : int
  ; classes : int array (** Words. *)
  ; class_of_words : int array (** The smallest class of at least that many. *)
  ; free : Addr.t array (** The first free block of each class, or null. *)
  ; page_table : Page_table.t
  ; mutable arenas : Arena.t list (** Oldest first: allocation is first fit. *)
  ; mutable regions : Region.t list (** Latest first. *)
  ; mutable regions_created : int
  ; handles : Handle.t Int.Table.t
  ; mutable next_handle : int
  ; mutable destroyed : bool
  }

let create (config : Config.t) =
  let { Config.page_bytes; arena_bytes; size_classes } = config in
  let is_power_of_two n = n > 0 && n land (n - 1) = 0 in
  let classes = Array.of_list size_classes in
  let largest = if Array.is_empty classes then 0 else Array.last_exn classes in
  if (not (is_power_of_two page_bytes))
     || page_bytes < 64
     || page_bytes > Config.max_page_bytes
     || (not (is_power_of_two arena_bytes))
     || arena_bytes < page_bytes
     || arena_bytes > Config.max_arena_bytes
     || Array.is_empty classes
     || classes.(0) < 1
     || (not
           (Array.for_alli classes ~f:(fun i words ->
              i = 0 || classes.(i - 1) < words)))
     || Descriptor.bytes + ((largest + 1) * 8) > page_bytes
  then fail [%message "Heap.create: invalid config" (config : Config.t)];
  let class_of_words = Array.create ~len:(largest + 1) 0 in
  let class_index = ref 0 in
  for words = 1 to largest do
    if classes.(!class_index) < words then incr class_index;
    class_of_words.(words) <- !class_index
  done;
  let t =
    { config
    ; shift = 2 * Int.ctz page_bytes
    ; classes
    ; class_of_words
    ; free = Array.create ~len:(Array.length classes) (Addr.null ())
    ; page_table = Page_table.create ~page_bytes
    ; arenas = []
    ; regions = []
    ; regions_created = 0
    ; handles = Int.Table.create ()
    ; next_handle = 0
    ; destroyed = false
    }
  in
  t
;;

let check_live t ~name =
  if t.destroyed then fail [%message "Heap: destroyed" (name : string)]
;;

let page_bytes t = t.config.page_bytes
let arena_pages t = t.config.arena_bytes / page_bytes t

(* [pages] contiguous pages, from the first arena with a run of them. *)
let take_pages t ~pages : Region.Backing.t =
  let rec first_fit : Arena.t list -> Region.Backing.t = function
    | arena :: arenas ->
      let first = Arena.take arena ~pages in
      if first >= 0 then Arena { arena; first } else first_fit arenas
    | [] ->
      let arena =
        Arena.create
          ~arena_bytes:t.config.arena_bytes
          ~page_bytes:(page_bytes t)
      in
      t.arenas <- t.arenas @ [ arena ];
      let first = Arena.take arena ~pages in
      if first < 0
      then fail [%message "Heap: a fresh arena has no room" (pages : int)];
      Arena { arena; first }
  in
  first_fit t.arenas
;;

let iter_pages t (region : Region.t) ~f =
  let base = Region.base region ~page_bytes:(page_bytes t) in
  for i = 0 to region.pages - 1 do
    f (Addr.add base (i * page_bytes t))
  done
;;

let add_region t ~pages ~kind ~slot_bytes ~slots ~magic =
  let backing : Region.Backing.t =
    if pages <= arena_pages t
    then take_pages t ~pages
    else
      Chunk (Chunk.create ~bytes:(pages * page_bytes t) ~align:(page_bytes t))
  in
  let region =
    { Region.index = t.regions_created
    ; backing
    ; pages
    ; kind
    ; slot_bytes
    ; slots
    ; removed = false
    }
  in
  let base = Region.base region ~page_bytes:(page_bytes t) in
  Descriptor.store base ~slot_bytes ~magic ~slots;
  t.regions_created <- t.regions_created + 1;
  t.regions <- region :: t.regions;
  iter_pages t region ~f:(fun page ->
    Page_table.add t.page_table ~page ~owner:base);
  region
;;

let remove_region t (region : Region.t) =
  region.removed <- true;
  iter_pages t region ~f:(fun page -> Page_table.remove t.page_table ~page);
  match region.backing with
  | Arena { arena; first } -> Arena.release arena ~first ~pages:region.pages
  | Chunk chunk -> Chunk.destroy chunk
;;

let push_free t ~class_index block =
  Memory.store_addr block t.free.(class_index);
  t.free.(class_index) <- block
;;

let add_page t class_index =
  let words = t.classes.(class_index) in
  let slot_bytes = (words + 1) * 8 in
  let slots = (page_bytes t - Descriptor.bytes) / slot_bytes in
  let magic = ((1 lsl t.shift) + slot_bytes - 1) / slot_bytes in
  let region =
    add_region t ~pages:1 ~kind:(Page { class_index }) ~slot_bytes ~slots ~magic
  in
  for i = slots - 1 downto 0 do
    let block = Region.block region i ~page_bytes:(page_bytes t) in
    Header.store block (Header.free ~size_words:words);
    push_free t ~class_index block
  done
;;

let large_pages t ~words =
  Int.round_up
    (Descriptor.bytes + ((words + 1) * 8))
    ~to_multiple_of:(page_bytes t)
  / page_bytes t
;;

let alloc_large t header ~words =
  let slot_bytes = (words + 1) * 8 in
  let pages = large_pages t ~words in
  let region = add_region t ~pages ~kind:Large ~slot_bytes ~slots:1 ~magic:0 in
  let block = Region.block region 0 ~page_bytes:(page_bytes t) in
  Header.store block header;
  Memory.fill_zero block ~bytes:(words * 8);
  block
;;

let alloc_block t header =
  check_live t ~name:"alloc";
  (match Header.kind header with
   | Free -> fail [%message "Heap.alloc: a Free header" (header : Header.t)]
   | Scanned | Bytes | Handle -> ());
  (match Header.color header with
   | Marked -> fail [%message "Heap.alloc: a Marked header" (header : Header.t)]
   | Unmarked | Immortal -> ());
  let words = Header.size_words header in
  if words = 0
  then (
    match Header.kind header with
    | Bytes -> Static_data.empty_bytes ()
    | Scanned | Free | Handle -> Static_data.empty ())
  else if words >= Array.length t.class_of_words
  then alloc_large t header ~words
  else (
    let class_index = t.class_of_words.(words) in
    if Addr.is_null t.free.(class_index) then add_page t class_index;
    let block = t.free.(class_index) in
    t.free.(class_index) <- Memory.load_addr block;
    Header.store block header;
    Memory.fill_zero block ~bytes:(t.classes.(class_index) * 8);
    block)
;;

let alloc t header =
  (match Header.kind header with
   | Handle -> fail [%message "Heap.alloc: use alloc_handle"]
   | Free | Scanned | Bytes -> ());
  alloc_block t header
;;

let alloc_handle t key value =
  let id = t.next_handle in
  if id = Int.max_value then fail [%message "Heap.alloc_handle: ID exhausted"];
  let block = alloc_block t (Header.handle ()) in
  t.next_handle <- id + 1;
  Hashtbl.set t.handles ~key:id ~data:{ Handle.block; payload = T (key, value) };
  Memory.store_64 block (Int64_u.of_int id);
  block
;;

let footprint_words t header =
  let words = Header.size_words header in
  if words = 0
  then 0
  else if words >= Array.length t.class_of_words
  then large_pages t ~words * (page_bytes t / 8)
  else t.classes.(t.class_of_words.(words)) + 1
;;

let[@zero_alloc] owns t addr =
  not (Addr.is_null (Page_table.find t.page_table addr))
;;

let[@zero_alloc] block_start t addr =
  let owner = Page_table.find t.page_table addr in
  if Addr.is_null owner
  then Addr.null ()
  else (
    let offset = Addr.diff addr owner - Descriptor.bytes in
    if Int.(offset < 0)
    then Addr.null ()
    else (
      let slot = (offset * Descriptor.magic owner) lsr t.shift in
      if Int.(slot >= Descriptor.slots owner)
      then Addr.null ()
      else (
        let slot_offset = slot * Descriptor.slot_bytes owner in
        let within = offset - slot_offset - 8 in
        if Int.(within < 0)
        then Addr.null ()
        else (
          let block = Addr.add owner (Descriptor.bytes + slot_offset + 8) in
          let header = Header.load block in
          match Header.kind header with
          | Free -> Addr.null ()
          | Scanned | Bytes | Handle ->
            if Int.(within < Header.size_words header * 8)
            then block
            else Addr.null ()))))
;;

let get_handle (type a) t block (key : a Type_equal.Id.t) : a option =
  check_live t ~name:"get_handle";
  let start = block_start t block in
  if Addr.is_null start || not (Addr.equal start block)
  then None
  else (
    match Header.kind (Header.load block) with
    | Scanned | Bytes | Free -> None
    | Handle ->
      (match Hashtbl.find t.handles (Handle.id block) with
       | Some { block = owner; payload = T (stored_key, value) }
         when Addr.equal owner block ->
         (match Type_equal.Id.same_witness stored_key key with
          | Some T -> Some value
          | None -> None)
       | Some _ | None -> None))
;;

let sweep t =
  check_live t ~name:"sweep";
  let page_bytes = page_bytes t in
  let freed_blocks = ref 0 in
  let freed_words = ref 0 in
  let kept_blocks = ref 0 in
  let kept_words = ref 0 in
  let kept_footprint_words = ref 0 in
  let free block header =
    (match Header.kind header with
     | Handle -> Hashtbl.remove t.handles (Handle.id block)
     | Scanned | Bytes | Free -> ());
    incr freed_blocks;
    freed_words := !freed_words + Header.size_words header
  in
  let keep header ~footprint_words =
    incr kept_blocks;
    kept_words := !kept_words + Header.size_words header;
    kept_footprint_words := !kept_footprint_words + footprint_words
  in
  for i = 0 to Array.length t.free - 1 do
    t.free.(i) <- Addr.null ()
  done;
  (* Frees the region's garbage, giving its pages back if nothing is left in it,
     and says whether it is kept. A page's free slots are pushed last slot
     first, and the regions are swept latest first, so the free lists end up in
     creation and address order. *)
  let sweep_region (region : Region.t) =
    match region.kind with
    | Page { class_index } ->
      let words = t.classes.(class_index) in
      let kept = ref 0 in
      for i = 0 to region.slots - 1 do
        let block = Region.block region i ~page_bytes in
        let header = Header.load block in
        match Header.kind header, Header.color header with
        | Free, _ -> ()
        | (Scanned | Bytes | Handle), Unmarked ->
          free block header;
          Header.store block (Header.free ~size_words:words)
        | (Scanned | Bytes | Handle), Marked ->
          incr kept;
          keep header ~footprint_words:(words + 1);
          Header.store block (Header.with_color header Unmarked)
        | (Scanned | Bytes | Handle), Immortal ->
          incr kept;
          keep header ~footprint_words:(words + 1)
      done;
      if !kept = 0
      then (
        remove_region t region;
        false)
      else (
        for i = region.slots - 1 downto 0 do
          let block = Region.block region i ~page_bytes in
          match Header.kind (Header.load block) with
          | Free -> push_free t ~class_index block
          | Scanned | Bytes | Handle -> ()
        done;
        true)
    | Large ->
      let block = Region.block region 0 ~page_bytes in
      let header = Header.load block in
      let footprint_words = region.pages * (page_bytes / 8) in
      (match Header.color header with
       | Unmarked ->
         free block header;
         remove_region t region;
         false
       | Marked ->
         keep header ~footprint_words;
         Header.store block (Header.with_color header Unmarked);
         true
       | Immortal ->
         keep header ~footprint_words;
         true)
  in
  (* Not [List.filter], so a sweep giving back no region allocates nothing. *)
  let all_kept =
    List.fold t.regions ~init:true ~f:(fun all_kept region ->
      sweep_region region && all_kept)
  in
  if not all_kept
  then (
    t.regions
    <- List.filter t.regions ~f:(fun (region : Region.t) -> not region.removed);
    (* Wholly free arenas but the first go back to the system. *)
    match t.arenas with
    | [] -> ()
    | first :: rest ->
      let empty, used = List.partition_tf rest ~f:Arena.is_empty in
      List.iter empty ~f:(fun (arena : Arena.t) -> Chunk.destroy arena.chunk);
      t.arenas <- first :: used);
  { Sweep_stats.freed_blocks = !freed_blocks
  ; freed_words = !freed_words
  ; kept_blocks = !kept_blocks
  ; kept_words = !kept_words
  ; kept_footprint_words = !kept_footprint_words
  }
;;

let iter t ~(f : _ @ local) =
  check_live t ~name:"iter";
  let page_bytes = page_bytes t in
  let regions = Array.of_list_rev t.regions in
  for r = 0 to Array.length regions - 1 do
    let region = regions.(r) in
    for i = 0 to region.Region.slots - 1 do
      let block = Region.block region i ~page_bytes in
      match Header.kind (Header.load block) with
      | Free -> ()
      | Scanned | Bytes | Handle -> f block
    done
  done
;;

let destroy t =
  check_live t ~name:"destroy";
  Hashtbl.clear t.handles;
  List.iter t.regions ~f:(remove_region t);
  List.iter t.arenas ~f:(fun (arena : Arena.t) -> Chunk.destroy arena.chunk);
  t.regions <- [];
  t.arenas <- [];
  for i = 0 to Array.length t.free - 1 do
    t.free.(i) <- Addr.null ()
  done;
  t.destroyed <- true
;;

let sexp_of_t t =
  let page_bytes = page_bytes t in
  let region_sexp (region : Region.t) =
    let base = Region.base region ~page_bytes in
    let slots =
      List.init region.slots ~f:(fun i ->
        let block = Region.block region i ~page_bytes in
        [%sexp (Addr.diff block base : int), (Header.load block : Header.t)])
    in
    match region.kind with
    | Page { class_index } ->
      [%sexp
        Page
          { index = (region.index : int)
          ; words = (t.classes.(class_index) : int)
          ; slots : Sexp.t list
          }]
    | Large ->
      [%sexp
        Large
          { index = (region.index : int)
          ; pages = (region.pages : int)
          ; slots : Sexp.t list
          }]
  in
  [%sexp (List.rev_map t.regions ~f:region_sexp : Sexp.t list)]
;;

module For_testing = struct
  let handles t = Hashtbl.length t.handles

  let locate t addr =
    let page_bytes = page_bytes t in
    List.find_map t.regions ~f:(fun (region : Region.t) ->
      let base = Region.base region ~page_bytes in
      let offset =
        Nativeint_u.to_int
          (Nativeint_u.sub (Addr.to_word addr) (Addr.to_word base))
      in
      if offset >= 0 && offset < region.pages * page_bytes
      then Some (region.index, offset)
      else None)
  ;;

  let regions t = List.length t.regions
  let pages t = Page_table.length t.page_table
  let arenas t = List.length t.arenas

  let free_pages t =
    List.map t.arenas ~f:(fun (arena : Arena.t) -> arena.free_pages)
  ;;

  let arena_page t addr =
    List.find_mapi t.arenas ~f:(fun i (arena : Arena.t) ->
      if Chunk.contains arena.chunk addr
      then Some (i, Addr.diff addr (Chunk.base arena.chunk) / page_bytes t)
      else None)
  ;;
end
