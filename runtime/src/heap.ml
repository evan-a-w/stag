open! Import
module Addr = Memory.Addr
module Chunk = Memory.Chunk

module Config = struct
  type t =
    { page_bytes : int
    ; size_classes : int list
    }
  [@@deriving sexp_of]

  let default =
    { page_bytes = 64 * 1024
    ; size_classes =
        List.range 1 9
        @ [ 10; 12; 14; 16; 20; 24; 28; 32; 40; 48; 56; 64 ]
        @ [ 80; 96; 112; 128; 160; 192; 224; 256 ]
    }
  ;;
end

module Sweep_stats = struct
  type t =
    { freed_blocks : int
    ; freed_words : int
    ; kept_blocks : int
    ; kept_words : int
    }
  [@@deriving sexp_of]
end

(* A region's descriptor, at its base: the bytes of each slot (its header and
   block), the offset of the first slot's header, and the number of slots. *)
let slot_bytes_offset = 0
let first_slot_offset = 8
let slots_offset = 16
let first_slot = 24

module Region = struct
  module Kind = struct
    type t =
      | Page of { class_index : int }
      | Large
  end

  type t =
    { index : int
    ; chunk : Chunk.t
    ; kind : Kind.t
    ; slot_bytes : int
    ; slots : int
    }

  (* The [i]th slot's block. *)
  let block t i =
    Addr.add (Chunk.base t.chunk) (first_slot + (i * t.slot_bytes) + 8)
  ;;

  let pages t ~page_bytes ~f =
    for i = 0 to (Chunk.bytes t.chunk / page_bytes) - 1 do
      f (Addr.add (Chunk.base t.chunk) (i * page_bytes))
    done
  ;;
end

type t =
  { config : Config.t
  ; classes : int array (** Words. *)
  ; class_of_words : int array (** The smallest class of at least that many. *)
  ; free : Addr.t array (** The first free block of each class, or null. *)
  ; page_table : Page_table.t
  ; mutable regions : Region.t list (** Latest first. *)
  ; mutable regions_created : int
  ; atoms : Chunk.t
  }

let[@cold] fail sexp = raise (Error.to_exn (Error.create_s sexp))

(* The zero-size atoms' headers are words 0 and 1, so their blocks are at 8
   and 16. *)
let scanned_atom t = Addr.add (Chunk.base t.atoms) 8
let bytes_atom t = Addr.add (Chunk.base t.atoms) 16

let create (config : Config.t) =
  let { Config.page_bytes; size_classes } = config in
  let classes = Array.of_list size_classes in
  let largest = if Array.is_empty classes then 0 else Array.last_exn classes in
  if page_bytes < 64
     || page_bytes land (page_bytes - 1) <> 0
     || Array.is_empty classes
     || classes.(0) < 1
     || (not
           (Array.for_alli classes ~f:(fun i words ->
              i = 0 || classes.(i - 1) < words)))
     || first_slot + ((largest + 1) * 8) > page_bytes
  then raise_s [%message "Heap.create: invalid config" (config : Config.t)];
  let class_of_words = Array.create ~len:(largest + 1) 0 in
  let class_index = ref 0 in
  for words = 1 to largest do
    if classes.(!class_index) < words then incr class_index;
    class_of_words.(words) <- !class_index
  done;
  let atoms = Chunk.create ~bytes:24 ~align:8 in
  let t =
    { config
    ; classes
    ; class_of_words
    ; free = Array.create ~len:(Array.length classes) (Addr.null ())
    ; page_table = Page_table.create ~page_bytes
    ; regions = []
    ; regions_created = 0
    ; atoms
    }
  in
  Header.store
    (scanned_atom t)
    (Header.scanned ~size_words:0 ~scalar_words:0 Immortal);
  Header.store (bytes_atom t) (Header.bytes ~length:0 Immortal);
  t
;;

let add_region t ~bytes ~kind ~slot_bytes ~slots =
  let page_bytes = t.config.page_bytes in
  let chunk = Chunk.create ~bytes ~align:page_bytes in
  let base = Chunk.base chunk in
  let store offset n =
    Memory.store_64 (Addr.add base offset) (Int64_u.of_int n)
  in
  store slot_bytes_offset slot_bytes;
  store first_slot_offset first_slot;
  store slots_offset slots;
  let region =
    { Region.index = t.regions_created; chunk; kind; slot_bytes; slots }
  in
  t.regions_created <- t.regions_created + 1;
  t.regions <- region :: t.regions;
  Region.pages region ~page_bytes ~f:(fun page ->
    Page_table.add t.page_table ~page ~owner:base);
  region
;;

let push_free t ~class_index block =
  Memory.store_addr block t.free.(class_index);
  t.free.(class_index) <- block
;;

let add_page t class_index =
  let words = t.classes.(class_index) in
  let slot_bytes = (words + 1) * 8 in
  let page_bytes = t.config.page_bytes in
  let slots = (page_bytes - first_slot) / slot_bytes in
  let region =
    add_region
      t
      ~bytes:page_bytes
      ~kind:(Page { class_index })
      ~slot_bytes
      ~slots
  in
  for i = slots - 1 downto 0 do
    let block = Region.block region i in
    Header.store block (Header.free ~size_words:words);
    push_free t ~class_index block
  done
;;

let alloc_large t header ~words =
  let page_bytes = t.config.page_bytes in
  let slot_bytes = (words + 1) * 8 in
  let bytes =
    Int.round_up (first_slot + slot_bytes) ~to_multiple_of:page_bytes
  in
  let region = add_region t ~bytes ~kind:Large ~slot_bytes ~slots:1 in
  let block = Region.block region 0 in
  Header.store block header;
  block
;;

let alloc t header =
  (match Header.kind header with
   | Free -> fail [%message "Heap.alloc: a Free header" (header : Header.t)]
   | Scanned | Bytes -> ());
  let words = Header.size_words header in
  if words = 0
  then (
    match Header.kind header with
    | Bytes -> bytes_atom t
    | Scanned | Free -> scanned_atom t)
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

let[@inline] [@zero_alloc] load_int addr = Int64_u.to_int (Memory.load_64 addr)

let[@zero_alloc] block_start t addr =
  let owner = Page_table.find t.page_table addr in
  if Addr.is_null owner
  then Addr.null ()
  else (
    let slot_bytes = load_int (Addr.add owner slot_bytes_offset) in
    let first = load_int (Addr.add owner first_slot_offset) in
    let offset = Addr.diff addr owner - first in
    if Int.(offset < 0)
    then Addr.null ()
    else (
      let slot = offset / slot_bytes in
      if Int.(slot >= load_int (Addr.add owner slots_offset))
      then Addr.null ()
      else (
        let block = Addr.add owner (first + (slot * slot_bytes) + 8) in
        let within = offset - (slot * slot_bytes) - 8 in
        if Int.(within < 0)
        then Addr.null ()
        else (
          let header = Header.load block in
          match Header.kind header with
          | Free -> Addr.null ()
          | Scanned | Bytes ->
            if Int.(within < Header.size_words header * 8)
            then block
            else Addr.null ()))))
;;

let sweep t =
  let freed_blocks = ref 0 in
  let freed_words = ref 0 in
  let kept_blocks = ref 0 in
  let kept_words = ref 0 in
  let count blocks words header =
    incr blocks;
    words := !words + Header.size_words header
  in
  for i = 0 to Array.length t.free - 1 do
    t.free.(i) <- Addr.null ()
  done;
  (* Latest region first and last slot first, so the free lists end up in
     creation and address order. *)
  let kept_regions =
    List.filter t.regions ~f:(fun (region : Region.t) ->
      match region.kind with
      | Page { class_index } ->
        for i = region.slots - 1 downto 0 do
          let block = Region.block region i in
          let header = Header.load block in
          match Header.kind header, Header.color header with
          | Free, _ -> push_free t ~class_index block
          | (Scanned | Bytes), Unmarked ->
            count freed_blocks freed_words header;
            Header.store block (Header.free ~size_words:t.classes.(class_index));
            push_free t ~class_index block
          | (Scanned | Bytes), Marked ->
            count kept_blocks kept_words header;
            Header.store block (Header.with_color header Unmarked)
          | (Scanned | Bytes), Immortal -> count kept_blocks kept_words header
        done;
        true
      | Large ->
        let block = Region.block region 0 in
        let header = Header.load block in
        (match Header.color header with
         | Unmarked ->
           count freed_blocks freed_words header;
           Region.pages region ~page_bytes:t.config.page_bytes ~f:(fun page ->
             Page_table.remove t.page_table ~page);
           Chunk.destroy region.chunk;
           false
         | Marked ->
           count kept_blocks kept_words header;
           Header.store block (Header.with_color header Unmarked);
           true
         | Immortal ->
           count kept_blocks kept_words header;
           true))
  in
  t.regions <- kept_regions;
  { Sweep_stats.freed_blocks = !freed_blocks
  ; freed_words = !freed_words
  ; kept_blocks = !kept_blocks
  ; kept_words = !kept_words
  }
;;

let iter t ~(f : _ @ local) =
  let regions = Array.of_list_rev t.regions in
  for r = 0 to Array.length regions - 1 do
    let region = regions.(r) in
    for i = 0 to region.Region.slots - 1 do
      let block = Region.block region i in
      match Header.kind (Header.load block) with
      | Free -> ()
      | Scanned | Bytes -> f block
    done
  done
;;

let sexp_of_t t =
  let region_sexp (region : Region.t) =
    let slots =
      List.init region.slots ~f:(fun i ->
        let block = Region.block region i in
        [%sexp
          (Addr.diff block (Chunk.base region.chunk) : int)
          , (Header.load block : Header.t)])
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
          ; bytes = (Chunk.bytes region.chunk : int)
          ; slots : Sexp.t list
          }]
  in
  [%sexp (List.rev_map t.regions ~f:region_sexp : Sexp.t list)]
;;

module For_testing = struct
  let locate t addr =
    List.find_map t.regions ~f:(fun (region : Region.t) ->
      if Chunk.contains region.chunk addr
      then Some (region.index, Addr.diff addr (Chunk.base region.chunk))
      else None)
  ;;

  let regions t = List.length t.regions
  let pages t = Page_table.length t.page_table
end
