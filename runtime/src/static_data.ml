open! Import
module Addr = Memory.Addr
module Chunk = Memory.Chunk

let[@cold] fail sexp = raise (Error.to_exn (Error.create_s sexp))
let segment_bytes = 64 * 1024

(* The headers are words 0 and 1, so the blocks are at 8 and 16. Never
   destroyed. *)
let atoms =
  let chunk = Chunk.create ~bytes:16 ~align:8 in
  Header.store
    (Addr.add (Chunk.base chunk) 8)
    (Header.scanned ~size_words:0 ~scalar_words:0 Immortal);
  Header.store
    (Addr.add (Chunk.base chunk) 16)
    (Header.bytes ~length:0 Immortal);
  chunk
;;

let[@zero_alloc] empty () = Addr.add (Chunk.base atoms) 8
let[@zero_alloc] empty_bytes () = Addr.add (Chunk.base atoms) 16

module Segment = struct
  type t =
    { chunk : Chunk.t
    ; mutable used : int (** Bytes, from the start of [chunk]. *)
    }

  let free t = Chunk.bytes t.chunk - t.used
end

type t =
  { mutable segments : Segment.t list (** Latest first. *)
  ; mutable destroyed : bool
  }

let create () = { segments = []; destroyed = false }

let check_live t ~name =
  if t.destroyed then fail [%message "Static_data: destroyed" (name : string)]
;;

let destroy t =
  check_live t ~name:"destroy";
  List.iter t.segments ~f:(fun (segment : Segment.t) ->
    Chunk.destroy segment.chunk);
  t.segments <- [];
  t.destroyed <- true
;;

(* [bytes] bytes, from a segment with room, or a new one: of its own if larger
   than a segment. *)
let take t ~bytes =
  let segment =
    match t.segments with
    | segment :: _ when Segment.free segment >= bytes -> segment
    | _ ->
      let segment =
        { Segment.chunk =
            Chunk.create ~bytes:(Int.max segment_bytes bytes) ~align:8
        ; used = 0
        }
      in
      t.segments <- segment :: t.segments;
      segment
  in
  let addr = Addr.add (Chunk.base segment.chunk) segment.used in
  segment.used <- segment.used + bytes;
  addr
;;

let alloc t header =
  check_live t ~name:"alloc";
  (match Header.kind header with
   | Free ->
     fail [%message "Static_data.alloc: a Free header" (header : Header.t)]
   | Scanned | Bytes -> ());
  let words = Header.size_words header in
  if words = 0
  then (
    match Header.kind header with
    | Bytes -> empty_bytes ()
    | Scanned | Free -> empty ())
  else (
    let block = Addr.add (take t ~bytes:((words + 1) * 8)) 8 in
    Header.store block (Header.with_color header Immortal);
    block)
;;

let contains t addr =
  List.exists t.segments ~f:(fun (segment : Segment.t) ->
    Chunk.contains segment.chunk addr
    && Addr.diff addr (Chunk.base segment.chunk) < segment.used)
;;

let iter_segment (segment : Segment.t) ~(f : _ @ local) =
  let base = Chunk.base segment.chunk in
  let mutable offset = 0 in
  while offset < segment.used do
    let block = Addr.add base (offset + 8) in
    f block;
    offset <- offset + ((Header.size_words (Header.load block) + 1) * 8)
  done
;;

let iter t ~(f : _ @ local) =
  let mutable segments = List.rev t.segments in
  while not (List.is_empty segments) do
    iter_segment (List.hd_exn segments) ~f;
    segments <- List.tl_exn segments
  done
;;

module Global = struct
  type t =
    { layout : Layout.t
    ; block : Addr.t
    ; mutable removed : bool
    }

  let layout t = t.layout
  let block t = t.block
  let scalars t = t.block

  let ptrs t =
    Addr.add t.block (round_up (Layout.scalar_bytes t.layout) ~align:8)
  ;;

  let iter_slots t ~(f : _ @ local) =
    let ptrs = ptrs t in
    for i = 0 to Layout.ptr_words t.layout - 1 do
      f (Addr.add ptrs (i * 8))
    done
  ;;
end

let add_global t roots layout =
  let bytes =
    round_up (Layout.scalar_bytes layout) ~align:8
    + (Layout.ptr_words layout * 8)
  in
  let block = alloc t (Header.bytes ~length:bytes Immortal) in
  let global = { Global.layout; block; removed = false } in
  Global.iter_slots global ~f:(fun slot -> Roots.add_global roots slot);
  global
;;

let remove_global roots (global : Global.t) =
  if global.removed then fail [%message "Static_data.remove_global: removed"];
  Global.iter_slots global ~f:(fun slot -> Roots.remove_global roots slot);
  global.removed <- true
;;

let sexp_of_t t =
  let segments =
    List.rev_map t.segments ~f:(fun segment ->
      let blocks = ref [] in
      let base = Chunk.base segment.chunk in
      iter_segment segment ~f:(fun block ->
        blocks
        := [%sexp (Addr.diff block base : int), (Header.load block : Header.t)]
           :: !blocks);
      [%sexp
        { bytes = (Chunk.bytes segment.chunk : int)
        ; used = (segment.used : int)
        ; blocks = (List.rev !blocks : Sexp.t list)
        }])
  in
  [%sexp (segments : Sexp.t list)]
;;
