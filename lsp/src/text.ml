open! Import

type t =
  { contents : string
  ; line_starts : int array
  }

let create contents =
  let starts = Queue.of_list [ 0 ] in
  String.iteri contents ~f:(fun i c ->
    if Char.equal c '\n' then Queue.enqueue starts (i + 1));
  { contents; line_starts = Queue.to_array starts }
;;

let contents t = t.contents

(* Bytes and UTF-16 code units of the character at [offset]. *)
let char_at t offset =
  let decode = Stdlib.String.get_utf_8_uchar t.contents offset in
  let bytes = Stdlib.Uchar.utf_decode_length decode in
  if Stdlib.Uchar.utf_decode_is_valid decode
  then bytes, Stdlib.Uchar.utf_16_byte_length (Stdlib.Uchar.utf_decode_uchar decode) / 2
  else bytes, bytes
;;

(* Excluding its line terminator. *)
let line_stop t line =
  if line + 1 < Array.length t.line_starts
  then (
    let stop = t.line_starts.(line + 1) - 1 in
    if stop > t.line_starts.(line) && Char.equal t.contents.[stop - 1] '\r'
    then stop - 1
    else stop)
  else String.length t.contents
;;

let offset_of_position t ({ line; character } : Lsp_types.Position.t) =
  if line < 0
  then 0
  else if line >= Array.length t.line_starts
  then String.length t.contents
  else (
    let stop = line_stop t line in
    let rec walk offset units =
      if offset >= stop
      then stop
      else (
        let bytes, width = char_at t offset in
        if units + width > character then offset else walk (offset + bytes) (units + width))
    in
    walk t.line_starts.(line) 0)
;;

let utf16_length t ~pos ~len =
  let stop = pos + len in
  let rec walk offset units =
    if offset >= stop
    then units
    else (
      let bytes, width = char_at t offset in
      walk (offset + bytes) (units + width))
  in
  walk pos 0
;;

let position_of_offset t offset =
  let offset = Int.clamp_exn offset ~min:0 ~max:(String.length t.contents) in
  let line =
    match
      Array.binary_search
        t.line_starts
        ~compare:Int.compare
        `Last_less_than_or_equal_to
        offset
    with
    | Some line -> line
    | None -> 0
  in
  let start = t.line_starts.(line) in
  (* Units of the characters that start before [offset]. *)
  let rec walk pos units =
    if pos >= offset
    then units
    else (
      let bytes, width = char_at t pos in
      if pos + bytes > offset then units else walk (pos + bytes) (units + width))
  in
  Lsp_types.Position.create ~line ~character:(walk start 0)
;;

let range_of_offsets t ~start ~stop =
  Lsp_types.Range.create
    ~start:(position_of_offset t start)
    ~end_:(position_of_offset t stop)
;;

let range_of_loc t (loc : Loc.t) =
  range_of_offsets t ~start:loc.start.offset ~stop:loc.stop.offset
;;

let full_range t = range_of_offsets t ~start:0 ~stop:(String.length t.contents)

let apply_change
      t
      ({ range; rangeLength = _; text } : Lsp_types.TextDocumentContentChangeEvent.t)
  =
  match range with
  | None -> create text
  | Some { start; end_ } ->
    let start = offset_of_position t start in
    let stop = Int.max start (offset_of_position t end_) in
    create
      (String.prefix t.contents start
       ^ text
       ^ String.drop_prefix t.contents stop)
;;
