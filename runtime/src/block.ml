open! Import

type t =
  { prefix : Layout.t
  ; element : Layout.Element.t option
  ; has_hidden_length : bool
  ; elements_offset : int
  }
[@@deriving sexp_of]

let fixed prefix =
  { prefix
  ; element = None
  ; has_hidden_length = false
  ; elements_offset = Layout.scalar_bytes prefix
  }
;;

let hidden_length_bytes = 8

(* Elements with pointers are counted by the pointer words, pointer-free ones by
   the exact length of [Bytes], or after a prefix with pointers by the scalar
   words: exactly if each element takes at least a word. *)
let variable ~prefix ~elt =
  let element : Layout.Element.t =
    { scalar_stride = Layout.scalar_bytes elt
    ; ptr_stride = Layout.ptr_words elt
    ; layout = elt
    }
  in
  let has_hidden_length =
    element.ptr_stride = 0
    && (element.scalar_stride = 0
        || (Layout.ptr_words prefix > 0 && element.scalar_stride < 8))
  in
  let prefix_offset = if has_hidden_length then hidden_length_bytes else 0 in
  { prefix
  ; element = Some element
  ; has_hidden_length
  ; elements_offset =
      round_up
        (prefix_offset + Layout.scalar_bytes prefix)
        ~align:(Layout.scalar_align elt)
  }
;;

let prefix t = t.prefix
let element t = t.element
let has_hidden_length t = t.has_hidden_length
let prefix_offset t = if t.has_hidden_length then hidden_length_bytes else 0
let elements_offset t = t.elements_offset

let pointer_free t =
  Layout.ptr_words t.prefix = 0
  &&
  match t.element with
  | None -> true
  | Some element -> element.ptr_stride = 0
;;

let header t ~length color =
  let scalar_stride, ptr_stride =
    match t.element with
    | Some { scalar_stride; ptr_stride; _ } -> scalar_stride, ptr_stride
    | None ->
      if length <> 0
      then raise_s [%message "Block.header: fixed" (length : int)];
      0, 0
  in
  if length < 0 then raise_s [%message "Block.header" (length : int)];
  let scalar_bytes = t.elements_offset + (length * scalar_stride) in
  if pointer_free t
  then Header.bytes ~length:scalar_bytes color
  else (
    let scalar_words = (scalar_bytes + 7) / 8 in
    Header.scanned
      ~size_words:
        (scalar_words + Layout.ptr_words t.prefix + (length * ptr_stride))
      ~scalar_words
      color)
;;

let length t header =
  let kind = Header.kind header in
  let expected : Header.Kind.t = if pointer_free t then Bytes else Scanned in
  if not (Header.Kind.equal kind expected)
  then
    raise_s
      [%message
        "Block.length: wrong kind"
          (kind : Header.Kind.t)
          (expected : Header.Kind.t)];
  match t.element with
  | None -> 0
  | Some _ when t.has_hidden_length ->
    raise_s [%message "Block.length: in the hidden length word"]
  | Some { scalar_stride; ptr_stride; _ } ->
    (match kind with
     | Bytes -> (Header.length header - t.elements_offset) / scalar_stride
     | Scanned | Free ->
       if ptr_stride > 0
       then
         (Header.size_words header
          - Header.scalar_words header
          - Layout.ptr_words t.prefix)
         / ptr_stride
       else
         ((8 * Header.scalar_words header) - t.elements_offset) / scalar_stride)
;;
