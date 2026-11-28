open! Import

type t =
  { scalar_bytes : int
  ; ptr_words : int
  }
[@@deriving sexp_of, equal]

let round_up n ~align =
  if align <= 1
  then n
  else (
    let rem = n % align in
    if rem = 0 then n else n + (align - rem))
;;

module Builder = struct
  type frame_layout = t

  type t =
    { mutable scalar_bytes : int
    ; mutable ptr_words : int
    }

  let create () = { scalar_bytes = 0; ptr_words = 0 }
  let ptr_words t = t.ptr_words

  let alloc t layout =
    let bytes = Layout.scalar_bytes layout in
    let ptrs = Layout.ptr_words layout in
    if bytes = 0 && ptrs = 0
    then { Slot.scalar_offset = 0; ptr_index = 0; layout }
    else (
      let scalar_offset =
        if bytes = 0
        then 0
        else (
          let offset =
            round_up t.scalar_bytes ~align:(Layout.scalar_align layout)
          in
          t.scalar_bytes <- offset + bytes;
          offset)
      in
      let ptr_index =
        if ptrs = 0
        then 0
        else (
          let index = t.ptr_words in
          t.ptr_words <- index + ptrs;
          index)
      in
      { Slot.scalar_offset; ptr_index; layout })
  ;;

  let finalize t : frame_layout =
    { scalar_bytes = round_up t.scalar_bytes ~align:8; ptr_words = t.ptr_words }
  ;;
end
