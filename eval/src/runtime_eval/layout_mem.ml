open! Import
module Addr = Memory.Addr

let[@cold] fail sexp = raise (Error.to_exn (Error.create_s sexp))
let[@inline] [@zero_alloc] round_up_word n = (n + 7) land lnot 7

let[@inline] [@zero_alloc] block_ptrs layout addr =
  Addr.add addr (round_up_word (Layout.scalar_bytes layout))
;;

let[@inline] [@zero_alloc] copy layout ~src_s ~src_p ~dst_s ~dst_p =
  let scalar_bytes = Layout.scalar_bytes layout in
  if Int.(scalar_bytes > 0)
  then Memory.copy ~src:src_s ~dst:dst_s ~bytes:scalar_bytes;
  let ptr_words = Layout.ptr_words layout in
  if Int.(ptr_words > 0)
  then Memory.copy ~src:src_p ~dst:dst_p ~bytes:(ptr_words * 8)
;;

let[@inline] [@zero_alloc] copy_to_block layout ~src_s ~src_p ~dst =
  copy layout ~src_s ~src_p ~dst_s:dst ~dst_p:(block_ptrs layout dst)
;;

let[@inline] [@zero_alloc] copy_from_block layout ~src ~dst_s ~dst_p =
  copy layout ~src_s:src ~src_p:(block_ptrs layout src) ~dst_s ~dst_p
;;

let[@inline] [@zero_alloc] zero_ptrs addr ~count =
  if Int.(count > 0) then Memory.fill_zero addr ~bytes:(count * 8)
;;

let[@inline] [@zero_alloc] load_scalar_int (scalar : Shape.Scalar.t) addr =
  match scalar with
  | I8 -> Int8_u.to_int (Memory.load_8 addr) land 0xff
  | I16 -> Int16_u.to_int (Memory.load_16 addr) land 0xffff
  | I32 -> Int32_u.to_int (Memory.load_32 addr) land 0xffff_ffff
  | I64 -> Int64_u.to_int (Memory.load_64 addr)
  | Code -> Nativeint_u.to_int (Addr.to_word (Memory.load_addr addr))
  | F32 | F64 ->
    fail
      [%message
        "Layout_mem.load_scalar_int: float scalar" (scalar : Shape.Scalar.t)]
;;

let[@inline] [@zero_alloc] store_scalar_int (scalar : Shape.Scalar.t) addr value
  =
  match scalar with
  | I8 -> Memory.store_8 addr (Int8_u.of_int value)
  | I16 -> Memory.store_16 addr (Int16_u.of_int value)
  | I32 -> Memory.store_32 addr (Int32_u.of_int value)
  | I64 -> Memory.store_64 addr (Int64_u.of_int value)
  | Code -> Memory.store_addr addr (Addr.of_word (Nativeint_u.of_int value))
  | F32 | F64 ->
    fail
      [%message
        "Layout_mem.store_scalar_int: float scalar" (scalar : Shape.Scalar.t)]
;;

let[@inline] [@zero_alloc] read_discriminant
  (disc : Layout.Discriminant.t)
  ~src_s
  ~src_p
  =
  match disc with
  | Uninhabited -> fail [%message "Layout_mem.read_discriminant: uninhabited"]
  | Single { case } -> case
  | Tag leaf ->
    (match leaf with
     | Scalar { scalar_offset; scalar } ->
       load_scalar_int scalar (Addr.add src_s scalar_offset)
     | Ptr { ptr_index } ->
       Nativeint_u.to_int
         (Addr.to_word (Memory.load_addr (Addr.add src_p (ptr_index * 8)))))
  | Niche { leaf; untagged; first; last; start } ->
    (match leaf with
     | Ptr { ptr_index } ->
       let word = Memory.load_addr (Addr.add src_p (ptr_index * 8)) in
       if not (Addr.is_non_pointer word)
       then untagged
       else (
         let v = Nativeint_u.to_int (Addr.to_word word) in
         let delta = v - start in
         if Int.(delta >= 0 && delta <= last - first)
         then (
           let c = first + delta in
           if Int.(c = untagged) then untagged else c)
         else untagged)
     | Scalar { scalar_offset; scalar } ->
       let v = load_scalar_int scalar (Addr.add src_s scalar_offset) in
       let delta =
         match scalar with
         | I8 -> (v - start) land 0xff
         | I16 -> (v - start) land 0xffff
         | I32 -> (v - start) land 0xffff_ffff
         | I64 | Code | F32 | F64 -> v - start
       in
       if Int.(delta >= 0 && delta <= last - first)
       then (
         let c = first + delta in
         if Int.(c = untagged) then untagged else c)
       else untagged)
;;

let[@inline] [@zero_alloc] write_discriminant
  ({ ptr_index; ptr_words; discriminant } : Layout.Write_plan.t)
  ~dst_s
  ~dst_p
  =
  if Int.(ptr_words > 0)
  then zero_ptrs (Addr.add dst_p (ptr_index * 8)) ~count:ptr_words;
  match discriminant with
  | None -> ()
  | Some (leaf, value) ->
    (match leaf with
     | Scalar { scalar_offset; scalar } ->
       store_scalar_int scalar (Addr.add dst_s scalar_offset) value
     | Ptr { ptr_index } ->
       Memory.store_addr
         (Addr.add dst_p (ptr_index * 8))
         (Addr.of_word (Nativeint_u.of_int value)))
;;

let read_bytes addr ~len =
  if len = 0
  then Bytes.create 0
  else (
    let buf = Bytes.create len in
    let mutable i = 0 in
    while i + 8 <= len do
      Bytes.unsafe_set_int64_le
        buf
        i
        (Int64_u.to_int64 (Memory.load_64 (Addr.add addr i)));
      i <- i + 8
    done;
    while i < len do
      Bytes.unsafe_set
        buf
        i
        (Char.unsafe_of_int
           (Int8_u.to_int (Memory.load_8 (Addr.add addr i)) land 0xff));
      i <- i + 1
    done;
    buf)
;;

let write_bytes buf ~dst =
  let len = Bytes.length buf in
  let mutable i = 0 in
  while i + 8 <= len do
    Memory.store_64
      (Addr.add dst i)
      (Int64_u.of_int64 (Bytes.unsafe_get_int64_le buf i));
    i <- i + 8
  done;
  while i < len do
    Memory.store_8
      (Addr.add dst i)
      (Int8_u.of_int (Char.to_int (Bytes.unsafe_get buf i)));
    i <- i + 1
  done
;;

let read_ptrs addr ~count =
  if count = 0
  then [||]
  else (
    let ptrs = Array.create ~len:count (Addr.null ()) in
    for i = 0 to count - 1 do
      ptrs.(i) <- Memory.load_addr (Addr.add addr (i * 8))
    done;
    ptrs)
;;

let[@zero_alloc] write_ptrs ptrs ~dst =
  for i = 0 to Array.length ptrs - 1 do
    Memory.store_addr (Addr.add dst (i * 8)) ptrs.(i)
  done
;;
