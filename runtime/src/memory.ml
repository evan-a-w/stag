open! Import

module Addr = struct
  type t = Native_pointer.t

  let[@inline] [@zero_alloc] to_word t = Native_pointer.Expert.to_nativeint_u t
  let[@inline] [@zero_alloc] of_word w = Native_pointer.Expert.of_nativeint_u w
  let[@inline] [@zero_alloc] null () = of_word #0n
  let[@inline] [@zero_alloc] is_null t = Nativeint_u.equal (to_word t) #0n

  let[@inline] [@zero_alloc] add t n =
    Native_pointer.advance t ~bytes:(Nativeint_u.of_int n)
  ;;

  let[@inline] [@zero_alloc] diff a b =
    Nativeint_u.to_int (Nativeint_u.sub (to_word a) (to_word b))
  ;;

  let[@inline] [@zero_alloc] align_up t align =
    let mask = Nativeint_u.of_int (align - 1) in
    of_word
      (Nativeint_u.logand
         (Nativeint_u.add (to_word t) mask)
         (Nativeint_u.lognot mask))
  ;;

  let[@inline] [@zero_alloc] is_aligned t align =
    Nativeint_u.equal
      (Nativeint_u.logand (to_word t) (Nativeint_u.of_int (align - 1)))
      #0n
  ;;

  let[@inline] [@zero_alloc] equal a b = Native_pointer.equal a b

  let[@inline] [@zero_alloc] compare a b =
    Nativeint_u.unsigned_compare (to_word a) (to_word b)
  ;;

  let non_pointer_bound = 4096

  let[@inline] [@zero_alloc] is_non_pointer t =
    Int.( < ) (compare t (of_word (Nativeint_u.of_int non_pointer_bound))) 0
  ;;

  (* Whether [a - b] is in [[0, bytes)], as words, so without overflow. *)
  let[@inline] [@zero_alloc] within a ~from ~bytes =
    Int.( < )
      (Nativeint_u.unsigned_compare
         (Nativeint_u.sub (to_word a) (to_word from))
         (Nativeint_u.of_int bytes))
      0
  ;;
end

module Chunk = struct
  type t =
    { bigstring : Bigstring.t
    ; base : Addr.t
    ; mutable bytes : int
    ; id : int
    }

  (* Every chunk not yet destroyed, so none is freed by its finaliser while
     addresses into it are in use. *)
  let live : t Int.Table.t = Int.Table.create ()
  let next_id = ref 0

  let create ~bytes ~align =
    if bytes < 0
       || align < 1
       || align land (align - 1) <> 0
       || bytes > Int.max_value - align
    then raise_s [%message "Memory.Chunk.create" (bytes : int) (align : int)];
    let bigstring = Bigstring.create (bytes + align - 1) in
    let base =
      Addr.align_up (Native_pointer.unsafe_of_bigstring bigstring ~pos:0) align
    in
    Native_pointer.unsafe_memset base '\000' ~pos:0 ~len:bytes;
    let id = !next_id in
    incr next_id;
    let t = { bigstring; base; bytes; id } in
    Hashtbl.add_exn live ~key:id ~data:t;
    t
  ;;

  let[@inline] [@zero_alloc] base t = t.base
  let[@inline] [@zero_alloc] bytes t = t.bytes

  let[@inline] [@zero_alloc] contains t addr =
    Addr.within addr ~from:t.base ~bytes:t.bytes
  ;;

  let destroy t =
    if not (Hashtbl.mem live t.id)
    then raise_s [%message "Memory.Chunk.destroy: already destroyed"];
    Hashtbl.remove live t.id;
    Bigstring.unsafe_destroy t.bigstring;
    t.bytes <- 0
  ;;

  module For_testing = struct
    let live () = Hashtbl.length live
  end
end

let () =
  if Sys.big_endian
  then raise_s [%message "Memory: big-endian hosts are not supported"]
;;

let[@inline] [@zero_alloc] load_8 addr =
  Int8_u.of_int (Char.to_int (Native_pointer.load_untagged_char addr))
;;

let[@inline] [@zero_alloc] store_8 addr x =
  Native_pointer.store_untagged_char addr (Char.unsafe_of_int (Int8_u.to_int x))
;;

let[@inline] [@zero_alloc] load_16 addr =
  let lo = Char.to_int (Native_pointer.load_untagged_char addr) in
  let hi = Char.to_int (Native_pointer.load_untagged_char (Addr.add addr 1)) in
  Int16_u.of_int (lo lor (hi lsl 8))
;;

let[@inline] [@zero_alloc] store_16 addr x =
  let x = Int16_u.to_int x in
  Native_pointer.store_untagged_char addr (Char.unsafe_of_int (x land 0xff));
  Native_pointer.store_untagged_char
    (Addr.add addr 1)
    (Char.unsafe_of_int ((x lsr 8) land 0xff))
;;

let[@inline] [@zero_alloc] load_32 addr =
  Int32_u.of_int32 (Native_pointer.load_unboxed_int32 addr)
;;

let[@inline] [@zero_alloc] store_32 addr x =
  Native_pointer.store_unboxed_int32 addr (Int32_u.to_int32 x)
;;

let[@inline] [@zero_alloc] load_64 addr =
  Int64_u.of_int64 (Native_pointer.load_unboxed_int64 addr)
;;

let[@inline] [@zero_alloc] store_64 addr x =
  Native_pointer.store_unboxed_int64 addr (Int64_u.to_int64 x)
;;

let[@inline] [@zero_alloc] load_f32 addr = Float32_u.of_bits (load_32 addr)

let[@inline] [@zero_alloc] store_f32 addr x =
  store_32 addr (Float32_u.to_bits x)
;;

let[@inline] [@zero_alloc] load_f64 addr =
  Float_u.of_float (Int64_u.float_of_bits (load_64 addr))
;;

let[@inline] [@zero_alloc] store_f64 addr x =
  store_64 addr (Int64_u.bits_of_float (Float_u.to_float x))
;;

let[@inline] [@zero_alloc] load_addr addr =
  Addr.of_word
    (Nativeint_u.of_nativeint (Native_pointer.load_unboxed_nativeint addr))
;;

let[@inline] [@zero_alloc] store_addr addr x =
  Native_pointer.store_unboxed_nativeint
    addr
    (Nativeint_u.to_nativeint (Addr.to_word x))
;;

let[@inline] [@zero_alloc] fill_zero addr ~bytes =
  Native_pointer.unsafe_memset addr '\000' ~pos:0 ~len:bytes
;;

let[@inline] [@zero_alloc] copy ~src ~dst ~bytes =
  Native_pointer.unsafe_blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len:bytes
;;
