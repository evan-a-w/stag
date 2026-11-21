open! Import
module Addr = Memory.Addr

type t =
  { shift : int
  ; mutable bits : int
  ; mutable keys : int array (** [empty] or a page number. *)
  ; mutable owners : Addr.t array
  ; mutable length : int
  }

let empty = -1
let initial_bits = 4

(* The odd integer nearest 2^63 / phi, in 63 bits. *)
let golden = 0x1E3779B97F4A7C15
let[@cold] fail sexp = raise (Error.to_exn (Error.create_s sexp))

let create ~page_bytes =
  if Int.(page_bytes < 8 || page_bytes land (page_bytes - 1) <> 0)
  then fail [%message "Page_table.create" (page_bytes : int)];
  { shift = Int.ctz page_bytes
  ; bits = initial_bits
  ; keys = Array.create ~len:(1 lsl initial_bits) empty
  ; owners = Array.create ~len:(1 lsl initial_bits) (Addr.null ())
  ; length = 0
  }
;;

let page_bytes t = 1 lsl t.shift
let length t = t.length
let capacity t = Array.length t.keys

let[@inline] [@zero_alloc] key t addr =
  Nativeint_u.to_int
    (Nativeint_u.shift_right_logical (Addr.to_word addr) t.shift)
;;

let[@inline] [@zero_alloc] home ~bits key =
  (key * golden) lsr (Int.num_bits - bits)
;;

(* The slot of [key], or the empty slot it would go in. *)
let[@inline] [@zero_alloc] slot t key =
  let mask = Array.length t.keys - 1 in
  let mutable i = home ~bits:t.bits key in
  while
    let k = Array.unsafe_get t.keys i in
    Int.(k <> key && k <> empty)
  do
    i <- (i + 1) land mask
  done;
  i
;;

let[@zero_alloc] find t addr =
  let key = key t addr in
  let i = slot t key in
  if Int.(Array.unsafe_get t.keys i = key)
  then Array.unsafe_get t.owners i
  else Addr.null ()
;;

let insert t key owner =
  let i = slot t key in
  Array.unsafe_set t.keys i key;
  Array.unsafe_set t.owners i owner;
  t.length <- t.length + 1
;;

let grow t =
  let keys = t.keys in
  let owners = t.owners in
  t.bits <- t.bits + 1;
  t.keys <- Array.create ~len:(1 lsl t.bits) empty;
  t.owners <- Array.create ~len:(1 lsl t.bits) (Addr.null ());
  t.length <- 0;
  for i = 0 to Array.length keys - 1 do
    let key = Array.unsafe_get keys i in
    if Int.(key <> empty) then insert t key (Array.unsafe_get owners i)
  done
;;

let check_aligned t page ~name =
  if not (Addr.is_aligned page (page_bytes t))
  then fail [%message "Page_table: unaligned page" name]
;;

let add t ~page ~owner =
  check_aligned t page ~name:"add";
  let key = key t page in
  if Int.(Array.unsafe_get t.keys (slot t key) = key)
  then fail [%message "Page_table.add: page already has an owner"];
  if Int.(2 * (t.length + 1) > Array.length t.keys) then grow t;
  insert t key owner
;;

(* Backward-shift deletion: each later entry of the cluster that may live in the
   hole moves into it, so lookups never cross a gap. *)
let remove t ~page =
  check_aligned t page ~name:"remove";
  let key = key t page in
  let mask = Array.length t.keys - 1 in
  let mutable hole = slot t key in
  if Int.(Array.unsafe_get t.keys hole <> key)
  then fail [%message "Page_table.remove: page has no owner"];
  let mutable j = (hole + 1) land mask in
  while Int.(Array.unsafe_get t.keys j <> empty) do
    let home = home ~bits:t.bits (Array.unsafe_get t.keys j) in
    (* Whether [home] is cyclically in [(hole, j]]: then the entry stays. *)
    let stays =
      if Int.(hole <= j)
      then Int.(hole < home && home <= j)
      else Int.(hole < home || home <= j)
    in
    if not stays
    then (
      Array.unsafe_set t.keys hole (Array.unsafe_get t.keys j);
      Array.unsafe_set t.owners hole (Array.unsafe_get t.owners j);
      hole <- j);
    j <- (j + 1) land mask
  done;
  Array.unsafe_set t.keys hole empty;
  Array.unsafe_set t.owners hole (Addr.null ());
  t.length <- t.length - 1
;;

module For_testing = struct
  let home t ~page = home ~bits:t.bits (key t page)

  let slot t ~page =
    let key = key t page in
    let i = slot t key in
    if Int.(t.keys.(i) = key) then Some i else None
  ;;
end
