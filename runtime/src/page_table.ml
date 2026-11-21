open! Import
module Addr = Memory.Addr

(* Entry [i] is [table.(2i)], [empty] or a page number, and [table.(2i + 1)],
   the page number of its owner: adjacent, so a lookup touches one cache line. *)
type t =
  { shift : int
  ; mutable bits : int
  ; mutable table : int array
  ; mutable length : int
  }

let empty = -1
let initial_bits = 4

(* The odd integer nearest 2^63 / phi, wrapped to 63 bits. *)
let golden = 0x4F1BBCDCBFA53E0B
let[@cold] fail sexp = raise (Error.to_exn (Error.create_s sexp))

let create ~page_bytes =
  if Int.(page_bytes < 8 || page_bytes land (page_bytes - 1) <> 0)
  then fail [%message "Page_table.create" (page_bytes : int)];
  { shift = Int.ctz page_bytes
  ; bits = initial_bits
  ; table = Array.create ~len:(2 lsl initial_bits) empty
  ; length = 0
  }
;;

let page_bytes t = 1 lsl t.shift
let length t = t.length
let capacity t = Array.length t.table / 2

let[@inline] [@zero_alloc] page_number t addr =
  Nativeint_u.to_int
    (Nativeint_u.shift_right_logical (Addr.to_word addr) t.shift)
;;

let[@inline] [@zero_alloc] home ~bits key =
  (key * golden) lsr (Int.num_bits - bits)
;;

(* The entry of [key], or the empty one it would go in. *)
let[@inline] [@zero_alloc] entry table ~bits key =
  let mask = (1 lsl bits) - 1 in
  let mutable i = home ~bits key in
  while
    let k = Array.unsafe_get table (2 * i) in
    Int.(k <> key && k <> empty)
  do
    i <- (i + 1) land mask
  done;
  i
;;

let[@zero_alloc] find t addr =
  let table = t.table in
  let key = page_number t addr in
  let i = entry table ~bits:t.bits key in
  if Int.(Array.unsafe_get table (2 * i) = key)
  then
    Addr.of_word
      (Nativeint_u.shift_left
         (Nativeint_u.of_int (Array.unsafe_get table ((2 * i) + 1)))
         t.shift)
  else Addr.null ()
;;

let insert t key owner =
  let i = entry t.table ~bits:t.bits key in
  Array.unsafe_set t.table (2 * i) key;
  Array.unsafe_set t.table ((2 * i) + 1) owner;
  t.length <- t.length + 1
;;

let grow t =
  let table = t.table in
  t.bits <- t.bits + 1;
  t.table <- Array.create ~len:(2 lsl t.bits) empty;
  t.length <- 0;
  for i = 0 to (Array.length table / 2) - 1 do
    let key = Array.unsafe_get table (2 * i) in
    if Int.(key <> empty)
    then insert t key (Array.unsafe_get table ((2 * i) + 1))
  done
;;

let check_aligned t page ~name =
  if not (Addr.is_aligned page (page_bytes t))
  then fail [%message "Page_table: unaligned" name]
;;

let add t ~page ~owner =
  check_aligned t page ~name:"page";
  check_aligned t owner ~name:"owner";
  let key = page_number t page in
  if Int.(Array.unsafe_get t.table (2 * entry t.table ~bits:t.bits key) = key)
  then fail [%message "Page_table.add: page already has an owner"];
  if Int.(2 * (t.length + 1) > capacity t) then grow t;
  insert t key (page_number t owner)
;;

(* Backward-shift deletion: each later entry of the cluster that may live in the
   hole moves into it, so lookups never cross a gap. *)
let remove t ~page =
  check_aligned t page ~name:"page";
  let table = t.table in
  let key = page_number t page in
  let mask = capacity t - 1 in
  let mutable hole = entry table ~bits:t.bits key in
  if Int.(Array.unsafe_get table (2 * hole) <> key)
  then fail [%message "Page_table.remove: page has no owner"];
  let mutable j = (hole + 1) land mask in
  while Int.(Array.unsafe_get table (2 * j) <> empty) do
    let home = home ~bits:t.bits (Array.unsafe_get table (2 * j)) in
    (* Whether [home] is cyclically in [(hole, j]]: then the entry stays. *)
    let stays =
      if Int.(hole <= j)
      then Int.(hole < home && home <= j)
      else Int.(hole < home || home <= j)
    in
    if not stays
    then (
      Array.unsafe_set table (2 * hole) (Array.unsafe_get table (2 * j));
      Array.unsafe_set
        table
        ((2 * hole) + 1)
        (Array.unsafe_get table ((2 * j) + 1));
      hole <- j);
    j <- (j + 1) land mask
  done;
  Array.unsafe_set table (2 * hole) empty;
  t.length <- t.length - 1
;;

module For_testing = struct
  let home t ~page = home ~bits:t.bits (page_number t page)

  let slot t ~page =
    let key = page_number t page in
    let i = entry t.table ~bits:t.bits key in
    if Int.(t.table.(2 * i) = key) then Some i else None
  ;;
end
