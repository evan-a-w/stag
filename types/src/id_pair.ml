open! Import

type t = int

let bits = 31
let max_id = (1 lsl bits) - 1

let[@cold] [@zero_alloc assume never_returns_normally] out_of_range id1 id2 =
  raise_s [%message "Id_pair.create: id out of range" (id1 : int) (id2 : int)]
;;

let[@zero_alloc] create id1 id2 =
  (* Negative ids have high bits set too. *)
  if Stdlib.( <> ) (id1 lor id2 land (max_id lxor -1)) 0
  then out_of_range id1 id2
  else if Stdlib.( <= ) id1 id2
  then (id1 lsl bits) lor id2
  else (id2 lsl bits) lor id1
;;

let ids t = t lsr bits, t land max_id
let sexp_of_t t = [%sexp (ids t : int * int)]

module Hash_set = struct
  type t = Int.Hash_set.t

  let create () = Int.Hash_set.create ()

  let mem_or_add t pair =
    match Hash_set.strict_add t pair with
    | Ok -> false
    | Duplicate -> true
  ;;
end
