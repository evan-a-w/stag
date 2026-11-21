open! Import
module Addr = Memory.Addr

(* A user-defined root source: an OCaml array of the addresses of root words,
   which may be anywhere (here, C memory). *)
type t =
  { mutable slots : Addr.t array
  ; mutable count : int
  }

let create () = { slots = Array.create ~len:4 (Addr.null ()); count = 0 }

let add t slot =
  if t.count = Array.length t.slots
  then (
    let slots = Array.create ~len:(2 * t.count) (Addr.null ()) in
    for i = 0 to t.count - 1 do
      slots.(i) <- t.slots.(i)
    done;
    t.slots <- slots);
  t.slots.(t.count) <- slot;
  t.count <- t.count + 1
;;

let clear t = t.count <- 0

module Source = struct
  type nonrec t = t

  let name = "table"
  let holds_locals = false

  let[@zero_alloc] iter t ~(f : _ @ local) =
    for i = 0 to t.count - 1 do
      (f [@zero_alloc assume]) t.slots.(i)
    done
  ;;
end

let root_source t = Root_source.create (module Source) t
