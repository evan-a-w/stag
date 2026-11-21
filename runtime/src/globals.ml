open! Import
module Addr = Memory.Addr

let[@cold] fail sexp = raise (Error.to_exn (Error.create_s sexp))

type t =
  { mutable slots : Addr.t array
  ; mutable count : int
  ; index : int Int.Table.t (** By address, as a word. *)
  }

let create () =
  { slots = Array.create ~len:4 (Addr.null ())
  ; count = 0
  ; index = Int.Table.create ()
  }
;;

let count t = t.count
let key addr = Nativeint_u.to_int (Addr.to_word addr)
let mem t addr = Hashtbl.mem t.index (key addr)

let add t addr =
  if mem t addr then fail [%message "Globals.add: already added"];
  if t.count = Array.length t.slots
  then (
    let slots = Array.create ~len:(2 * t.count) (Addr.null ()) in
    for i = 0 to t.count - 1 do
      slots.(i) <- t.slots.(i)
    done;
    t.slots <- slots);
  t.slots.(t.count) <- addr;
  Hashtbl.set t.index ~key:(key addr) ~data:t.count;
  t.count <- t.count + 1
;;

let remove t addr =
  match Hashtbl.find t.index (key addr) with
  | None -> fail [%message "Globals.remove: not added"]
  | Some i ->
    Hashtbl.remove t.index (key addr);
    t.count <- t.count - 1;
    if i < t.count
    then (
      let last = t.slots.(t.count) in
      t.slots.(i) <- last;
      Hashtbl.set t.index ~key:(key last) ~data:i);
    t.slots.(t.count) <- Addr.null ()
;;

module Source = struct
  type nonrec t = t

  let name = "globals"
  let holds_locals = false

  let[@zero_alloc] iter t ~(f : _ @ local) =
    for i = 0 to t.count - 1 do
      (f [@zero_alloc assume]) t.slots.(i)
    done
  ;;
end

let root_source t = Root_source.create (module Source) t
