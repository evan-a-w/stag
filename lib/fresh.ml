open! Core

type t = { mutable next_id : int }

let create () = { next_id = 0 }

let id t =
  let id = t.next_id in
  t.next_id <- id + 1;
  id
;;

let var t ~rank = Type.Var.create (id t) (Unbound rank)
let record t row = Type.TRecord { id = id t; row }
let variant t row = Type.TVariant { id = id t; row }
