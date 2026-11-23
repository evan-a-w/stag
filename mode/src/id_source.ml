open! Import

type t = { mutable next : int }

let create () = { next = 0 }

let next t =
  let id = t.next in
  t.next <- id + 1;
  id
;;
