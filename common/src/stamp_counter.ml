open! Import

type t = { mutable next : int }

let create () = { next = 0 }

let next t =
  let stamp = t.next in
  t.next <- stamp + 1;
  stamp
;;
