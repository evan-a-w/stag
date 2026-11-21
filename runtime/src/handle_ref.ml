open! Import

type t =
  { addr : Memory.Addr.t
  ; id : int
  }

let create ~addr ~id = { addr; id }
let addr t = t.addr
let id t = t.id
let sexp_of_t t = [%sexp { id = (t.id : int) }]
