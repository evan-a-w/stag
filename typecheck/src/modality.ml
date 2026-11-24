open! Import

type t =
  { global : bool
  ; held : Mode_arg.t list
  }
[@@deriving sexp_of]

let id = { global = false; held = [] }
let of_global global = { id with global }
let of_cell held = { global = true; held = [ held ] }

let compose outer inner =
  { global = outer.global || inner.global; held = outer.held @ inner.held }
;;
