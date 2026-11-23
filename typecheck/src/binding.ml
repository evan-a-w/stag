open! Import

type t = Mode.Var.t [@@deriving sexp_of]

let of_mode t = t
