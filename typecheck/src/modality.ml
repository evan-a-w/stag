open! Import

type t =
  | Id
  | Global
[@@deriving sexp_of, equal]

let of_global global = if global then Global else Id

let compose outer inner =
  match outer with
  | Id -> inner
  | Global -> Global
;;
