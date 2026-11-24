open! Import

type t =
  { global : bool
  ; external_ : bool
  }
[@@deriving sexp_of, equal]

let id = { global = false; external_ = false }
let of_global global = { id with global }

let of_pointee : Type.Pointee.t -> t = function
  | Gc -> { global = true; external_ = false }
  | External -> { global = true; external_ = true }
;;

let compose outer inner =
  { global = outer.global || inner.global
  ; external_ = outer.external_ || inner.external_
  }
;;
