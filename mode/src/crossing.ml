open! Import

type t =
  { stage : bool
  ; provenance : bool
  ; locality : bool
  }
[@@deriving sexp_of]

let none = { stage = false; provenance = false; locality = false }
let immediate = { stage = true; provenance = true; locality = true }
