open! Import

type t =
  { schemes : (string * Type.ty) list
  ; mutable uses : Quoted_use.t list
  }

let sexp_of_t { uses; _ } = [%sexp (uses : Quoted_use.t list)]
