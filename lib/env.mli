open! Core

module Level : sig
  type t =
    | Any
    | Level of int
  [@@deriving sexp, compare, equal]
end

module Entry : sig
  type t =
    { scheme : Type.ty
    ; level : Level.t
    }
  [@@deriving sexp, compare, equal]
end

type t = { mutable entries : Entry.t String.Map.t }
[@@deriving sexp, compare, equal]
