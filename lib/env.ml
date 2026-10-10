open! Core

module Level = struct
  type t =
    | Any
    | Level of int
  [@@deriving sexp, compare, equal]
end

module Entry = struct
  type t =
    { scheme : Type.ty
    ; level : Level.t
    }
  [@@deriving sexp, compare, equal]
end

type t = { mutable entries : Entry.t String.Map.t }
[@@deriving sexp, compare, equal]
