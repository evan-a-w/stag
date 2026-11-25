open! Import

module Kind = struct
  type t =
    | Type
    | Constructor
    | Field
  [@@deriving sexp_of]
end

type t =
  { kind : Kind.t
  ; name : string
  ; declaration : Loc.t
  }
[@@deriving sexp_of]
