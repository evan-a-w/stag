open! Import

module Placement = struct
  type t =
    | Own_line of
        { blank_before : bool
        ; blank_after : bool
        }
    | Trailing
    | Inline
  [@@deriving sexp_of, equal]
end

type t =
  { text : string
  ; loc : Loc.t
  ; placement : Placement.t
  ; next_token : int
  }
[@@deriving sexp_of]
