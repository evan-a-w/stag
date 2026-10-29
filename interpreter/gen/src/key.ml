open! Import

module T = struct
  type t = string list [@@deriving sexp_of, compare, hash]
end

include T
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)

let to_string = String.concat ~sep:"."
