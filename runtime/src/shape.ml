open! Import

module Scalar = struct
  type t =
    | I8
    | I16
    | I32
    | I64
    | F32
    | F64
    | Code
  [@@deriving sexp_of, equal]

  let bytes = function
    | I8 -> 1
    | I16 -> 2
    | I32 | F32 -> 4
    | I64 | F64 | Code -> 8
  ;;
end

module Repr = struct
  type t =
    | Default
    | C
  [@@deriving sexp_of, equal]
end

type t =
  | Scalar of
      { scalar : Scalar.t
      ; valid : (int * int) option
      }
  | Ptr
  | Struct of
      { repr : Repr.t
      ; fields : t list
      }
  | Union of t list
  | Array of
      { elt : t
      ; length : int
      }
[@@deriving sexp_of]
