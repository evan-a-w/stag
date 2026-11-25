open! Import

type t =
  { var : int
  ; axis : Mode.Axis.Any.t
  }

let sexp_of_t { var; axis } = [%sexp (var : int), (axis : Mode.Axis.Any.t)]
let key t = t.var, Mode.Axis.Any.index t.axis
let compare a b = [%compare: int * int] (key a) (key b)
let hash_fold_t state t = [%hash_fold: int * int] state (key t)
let hash t = [%hash: int * int] (key t)

include functor Hashable.Make_plain

let equal a b = compare a b = 0
let of_var var axis = { var = Mode.Var.id var; axis }
let max axis = Mode.Axis.Any.size axis - 1

let axis_name : Mode.Axis.Any.t -> string = function
  | T Stage -> "stage"
  | T Provenance -> "provenance"
  | T Locality -> "locality"
;;

let atom (T axis : Mode.Axis.Any.t) index =
  let module C = (val Mode.Axis.chain axis) in
  Sexp.to_string (C.sexp_of_t (C.of_index_exn index)) |> String.lowercase
;;
