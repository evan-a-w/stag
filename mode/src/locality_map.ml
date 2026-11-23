open! Import

type t = Morphism.t option [@@deriving compare]

let sexp_of_t = function
  | None -> Sexp.Atom "ignore"
  | Some morphism -> Morphism.sexp_of_t morphism
;;

let id = Some (Morphism.identity (T Locality))
let borrow = Some Morphism.borrow
let release = Some Morphism.release
let ignore = None

let compose m1 m2 =
  match m1, m2 with
  | None, _ | _, None -> None
  | Some m1, Some m2 -> Some (Morphism.compose m1 m2)
;;

let apply t point =
  match t with
  | None -> Axis.Locality.max
  | Some morphism ->
    Axis.Locality.of_index_exn
      (Morphism.right morphism (Axis.Locality.index point))
;;

let morphism = Fn.id
