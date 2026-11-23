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

let morphism = Fn.id
