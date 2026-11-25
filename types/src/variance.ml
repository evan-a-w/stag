open! Import

type t =
  | Bivariant
  | Covariant
  | Contravariant
  | Invariant
[@@deriving compare, equal, sexp_of]

let compose ~outer inner =
  match outer, inner with
  | Bivariant, _ | _, Bivariant -> Bivariant
  | Invariant, _ | _, Invariant -> Invariant
  | Covariant, v -> v
  | Contravariant, Covariant -> Contravariant
  | Contravariant, Contravariant -> Covariant
;;

let join a b =
  match a, b with
  | Bivariant, v | v, Bivariant -> v
  | Covariant, Covariant -> Covariant
  | Contravariant, Contravariant -> Contravariant
  | Invariant, _
  | _, Invariant
  | Covariant, Contravariant
  | Contravariant, Covariant -> Invariant
;;

type of_con = string -> t list option
