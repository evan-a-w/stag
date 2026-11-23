open! Import
include Axis_intf

module Make_chain (T : sig
    type t [@@deriving sexp, compare, equal, enumerate]
  end) =
struct
  open T

  let index t = List.findi_exn all ~f:(fun _ t' -> equal t t') |> fst
  let of_index_exn i = List.nth_exn all i
  let min = List.hd_exn all
  let max = List.last_exn all
  let le a b = compare a b <= 0
  let join a b = if le a b then b else a
  let meet a b = if le a b then a else b
end

module Stage = struct
  type t =
    | Const
    | Portable
    | Nonportable
  [@@deriving sexp, compare, equal, enumerate]

  include functor Make_chain
end

module Provenance = struct
  type t =
    | Static
    | Gc
    | Unknown
  [@@deriving sexp, compare, equal, enumerate]

  include functor Make_chain
end

module Locality = struct
  type t =
    | Global
    | Regional
    | Local
  [@@deriving sexp, compare, equal, enumerate]

  include functor Make_chain
end

type 'a t =
  | Stage : Stage.t t
  | Provenance : Provenance.t t
  | Locality : Locality.t t

let chain (type a) (t : a t) : (module Chain with type t = a) =
  match t with
  | Stage -> (module Stage)
  | Provenance -> (module Provenance)
  | Locality -> (module Locality)
;;

module Any = struct
  type 'a axis = 'a t
  type t = T : 'a axis -> t

  let all = [ T Stage; T Provenance; T Locality ]

  let index (T axis) =
    match axis with
    | Stage -> 0
    | Provenance -> 1
    | Locality -> 2
  ;;

  let equal a b = index a = index b

  let sexp_of_t (T axis) =
    match axis with
    | Stage -> Sexp.Atom "stage"
    | Provenance -> Atom "provenance"
    | Locality -> Atom "locality"
  ;;

  let size (T axis) =
    let module C = (val chain axis) in
    List.length C.all
  ;;

  let sexp_of_point (T axis) i =
    let module C = (val chain axis) in
    C.sexp_of_t (C.of_index_exn i)
  ;;
end
