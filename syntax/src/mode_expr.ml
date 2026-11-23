open! Import

module Axis = struct
  type t =
    | Stage
    | Provenance
    | Locality
  [@@deriving sexp_of, compare, equal]

  let to_string = function
    | Stage -> "stage"
    | Provenance -> "provenance"
    | Locality -> "locality"
  ;;
end

module Atom = struct
  type t =
    | Const
    | Portable
    | Nonportable
    | Static
    | Gc
    | Unknown
    | Global
    | Regional
    | Local
  [@@deriving sexp, compare, equal, enumerate]

  let axis : t -> Axis.t = function
    | Const | Portable | Nonportable -> Stage
    | Static | Gc | Unknown -> Provenance
    | Global | Regional | Local -> Locality
  ;;

  let to_string t = String.lowercase (Sexp.to_string (sexp_of_t t))

  let of_string =
    let table =
      String.Table.of_alist_exn (List.map all ~f:(fun t -> to_string t, t))
    in
    Hashtbl.find table
  ;;
end

type desc =
  | Var of string
  | Atoms of Atom.t list
[@@deriving sexp_of, compare, equal]

type t = desc Located.t [@@deriving sexp_of, compare, equal]

let to_string (t : t) =
  match t.txt with
  | Var name -> "'" ^ name
  | Atoms atoms -> String.concat ~sep:" " (List.map atoms ~f:Atom.to_string)
;;

module Constraint = struct
  type mode = t [@@deriving sexp_of, compare, equal]

  type t =
    { lower : mode
    ; upper : mode
    }
  [@@deriving sexp_of, compare, equal]

  let to_string { lower; upper } = to_string lower ^ " <= " ^ to_string upper
end
