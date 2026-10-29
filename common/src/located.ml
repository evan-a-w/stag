open! Import

type 'a t =
  { txt : 'a
  ; loc : Loc.t
  }
[@@deriving rust]

let compare compare_txt a b = compare_txt a.txt b.txt
let equal equal_txt a b = equal_txt a.txt b.txt

let sexp_of_t sexp_of_txt { txt; loc } =
  if Loc.is_none loc
  then sexp_of_txt txt
  else Sexp.List [ sexp_of_txt txt; Atom "@"; Loc.sexp_of_t loc ]
;;

let create ?(loc = Loc.none) txt = { txt; loc }
