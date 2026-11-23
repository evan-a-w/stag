open! Import

type t =
  { loc : Loc.t
  ; what : string
  }

let create ?(loc = Loc.none) what = { loc; what }

let sexp_of_t { loc; what } =
  if Loc.is_none loc
  then Sexp.Atom what
  else List [ Atom what; Loc.sexp_of_t loc ]
;;
