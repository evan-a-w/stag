open! Import

type t =
  { loc : Loc.t
  ; what : string
  ; layout : bool
  }

let create ?(loc = Loc.none) what = { loc; what; layout = false }
let layout ?(loc = Loc.none) what = { loc; what; layout = true }

let sexp_of_t { loc; what; layout = _ } =
  if Loc.is_none loc
  then Sexp.Atom what
  else List [ Atom what; Loc.sexp_of_t loc ]
;;
