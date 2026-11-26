open! Import

module Shown = struct
  type t =
    | Always
    | Once
    | If_alone
end

type t =
  { loc : Loc.t
  ; what : string
  ; shown : Shown.t
  }

let create ?(loc = Loc.none) ?(shown = Shown.Always) what = { loc; what; shown }
let layout ?loc what = create ?loc ~shown:Once what

let sexp_of_t { loc; what; shown = _ } =
  if Loc.is_none loc
  then Sexp.Atom what
  else List [ Atom what; Loc.sexp_of_t loc ]
;;
