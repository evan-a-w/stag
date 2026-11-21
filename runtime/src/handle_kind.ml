open! Import

type 'a t =
  { id : 'a Type_equal.Id.t
  ; refs : 'a -> Memory.Addr.t array
  }

let create ~name sexp_of ~refs =
  { id = Type_equal.Id.create ~name sexp_of; refs }
;;

let name t = Type_equal.Id.name t.id
let refs t value = t.refs value
let same_witness t t' = Type_equal.Id.same_witness t.id t'.id
