open! Import

type t =
  { stage : Axis.Stage.t
  ; provenance : Axis.Provenance.t
  ; locality : Axis.Locality.t
  }
[@@deriving sexp, compare, equal]

let min =
  { stage = Axis.Stage.min
  ; provenance = Axis.Provenance.min
  ; locality = Axis.Locality.min
  }
;;

let max =
  { stage = Axis.Stage.max
  ; provenance = Axis.Provenance.max
  ; locality = Axis.Locality.max
  }
;;

let le a b =
  Axis.Stage.le a.stage b.stage
  && Axis.Provenance.le a.provenance b.provenance
  && Axis.Locality.le a.locality b.locality
;;

let join a b =
  { stage = Axis.Stage.join a.stage b.stage
  ; provenance = Axis.Provenance.join a.provenance b.provenance
  ; locality = Axis.Locality.join a.locality b.locality
  }
;;

let meet a b =
  { stage = Axis.Stage.meet a.stage b.stage
  ; provenance = Axis.Provenance.meet a.provenance b.provenance
  ; locality = Axis.Locality.meet a.locality b.locality
  }
;;

let get (type a) t (axis : a Axis.t) : a =
  match axis with
  | Stage -> t.stage
  | Provenance -> t.provenance
  | Locality -> t.locality
;;
