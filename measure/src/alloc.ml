open! Import

type t =
  { minor_words : float
  ; promoted_words : float
  ; major_words : float
  ; minor_collections : float
  ; major_collections : float
  }
[@@deriving sexp]

let zero =
  { minor_words = 0.
  ; promoted_words = 0.
  ; major_words = 0.
  ; minor_collections = 0.
  ; major_collections = 0.
  }
;;

let read () =
  let s = Gc.quick_stat () in
  { minor_words = Stdlib.Gc.minor_words ()
  ; promoted_words = Gc.Stat.promoted_words s
  ; major_words = Gc.Stat.major_words s
  ; minor_collections = Float.of_int (Gc.Stat.minor_collections s)
  ; major_collections = Float.of_int (Gc.Stat.major_collections s)
  }
;;

let map2 a b ~f =
  { minor_words = f a.minor_words b.minor_words
  ; promoted_words = f a.promoted_words b.promoted_words
  ; major_words = f a.major_words b.major_words
  ; minor_collections = f a.minor_collections b.minor_collections
  ; major_collections = f a.major_collections b.major_collections
  }
;;

let diff after before = map2 after before ~f:( -. )
let add = map2 ~f:( +. )
let div t ~by = map2 t t ~f:(fun x _ -> x /. by)
