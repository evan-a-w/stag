open! Import

(** Ids of mode variables. Ids appear in sexps, so depend only on what was done
    with the [t]. *)
type t

val create : unit -> t
val next : t -> int
