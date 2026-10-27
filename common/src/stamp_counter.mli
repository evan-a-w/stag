open! Import

(** Hands out stamps for [Typed.Ident]s, unique among the stamps of one
    counter. *)
type t

val create : unit -> t
val next : t -> int
