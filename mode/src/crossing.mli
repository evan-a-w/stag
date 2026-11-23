open! Import

(** Which axes the values of a type cross: on those, a value may be used at any
    mode. On stage, crossing is to [portable] only, never to [const]. *)
type t =
  { stage : bool
  ; provenance : bool
  ; locality : bool
  }
[@@deriving sexp_of]

val none : t

(** Plain data such as [int]: crosses every axis. *)
val immediate : t
