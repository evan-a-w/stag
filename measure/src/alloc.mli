open! Import

(** The GC's counters, or their differences: what a routine allocated on the
    heap (not on the stack). Floats, so that they can be averaged per iteration. *)
type t =
  { minor_words : float
  ; promoted_words : float
  ; major_words : float
  ; minor_collections : float
  ; major_collections : float
  }
[@@deriving sexp]

val zero : t

(** The counters of this process now, from [Gc.quick_stat], except the minor
    words, which it counts only up to the last minor collection in OCaml 5, from
    [Gc.minor_words]. *)
val read : unit -> t

(** [diff after before]. *)
val diff : t -> t -> t

val add : t -> t -> t

(** Each counter divided by [by]. *)
val div : t -> by:float -> t
