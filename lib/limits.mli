open! Core

(** Bounds on one phase of evaluation, eg. compile time or runtime. *)
type t =
  { fuel : int option (** Evaluation steps. *)
  ; max_depth : int option
    (** Nesting of evaluation steps, below the depth at which the OCaml stack
        would overflow. *)
  ; time_limit : Time_ns.Span.t option
    (** Wall clock time, checked every few thousand steps. *)
  }

val unlimited : t
