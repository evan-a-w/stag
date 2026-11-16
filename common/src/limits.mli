open! Import

(** Bounds on one phase of evaluation, eg. compile time or runtime. *)
type t =
  { fuel : int option
  (** Evaluation steps, including one per node of code built or instantiated. *)
  ; max_depth : int option
  (** Nesting of evaluation steps, and of code built or instantiated, below the
      depth at which the OCaml stack would overflow. *)
  ; time_limit : Time_ns.Span.t option
  (** Wall clock time, checked every few thousand steps. *)
  }

val unlimited : t
