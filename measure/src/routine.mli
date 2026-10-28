open! Import

(** What is measured: an iteration is one call of [run]. *)
type t =
  | Plain of (unit -> unit Or_error.t)
  | Batched :
      { setup : unit -> 'input Or_error.t
      ; run : 'input -> unit Or_error.t
      }
      -> t
  (** [setup] makes the input of each iteration, outside the timing, eg. for
      a routine that consumes its input. Inputs are made in chunks, each
      just before it is timed, see [Measurement]. *)
