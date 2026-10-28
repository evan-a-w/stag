open! Import

type t =
  | Plain of (unit -> unit Or_error.t)
  | Batched :
      { setup : unit -> 'input Or_error.t
      ; run : 'input -> unit Or_error.t
      }
      -> t
