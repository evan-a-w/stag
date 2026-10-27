open! Core

(** An error raised by the program being evaluated, eg. division by zero, at
    the application that raised it. *)
type t =
  { loc : Loc.t
  ; message : string
  }
[@@deriving sexp_of]

(** Sexps as [(message @ loc)]. *)
val to_error : t -> Error.t

(** The [t] an error was made from by [to_error], if any. *)
val of_error : Error.t -> t option
