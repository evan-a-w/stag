open! Core

(** An error to show a user, rendered as OCaml's compiler does. *)
type t =
  { loc : Loc.t (** [Loc.none] if it has none. *)
  ; message : string
  }
[@@deriving sexp_of]

val of_parse_error : Parser.Error.t -> t
val of_source_file_error : Source_file.Error.t -> t
val of_typecheck_error : Typecheck_error.t -> t
val of_driver_error : Driver.Error.t -> t

(** An error of evaluation, located if it is a [Runtime_error]. *)
val of_runtime_error : Error.t -> t

(** {v
File "a.stag", line 3, characters 4-9:
3 | let x = f y
        ^^^^^
Error: message
    v}

    The quoted source is that of [contents] of the file, if given and the span
    is in it. Spans of several lines quote them (elided in the middle when
    there are many) without underlining. *)
val to_string_hum : ?contents:(string -> string option) -> t -> string
