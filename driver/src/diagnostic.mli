open! Import

(** An error to show a user, rendered as OCaml's compiler does. *)

(** A secondary location, eg. where an error that made this one was raised. *)
module Note : sig
  type t =
    { loc : Loc.t
    ; message : string
    }
  [@@deriving sexp_of]
end

type t =
  { loc : Loc.t (** [Loc.none] if it has none. *)
  ; whole_file : bool
    (** About [loc.file] as a whole, eg. its name, so shown without a line or
        source. *)
  ; message : string
  ; notes : Note.t list
  }
[@@deriving sexp_of]

val create : ?notes:Note.t list -> ?whole_file:bool -> loc:Loc.t -> string -> t

(** With each location [f loc], eg. to name files differently. *)
val relocate : t -> f:(Loc.t -> Loc.t) -> t

val of_parse_error : Parser.Error.t -> t
val of_source_file_error : Source_file.Error.t -> t
val of_typecheck_error : Typecheck_error.t -> t
val of_driver_error : Driver.Error.t -> t

(** An error of evaluation, located if it is a [Runtime_error]. *)
val of_runtime_error : Error.t -> t

(** Why the test failed other than by mismatches, which [corrected] corrects:
    its runtime error, with the output since the last [[%expect]], and each
    [[%expect]] reached with different outputs. *)
val of_test_result : Expect_test_runner.Test_result.t -> t list

(** {v
File "a.stag", line 3, characters 4-9:
3 | let x = f y
        ^^^^^
Error: message
    v}

    Then each note likewise, with [Note:] for [Error:]. The quoted source is
    that of [contents] of the file, if given and the span is in it. Spans of
    several lines quote them (elided in the middle when there are many)
    without underlining. With [warning], [Warning:] for [Error:]. *)
val to_string_hum
  :  ?contents:(string -> string option)
  -> ?warning:bool
  -> t
  -> string

(** Each as [to_string_hum], the first [max_shown] only, then a line of how
    many more there are. *)
val list_to_string_hum
  :  ?contents:(string -> string option)
  -> ?warning:bool
  -> t list
  -> string

(** 50. *)
val max_shown : int
