open! Import

(** A replacement of the bytes [start, stop) of a file's contents. *)
type t =
  { start : int
  ; stop : int
  ; replacement : string
  }
[@@deriving sexp_of]

(** [contents] with the edits made, which must not overlap. Edits at the same
    offset are made in the order given. If [contents] has [\r\n] line endings,
    so do the replacements. *)
val apply : string -> t list -> string

val leading_whitespace : string -> string

(** The leading whitespace of the line of [contents] at [offset], up to
    [offset]. *)
val indentation : string -> offset:int -> string

(** Whether [offset] is the first non-whitespace on its line. *)
val first_on_line : string -> offset:int -> bool
