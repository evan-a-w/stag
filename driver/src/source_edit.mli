open! Import

(** A replacement of the bytes [start, stop) of a file's contents. *)
type t =
  { start : int
  ; stop : int
  ; replacement : string
  }
[@@deriving sexp_of]

(** [contents] with the edits made, which must not overlap. Edits at the same
    offset are made in the order given. *)
val apply : string -> t list -> string
