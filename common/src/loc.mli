open! Import

(** A position in a source file. [line] is 1-based, [col] and [offset] (from
    the start of the file) are 0-based, in bytes. *)
module Pos : sig
  type t =
    { line : int
    ; col : int
    ; offset : int
    }
  [@@deriving sexp_of, compare, equal]
end

(** A span of source, from [start] up to but excluding [stop]. *)
type t =
  { file : string
  ; start : Pos.t
  ; stop : Pos.t
  }
[@@deriving compare, equal]

(** For syntax not written in a file, eg. built by tests or generated. *)
val none : t

val is_none : t -> bool

(** Zero width, at the start of [file]. *)
val start_of_file : string -> t

(** The smallest span containing both, which must be in the same file. *)
val merge : t -> t -> t

(** Stably sorted by the locations [f] gives, which [compare] orders by file,
    then position. *)
val sort_by : 'a list -> f:('a -> t) -> 'a list

(** [file:line:col-line:col], or [file:line:col-col] on one line. *)
val to_string : t -> string

(** As [to_string]: an atom, so locations don't drown out the rest of a sexp. *)
val sexp_of_t : t -> Sexp.t
