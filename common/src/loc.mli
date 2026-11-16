open! Import

(** A file name, interned: equal names are the same [t]. Ordered by name. The
    table of names is global, never shrinks, and is not safe to use from more
    than one domain. *)
module File : sig
  type t : immediate [@@deriving compare, equal, hash, sexp_of]

  val of_string : string -> t
  val to_string : t -> string
end

(** A position in a source file. [line] is 1-based, [col] and [offset] (from the
    start of the file) are 0-based, in bytes. *)
module Pos : sig
  type t =
    #{ line : int
     ; col : int
     ; offset : int
     }
  [@@deriving sexp_of, compare, equal, hash]
end

(** A span of source, from [start] up to but excluding [stop]. *)
type t =
  { file : File.t
  ; start : Pos.t
  ; stop : Pos.t
  }
[@@deriving compare, equal, hash]

val create : file:string -> start:Pos.t -> stop:Pos.t -> t
val file_name : t -> string

(** For syntax not written in a file, eg. built by tests or generated. *)
val none : t

val is_none : t -> bool

(** Zero width, at the start of [file]. *)
val start_of_file : File.t -> t

(** The smallest span containing both, which must be in the same file. *)
val merge : t -> t -> t

(** Stably sorted by the locations [f] gives, which [compare] orders by file,
    then position. *)
val sort_by : 'a list -> f:('a -> t) -> 'a list

(** [file:line:col-line:col], or [file:line:col-col] on one line. *)
val to_string : t -> string

(** As [to_string]: an atom, so locations don't drown out the rest of a sexp. *)
val sexp_of_t : t -> Sexp.t

include Hashable.S_plain with type t := t
