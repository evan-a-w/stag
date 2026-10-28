open! Import

(** The per-iteration times of the samples of benches, by name, saved to
    compare later runs with. *)
type t [@@deriving sexp]

val empty : t

(** [t] with the times of [results], replacing those of the same names. *)
val update : t -> Bench_result.t list -> t

val find : t -> string -> float array option

(** Names are letters, digits, [-], [_] and [.], not starting with [.]. *)
val validate_name : string -> unit Or_error.t

(** [dir/NAME.sexp]. *)
val path : dir:string -> name:string -> string

(** Errs if the file does not exist or is malformed. *)
val load : path:string -> t Or_error.t

(** Creates [path]'s directory if needed. *)
val save : t -> path:string -> unit Or_error.t
