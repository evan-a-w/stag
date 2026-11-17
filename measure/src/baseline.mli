open! Import

(** The per-iteration times of the samples of benches, by name, saved to compare
    later runs with. *)
type t = float array String.Map.t [@@deriving sexp]

val empty : t

(** [t] with the times of [results], replacing those of the same names. *)
val update : t -> Bench_result.t list -> t

(** Names are letters, digits, [-], [_] and [.], not starting with [.]. *)
val validate_name : string -> unit Or_error.t

(** [dir/NAME.sexp]. *)
val path : dir:string -> name:string -> string

(** [None] if the file does not exist, an error if it is malformed. *)
val load : path:string -> t option Or_error.t

(** Creates [path]'s directory if needed, and replaces [path] atomically, by
    renaming a file of a unique name to it. *)
val save : t -> path:string -> unit Or_error.t
