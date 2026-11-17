open! Import

(** Checks [contents] as the file [a.stag], printing its signature, or the
    diagnostic. With [run], also evaluates it, printing its output, and runs its
    expect tests, printing those that fail and the corrected file.
    [filter_diagnostic] rewrites the printed diagnostic. *)
val check : ?run:bool -> ?filter_diagnostic:(string -> string) -> string -> unit

(** As [check], for the files given as [(path, contents)]. *)
val check_files
  :  ?run:bool
  -> ?filter_diagnostic:(string -> string)
  -> (string * string) list
  -> unit

(** A [filter_diagnostic]: without the lines locating a raise within the core
    library's sources, which move as they change. *)
val without_core_location : string -> string
