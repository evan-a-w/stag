open! Import

(** Checks [contents] as the file [a.stag], printing its signature, or the
    diagnostic. With [run], also evaluates it, printing its output, and runs
    its expect tests, printing those that fail and the corrected file. *)
val check : ?run:bool -> string -> unit
