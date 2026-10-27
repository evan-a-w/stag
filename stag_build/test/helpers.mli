open! Core

(** Runs [f] on a fresh directory holding [files] (paths and contents), then
    removes it. *)
val with_dir : (string * string) list -> f:(string -> 'a) -> 'a

(** Prints [diagnostics] as [stag-build] does, quoting the files in [root]. *)
val print_diagnostics : root:string -> Stag.Diagnostic.t list -> unit
