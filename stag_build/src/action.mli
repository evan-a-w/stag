open! Import

(** The commands of [stag-build]. Each prints diagnostics to stderr, with paths
    relative to the project root, and returns whether it succeeded. *)

val corrections_dir : string

(** Checks every target. A target is not checked if a library it depends on
    has errors. Output at compile time is discarded. *)
val build : Project.t -> bool

(** Checks and evaluates the executable [name]. *)
val exec : Project.t -> string -> bool

(** Runs the expect tests in the files of each library and test named, or all
    of them. A file whose tests fail has its corrected version written to
    [corrections_dir], mirroring its path, and the diff printed; a file whose
    tests pass has its stale correction removed. Targets depending on a library
    with errors are not tested. *)
val test : Project.t -> string list -> bool

(** Moves the corrections in [corrections_dir] over their sources. *)
val promote : root:string -> unit
