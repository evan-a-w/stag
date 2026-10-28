open! Import

(** The commands of [stag-build]. Each prints diagnostics to stderr, with paths
    relative to the project root, and returns whether it succeeded. *)

val corrections_dir : string

(** Each command checks a target at most once, and not if a library it depends
    on has errors. Output at compile time is discarded. An expect test in an
    executable is warned about, as it is never run. *)

(** Checks every target. *)
val build : Project.t -> bool

(** Checks and evaluates the executable [name]. *)
val exec : Project.t -> string -> bool

(** Runs the expect tests in the files of each library and test named, or all
    of them. A file whose tests fail has its corrected version written to
    [corrections_dir], mirroring its path, with the digest of the version it
    corrects, and the diff printed. A file whose tests pass, or whose target
    doesn't check or evaluate, has its stale correction removed. *)
val test : Project.t -> string list -> bool

(** Where [-save-baseline] saves baselines. *)
val bench_dir : string

(** Measures the [let%bench]es of every target, each in its own program and
    named [TARGET/NAME], or with [phases] the interpreter's phases on the
    program of each target ([TARGET/parse], etc., see [Phase_bench]), those
    containing any of [filters] (all without). Output is discarded. *)
val bench
  :  Project.t
  -> Bench_command.Options.t
  -> filters:string list
  -> phases:bool
  -> bool

(** Moves the corrections in [corrections_dir] over their sources. Refuses,
    leaving it, a correction whose source is not one of the project's or has
    changed since it was made. *)
val promote : Project.t -> bool

(** Formats every source of the project, printing the diff of each file that
    is not formatted, and rewriting it in [Inplace] mode. Fails on a parse
    error, or in [Check] mode if a file is not formatted. *)
val fmt : Project.t -> mode:Format.Mode.t -> bool
