open! Import

(** Running a checked program's expect tests and correcting its files, as
    [stag test] and [stag-build test] do. Stale inline expansions are errors,
    corrected like failing tests. *)

module Outcome : sig
  type t =
    { evaluated : bool (** The program evaluated, so its tests ran. *)
    ; tests : int
    ; failed : int
    ; errors : int
    (** Of [failed], those with a runtime error or different outputs at an
        [[%expect]], which correcting can't fix. *)
    ; stale : int (** Stale inline expansions. *)
    ; corrected : int (** Files with a correction. *)
    ; write_failed : bool
    }
  [@@deriving sexp_of]

  (** Prints, to stderr, how many tests failed and expansions are stale, if
      any, prefixed by [target], saying they are promoted if [promoted]. *)
  val print_summary : ?target:string -> ?promoted:bool -> t -> unit

  (** It evaluated, its errors are none, its corrections were written and, if
      [mismatches_fail], it has no correction. *)
  val passed : mismatches_fail:bool -> t -> bool
end

(** Evaluates [driver], whose interpreter's output is as set, then runs the
    tests in [files], within [limits]. Its diagnostics (stale expansions, a
    runtime error, failed tests) are given to [report], then each file's
    correction, or [None] if it has none, to [write], whose error is reported
    too. *)
val run
  :  limits:Limits.t
  -> report:(Diagnostic.t -> unit)
  -> write:(Source_file.t -> corrected:string option -> (unit, Diagnostic.t) Result.t)
  -> files:Source_file.t list
  -> Driver.t
  -> Outcome.t
