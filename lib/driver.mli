open! Core

(** A program checked, with the interpreter that ran its toplevel splices and
    the toplevels they used, ready to evaluate the rest. *)
type t

(** Typechecks [program], running its toplevel splices as they are reached.
    All evaluation at compile time shares [limits], unlimited by default.
    Prims print to [output], stdout by default, at compile time and when
    evaluated. *)
val check
  :  ?limits:Limits.t
  -> ?output:(string -> unit)
  -> Syntax.Program.t
  -> (t, Typecheck_error.t) Result.t

val checked : t -> Typecheck.Checked.t
val interpreter : t -> Treewalk.t

(** Evaluates the toplevels not yet evaluated at compile time, in order, within
    [limits] of its own, unlimited by default. Those evaluated at compile time
    keep their values, charged to the limits of [check]. *)
val eval : ?limits:Limits.t -> t -> unit Or_error.t
