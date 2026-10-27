open! Core

(** A program checked, with the interpreter that ran its toplevel splices and
    the toplevels they used, ready to evaluate the rest. *)
type t

(** Typechecks [program], running its toplevel splices as they are reached. *)
val check : Syntax.Program.t -> (t, Typecheck_error.t) Result.t

val checked : t -> Typecheck.Checked.t
val interpreter : t -> Treewalk.t

(** Evaluates the toplevels not yet evaluated at compile time, in order. *)
val eval : t -> unit Or_error.t
