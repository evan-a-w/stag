open! Core

module Error : sig
  type t =
    | Source of Source_file.Error.t
    | Typecheck of Typecheck_error.t
  [@@deriving sexp_of]
end

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

(** As [check], for the program of [files], each the module named after it
    (see [Source_file]). Toplevels of different files may refer to each other
    in any order. *)
val check_files
  :  ?limits:Limits.t
  -> ?output:(string -> unit)
  -> Source_file.t list
  -> (t, Error.t) Result.t

(** As [check_files], for modules already parsed: each of [modules] is a
    [Toplevel.Module], evaluated as [check_files]'s files are. *)
val check_modules
  :  ?limits:Limits.t
  -> ?output:(string -> unit)
  -> Syntax.Toplevel.t list
  -> (t, Typecheck_error.t) Result.t

val checked : t -> Typecheck.Checked.t
val interpreter : t -> Treewalk.t

(** Evaluates the toplevels not yet evaluated at compile time, within [limits]
    of its own, unlimited by default. Those evaluated at compile time keep
    their values, charged to the limits of [check].

    Each toplevel is evaluated after those it refers to. Otherwise, a program
    is evaluated in order and the files of [check_files] in dependency order:
    a file depends on those its toplevels refer to, and each strongly connected
    component of files is evaluated after those it depends on, else in the
    order given, with its files in the order given. *)
val eval : ?limits:Limits.t -> t -> unit Or_error.t
