open! Import

module Error : sig
  type t =
    | Source of Source_file.Error.t
    | Typecheck of Typecheck_error.t
  [@@deriving sexp_of]

  val loc : t -> Loc.t
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
  -> (t, Typecheck.Failed.t) Result.t

(** As [check], for the program of [files], each the module named after it
    (see [Source_file]). Toplevels of different files may refer to each other
    in any order. Files with syntax errors are checked as far as they parse;
    the errors are of all files, by location. *)
val check_files
  :  ?limits:Limits.t
  -> ?output:(string -> unit)
  -> Source_file.t list
  -> (t, Error.t list) Result.t

(** The modules of a program, for [check_modules]. *)
module Module_group : sig
  type t =
    | File of Syntax.Toplevel.t
    (** A [Toplevel.Module], as [Source_file.parse] makes. *)
    | Group of Syntax.Name.t * t list
    (** The module of these, eg. a library of files, which are ordered among
        themselves as the files of [check_files] are. *)
end

(** As [check_files], for modules already parsed. *)
val check_modules
  :  ?limits:Limits.t
  -> ?output:(string -> unit)
  -> Module_group.t list
  -> (t, Typecheck.Failed.t) Result.t

(** Checks [prelude] (eg. [Prelude.parse] of sources being edited) as
    [Typecheck.check_prelude] does. Its toplevels are not evaluated. *)
val check_prelude
  :  ?limits:Limits.t
  -> ?output:(string -> unit)
  -> Syntax.Toplevel.t list
  -> (t, Typecheck.Failed.t) Result.t

val checked : t -> Typecheck.Checked.t
val interpreter : t -> Treewalk.t

(** Evaluates the toplevels not yet evaluated at compile time, within [limits]
    of its own, unlimited by default. Those evaluated at compile time keep
    their values, charged to the limits of [check].

    Toplevels are evaluated top to bottom: those of [check]'s program in
    order, and the files of [check_files] (and the members of each
    [Module_group]) in dependency order, each top to bottom. A file depends on those its toplevels refer to, and each
    strongly connected component of files comes after those it depends on,
    else in the order given, with its files in the order given. A toplevel
    used before its turn, eg. by one earlier, is evaluated then, on demand,
    and not again. Those used at compile time were evaluated then. *)
val eval : ?limits:Limits.t -> t -> unit Or_error.t
