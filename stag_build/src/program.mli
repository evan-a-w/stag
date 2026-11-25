open! Import

(** The program of a target: the libraries it depends on, then the target. A
    library [foo] is the module [Foo] of its files' modules, so they refer to
    each other as siblings, and to the libraries as [Bar]. The files of an
    executable or test are modules of the program themselves. Each library is a
    [Driver.Module_group.Group], so its files are evaluated in dependency order
    as a program's are. *)

(** The program as far as it parses ([Source_file.parse_recovering]), unless a
    file's name is invalid, and the errors of all its files. *)
val parse_recovering
  :  Project.t
  -> Project.Target.t
  -> Driver.Module_group.t list option * Diagnostic.t list

(** Which externals of [target]'s program may be called at compile time: those
    of the libraries (and executable or test) whose stanza has
    [(compile_time_c_calls)]. *)
val compile_time_c_calls
  :  Project.t
  -> Project.Target.t
  -> Compile_time_c_calls.t

(** [parse_recovering], if without errors. *)
val parse
  :  Project.t
  -> Project.Target.t
  -> (Driver.Module_group.t list, Diagnostic.t list) Result.t

(** The absolute paths of the files of [target]'s program. *)
val paths : Project.t -> Project.Target.t -> string list

(** The prelude [target]'s program sees, as
    [Pipeline_config.Prelude_choice.of_paths] of its [paths]. *)
val prelude_choice
  :  Project.t
  -> Project.Target.t
  -> Pipeline_config.Prelude_choice.t

(** [Pipeline.check_sources] of [parse_recovering] seeing [prelude] (eg. loaded
    as [prelude_choice] says), failing with all the errors. [output] is as for
    [Driver.check]. *)
val check
  :  ?output:(string -> unit)
  -> ?prelude:Syntax.Toplevel.t list
  -> limits:Limits.t
  -> Project.t
  -> Project.Target.t
  -> (Driver.t, Diagnostic.t list) Result.t
