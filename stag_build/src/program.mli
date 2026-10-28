open! Import

(** The program of a target: the libraries it depends on, then the target. A
    library [foo] is the module [Foo] of its files' modules, so they refer to
    each other as siblings, and to the libraries as [Bar]. The files of an
    executable or test are modules of the program themselves. Each library is
    a [Driver.Module_group.Group], so its files are evaluated in dependency
    order as a program's are. *)

(** The program as far as it parses ([Source_file.parse_recovering]), unless
    a file's name is invalid, and the errors of all its files. *)
val parse_recovering
  :  Project.t
  -> Project.Target.t
  -> Driver.Module_group.t list option * Diagnostic.t list

(** [parse_recovering], if without errors. *)
val parse
  :  Project.t
  -> Project.Target.t
  -> (Driver.Module_group.t list, Diagnostic.t list) Result.t

(** [parse_recovering], then [Driver.check_modules] if a program parsed,
    failing with all the errors of both, by location. [output] is as for
    [Driver.check]. *)
val check
  :  ?output:(string -> unit)
  -> Project.t
  -> Project.Target.t
  -> (Driver.t, Diagnostic.t list) Result.t
