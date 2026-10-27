open! Import

(** The program of a target: the libraries it depends on, then the target. A
    library [foo] is the module [Foo] of its files' modules, so they refer to
    each other as siblings, and to the libraries as [Bar]. The files of an
    executable or test are modules of the program themselves. Each library is
    a [Driver.Module_group.Group], so its files are evaluated in dependency
    order as a program's are. *)

(** Prims print to [output], stdout by default, as for [Driver.check]. *)
val check
  :  ?output:(string -> unit)
  -> Project.t
  -> Project.Target.t
  -> (Driver.t, Diagnostic.t list) Result.t
