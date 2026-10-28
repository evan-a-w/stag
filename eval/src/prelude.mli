open! Import

(** The prelude, [prelude/prelude.stag], then the module [Core], whose
    submodules are the files of the top-level [core/] directory ([core/list.stag]
    is [Core.List]): in scope in every program, outside its root module, its
    canonical names those of [Prelude_path]. It has [list], [option], [result],
    the module [Stag], the syntax of stag as data, for declarations generated at
    compile time (see [Syntax_value]), and [Core], a standard library. *)

(** The file of the prelude's locations. *)
val path : string

(** The contents of a file of the prelude's or [Core]'s locations (eg.
    [<core>/list.stag]). *)
val source : string -> string option

(** Parsed once. *)
val toplevels : Syntax.Toplevel.t list Lazy.t
