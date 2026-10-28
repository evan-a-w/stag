open! Import

(** The prelude, [prelude/prelude.stag], then the module [Core], whose
    submodules are the files of the top-level [core/] directory
    ([core/list.stag] is [Core.List]): in
    scope in every program, outside its root module, its canonical names those
    of [Prelude_path]. It has [list], [option], [result], the module [Stag], the
    syntax of stag as data, for declarations generated at compile time (see
    [Syntax_value]), and [Core], a standard library. *)

(** The file of the prelude's locations. *)
val path : string

(** The directory of [Core]'s files, in their locations, eg. [<core>/list.stag]. *)
val core_dir : string

(** Parsed once. *)
val toplevels : Syntax.Toplevel.t list Lazy.t
