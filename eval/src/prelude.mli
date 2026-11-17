open! Import

(** The prelude, the top-level [prelude/prelude.stag], then the module [Core],
    whose submodules are the files of the top-level [core/] directory
    ([core/list.stag] is [Core.List]): in scope in every program, outside its
    root module, its canonical names those of [Prelude_path]. It has [list],
    [option], [result], the module [Stag], the syntax of stag as data, for
    declarations generated at compile time (see [Syntax_value]), and [Core], a
    standard library. *)

(** The file of the prelude's locations. *)
val path : string

module Sources : sig
  type t =
    { prelude : Source_file.t
    ; core : Source_file.t list (** The modules of [Core], by name. *)
    }

  (** As embedded in stag, at the files of their locations: [path] and
      [<core>/<x>.stag]. *)
  val embedded : t
end

(** The module [Core] is located at its first file, or the prelude's if it has
    none. *)
val parse : Sources.t -> (Syntax.Toplevel.t list, Source_file.Error.t) Result.t

(** The contents of a file of the embedded prelude's locations (eg.
    [<core>/list.stag]). *)
val source : string -> string option

(** Of [Sources.embedded], parsed once. *)
val toplevels : Syntax.Toplevel.t list Lazy.t
