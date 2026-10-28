open! Import

(** A checkout of stag's sources: a directory with a [marker_file], whose
    [prelude/prelude.stag] and [core/*.stag] are the sources of the prelude
    (see [Prelude]). They are checked as the prelude, not as programs. *)

type t [@@deriving sexp_of]

val marker_file : string

(** Absolute. *)
val root : t -> string

(** The checkout that [path] (absolute) is a source of the prelude of. *)
val of_source : string -> t option

(** The nearest of [dir] (absolute) and its ancestors that is a checkout. *)
val find : string -> t option

(** The checkout of an executable (absolute) built in it, outside its
    [_build] directory, whose copies of sources are not to be edited. *)
val of_executable : string -> t option

(** The paths of [t]'s sources of the prelude, [prelude/prelude.stag] then
    those of [core/], by name. *)
val source_paths : t -> (string list, Diagnostic.t) Result.t

(** The prelude of [t]'s sources, with the contents of each [contents path] if
    some (eg. an editor's unsaved buffer), else read from disk. *)
val sources
  :  ?contents:(string -> string option)
  -> t
  -> (Prelude.Sources.t, Diagnostic.t) Result.t

(** Locations in the embedded prelude's files (eg. [<core>/list.stag]) in
    [t]'s sources instead, for those whose contents on disk are those
    embedded, so that the locations are right. Reads them once, when
    applied to [t]. *)
val relocate_embedded : t -> Loc.t -> Loc.t
