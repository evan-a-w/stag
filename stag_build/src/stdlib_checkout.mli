open! Import

(** A checkout of stag's sources: a directory with a [marker_file] (whose
    contents do not matter), whose [prelude/prelude.stag] and [core/*.stag]
    are the sources of the prelude (see [Prelude]). They are checked as the
    prelude, not as programs. Copies of a checkout in its own [_build]
    directory (eg. made by dune) are not checkouts. Paths are absolute and
    canonical. *)

type t [@@deriving sexp_of]

val marker_file : string

(** [prelude/prelude.stag]. *)
val prelude_file : string

(** [core]. *)
val core_dir : string

(** Its directory, symlinks resolved. *)
val root : t -> string

(** The checkout that [path] is a source of the prelude of. *)
val of_source : string -> t option

(** The nearest of [dir] and its ancestors that is a checkout. *)
val find : string -> t option

(** The checkout of an executable built in it, outside its [_build]
    directory, whose copies of sources are not to be edited. *)
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

(** [sources], parsed, for programs in [t] to see instead of the embedded
    prelude. *)
val prelude
  :  ?contents:(string -> string option)
  -> t
  -> (Syntax.Toplevel.t list, Diagnostic.t) Result.t

module Checked : sig
  type t =
    { prelude : Syntax.Toplevel.t list
    ; driver : Driver.t
    }
end

(** Checks [prelude] as the prelude ([Driver.check_prelude]), failing with
    all its errors. *)
val check
  :  ?contents:(string -> string option)
  -> ?output:(string -> unit)
  -> limits:Limits.t
  -> t
  -> (Checked.t, Diagnostic.t list) Result.t

(** The part of the prelude's [signature] that its source [path] declares:
    all but [Core] for [prelude/prelude.stag], else [Core] with only the
    module of [path]. Errs if [path] names no module. *)
val signature_of_source
  :  t
  -> Signature.t
  -> string
  -> (Signature.t, Diagnostic.t) Result.t

(** Where a file of the embedded prelude's locations is in a checkout,
    relative to its root, eg. [core/list.stag] for [<core>/list.stag]. *)
val checkout_path : string -> string option

(** Locations in the embedded prelude's files (eg. [<core>/list.stag]) in
    [t]'s sources instead, for those whose contents (as for [sources]) are
    those embedded, so that the locations are right. Reads them once, when
    applied to [t]. *)
val relocate_embedded
  :  ?contents:(string -> string option)
  -> t
  -> Loc.t
  -> Loc.t
