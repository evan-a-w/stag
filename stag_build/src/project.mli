open! Import

(** A project: the directory with a [stag-project] file, and the stanzas of the
    [stag] files in it and its subdirectories. *)

val project_file : string

(** The nearest of [dir] and its ancestors with a [project_file]. [dir] should
    be absolute. *)
val find_root : string -> string Or_error.t

module Target : sig
  (** A stanza and the files of its directory. *)
  type t =
    { stanza : Stanza.t
    ; dir : string (** Relative to the root, [.] for the root. *)
    ; files : Source_file.t list
    (** The [.stag] files of [dir], by name, at paths relative to the root. *)
    }
  [@@deriving sexp_of]

  val name : t -> string
end

type t

(** Relative paths are relative to [root]. Skips directories starting with [.]
    or [_]. Errs with every error found. A file's contents are [contents path]
    (its path relative to [root]) if some, eg. an editor's unsaved buffer, else
    read from disk. *)
val load
  :  ?contents:(string -> string option)
  -> root:string
  -> unit
  -> (t, Diagnostic.t list) Result.t

val root : t -> string

(** By directory. *)
val targets : t -> Target.t list

val find : t -> string -> Target.t option

(** The libraries [target] depends on, directly or not, each after those it
    depends on. *)
val closure : t -> Target.t -> Target.t list
