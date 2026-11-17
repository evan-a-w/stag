open! Import

(** File system access, with errors as diagnostics. *)

module Kind : sig
  type t =
    | File
    | Directory
    | Other

  (** Symlinks are followed to files, not to directories, which could loop or
      reach a directory twice: a symlink to a directory is [Other]. *)
  val of_path : string -> t
end

(** [dir/name], or [name] for the directory [.]. *)
val relative : dir:string -> string -> string

(** [f ()], or its [Sys_error] or [Unix_error] as [path: reason]. *)
val try_ : path:string -> (unit -> 'a) -> ('a, Diagnostic.t) Result.t

(** The names in the directory [root/dir], sorted, with their kinds, or none if
    it can't be read. Those whose kind can't be read are omitted, and an error
    for those that [must_read]. *)
val read_dir
  :  ?must_read:(string -> bool)
  -> error:(Diagnostic.t -> unit)
  -> root:string
  -> string
  -> (string * Kind.t) list
