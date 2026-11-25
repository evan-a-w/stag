open! Import

(** A global log of how to undo changes, so that a group of changes either all
    happen or none do. *)

(** Records how to undo a change. Only under [atomically]. *)
val record : (unit -> unit) -> unit

(** Runs [f], undoing the changes it recorded if it returns [Error] or raises.
    May be nested. *)
val atomically : (unit -> ('a, 'e) Result.t) -> ('a, 'e) Result.t

(** Raises if under [atomically], for changes that can't be undone. *)
val assert_not_atomic : string -> unit

(** Runs [f] as if not under [atomically]: what it changes stays even if an
    enclosing [atomically] is undone. Only for changes to what [f] creates. *)
val detached : (unit -> 'a) -> 'a
