open! Import

(** A global log of how to undo changes, so that a group of changes either all
    happen or none do. *)

(** Records how to undo a change. Only under [atomically]. *)
val record : (unit -> unit) -> unit

(** Runs [f], undoing the changes it recorded if it returns [Error] or raises.
    May be nested. *)
val atomically : (unit -> ('a, 'e) Result.t) -> ('a, 'e) Result.t
