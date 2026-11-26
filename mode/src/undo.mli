open! Import

(** A global log of how to undo changes, so that a group of changes either all
    happen or none do. *)

(** Records how to undo a change. Only under [atomically]. *)
val record : (unit -> unit) -> unit

(** Makes a change with [f], recording [undo] if under [atomically]. *)
val change : undo:(unit -> unit) -> (unit -> unit) -> unit

(** Runs [f], undoing the changes it recorded if it returns [Error] or raises.
    May be nested. *)
val atomically : (unit -> ('a, 'e) Result.t) -> ('a, 'e) Result.t

(** As [atomically], for an [f] that fails only by raising. *)
val atomically_exn : (unit -> 'a) -> 'a

(** Runs [f] outside of any [atomically], so that its changes are kept whatever
    the enclosing groups do. For computing what is cached for good: [f] may only
    change state that it creates, or that nothing under [atomically] has changed
    (such as a declaration's own variables). *)
val permanent : (unit -> 'a) -> 'a

(** Runs [f] now if neither under [atomically] nor in [permanent] under it, or
    else once the outermost [atomically] group finishes, whatever its outcome:
    for consequences of [permanent] changes on what [atomically] may undo. *)
val when_settled : (unit -> unit) -> unit

(** Raises if under [atomically], for changes that can't be undone. *)
val assert_not_atomic : string -> unit
