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

(** Registers state that belongs to the enclosing [atomically] groups (such as
    what an attempt collects), which [permanent] sets aside while it runs:
    [set_aside ()] resets it, and returns how to restore it. *)
val register_scoped : set_aside:(unit -> unit -> unit) -> unit

(** Runs [f] now if neither under [atomically] nor in [permanent] under it, or
    else once the outermost [atomically] group finishes, whatever its outcome:
    for consequences of [permanent] changes on what [atomically] may undo. All
    that is queued then runs, even if one raises: the first exception is raised
    after the rest. *)
val when_settled : (unit -> unit) -> unit

(** Whether a cache of what [atomically] may undo can be filled now: not in
    [permanent] under [atomically], where filling it couldn't be undone with
    what it was computed from. *)
val cacheable : unit -> bool

(** Raises if under [atomically], for changes that can't be undone. *)
val assert_not_atomic : string -> unit
