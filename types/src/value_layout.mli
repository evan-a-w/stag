open! Import

(** [None] conservatively means all parameters are stored by value. *)
type parameters = string -> Int.Set.t option

(** Follow only by-value edges. Calls [con] for nominal types (other than
    [ptr]), and uses the returned parameter summary to visit their arguments. *)
val iter
  :  con:(string -> (Int.Set.t option, 'e) Result.t)
  -> var:(Type.id -> (unit, 'e) Result.t)
  -> cycle:(unit -> (unit, 'e) Result.t)
  -> Type.arg list
  -> (unit, 'e) Result.t

(** Whether [roots] reach the variable [var] by a path through no pointer: a
    [ptr], or a parameter of a nominal type that [con] says is not stored by
    value. If [needs_structure], a path must also pass through a type
    constructor other than [ptr] to be guarded. Functions don't guard paths. If
    [through_structure], only paths through a type constructor other than [ptr]
    count. *)
val reaches_unguarded
  :  ?through_structure:bool
  -> con:(string -> Int.Set.t option)
  -> needs_structure:bool
  -> var:Type.id
  -> Type.arg list
  -> bool
