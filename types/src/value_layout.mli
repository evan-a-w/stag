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
