open! Import

(** What a client keeps on a type variable, run as the variable is bound,
    instantiated or generalized. ['a] is what the variable is bound to, and ['e]
    what binding or generalizing it can fail with. *)
type ('a, 'e) t

val create
  :  'w Type_equal.Id.t
  -> 'w
  -> bind:('w -> 'a -> (unit, 'e) Result.t)
  -> instantiate:('w -> at:Loc.t option -> 'w option)
       (** The state of an instance's copy, if it keeps one, made [at] a use. *)
  -> generalize:('w -> (unit, 'e) Result.t)
  -> ('a, 'e) t

val find : ('a, _) t list -> 'w Type_equal.Id.t -> 'w option

(** Once the variable is bound to the ['a]: the first error, if one fails, and
    then none of their changes are made. *)
val bind : ('a, 'e) t list -> 'a -> (unit, 'e) Result.t

val instantiate : ?at:Loc.t -> ('a, 'e) t list -> ('a, 'e) t list

(** Before the variable is generalized: the errors of each. *)
val generalize : (_, 'e) t list -> 'e list
