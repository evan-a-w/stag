open! Import

(** What a client keeps on a type variable, run as the variable is bound,
    instantiated or generalized. ['a] is what the variable is bound to. *)
type 'a t

val create
  :  'w Type_equal.Id.t
  -> 'w
  -> bind:('w -> 'a -> (unit, Mode.Error.t) Result.t)
  -> instantiate:('w -> 'w option)
       (** The state of an instance's copy, if it keeps one. *)
  -> generalize:('w -> (unit, Mode.Error.t) Result.t)
  -> 'a t

val find : 'a t list -> 'w Type_equal.Id.t -> 'w option

(** Once the variable is bound to the ['a]. *)
val bind : 'a t list -> 'a -> (unit, Mode.Error.t) Result.t

val instantiate : 'a t list -> 'a t list

(** Before the variable is generalized: the errors of each. *)
val generalize : 'a t list -> Mode.Error.t list
