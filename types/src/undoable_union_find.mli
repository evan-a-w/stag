open! Import

(** A union-find whose changes made under [Mode.Private.Undo.atomically] are
    undone with it. *)

type 'a t

val create : 'a -> 'a t
val get : 'a t -> 'a
val set : 'a t -> 'a -> unit
val same_class : 'a t -> 'a t -> bool

(** Joins the two classes, whose value becomes [f (get t) (get other)]. *)
val union : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> unit
