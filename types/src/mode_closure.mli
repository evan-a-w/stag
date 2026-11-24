open! Import

(** What the constraints of some mode variables imply of them: each axis's
    bounds, and how far raising one node raises the others. *)
type t

(** Of [vars], with each node of [floors] raised to its point, as reading back a
    [with] clause assumes (a result's [portable]). Fails if one can't be. *)
val create : Mode.Var.t list -> floors:(Mode_node.t * int) list -> t

val lower : t -> Mode_node.t -> int
val upper : t -> Mode_node.t -> int

(** The points a node may be raised to: above its lower bound, up to its upper. *)
val domain : t -> Mode_node.t -> int list

(** [raise t x p y]: [y]'s least point when [x] is at least [p]. *)
val raise : t -> Mode_node.t -> int -> Mode_node.t -> int

(** The nodes raising [x] raises above their lower bounds. *)
val reached : t -> Mode_node.t -> Mode_node.t list

(** The nodes raising [x] to [p] raises, with their points (others stay at their
    lower bounds). *)
val raised_alist : t -> Mode_node.t -> int -> (Mode_node.t * int) list
