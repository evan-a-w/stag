open! Import

(** The solver, on one axis at a time: a node is a variable of one axis, with a
    lower and an upper bound (points of the axis, by index), and edges
    [lo <= m(hi)] to other nodes, along which bounds propagate. *)
type t [@@deriving sexp_of]

val create : var_id:int -> Axis.Any.t -> rank:int -> t
val lower : t -> int
val upper : t -> int
val is_generic : t -> bool
val lower_rank : t -> rank:int -> unit

(** Runs [f], undoing what it changed if it fails. The functions below that
    change bounds or edges must be run under it. *)
val atomically
  :  (unit -> (unit, Mode_error.t) Result.t)
  -> (unit, Mode_error.t) Result.t

(** [lo <= m(hi)]. *)
val add_edge : t -> Morphism.t -> t -> Reason.t -> (unit, Mode_error.t) Result.t

val at_least : t -> int -> Reason.t -> (unit, Mode_error.t) Result.t
val at_most : t -> int -> Reason.t -> (unit, Mode_error.t) Result.t

(** Fixes the node to its lower (upper) bound. Never fails. *)
val default_to_lower : t -> unit

val default_to_upper : t -> unit

(** Makes generic the nodes in the list of rank greater than [rank]. Each keeps
    the constraints the graph implies between it and the other such nodes or
    nodes of rank at most [rank], through paths in the remaining nodes, which
    stay as they are. Constraints implied by the others kept are dropped. Not
    allowed under [atomically]. *)
val generalize : t list -> rank:int -> unit

module Instance : sig
  type node := t
  type t

  val create : Id_source.t -> rank:int -> t

  (** A copy of a generic node with its bounds and constraints, the same copy
      for each node in [t]. Other nodes are returned as they are. *)
  val instantiate : t -> node -> node

  (** The id of the variable that the copies of a variable's nodes belong to. *)
  val var_id : t -> int -> int
end

(** [None] once generic. *)
val rank : t -> int option

val var_id : t -> int

(** The nodes [t] is related to on its axis, by a morphism implying the relation
    without it: [`Le n] for [t <= n], [`Ge n] for [n <= t]. A generic node's are
    the constraints its scheme keeps, others' their edges. *)
val relations : t -> ([ `Le | `Ge ] * t) list

(** The lower bound [t] would have if the variables [raised] (by variable id),
    but generic ones, were raised to their upper bounds. *)
val lower_assuming : t -> raised:(int -> bool) -> int
