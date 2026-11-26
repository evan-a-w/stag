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

(** What generalizing [kept] at [rank] does with a node: it is left to the code
    outside ([Outer]), made generic ([Kept]), already of a scheme ([Generic]),
    or summarized away ([Eliminated]), as it has every constraint it will have. *)
module Fate : sig
  type t =
    | Outer
    | Kept
    | Generic
    | Eliminated
  [@@deriving equal]
end

val fate : t -> rank:int -> kept:(t -> bool) -> Fate.t

(** Makes generic the nodes in the list of rank greater than [rank]. Each keeps
    the constraints the graph implies between it and the other such nodes or
    nodes of rank at most [rank], through paths in the remaining nodes, which
    stay as they are. Constraints implied by the others kept are dropped. Not
    allowed under [atomically]. *)
val generalize : t list -> rank:int -> unit

(** Makes a generic node one of rank [rank] again, constrained by its edges in
    the graph, which it kept: generalizing it anew summarizes the constraints
    added since too. Not allowed under [atomically]. *)
val reopen : t -> rank:int -> unit

(** New generic nodes, one per axis (found by axis in the result and in
    [edges]), constrained only by [edges] [(lo, m, hi, reason)] between them,
    which must not tighten their bounds. Nothing else refers to them yet, so
    this is allowed under [atomically]: an enclosing one being undone leaves
    them as they are. *)
val create_generic
  :  var_id:int
  -> Axis.Any.t list
  -> edges:((Axis.Any.t -> t) -> (t * Morphism.t * t * Reason.t) list)
  -> Axis.Any.t
  -> t

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

(** [t] and the nodes its edges [t <= m(hi)] lead to, transitively: those whose
    upper bounds bound [t]'s. *)
val above : t -> t list

val axis : t -> Axis.Any.t

(** The nodes that rise, with their least points, if each node given is raised
    to its point, by the constraints that hold: a generic node's are those its
    scheme keeps. [None] if one can't be. *)
val raised : (t * int) list -> (t * int) list option

(** The lower bound [t] would have if the variables [raised] (by variable id),
    but generic ones, were raised to their upper bounds. *)
val lower_assuming : t -> raised:(int -> bool) -> int
