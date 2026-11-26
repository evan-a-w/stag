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

(** Whether a type variable's values may hold pointers: [Unknown] until its
    variable is bound, and while what decides it is (the layouts it is above). A
    constraint can be guarded by layouts: it is added once any of them has
    pointers. *)
module Layout : sig
  type node := t
  type t

  val create : Id_source.t -> t
  val id : t -> int
  val has_pointers : t -> bool

  (** Why it was annotated pointer-free, if it was. *)
  val bound : t -> Reason.t option

  (** Its values have pointers: adds the constraints it guards, and raises the
      layouts above it. Fails if one is annotated pointer-free. *)
  val raise : t -> because:Reason.t list -> (unit, Mode_error.t) Result.t

  (** Its values never have pointers: what it guards is never added. *)
  val pointer_free : t -> unit

  (** Annotates it as pointer-free, and the layouts below it: as the [instance],
      used there, of what [reason] annotates, if it is one. *)
  val bound_by
    :  ?instance:Loc.t
    -> t
    -> Reason.t
    -> (unit, Mode_error.t) Result.t

  (** A new layout, which has pointers once both [a] and [b] do: once either is
      pointer-free, so is it. *)
  val both : Id_source.t -> t -> t -> t

  (** [t] has pointers if [lower] does. *)
  val add_below : lower:t -> t -> (unit, Mode_error.t) Result.t

  (** Runs [f] atomically. If it fails, the guarded constraints that failed are
      dropped as well: they are never added later, so they fail only once. *)
  val attempt : (unit -> ('a, 'e) Result.t) -> ('a, 'e) Result.t

  (** The layouts that decide whether those given have pointers, by id: those
      not yet known, and below no other. *)
  val roots : t list -> t list

  (** [lo <= m(hi)], once any of [guards] has pointers. *)
  val guard_edge
    :  node
    -> Morphism.t
    -> node
    -> guards:t list
    -> Reason.t
    -> (unit, Mode_error.t) Result.t

  (** [node <= point], once any of [guards] has pointers. *)
  val guard_upper
    :  node
    -> int
    -> guards:t list
    -> Reason.t
    -> (unit, Mode_error.t) Result.t

  (** Whether it is not known yet: not annotated, and not yet found to have
      pointers or not. *)
  val is_unknown : t -> bool
end

(** Makes generic the nodes in the list of rank greater than [rank]. Each keeps
    the constraints the graph implies between it and the other such nodes or
    nodes of rank at most [rank], through paths in the remaining nodes, which
    stay as they are. Constraints implied by the others kept are dropped. So are
    constraints guarded by layouts annotated pointer-free, which never fire. Not
    allowed under [atomically]. *)
val generalize : t list -> rank:int -> unit

(** Makes a generic node one of rank [rank] again, constrained by its edges in
    the graph, which it kept: generalizing it anew summarizes the constraints
    added since too. Not allowed under [atomically]. *)
val reopen : t -> rank:int -> unit

(** New generic nodes, one per axis (found by axis in the result and in
    [edges]), constrained only by [uppers] [(axis, point, reason)] and [edges]
    [(lo, m, hi, reason)] between them, which must not tighten their bounds.
    Nothing else refers to them yet, so this is allowed under [atomically]: an
    enclosing one being undone leaves them as they are. *)
val create_generic
  :  ?uppers:(Axis.Any.t * int * Reason.t) list
  -> var_id:int
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

(** The nodes [t]'s lower bound may come from, through edges and dormant edges
    not fired, and the layouts, not known, those wait for. *)
val lower_sources : t -> t list * Layout.t list

(** Bounds: a generic node's, those its scheme gives its instances, with what
    it adds once a layout guarding it has pointers. *)
val scheme_lower : t -> int

val scheme_upper : t -> int

(** The lower bound [t] would have if the nodes but generic ones were raised to
    the points [raised] gives, by the edges added. *)
val lower_assuming : t -> raised:(t -> int option) -> int

module Guarded : sig
  type node := t

  type kind =
    | Edge of node * Morphism.t * node (** [lo <= m(hi)] *)
    | Upper of node * int
    | Lower of node * int

  (** [kind], once any of [guards] has pointers: [holds] if one has. *)
  type t =
    { kind : kind
    ; guards : Layout.t list
    ; holds : bool
    }

  (** What the constraints not added, as the layouts guarding them aren't known,
      imply between the nodes given, or of their bounds: through other nodes not
      generic, and as a generic node's scheme keeps them. A generic node's kept
      once a guard has pointers [holds], as its instances have them. *)
  val among : node list -> t list
end
