open! Import

(** A mode variable: a variable on each axis, between a lower and an upper
    bound. Its provenance is at most [static] when its stage is [const]. Every
    function that adds a constraint either succeeds or changes nothing. *)
type t [@@deriving sexp_of]

(** Unconstrained, of the given rank (as for type variables: generalizing at a
    rank makes generic the variables of greater ranks). *)
val create : Id_source.t -> rank:int -> t

(** Fixed to the constant. Fails if it is [const] but not [static]. *)
val of_const : Id_source.t -> rank:int -> Const.t -> (t, Mode_error.t) Result.t

val id : t -> int
val lower : t -> Const.t
val upper : t -> Const.t
val is_generic : t -> bool

(** What [read] gives once the constraints [f] adds hold, which are then undone:
    [None] if they can't hold. *)
val assuming
  :  (unit -> (unit, Mode_error.t) Result.t)
  -> read:(unit -> 'a)
  -> 'a option

val lower_rank : t -> rank:int -> unit

(** The lower bound [t] would have on [axis] if the variables (by [id]), but
    generic ones, were raised to the points [raised] gives: what [t] is at least
    however they turn out, by the constraints added. *)
val lower_assuming : t -> 'a Axis.t -> raised:(int -> 'a option) -> 'a

(** The layouts, not known, on which constraints [t] may get on [axis] from
    below wait. *)
val waits_on : t -> _ Axis.t -> Graph.Layout.t list

(** [None] once generic. *)
val rank : t -> int option

(** A variable's node on one axis, to walk the constraints between variables. *)
module Node : sig
  type t

  (** The id of the variable it belongs to. *)
  val var_id : t -> int

  val axis : t -> Axis.Any.t

  (** Bounds, as points of the axis by index: a generic node's, those its
      instances get. *)
  val lower : t -> int

  val upper : t -> int

  (** [lower]s, shared by queries made while the modes don't change. *)
  module Floors : sig
    type node := t
    type t

    val create : unit -> t
    val lower : t -> node -> int
  end

  (** The nodes that rise, with their least points (by index), if each node
      given is raised to its point: what the constraints imply of them then.
      [None] if one can't be. *)
  val raised : ?floors:Floors.t -> (t * int) list -> (t * int) list option

  module Fate = Graph.Fate

  (** What generalizing at [rank], keeping the nodes [kept] holds, does with
      [t]. *)
  val fate : t -> rank:int -> kept:(t -> bool) -> Fate.t

  (** [t] and the nodes it is constrained to be below, transitively: those whose
      upper bounds bound [t]'s. *)
  val above : t -> t list

  (** Only under [assuming]. *)
  val at_most : t -> int -> Reason.t -> (unit, Mode_error.t) Result.t

  val at_least : t -> int -> Reason.t -> (unit, Mode_error.t) Result.t

  module Guarded : sig
    type node := t

    type kind =
      | Edge of node * Morphism.t * node (** [lo <= m(hi)] *)
      | Upper of node * int
      | Lower of node * int

    (** [kind], once any of [guards] has pointers: [holds] if one has. *)
    type t =
      { kind : kind
      ; guards : Graph.Layout.t list
      ; holds : bool
      }

    (** What the constraints not added, as the layouts guarding them aren't
        known, imply between the nodes given, or of their bounds. A generic
        node's scheme keeps them, its instances adding them: those whose guard
        has pointers since [hold]. *)
    val among : node list -> t list
  end
end

val node : t -> _ Axis.t -> Node.t

(** [relate a m b]: [a <= m(b)], of [a] on [Morphism.dst m] and [b] on
    [Morphism.src m]. *)
val relate : t -> Morphism.t -> t -> Reason.t -> (unit, Mode_error.t) Result.t

(** [submode a b]: a value of mode [a] may be used at mode [b]. With [crossing],
    [a] is first lowered on the axes it crosses. On locality, [a <= locality(b)]
    (by default, [a <= b]). *)
val submode
  :  ?crossing:Crossing.t
  -> ?locality:Locality_map.t
  -> t
  -> t
  -> Reason.t
  -> (unit, Mode_error.t) Result.t

(** [t] is allocated at run time unless it is [const]: its provenance is at
    least [gc] unless its stage is [const]. *)
val allocated : t -> Reason.t -> (unit, Mode_error.t) Result.t

(** [submode a b] on [axis] only. *)
val submode_on
  :  ?crossing:Crossing.t
  -> t
  -> t
  -> _ Axis.t
  -> Reason.t
  -> (unit, Mode_error.t) Result.t

(** [submode_meet a c b]: [meet a c <= b], on every axis. *)
val submode_meet
  :  t
  -> Const.t
  -> t
  -> Reason.t
  -> (unit, Mode_error.t) Result.t

(** [submode_meet_on a axis c b]: [meet a c <= b] on [axis]. *)
val submode_meet_on
  :  t
  -> 'a Axis.t
  -> 'a
  -> t
  -> Reason.t
  -> (unit, Mode_error.t) Result.t

val equate : t -> t -> Reason.t -> (unit, Mode_error.t) Result.t
val at_least : t -> 'a Axis.t -> 'a -> Reason.t -> (unit, Mode_error.t) Result.t

val at_most
  :  ?crossing:Crossing.t
  -> t
  -> 'a Axis.t
  -> 'a
  -> Reason.t
  -> (unit, Mode_error.t) Result.t

val at_least_const : t -> Const.t -> Reason.t -> (unit, Mode_error.t) Result.t

val at_most_const
  :  ?crossing:Crossing.t
  -> t
  -> Const.t
  -> Reason.t
  -> (unit, Mode_error.t) Result.t

(** Fixes the variable to its lower bound: the default for variables left
    unconstrained. Never fails. *)
val default_to_lower : t -> unit

(** Fixes the variable to its upper bound: erasing a variable the code does not
    depend on. Never fails. *)
val default_to_upper : t -> unit

(** Makes generic the variables in the list of rank greater than [rank]. They
    keep their bounds and the constraints the graph implies among them and with
    variables of rank at most [rank], through the variables not in the list,
    which stay as they are. Not allowed under [atomically], as it can't be
    undone.

    Generic variables stay in the graph: their bounds follow later changes to
    the variables they are constrained by, so a change to such a variable may
    fail with a mismatch at a generic variable (one of its instances would be
    inconsistent anyway). *)
val generalize : t list -> rank:int -> unit

(** Generic and unconstrained: a placeholder each instance copies afresh. *)
val create_generic : Id_source.t -> t

(** Makes a generic variable one of rank [rank] again, with its constraints, to
    refine a scheme in place before generalizing it anew. Not allowed under
    [atomically]. *)
val reopen : t -> rank:int -> unit

module Instance : sig
  type var := t
  type t

  val create : Id_source.t -> rank:int -> t

  (** A fresh copy of a generic variable, with its bounds and constraints, the
      same for each variable in [t]. Other variables are returned as they are.
      Under [atomically], copies made are forgotten if it is undone. *)
  val instantiate : t -> var -> var
end
