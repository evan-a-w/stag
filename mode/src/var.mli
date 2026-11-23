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
val lower_rank : t -> rank:int -> unit

(** The lower bound [t] would have on [axis] if the variables [raised] (by [id])
    were raised to their upper bounds, but generic ones: what [t] is at least
    however they turn out. *)
val lower_assuming : t -> 'a Axis.t -> raised:(int -> bool) -> 'a

(** [None] once generic. *)
val rank : t -> int option

(** A variable's node on one axis, to walk the constraints between variables. *)
module Node : sig
  type t

  (** The id of the variable it belongs to. *)
  val var_id : t -> int

  (** [`Le n] for [this <= n], [`Ge n] for [n <= this], on the same axis and
      without crossing: of a generic variable, the constraints its scheme keeps;
      of another, those it has now. A relation through a crossing morphism is
      left out: no annotation can write it, and nothing crosses modes until
      crossing by kind. *)
  val relations : t -> ([ `Le | `Ge ] * t) list
end

val node : t -> _ Axis.t -> Node.t

(** [submode a b]: a value of mode [a] may be used at mode [b]. With [crossing],
    [a] is first lowered on the axes it crosses. *)
val submode
  :  ?crossing:Crossing.t
  -> t
  -> t
  -> Reason.t
  -> (unit, Mode_error.t) Result.t

(** [submode a b] on [axis] only. *)
val submode_on : t -> t -> _ Axis.t -> Reason.t -> (unit, Mode_error.t) Result.t

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

module Instance : sig
  type var := t
  type t

  val create : Id_source.t -> rank:int -> t

  (** A fresh copy of a generic variable, with its bounds and constraints, the
      same for each variable in [t]. Other variables are returned as they are.
      Under [atomically], copies made are forgotten if it is undone. *)
  val instantiate : t -> var -> var
end
