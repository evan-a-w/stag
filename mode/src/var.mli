open! Import

(** A mode variable: a variable on each axis, between a lower and an upper
    bound. Its provenance is at most [static] when its stage is [const]. Every
    function that adds a constraint either succeeds or changes nothing. *)
type t [@@deriving sexp_of]

(** Unconstrained, of the given rank (as for type variables: generalizing at a
    rank makes generic the variables of greater ranks). *)
val create : Id_source.t -> rank:int -> t

val of_const : Id_source.t -> rank:int -> Const.t -> t
val id : t -> int
val lower : t -> Const.t
val upper : t -> Const.t
val is_generic : t -> bool
val lower_rank : t -> rank:int -> unit

(** [submode a b]: a value of mode [a] may be used at mode [b]. With [crossing],
    [a] is first lowered on the axes it crosses. *)
val submode
  :  ?crossing:Crossing.t
  -> t
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

(** Makes generic the variables of rank greater than [rank]. They keep their
    bounds and the constraints the graph implies among them and with variables
    of rank at most [rank] (through other variables, which stay as they are). *)
val generalize : t list -> rank:int -> unit

module Instance : sig
  type var := t
  type t

  val create : Id_source.t -> rank:int -> t

  (** A fresh copy of a generic variable, with its bounds and constraints, the
      same for each variable in [t]. Other variables are returned as they are. *)
  val instantiate : t -> var -> var
end
