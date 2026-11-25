open! Import

(** The modes of an arrow's argument and result. *)

module Side : sig
  type t =
    | Fixed of Mode.Const.t
    (** As in declarations and builtins, whose arrows are not polymorphic in
        their modes. *)
    | Var of Mode.Var.t
end

type t =
  { arg : Side.t
  ; res : Side.t
  ; curry : Side.t
  (** On the locality axis only: how local the result may be where a tail call
      gives it further arguments. The closure of a partial application of a
      function of several parameters is never made then, so its arrows may leave
      it unbounded; any other function value returned must outlive the region
      the tail call ends, so is at most [regional]. *)
  }

(** The modes of an arrow written without them in a declaration. *)
val default_const : Mode.Const.t

(** The [curry] of an arrow written in a declaration: any function value may be
    held by what it types. *)
val default_curry : Side.t

val default : t
val fresh : Mode.Id_source.t -> rank:int -> t

(** Unifies the modes of two arrows. *)
val equate : Mode.Id_source.t -> t -> t -> (unit, Mode.Error.t) Result.t

(** Applies [f] to each variable, stopping at the first error. *)
val iter_result
  :  t
  -> f:(Mode.Var.t -> (unit, 'e) Result.t) @ local
  -> (unit, 'e) Result.t

val exists : t -> f:(Mode.Var.t -> bool) @ local -> bool

(** Keeps [t] (physically) if [f] keeps each variable. *)
val map : t -> f:(Mode.Var.t -> Mode.Var.t) -> t

(** [Var v] as is, a [Fixed] mode as a new variable fixed to it. *)
val side_var : Mode.Id_source.t -> rank:int -> Side.t -> Mode.Var.t

(** Whether the modes are the same: equal fixed modes, or the same variables. *)
val same : t -> t -> bool

(** Constrains the arrow of modes [lower] to be usable as one of modes [upper]:
    its argument takes at least what [upper]'s does, its result is at most
    [upper]'s. [curry] is equated. *)
val sub
  :  Mode.Id_source.t
  -> lower:t
  -> upper:t
  -> (unit, Mode.Error.t) Result.t
