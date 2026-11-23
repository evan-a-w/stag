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
  }

(** The modes of an arrow written without them in a declaration. *)
val default_const : Mode.Const.t

val default : t
val fresh : Mode.Id_source.t -> rank:int -> t

(** Unifies the modes of two arrows. *)
val equate : Mode.Id_source.t -> t -> t -> (unit, Mode.Error.t) Result.t

val vars : t -> Mode.Var.t list

(** Keeps [t] (physically) if [f] keeps each variable. *)
val map : t -> f:(Mode.Var.t -> Mode.Var.t) -> t

(** [Var v] as is, a [Fixed] mode as a new variable fixed to it. *)
val side_var : Mode.Id_source.t -> rank:int -> Side.t -> Mode.Var.t
