open! Import

(** A type argument that is a mode: a type's mode parameter, as in
    [cell[string, external]], or what a [ptr]'s cell holds, as in
    [ptr[t @@ external]]. As part of the type, it is unified exactly: every
    alias of a value of the type agrees on it, whatever the aliases' own modes.
    It relates only stage and provenance, the axes a part or a cell reads: a
    variable's locality is left as it is. *)

module Const : sig
  type t =
    { stage : Mode.Stage.t
    ; provenance : Mode.Provenance.t
    }
  [@@deriving sexp_of, equal]

  (** The axes not written in a mode argument: [nonportable gc], as for a
      declared arrow. *)
  val default : t

  (** Its axes of a mode. *)
  val of_mode : Mode.Const.t -> t
end

type t =
  | Fixed of Const.t
  | Var of Mode.Var.t

(** [ptr[t]]'s: [Const.default]. *)
val gc : t

(** Equates the two, on stage and provenance. *)
val equate : t -> t -> (unit, Mode.Error.t) Result.t

val iter_result
  :  t
  -> f:(Mode.Var.t -> (unit, 'e) Result.t) @ local
  -> (unit, 'e) Result.t

val exists : t -> f:(Mode.Var.t -> bool) @ local -> bool

(** Keeps [t] (physically) if [f] keeps the variable. *)
val map : t -> f:(Mode.Var.t -> Mode.Var.t) -> t

(** Equal constants, or the same variable. *)
val same : t -> t -> bool

(** The constant it is fixed to: [Fixed], or a variable whose bounds meet on
    stage and provenance. *)
val known : t -> Const.t option

val sexp_of_t : t -> Sexp.t
