open! Import

(** A type argument that is a mode: a type's mode parameter, as in
    [cell[string, external]], or what a [ptr]'s cell holds, as in
    [ptr[t @@ external]]. As part of the type, it is unified exactly: every
    alias of a value of the type agrees on it, whatever the aliases' own modes. *)
type t =
  | Fixed of Mode.Const.t
  | Var of Mode.Var.t

(** The axes not written in a mode argument: [nonportable gc global], as for a
    declared arrow. *)
val default : Mode.Const.t

(** [ptr[t]]'s: [default]. *)
val gc : t

(** Equates the two, on stage and provenance: a mode argument has no locality. *)
val equate : Mode.Id_source.t -> t -> t -> (unit, Mode.Error.t) Result.t

val iter_result
  :  t
  -> f:(Mode.Var.t -> (unit, 'e) Result.t) @ local
  -> (unit, 'e) Result.t

val exists : t -> f:(Mode.Var.t -> bool) @ local -> bool

(** Keeps [t] (physically) if [f] keeps the variable. *)
val map : t -> f:(Mode.Var.t -> Mode.Var.t) -> t

(** Equal constants (on stage and provenance), or the same variable. *)
val same : t -> t -> bool

(** The constant it is fixed to: [Fixed], or a variable whose bounds meet on
    stage and provenance. *)
val known : t -> Mode.Const.t option

val sexp_of_t : t -> Sexp.t

(** [lower <= upper], on stage and provenance: for a parameter that is
    covariant. *)
val sub
  :  Mode.Id_source.t
  -> lower:t
  -> upper:t
  -> (unit, Mode.Error.t) Result.t
