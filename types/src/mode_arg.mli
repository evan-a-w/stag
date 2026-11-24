open! Import

(** A type argument that is a mode: for now, the provenance a [ptr]'s cell may
    hold, as in [ptr[t @@ external]]. Only the provenance axis of a variable is
    meaningful here. As part of the type, it is unified exactly: every alias of
    the cell agrees on it, whatever the aliases' own modes. *)
type t =
  | Fixed of Mode.Provenance.t
  | Var of Mode.Var.t

(** The default, written [ptr[t]]. *)
val gc : t

val fresh : Mode.Id_source.t -> rank:int -> t

(** Equates the provenances of the two. *)
val equate : Mode.Id_source.t -> t -> t -> (unit, Mode.Error.t) Result.t

(** [Var v] as is, a [Fixed] one as a new variable whose provenance is fixed to
    it. *)
val var : Mode.Id_source.t -> t -> Mode.Var.t

val iter_result
  :  t
  -> f:(Mode.Var.t -> (unit, 'e) Result.t) @ local
  -> (unit, 'e) Result.t

val exists : t -> f:(Mode.Var.t -> bool) @ local -> bool

(** Keeps [t] (physically) if [f] keeps the variable. *)
val map : t -> f:(Mode.Var.t -> Mode.Var.t) -> t

(** Equal fixed provenances, or the same variable. *)
val same : t -> t -> bool

(** The provenance it is fixed to: [Fixed], or a variable whose bounds meet. *)
val known : t -> Mode.Provenance.t option

val sexp_of_t : t -> Sexp.t
