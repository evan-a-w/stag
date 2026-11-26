open! Import

(** The mode of a value, and those of its by-value components where they are
    known: the parts of a tuple, record or constructor each keep their own mode
    until the value is used as a whole, at the join of them. *)

(** Where a component is in its value. *)
module Step : sig
  type t =
    | Index of int (** Of a tuple. *)
    | Field of string (** Of a nominal record. *)
    | Arg of string * int (** Of a constructor's arguments. *)
    | Label of string (** Of a polymorphic record. *)
    | Tag of string (** The argument of a polymorphic variant. *)
  [@@deriving sexp_of, equal]
end

type t = private
  { whole : Mode.Var.t
  ; parts : (Step.t * t) list option
  (** Each at least the mode of that component, if they are known: then a
      component not there (of another constructor) is not in the value. *)
  }
[@@deriving sexp_of]

val known : whole:Mode.Var.t -> (Step.t * t) list -> t

(** Of a value whose components' modes are not known. *)
val opaque : Mode.Var.t -> t

(** [whole]'s, its components at least [component] of [t]'s. *)
val map : t -> whole:Mode.Var.t -> component:(Mode.Var.t -> Mode.Var.t) -> t

(** Of a value that is one of [ts], at [whole]: each component at [component] of
    what it may be. *)
val join
  :  t list
  -> whole:Mode.Var.t
  -> component:(Mode.Var.t list -> Mode.Var.t)
  -> t

(** The component at [step], if its mode is known. *)
val part : t -> Step.t -> t option

(** The path from a value to a part of it bound by a pattern, outermost first. *)
module Path : sig
  module Step : sig
    type nonrec t =
      | Part of Step.t * Modality.t
      (** A component, held through the modality. *)
      | Through of Modality.t (** What it points to. *)
  end

  type t = Step.t list
end

(** The part at the end of [path], as far as the modes of its components are
    known: the mode of the component reached, what is still to be read from it
    through, and its own [t] if [path] was followed to its end. *)
val follow : t -> Path.t -> Mode.Var.t * Modality.t * t option

(** Of each of its modes: of a binding not generalized. *)
val lower_rank : t -> rank:int -> unit
