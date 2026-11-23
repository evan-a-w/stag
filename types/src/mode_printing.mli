open! Import

(** The mode variables of the arrows a [Type.Printer] renders, on the stage axis
    (the only one checked so far), and the constraints to print after them,
    simplified: variables equal by their constraints print as one, a variable
    fixed to a constant prints as it, constraints and bounds the others imply
    are left out, and a variable named once and in no constraint is elided (as
    an annotation's side without a mode is a fresh variable, it reads back as
    it). *)

type t

val create : unit -> t

(** Counts an occurrence, while counting what a rendering names. *)
val count : t -> Mode.Var.t -> unit

(** After counting a rendering: groups the variables counted so far. *)
val settle : t -> unit

(** How to print a side of an arrow with the variable: [None] if elided, else
    the mode, as in [" @ 'm0"]. *)
val side : t -> Mode.Var.t -> string option

(** The constraints between the variables counted, as [["'m0 <= 'm1"]]. *)
val constraints : t -> string list
