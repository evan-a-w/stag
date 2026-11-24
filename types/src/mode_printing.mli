open! Import

(** The mode variables of the arrows a [Type.Printer] renders, related on the
    stage axis, and the constraints to print after them, simplified: variables
    equal by their constraints print as one, a variable fixed to a constant
    prints as it, constraints and bounds the others imply are left out, and a
    variable named once and in no constraint is elided (as an annotation's side
    without a mode is a fresh variable, it reads back as it). A [portable] lower
    bound of a variable only on arrows' results is left out too, as an arrow's
    result is never [const]. On provenance, only a variable's bounds print: a
    lower bound above [static], as [gc <= 'm0], and an upper bound below
    [external], as ['m0 <= gc]. *)

type t

val create : unit -> t

(** Counts an occurrence, on an arrow's argument or result or as what a
    pointer's cell holds, while counting what a rendering names. *)
val count : t -> Mode.Var.t -> side:[ `Arg | `Res | `Held ] -> unit

(** After counting a rendering: groups the variables counted so far. *)
val settle : t -> unit

(** How to print a side of an arrow with the variable: [None] if elided, else
    the mode, as in [" @ 'm0"]. *)
val side : t -> Mode.Var.t -> string option

(** The name of a variable a pointer's cell holds, never elided: [ptr[t]] reads
    back as [gc], not as a fresh variable. Its provenance is not fixed. *)
val held : t -> Mode.Var.t -> string

(** The constraints between the variables counted, as [["'m0 <= 'm1"]]. *)
val constraints : t -> string list
