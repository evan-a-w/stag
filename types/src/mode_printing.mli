open! Import

(** The mode variables of the arrows a [Type.Printer] renders, and the
    constraints to print after them, exact: they read back ([Type_env]) as
    implying just what the variables' constraints do, every axis's bounds and
    relations, through the functions of [Mode.Written_morphism]. Variables equal
    on every axis print as one, and a constraint the others imply as read back
    is left out, as is a result's [portable] lower bound, which every result
    reads back with. A variable in no relation, named once or fixed on every
    axis, prints as the atoms of its fixed axes (none: elided), which read back
    as it. *)

type t

val create : unit -> t

(** Counts an occurrence, on an arrow's argument or result, or as a type's mode
    argument, which has no locality, while counting what a rendering names. *)
val count : t -> Mode.Var.t -> side:[ `Arg | `Res | `Mode_arg ] -> unit

(** After counting a rendering: groups the variables counted so far. *)
val settle : t -> unit

(** How to print a side of an arrow with the variable: [None] if elided, else
    the mode, as in [" @ 'm0"]. *)
val side : t -> Mode.Var.t -> string option

(** A variable of a mode argument: its name, or, if fixed and related to
    nothing, its fixed points on stage and provenance. *)
val held
  :  t
  -> Mode.Var.t
  -> [ `Name of string | `Atoms of (Mode.Axis.Any.t * int) list ]

(** The constraints between the variables counted, as [["'m0 <= 'm1"]]. *)
val constraints : t -> string list
