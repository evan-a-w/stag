open! Import

(** The mode variables of the arrows a [Type.Printer] renders, and the
    constraints to print after them, exact: they read back ([Type_env]) as
    implying just what the variables' constraints do, every axis's bounds and
    relations, through the functions of [Mode.Written_morphism]. Variables equal
    on every axis print as one, and a constraint the others imply as read back
    is left out, as is a result's [portable] lower bound, which every result
    reads back with. A variable in no relation and fixed on every axis prints as
    those points, as a constant (unwritten axes the default) reads back. *)

type t

val create : unit -> t

(** Counts an occurrence, on an arrow's argument or result, or as a type's mode
    argument, which has no locality, while counting what a rendering names. *)
val count : t -> Mode.Var.t -> side:[ `Arg | `Res | `Mode_arg ] -> unit

(** After counting a rendering: groups the variables counted so far. *)
val settle : t -> unit

(** How to print the variable of an arrow side or mode argument: its name, or,
    if related to nothing and fixed on every axis it has, its points, which a
    written constant reads back as. *)
val side
  :  t
  -> Mode.Var.t
  -> [ `Name of string | `Atoms of (Mode.Axis.Any.t * int) list ]

(** The constraints between the variables counted, as [["'m0 <= 'm1"]]. *)
val constraints : t -> string list
