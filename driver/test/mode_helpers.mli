open! Import

(** The signature of a program's toplevels, past the prelude's, with modes. *)
val signature : string -> (string, string list) Result.t

(** Annotates two copies of each toplevel value with its printed type: one bound
    to the value ([x_copy]), and one to a function that never returns
    ([x_alone]), whose type is just what the annotation reads back as. Prints
    any whose modes then imply other than the value's do (each axis's bounds,
    and how far raising one side raises the others, with the [curry]s, which
    aren't printed, left out): the printed type implies just what the value's
    does. One it can't compare (weak, with [where] or [as], or a mutable global)
    is listed as such. Prints the signature with [`Signature], the [_copy]
    annotations with [`Copies], neither with [`Quiet] (which also leaves out
    those it can't compare). *)
val round_trip : string -> show:[ `Signature | `Copies | `Quiet ] -> unit

val checked : string -> (Signature.Item.t list, string list) Result.t

(** Prints the signature of [source], as [signature], then where each allocation
    of its toplevels is made (see [Typed.Allocation]), or its errors. *)
val allocations : string -> unit

(** As [allocations], each with where it is in the source. *)
val placements
  :  string
  -> (Signature.Item.t list * (Loc.t * string) list, string list) Result.t

(** Prints the errors of [source], if any, then where each allocation of what
    checked of its toplevels is made, as [allocations] does. *)
val allocations_despite_errors : string -> unit
