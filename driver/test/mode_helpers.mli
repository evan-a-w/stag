open! Import

(** The signature of a program's toplevels, past the prelude's, with modes. *)
val signature : string -> (string, string list) Result.t

(** Annotates two copies of each toplevel value with its printed type: one bound
    to the value ([x_copy]), and one to [raise] ([x_alone]), whose type is just
    what the annotation reads back as. Prints any whose type then prints
    differently from the value's: the printed type implies just what the value's
    does. Prints the signature with [`Signature], the [_copy] annotations with
    [`Copies]. *)
val round_trip : string -> show:[ `Signature | `Copies ] -> unit
