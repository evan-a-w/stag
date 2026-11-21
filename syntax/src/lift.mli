include Lift_intf.Lift

(** The prelude's [Stag] constructors whose one argument is a [ptr], with how
    many components (of a tuple) it points to. *)
val indirect_constructors : (string * int) list

val indirect_arity : string -> int option
