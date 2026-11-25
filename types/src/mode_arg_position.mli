open! Import

(** How a mode argument's variable occurs in a type, relative to the innermost
    position around it that is unified exactly (a [ptr]'s pointee, an invariant
    parameter, a record): where a value of the type, or of that part, may be
    used at another type with this argument greater ([Covariant]) or lesser
    ([Contravariant]), or only the same ([Held]: the argument is invariant
    itself, as what a [ptr]'s cell holds is). *)
type t =
  | Held
  | Covariant
  | Contravariant
[@@deriving equal, sexp_of]

(** Each mode argument's variables in [ty], with how they occur; [con] gives
    types' parameters' variances. A variable occurring several times is given
    once per occurrence. *)
val iter : con:Variance.of_con -> Type.ty -> f:(Mode.Var.t -> t -> unit) -> unit
