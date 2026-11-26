open! Import

(** The toplevels [expr] uses when run, rather than in the code it builds or at
    compile time. *)
val run_toplevels
  :  Typed.Expr.t
  -> depth:int
  -> f:(Typed.Ident.t -> unit)
  -> unit

(** A toplevel's value is computed when first needed, after its component's
    functions are bound: it can't be if computing it may need it again. Calls
    are assumed to need everything the callee refers to. *)
val check_initialization_cycles : Program_env.t -> Typed.Toplevel.t list -> bool
