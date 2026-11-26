open! Import

(** The toplevels [expr] uses when run, rather than in the code it builds or at
    compile time. *)
val run_toplevels
  :  Typed.Expr.t
  -> depth:int
  -> f:(Typed.Ident.t -> unit)
  -> unit

(** Where computing a local value, [body], may use the value itself, which is
    not there until [body]'s is: a local is bound in its own definition, so that
    functions may recurse. As [check_initialization_cycles], a call is assumed
    to need everything the callee refers to. *)
val self_uses : body:Typed.Expr.t -> Typed.Ident.t -> Loc.t list

(** A toplevel's value is computed when first needed, after its component's
    functions are bound: it can't be if computing it may need it again. Calls
    are assumed to need everything the callee refers to. *)
val check_initialization_cycles : Program_env.t -> Typed.Toplevel.t list -> bool
