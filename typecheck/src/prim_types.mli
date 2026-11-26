open! Import

val prim_type : Scope.t -> Prim.t -> Type.ty

(** What applying [var] allocates, beyond what it runs: the closure of a partial
    application of a prim, or the block of [Box.make] or [Ptr.make]. *)
val value_allocation
  :  Scope.t
  -> Typed.Var_ref.t
  -> Typed.Allocation.Kind.t option

(** The prim [expr] applies, its reference, and to how many arguments. *)
val prim_head : Typed.Expr.t -> (Prim.t * Typed.Expr.t * int) option

(** The mode of the closure of a prim of type [ty] given [args] arguments. *)
val prim_partial : Type.ty -> args:int -> Mode.Var.t option

(** Where a prim, [Box.make] or [Ptr.make] is named as a value: what it makes is
    on the heap. Where it heads an application, it isn't a value, and the
    application decides. *)
val value_sites : Scope.t -> Typed.Expr.t -> Typed.Expr.t

val intrinsic_type : Fresh.t -> Scope.t -> Intrinsic.t -> Type.ty
