open! Core

(** Simplifies the types of entries in a scheme being generalized. An entry
    type that is a variable [a] used only as the type of entries whose
    presence makes [p] [Present], with an equality [p => a = t], is replaced
    by [t]: where [p] is [Present] they are equal, and where it is [Absent]
    so are those entries, whose types don't matter. This keeps instances from
    piling up equivalent bounds. *)

(** [is_generalized] says which variables are generalized: [tys] are all the
    types generalized together, [presences] and [rows] the generalized
    variables with bounds, and [kept] the bounds kept with them. *)
val simplify
  :  is_generalized:(Type.id -> bool)
  -> tys:Type.ty list
  -> presences:(Type.presence, Type.presence_bound) Type.Var.t list
  -> rows:(Type.row, Type.bound) Type.Var.t list
  -> kept:Type.kept list
  -> unit

(** Removes the bounds that are the same as earlier ones. *)
val dedup
  :  presences:(Type.presence, Type.presence_bound) Type.Var.t list
  -> rows:(Type.row, Type.bound) Type.Var.t list
  -> unit

val dedup_kept : Type.kept list -> Type.kept list
