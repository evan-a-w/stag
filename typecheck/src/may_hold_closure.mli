open! Import

(** Whether a value of type [ty] may hold a closure, so a closure capturing a
    mutable local: a function, or data with one inside (through declared types'
    definitions), or a type not yet known. Code is data: it only embeds
    persistent values. *)
val ty : Type_env.t -> Type.ty -> bool
