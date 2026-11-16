open! Import

(** The bindings of one activation: a closure's captures and arguments, and the
    locals bound while evaluating its body (or the toplevels). Idents are
    unique, so a binding is never shadowed or removed. Lookups fall back to
    [fallback] (the toplevels). *)

type 'value t

val create : unit -> _ t

(** Shares the slots of [t], but bindings added to either are not seen by the
    other. *)
val copy : 'value t -> 'value t

val mem : _ t -> Typed.Ident.t -> bool

(** The slot of [ident], empty until [set]. *)
val find_slot
  :  ?fallback:'value t
  -> 'value t
  -> Typed.Ident.t
  -> 'value option ref option

(** Errors if [ident] is unbound, or declared but not yet [set]. *)
val lookup
  :  ?fallback:'value t
  -> 'value t
  -> ident:Typed.Ident.t
  -> 'value Or_error.t

(** Binds [ident] to an empty slot, to be [set] later, eg. for each member of a
    recursive binding before evaluating their bodies. *)
val declare : _ t -> ident:Typed.Ident.t -> unit

(** Fills the slot of [ident], binding it first if needed. *)
val set : 'value t -> ident:Typed.Ident.t -> value:'value -> unit

(** A new env sharing the slots of [captures] with [t], so slots filled later
    are seen by both. *)
val of_captures
  :  ?fallback:'value t
  -> 'value t
  -> captures:Typed.Ident.Hash_set.t
  -> 'value t Or_error.t
