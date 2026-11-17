open! Import

(** The bindings of one activation: a closure's captures and arguments, and the
    locals bound while evaluating its body (or the toplevels). Idents are
    unique, so a binding is never shadowed or removed. Lookups fall back to the
    env [t] [extend]s, then to [fallback] (the toplevels). *)

type 'value t

module Slot : sig
  (** [Null] until set. Shared by the envs of the closures capturing it. *)
  type 'value t = { mutable value : 'value or_null }
end

val create : unit -> _ t

(** An env whose own bindings are seen only by it, falling back to [t]'s. [t]
    gets no new bindings afterwards, as they would be seen through the new env
    (its slots may still be set): [declare] or [set] binding in it raise. *)
val extend : 'value t -> 'value t

val mem : _ t -> Typed.Ident.t -> bool

(** The slot of [ident], empty until [set]. *)
val find_slot
  :  'value t
  -> fallback:'value t
  -> Typed.Ident.t
  -> 'value Slot.t or_null
[@@zero_alloc]

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
  :  'value t
  -> fallback:'value t
  -> captures:Typed.Ident.Hash_set.t
  -> 'value t Or_error.t
