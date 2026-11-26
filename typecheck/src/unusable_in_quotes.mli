open! Import

(** The free variables of quotes that a binding of their name, unusable there,
    did not bind: to explain an error if the code is spliced where the name is
    unbound. Each is of the entry its quote's context got for it, so is found
    only by code of that quote. *)
type t

val create : unit -> t

(** [entry] is the type of the entry of the quote's context. *)
val record
  :  t
  -> name:string
  -> entry:Type.ty
  -> Typecheck_error.Unusable_in_quote.t
  -> unit

(** Of the context entry of type [entry] of spliced code. *)
val find
  :  t
  -> name:string
  -> entry:Type.ty
  -> Typecheck_error.Unusable_in_quote.t option
