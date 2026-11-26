open! Import

(** The free variables of quotes that a binding of their name, unusable there,
    did not bind: to explain an error if the code is spliced where the name is
    unbound. Each is kept on the type of the entry its quote's context got for
    it (and its instances, and what it is bound to), so is found only by code of
    that quote. *)

(** [entry] is the type of the entry of the quote's context. An entry whose type
    is not a variable gets none. *)
val record
  :  entry:Type.ty
  -> name:string
  -> Typecheck_error.Unusable_in_quote.t
  -> unit

(** Of the context entry of type [entry] of spliced code, for [name]. *)
val find
  :  entry:Type.ty
  -> name:string
  -> Typecheck_error.Unusable_in_quote.t option
