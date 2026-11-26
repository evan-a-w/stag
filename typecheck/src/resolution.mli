open! Import

(** Where a name is used, given its entry if it has one. *)
type t =
  | Local of Env.Entry.t
  (** Usable, and bound in the enclosing quote if there is one. *)
  | Outside_quote of
      { quote : Scope.quote
      ; usable : Env.Entry.t option
      ; unusable : Env.Entry.t option
      }
  (** Not bound in the enclosing [quote], so a free variable of it, unless used
      directly while [usable]. Splices never capture the name. [unusable] is the
      entry of the name, if it has one that isn't [usable]. *)
  | By_symbol of Env.Entry.t
  (** A toplevel from outside the enclosing quote whose value can't be embedded:
      the code refers to the toplevel itself, by symbol. *)
  | Unusable of Typecheck_error.Kind.t (** The error if the name is used. *)

(** [spliced]: [name] is a free variable of code spliced here, not written here.
    Such code is of the quote's level, so a local of an earlier level doesn't
    bind it, and it stays a free variable of the quote. *)
val of_entry
  :  ?spliced:bool
  -> Scope.t
  -> name:string
  -> Env.Entry.t option
  -> t
