open! Import

(** Errs at the second occurrence of the first name repeated. *)
val check
  :  Syntax.Name.t list
  -> error:(string -> Typecheck_error.Kind.t)
  -> (unit, Typecheck_error.t) Result.t

(** Reports each repeated occurrence of a name. *)
val report
  :  Scope.t
  -> Syntax.Name.t list
  -> error:(string -> Typecheck_error.Kind.t)
  -> unit
