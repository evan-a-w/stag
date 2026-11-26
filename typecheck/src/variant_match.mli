open! Import

(** Closing of the scrutinee and residuals of catch-alls in matches on
    polymorphic variants. *)

(** The tags handled irrefutably by the cases so far, in order, each once. *)
type t

val empty : t

(** A top level variable pattern matches what the handled tags' cases have not:
    the scrutinee without their first entries. *)
val pattern_type
  :  Fresh.t
  -> Scope.t
  -> t
  -> pattern:Source.Pattern.t
  -> scrutinee:Type.ty
  -> Typed.Pattern.t * Pattern_var.t list

val add_case : t -> Typed.Pattern.t -> t

(** Without a top level catch-all, a variant scrutinee may only have the tags of
    the cases, and an empty match's none, unless the scrutinee is already known
    to be an empty nominal type. *)
val close
  :  Fresh.t
  -> Scope.t
  -> scrutinee:Type.ty
  -> loc:Loc.t
  -> Typed.Pattern.t list
  -> (unit, Typecheck_error.t) Result.t
