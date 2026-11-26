open! Import

(** How errors in some code are reported: in the component it is part of, and as
    in the module and the splice it was written in. *)
type t =
  { component : Component.t
  ; module_ : Module_scope.t
  ; syntax_splice : Syntax_splice.t option
  ; generated : Generated.t option
  }

val wrap_error : t -> Typecheck_error.t -> Typecheck_error.t
