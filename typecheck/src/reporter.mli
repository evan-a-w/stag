open! Import

(** How errors in some code are reported: in the component it is part of, and as
    in the module and the splice it was written in. *)
type t =
  { component : Component.t
  ; module_ : Module_scope.t
  ; syntax_splice : Syntax_splice.t option
  ; generated : Generated.t option
  ; expansion : bool
  (** The code is the expansion of a splice of code checked as such: its quotes
      and the splice's captures made the uses of toplevels being checked that
      its own repeat, so an error of these is shown only if no other of its
      component is (see [Program_env.report_use]). *)
  }

val wrap_error : t -> Typecheck_error.t -> Typecheck_error.t
