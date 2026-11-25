open! Import

(** A name in the source resolved to the declaration of a type, constructor or
    field, for tools to follow (eg. the language server's go to definition). *)

type t =
  { name : string (** Canonical. *)
  ; declaration : Loc.t (** Of its declared name. *)
  }
