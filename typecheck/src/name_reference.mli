open! Import

(** A name in the source resolved to the declaration of a type, constructor or
    field, for tools to follow (eg. the language server's go to definition). *)

module Kind : sig
  type t =
    | Type
    | Constructor
    | Field
  [@@deriving sexp_of]
end

type t =
  { kind : Kind.t
  ; name : string (** Canonical. *)
  ; declaration : Loc.t (** Of its declared name: none for builtin types. *)
  }
[@@deriving sexp_of]
