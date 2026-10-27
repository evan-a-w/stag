open! Import

(** The kinds of names in syntax, by the tokens they are written as. *)

type t =
  | Lower (** A lowercase identifier, not a keyword. *)
  | Binder (** As [Lower], or [_]: a let's name or a parameter. *)
  | Upper
  | Label (** Written after a backtick. *)
  | Tyvar (** Written after a quote, not weak. *)
[@@deriving sexp_of]

(** Eg. ["a lowercase identifier"]. *)
val describe : t -> string

(** Whether [text] is exactly one token of the kind as the lexer reads it, so
    printed syntax would parse back. *)
val accepts : t -> string -> bool
