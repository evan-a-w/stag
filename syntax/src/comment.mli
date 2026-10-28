open! Import

(** A comment of a source file, which the lexer keeps beside the tokens, so
    that a formatter can put it back. *)

module Placement : sig
  type t =
    | Own_line of
        { blank_before : bool
        ; blank_after : bool
        }
    (** No token on its last line after it, nor on its first line before it
        but one that does not end code (eg. [=] or [(]), so it is about what
        follows. The blank lines are those between it and the previous or next
        token or comment. *)
    | Trailing
    (** After a token on its line that may end code, or after such a comment
        ending on its line, with no token after it on its line. *)
    | Inline (** With a token after it on its line. *)
  [@@deriving sexp_of, equal]
end

type t =
  { text : string (** As written, delimiters and nested comments included. *)
  ; loc : Loc.t
  ; placement : Placement.t
  ; next_token : int (** The index of the token after it, in the file's. *)
  }
[@@deriving sexp_of]
