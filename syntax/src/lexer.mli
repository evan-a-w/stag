open! Import

module Located : sig
  type t =
    { token : Token.t
    ; loc : Loc.t
    }
  [@@deriving sexp_of]
end

module Error : sig
  module Kind : sig
    type t =
      | Illegal_character of char
      | Unterminated_comment
      | Unterminated_string
      | Unterminated_char
      | Illegal_escape of string
      | Invalid_int_literal of string
      | Int_literal_out_of_range of string
    [@@deriving sexp_of]
  end

  type t =
    { kind : Kind.t
    ; loc : Loc.t
    }
  [@@deriving sexp_of]

  (** Without the location. *)
  val message : t -> string

  (** Whether its lexeme was skipped, without making a token: an illegal
      character, or an unterminated comment. *)
  val skipped : t -> bool

  val to_string : t -> string
end

(** The tokens of a file, always ending in [Eof] (which is zero width, at the
    end of the file). *)
val tokenize : file:string -> string -> (Located.t array, Error.t) Result.t

(** The tokens, and the comments between them, in order. *)
val tokenize_with_comments
  :  file:string
  -> string
  -> (Located.t array * Comment.t list, Error.t) Result.t

module Recovered : sig
  type t =
    { tokens : Located.t array (** Without the lexemes with errors. *)
    ; comments : Comment.t list
    ; errors : Error.t list (** In order. *)
    }
end

(** Lexes past errors: skips illegal characters, and ends unterminated strings
    and comments at the end of the file. *)
val tokenize_recovering : file:string -> string -> Recovered.t

module Prefix : sig
  type t =
    { tokens : Located.t array (** Ending in [Eof], at the error if any. *)
    ; comments : Comment.t list
    ; error : Error.t option
    }
end

(** The tokens and comments of the longest prefix of the file that lexes, which
    ends at the first error, if any. *)
val tokenize_prefix : file:string -> string -> Prefix.t
