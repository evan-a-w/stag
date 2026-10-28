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
