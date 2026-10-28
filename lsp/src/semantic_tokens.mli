open! Import

(** [textDocument/semanticTokens]: tokens classified by the lexer, then by
    where the parser puts names, then by their types if typechecked. *)

module Token_type : sig
  type t =
    | Namespace
    | Type
    | Type_parameter
    | Enum_member (** Constructors, exceptions and polymorphic variant tags. *)
    | Property (** Record fields. *)
    | Variable
    | Function
    | Parameter
    | Keyword
    | Comment
    | String
    | Number
    | Operator
    | Macro (** Staging: quotes [.< >.], splices [.~] and [.{]. *)
  [@@deriving sexp_of, enumerate]
end

module Token : sig
  type t =
    { line : int
    ; character : int (** In UTF-16 code units, as [length]. *)
    ; length : int
    ; token_type : Token_type.t
    ; declaration : bool
    }
  [@@deriving sexp_of]
end

val legend : Lsp_types.SemanticTokensLegend.t

(** The tokens of [text] (of [file], as named in locations), each on one
    line, in order. No tokens if it doesn't lex. [occurrences] are those of the
    syntax, if it parses. [is_function offset] is whether the name at
    [offset] is of a function. *)
val compute
  :  Text.t
  -> file:string
  -> occurrences:Syntax_names.Occurrence.t list
  -> is_function:(int -> bool)
  -> Token.t list

(** As relative positions, for [SemanticTokens.data]. *)
val encode : Token.t list -> int array

(** The inverse of [encode], for tests. *)
val decode : int array -> Token.t list
