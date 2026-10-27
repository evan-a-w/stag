open! Core

module Error : sig
  type t =
    | Lex of Lexer.Error.t
    | Syntax of
        { loc : Loc.t
        ; message : string
        }
  [@@deriving sexp_of]

  val loc : t -> Loc.t

  (** [file:line:col-col: message]. *)
  val to_string : t -> string
end

(** The toplevels of a file. *)
val program : file:string -> string -> (Syntax.Program.t, Error.t) Result.t

val expr : file:string -> string -> (Syntax.Expr.t, Error.t) Result.t
val pattern : file:string -> string -> (Syntax.Pattern.t, Error.t) Result.t
val type_expr : file:string -> string -> (Syntax.Type_expr.t, Error.t) Result.t
