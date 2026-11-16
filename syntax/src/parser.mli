open! Import

module Error : sig
  type t =
    | Lex of Lexer.Error.t
    | Syntax of
        { loc : Loc.t
        ; message : string
        }
  [@@deriving sexp_of]

  val loc : t -> Loc.t

  (** Without the location. *)
  val message : t -> string

  (** [file:line:col-col: message]. *)
  val to_string : t -> string
end

(** The toplevels of a file. *)
val program : file:string -> string -> (Syntax.Program.t, Error.t) Result.t

(** The toplevels of a file, parsed past errors, and the errors, in order. A
    toplevel with an error is dropped, or, if it parsed far enough to declare
    something, kept with [Expr.Error] for the rest: a [let] with its name, a
    [type] [Malformed] with the members that parsed, a [module] or [include] as
    a splice of [Expr.Error], a [struct] with the toplevels in it. *)
val program_recovering
  :  file:string
  -> string
  -> Syntax.Program.t * Error.t list

(** How many times the parser has looked at a token so far, to test that it
    takes linear time. *)
val steps : unit -> int

(** Also the tokens and the comments, in order. *)
val program_with_comments
  :  file:string
  -> string
  -> ( Syntax.Program.t * Lexer.Located.t array * Comment.t list
       , Error.t )
       Result.t

val expr : file:string -> string -> (Syntax.Expr.t, Error.t) Result.t
val pattern : file:string -> string -> (Syntax.Pattern.t, Error.t) Result.t
val type_expr : file:string -> string -> (Syntax.Type_expr.t, Error.t) Result.t
