open! Import

type t =
  | Int of int
  | Char of char
  | String of string
  | Lident of string
  | Uident of string
  | Label of string (** [`A], [`x], without the backtick. *)
  | Tyvar of string (** ['a], without the quote. *)
  | Weak_tyvar of string (** ['_a], as ["a"]. Printed but never written. *)
  | True
  | False
  | Let
  | Rec
  | Nonrec
  | Mut
  | Stack_ (** [stack_] *)
  | Exclave_ (** [exclave_] *)
  | Region_ (** [region_] *)
  | Global_ (** [global_] *)
  | Forget (** [forget] *)
  | Let_expect_test (** [let%expect_test] *)
  | Let_bench (** [let%bench] *)
  | In
  | Match
  | With
  | Fun
  | Exception
  | External
  | Type
  | Of
  | Module
  | Struct
  | Include
  | End
  | Module_inline (** [module%inline] *)
  | Include_inline (** [include%inline] *)
  | End_inline (** [end%inline] *)
  | Lparen
  | Rparen
  | Lbracket
  | Expect_open (** [[%expect] *)
  | Rbracket
  | Lbrace
  | Rbrace
  | Comma
  | Semi
  | Colon
  | Equal
  | Bar
  | Arrow
  | Larrow
  | Star
  | Dot
  | Dot_star
  | Root (** [.] starting a path from the root scope, as [.M.x] *)
  | Dot_lbrace (** [.{], substitutions of a splice *)
  | Dotdot
  | Underscore
  | Backslash
  | Quote_open (** [.<] *)
  | Quote_close (** [>.] *)
  | Less
  | Greater
  | Splice (** [.~] *)
  | At
  | At_at
  | Eof
[@@deriving sexp_of, compare, equal]

val keyword : string -> t option

(** The token as written, for error messages. *)
val to_string : t -> string

(** Whether a comment after the token, ending its line, is about the code before
    it, rather than what follows (as after [=] or [(]). *)
val ends_code : t -> bool

(** Whether it ends an atom, so that a [.] glued after it is a projection, not
    the start of a root path. *)
val ends_atom : t -> bool

(** Whether the token only ends or continues what is before it, so that a
    comment before it on its line is about that, eg. [)], [;] or [in]. *)
val follows : t -> bool
