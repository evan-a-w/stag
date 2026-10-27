open! Core

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
  | Let_expect_test (** [let%expect_test] *)
  | In
  | Match
  | With
  | Type
  | Of
  | Module
  | Struct
  | End
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
  | Star
  | Dot
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
