open! Core

type t =
  | Int of string
  | Char of char
  | String of string
  | Lident of string
  | Uident of string
  | Label of string
  | Tyvar of string
  | Weak_tyvar of string
  | True
  | False
  | Let
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
  | Dotdot
  | Underscore
  | Backslash
  | Quote_open
  | Quote_close
  | Splice
  | At
  | At_at
  | Eof
[@@deriving sexp_of, compare, equal]

let keyword = function
  | "true" -> Some True
  | "false" -> Some False
  | "let" -> Some Let
  | "in" -> Some In
  | "match" -> Some Match
  | "with" -> Some With
  | "type" -> Some Type
  | "of" -> Some Of
  | "module" -> Some Module
  | "struct" -> Some Struct
  | "end" -> Some End
  | _ -> None
;;

let to_string = function
  | Int s -> s
  | Char c -> sprintf "'%s'" (Char.escaped c)
  | String s -> sprintf "\"%s\"" (String.escaped s)
  | Lident s | Uident s -> s
  | Label s -> "`" ^ s
  | Tyvar s -> "'" ^ s
  | Weak_tyvar s -> "'_" ^ s
  | True -> "true"
  | False -> "false"
  | Let -> "let"
  | In -> "in"
  | Match -> "match"
  | With -> "with"
  | Type -> "type"
  | Of -> "of"
  | Module -> "module"
  | Struct -> "struct"
  | End -> "end"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbracket -> "["
  | Rbracket -> "]"
  | Lbrace -> "{"
  | Rbrace -> "}"
  | Comma -> ","
  | Semi -> ";"
  | Colon -> ":"
  | Equal -> "="
  | Bar -> "|"
  | Arrow -> "->"
  | Star -> "*"
  | Dot -> "."
  | Dotdot -> ".."
  | Underscore -> "_"
  | Backslash -> "\\"
  | Quote_open -> ".<"
  | Quote_close -> ">."
  | Splice -> ".~"
  | At -> "@"
  | At_at -> "@@"
  | Eof -> "end of file"
;;
