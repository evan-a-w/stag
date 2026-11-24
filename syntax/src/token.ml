open! Import

type t =
  | Int of int
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
  | Rec
  | Mut
  | Stack_ (** [stack_] *)
  | Exclave_ (** [exclave_] *)
  | Global_ (** [global_] *)
  | Forget
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
  | Root
  | Dot_lbrace (** [.{], substitutions of a splice *)
  | Dotdot
  | Underscore
  | Backslash
  | Quote_open
  | Quote_close
  | Less
  | Greater
  | Splice
  | At
  | At_at
  | Eof
[@@deriving sexp_of, compare, equal]

let keyword = function
  | "true" -> Some True
  | "false" -> Some False
  | "let" -> Some Let
  | "rec" -> Some Rec
  | "mut" -> Some Mut
  | "stack_" -> Some Stack_
  | "exclave_" -> Some Exclave_
  | "global_" -> Some Global_
  | "forget" -> Some Forget
  | "in" -> Some In
  | "match" -> Some Match
  | "with" -> Some With
  | "fun" -> Some Fun
  | "exception" -> Some Exception
  | "external" -> Some External
  | "type" -> Some Type
  | "of" -> Some Of
  | "module" -> Some Module
  | "struct" -> Some Struct
  | "include" -> Some Include
  | "end" -> Some End
  | _ -> None
;;

let to_string = function
  | Int n -> Int.to_string n
  | Char c -> sprintf "'%s'" (Char.escaped c)
  | String s -> sprintf "\"%s\"" (String.escaped s)
  | Lident s | Uident s -> s
  | Label s -> "`" ^ s
  | Tyvar s -> "'" ^ s
  | Weak_tyvar s -> "'_" ^ s
  | True -> "true"
  | False -> "false"
  | Let -> "let"
  | Rec -> "rec"
  | Mut -> "mut"
  | Stack_ -> "stack_"
  | Exclave_ -> "exclave_"
  | Global_ -> "global_"
  | Forget -> "forget"
  | Let_expect_test -> "let%expect_test"
  | Let_bench -> "let%bench"
  | In -> "in"
  | Match -> "match"
  | With -> "with"
  | Fun -> "fun"
  | Exception -> "exception"
  | External -> "external"
  | Type -> "type"
  | Of -> "of"
  | Module -> "module"
  | Struct -> "struct"
  | Include -> "include"
  | End -> "end"
  | Module_inline -> "module%inline"
  | Include_inline -> "include%inline"
  | End_inline -> "end%inline"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbracket -> "["
  | Expect_open -> "[%expect"
  | Rbracket -> "]"
  | Lbrace -> "{"
  | Rbrace -> "}"
  | Comma -> ","
  | Semi -> ";"
  | Colon -> ":"
  | Equal -> "="
  | Bar -> "|"
  | Arrow -> "->"
  | Larrow -> "<-"
  | Star -> "*"
  | Dot -> "."
  | Dot_star -> ".*"
  | Root -> "."
  | Dot_lbrace -> ".{"
  | Dotdot -> ".."
  | Underscore -> "_"
  | Backslash -> "\\"
  | Quote_open -> ".<"
  | Quote_close -> ">."
  | Less -> "<"
  | Greater -> ">"
  | Splice -> ".~"
  | At -> "@"
  | At_at -> "@@"
  | Eof -> "end of file"
;;

let ends_code : t -> bool = function
  | Lparen
  | Lbracket
  | Lbrace
  | Dot_lbrace
  | Expect_open
  | Quote_open
  | Splice
  | Equal
  | Arrow
  | Larrow
  | Colon
  | Bar
  | Of
  | Star
  | Dot
  | Root
  | Backslash
  | Less
  | Let
  | Let_expect_test
  | Let_bench
  | Match
  | Fun
  | Exception
  | External
  | Type
  | Module
  | Include
  | Module_inline
  | Include_inline
  | Stack_
  | Exclave_
  | Global_
  | Forget
  | At
  | At_at -> false
  | Int _
  | Char _
  | String _
  | Lident _
  | Uident _
  | Label _
  | Tyvar _
  | Weak_tyvar _
  | True
  | False
  | Rec
  | Mut
  | In
  | With
  | Struct
  | End
  | End_inline
  | Rparen
  | Rbracket
  | Rbrace
  | Comma
  | Semi
  | Dotdot
  | Dot_star
  | Underscore
  | Quote_close
  | Greater
  | Eof -> true
;;

let follows = function
  | Rparen
  | Rbracket
  | Rbrace
  | End
  | End_inline
  | Quote_close
  | Semi
  | Comma
  | In
  | With
  | Arrow
  | Equal -> true
  | _ -> false
;;
