open! Import

(** One line [(kind start stop)] per token and comment of [src], in order, with
    byte offsets, for comparing editor grammars with the lexer. On a lexing
    error, the tokens before it, then [(error start stop message)]. *)
val print : string -> unit
