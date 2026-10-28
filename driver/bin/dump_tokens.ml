open! Import

let kind ~src (located : Lexer.Located.t) =
  match located.token with
  | String _ when Char.equal src.[located.loc.start.offset] '{' ->
    "quoted_string"
  | String _ -> "string"
  | Int _ -> "int"
  | Char _ -> "char"
  | Lident _ -> "lident"
  | Uident _ -> "uident"
  | Label _ -> "label"
  | Tyvar _ -> "tyvar"
  | Weak_tyvar _ -> "weak_tyvar"
  | Let_expect_test | Let_bench | Expect_open -> "expect"
  | Module_inline | Include_inline | End_inline -> "inline"
  | token when Option.is_some (Token.keyword (Token.to_string token)) ->
    "keyword"
  | _ -> "punct"
;;

(* The longest prefix that lexes, which ends before the error: lexing stops at
   the first error, and the tokens before it are as in the whole source. *)
let rec lex_prefix src =
  match Lexer.tokenize_with_comments ~file:"" src with
  | Ok result -> result
  | Error error -> lex_prefix (String.prefix src error.loc.start.offset)
;;

let print src =
  let tokens, comments = lex_prefix src in
  let line kind (loc : Loc.t) = loc.start.offset, kind, loc.stop.offset in
  let lines =
    List.map comments ~f:(fun (comment : Comment.t) ->
      line "comment" comment.loc)
    @ List.filter_map (Array.to_list tokens) ~f:(fun located ->
      match located.token with
      | Eof -> None
      | _ -> Some (line (kind ~src located) located.loc))
  in
  List.sort lines ~compare:[%compare: int * string * int]
  |> List.iter ~f:(fun (start, kind, stop) ->
    printf "(%s %d %d)\n" kind start stop);
  match Lexer.tokenize ~file:"" src with
  | Ok _ -> ()
  | Error error ->
    printf
      "(error %d %d %s)\n"
      error.loc.start.offset
      error.loc.stop.offset
      (Sexp.to_string [%sexp (Lexer.Error.message error : string)])
;;
