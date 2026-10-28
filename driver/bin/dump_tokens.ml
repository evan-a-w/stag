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

let print src =
  let { Lexer.Prefix.tokens; comments; error } =
    Lexer.tokenize_prefix ~file:"" src
  in
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
  Option.iter error ~f:(fun error ->
    printf
      "(error %d %d %s)\n"
      error.loc.start.offset
      error.loc.stop.offset
      (Sexp.to_string [%sexp (Lexer.Error.message error : string)]))
;;
