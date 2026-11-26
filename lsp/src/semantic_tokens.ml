open! Import

module Token_type = struct
  type t =
    | Namespace
    | Type
    | Type_parameter
    | Enum_member
    | Property
    | Variable
    | Function
    | Parameter
    | Keyword
    | Comment
    | String
    | Number
    | Operator
    | Macro
  [@@deriving sexp_of, enumerate, equal]

  let to_lsp = function
    | Namespace -> "namespace"
    | Type -> "type"
    | Type_parameter -> "typeParameter"
    | Enum_member -> "enumMember"
    | Property -> "property"
    | Variable -> "variable"
    | Function -> "function"
    | Parameter -> "parameter"
    | Keyword -> "keyword"
    | Comment -> "comment"
    | String -> "string"
    | Number -> "number"
    | Operator -> "operator"
    | Macro -> "macro"
  ;;

  let to_index t = List.findi_exn all ~f:(fun _ other -> equal t other) |> fst
  let of_index index = List.nth_exn all index
end

module Token = struct
  type t =
    { line : int
    ; character : int
    ; length : int
    ; token_type : Token_type.t
    ; declaration : bool
    }
  [@@deriving sexp_of]
end

let legend =
  Lsp_types.SemanticTokensLegend.create
    ~tokenTypes:(List.map Token_type.all ~f:Token_type.to_lsp)
    ~tokenModifiers:[ "declaration" ]
;;

let of_lexical : Stag_syntax.Token.t -> Token_type.t option = function
  | Int _ -> Some Number
  | Char _ | String _ -> Some String
  | Lident _ -> Some Variable
  | Uident _ -> Some Enum_member
  | Root -> Some Namespace
  | Label _ -> Some Enum_member
  | Tyvar _ | Weak_tyvar _ -> Some Type_parameter
  | True
  | False
  | Let
  | Rec
  | Mut
  | Stack_
  | Exclave_
  | Region_
  | Global_
  | Forget
  | Let_expect_test
  | Let_bench
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
  | Module_inline
  | Include_inline
  | End_inline
  | End
  | Expect_open -> Some Keyword
  | Quote_open | Quote_close | Splice | Dot_lbrace -> Some Macro
  | Arrow
  | Larrow
  | Star
  | Bar
  | Equal
  | Less
  | Greater
  | Backslash
  | At
  | At_at
  | Dotdot -> Some Operator
  | Lparen
  | Rparen
  | Lbracket
  | Rbracket
  | Lbrace
  | Rbrace
  | Comma
  | Semi
  | Colon
  | Dot
  | Dot_star
  | Underscore
  | Eof -> None
;;

let of_kind : Syntax_names.Kind.t -> Token_type.t = function
  | Module -> Namespace
  | Type -> Type
  | Constructor | Poly_tag -> Enum_member
  | Field | Poly_field -> Property
  | Value | Pattern_var -> Variable
  | Let { is_function } -> if is_function then Function else Variable
  | Parameter -> Parameter
;;

module Classified = struct
  type t =
    { start : int
    ; stop : int
    ; token_type : Token_type.t
    ; declaration : bool
    }
end

(* Each name classified. *)
let work = Work.create "lsp.tokens"

let compute text ~file ~occurrences ~is_function =
  let { Lexer.Prefix.tokens; comments; error = _ } =
    Lexer.tokenize_prefix ~file (Text.contents text)
  in
  let by_start = Int.Table.create () in
  Array.iter tokens ~f:(fun { token; loc } ->
    Option.iter (of_lexical token) ~f:(fun token_type ->
      Hashtbl.set
        by_start
        ~key:loc.start.#offset
        ~data:
          { Classified.start = loc.start.#offset
          ; stop = loc.stop.#offset
          ; token_type
          ; declaration = false
          }));
  List.iter comments ~f:(fun (comment : Comment.t) ->
    Hashtbl.set
      by_start
      ~key:comment.loc.start.#offset
      ~data:
        { start = comment.loc.start.#offset
        ; stop = comment.loc.stop.#offset
        ; token_type = Comment
        ; declaration = false
        });
  let is_name i =
    match tokens.(i).token with
    | Lident _ | Uident _ | Label _ -> true
    | _ -> false
  in
  (* The dotted path starting [loc], eg. [M.N.x]. *)
  let name_tokens (loc : Loc.t) =
    match
      Array.binary_search
        tokens
        ~compare:(fun (token : Lexer.Located.t) offset ->
          Int.compare token.loc.start.#offset offset)
        `First_greater_than_or_equal_to
        loc.start.#offset
    with
    | None -> [||]
    | Some first ->
      let first =
        if Stag_syntax.Token.equal tokens.(first).token Root
        then first + 1
        else first
      in
      let rec stop i =
        if i + 2 < Array.length tokens
           && Stag_syntax.Token.equal tokens.(i + 1).token Dot
           && is_name (i + 2)
           && tokens.(i + 2).loc.stop.#offset <= loc.stop.#offset
        then stop (i + 2)
        else i
      in
      if is_name first && tokens.(first).loc.stop.#offset <= loc.stop.#offset
      then
        Array.filteri
          (Array.sub tokens ~pos:first ~len:(stop first - first + 1))
          ~f:(fun i _ -> i % 2 = 0)
      else [||]
  in
  List.iter
    occurrences
    ~f:(fun ({ loc; kind; declaration; _ } : Syntax_names.Occurrence.t) ->
      let names = name_tokens loc in
      Array.iteri names ~f:(fun i { token = _; loc = name_loc } ->
        Work.incr work;
        let start = name_loc.start.#offset in
        let token_type : Token_type.t =
          if i < Array.length names - 1
          then Namespace
          else (
            match of_kind kind with
            | (Variable | Parameter) when is_function loc.start.#offset ->
              Function
            | token_type -> token_type)
        in
        Hashtbl.set
          by_start
          ~key:start
          ~data:{ start; stop = name_loc.stop.#offset; token_type; declaration }));
  (* Unclassified module paths, eg. of [include .~M.x]: *)
  Array.iteri tokens ~f:(fun i { token; loc } ->
    match token with
    | Uident _ when i + 1 < Array.length tokens ->
      (match tokens.(i + 1).token with
       | Dot ->
         Hashtbl.change by_start loc.start.#offset ~f:(function
           | Some ({ token_type = Enum_member; _ } as classified) ->
             Some { classified with token_type = Namespace }
           | other -> other)
       | _ -> ())
    | _ -> ());
  Hashtbl.data by_start
  |> List.sort
       ~compare:
         (Comparable.lift Int.compare ~f:(fun (c : Classified.t) -> c.start))
  |> List.concat_map
       ~f:(fun { Classified.start; stop; token_type; declaration } ->
         List.map (Text.split_lines text ~start ~stop) ~f:(fun (start, stop) ->
           let position = Text.position_of_offset text start in
           { Token.line = position.line
           ; character = position.character
           ; length = Text.length text ~pos:start ~len:(stop - start)
           ; token_type
           ; declaration
           }))
;;

let encode tokens =
  let data = Queue.create () in
  ignore
    (List.fold
       tokens
       ~init:(0, 0)
       ~f:(fun (line, character) (token : Token.t) ->
         let delta_line = token.line - line in
         Queue.enqueue data delta_line;
         Queue.enqueue
           data
           (if delta_line = 0
            then token.character - character
            else token.character);
         Queue.enqueue data token.length;
         Queue.enqueue data (Token_type.to_index token.token_type);
         Queue.enqueue data (if token.declaration then 1 else 0);
         token.line, token.character)
     : int * int);
  Queue.to_array data
;;

let decode data =
  List.groupi (Array.to_list data) ~break:(fun i _ _ -> i % 5 = 0)
  |> List.folding_map ~init:(0, 0) ~f:(fun (line, character) group ->
    match group with
    | [ delta_line; delta_character; length; token_type; modifiers ] ->
      let line = line + delta_line in
      let character =
        if delta_line = 0 then character + delta_character else delta_character
      in
      ( (line, character)
      , { Token.line
        ; character
        ; length
        ; token_type = Token_type.of_index token_type
        ; declaration = modifiers land 1 = 1
        } )
    | _ -> raise_s [%message "Malformed semantic tokens" (data : int array)])
;;
