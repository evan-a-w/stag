open! Core

module Located = struct
  type t =
    { token : Token.t
    ; loc : Loc.t
    }
  [@@deriving sexp_of]
end

module Error = struct
  module Kind = struct
    type t =
      | Illegal_character of char
      | Unterminated_comment
      | Unterminated_string
      | Unterminated_char
      | Illegal_escape of string
      | Invalid_int_literal of string
    [@@deriving sexp_of]

    let to_string = function
      | Illegal_character c -> sprintf "illegal character '%s'" (Char.escaped c)
      | Unterminated_comment -> "unterminated comment"
      | Unterminated_string -> "unterminated string literal"
      | Unterminated_char -> "unterminated character literal"
      | Illegal_escape s -> sprintf "illegal escape sequence %s" s
      | Invalid_int_literal s -> sprintf "invalid integer literal %s" s
    ;;
  end

  type t =
    { kind : Kind.t
    ; loc : Loc.t
    }
  [@@deriving sexp_of]

  let to_string { kind; loc } =
    sprintf "%s: %s" (Loc.to_string loc) (Kind.to_string kind)
  ;;
end

exception Lex_error of Error.t

module State = struct
  type t =
    { file : string
    ; src : string
    ; mutable offset : int
    ; mutable line : int
    ; mutable bol : int
    }

  let peek_at t n =
    let i = t.offset + n in
    if i < String.length t.src then Some t.src.[i] else None
  ;;

  let peek t = peek_at t 0
  let at_end t = t.offset >= String.length t.src

  let bump t =
    if Char.equal t.src.[t.offset] '\n'
    then (
      t.line <- t.line + 1;
      t.bol <- t.offset + 1);
    t.offset <- t.offset + 1
  ;;

  let bump_n t n =
    for _ = 1 to n do
      bump t
    done
  ;;

  let pos t : Loc.Pos.t =
    { line = t.line; col = t.offset - t.bol; offset = t.offset }
  ;;

  let loc_from t (start : Loc.Pos.t) : Loc.t =
    { file = t.file; start; stop = pos t }
  ;;

  let error t start kind = raise (Lex_error { kind; loc = loc_from t start })

  (* For an opening delimiter of [len] bytes, which never spans lines. *)
  let error_opening t (start : Loc.Pos.t) ~len kind =
    let stop =
      { start with col = start.col + len; offset = start.offset + len }
    in
    raise (Lex_error { kind; loc = { file = t.file; start; stop } })
  ;;

  let text_from t (start : Loc.Pos.t) =
    String.sub t.src ~pos:start.offset ~len:(t.offset - start.offset)
  ;;
end

let is_ident_char c =
  Char.is_alphanum c || Char.equal c '_' || Char.equal c '\''
;;

let is_ident_start c = Char.is_alpha c || Char.equal c '_'

let skip_while (t : State.t) ~f =
  while Option.exists (State.peek t) ~f do
    State.bump t
  done
;;

let ident (t : State.t) =
  let start = State.pos t in
  skip_while t ~f:is_ident_char;
  State.text_from t start
;;

(* At a backslash. *)
let escape (t : State.t) =
  let start = State.pos t in
  State.bump t;
  let illegal () =
    State.error t start (Illegal_escape (State.text_from t start))
  in
  let simple c =
    State.bump t;
    c
  in
  let digits ~n ~f =
    let digits_start = t.offset in
    for _ = 1 to n do
      if Option.exists (State.peek t) ~f then State.bump t else illegal ()
    done;
    String.sub t.src ~pos:digits_start ~len:n
  in
  match State.peek t with
  | Some 'n' -> simple '\n'
  | Some 't' -> simple '\t'
  | Some 'r' -> simple '\r'
  | Some 'b' -> simple '\b'
  | Some ' ' -> simple ' '
  | Some '\\' -> simple '\\'
  | Some '"' -> simple '"'
  | Some '\'' -> simple '\''
  | Some 'x' ->
    State.bump t;
    let digits = digits ~n:2 ~f:Char.is_hex_digit in
    Char.of_int_exn (Int.of_string ("0x" ^ digits))
  | Some c when Char.is_digit c ->
    let digits = digits ~n:3 ~f:Char.is_digit in
    (match Char.of_int (Int.of_string digits) with
     | Some c -> c
     | None -> illegal ())
  | Some '\n' | None -> illegal ()
  | Some _ ->
    State.bump t;
    illegal ()
;;

(* At the opening quote. *)
let string (t : State.t) =
  let start = State.pos t in
  State.bump t;
  let buf = Buffer.create 16 in
  let rec loop () =
    match State.peek t with
    | None -> State.error_opening t start ~len:1 Unterminated_string
    | Some '"' -> State.bump t
    | Some '\\' ->
      Buffer.add_char buf (escape t);
      loop ()
    | Some c ->
      Buffer.add_char buf c;
      State.bump t;
      loop ()
  in
  loop ();
  Buffer.contents buf
;;

(* Strings and char literals are skipped as such, so that eg. ["*)"] does not
   end a comment, as in OCaml. *)
let comment (t : State.t) =
  let start = State.pos t in
  State.bump_n t 2;
  let unterminated () =
    State.error_opening t start ~len:2 Unterminated_comment
  in
  let rec skip_string () =
    match State.peek t with
    | None -> unterminated ()
    | Some '"' -> State.bump t
    | Some '\\' ->
      State.bump t;
      if not (State.at_end t) then State.bump t;
      skip_string ()
    | Some _ ->
      State.bump t;
      skip_string ()
  in
  let rec loop depth =
    if depth > 0
    then (
      match
        State.peek t, State.peek_at t 1, State.peek_at t 2, State.peek_at t 3
      with
      | None, _, _, _ -> unterminated ()
      | Some '(', Some '*', _, _ ->
        State.bump_n t 2;
        loop (depth + 1)
      | Some '*', Some ')', _, _ ->
        State.bump_n t 2;
        loop (depth - 1)
      | Some '"', _, _, _ ->
        State.bump t;
        skip_string ();
        loop depth
      | Some '\'', Some '\\', Some _, Some '\'' ->
        State.bump_n t 4;
        loop depth
      | Some '\'', Some c, Some '\'', _ when not (Char.equal c '\n') ->
        State.bump_n t 3;
        loop depth
      | Some _, _, _, _ ->
        State.bump t;
        loop depth)
  in
  loop 1
;;

(* At a single quote: a char literal, or a type variable. *)
let quote (t : State.t) : Token.t =
  let start = State.pos t in
  match State.peek_at t 1, State.peek_at t 2 with
  | Some '\\', _ ->
    State.bump t;
    let c = escape t in
    (match State.peek t with
     | Some '\'' ->
       State.bump t;
       Char c
     | _ -> State.error t start Unterminated_char)
  | Some c, Some '\'' when not (Char.equal c '\n' || Char.equal c '\'') ->
    State.bump_n t 3;
    Char c
  | Some '_', Some c when is_ident_char c ->
    State.bump_n t 2;
    Weak_tyvar (ident t)
  | Some c, _ when is_ident_start c && not (Char.equal c '_') ->
    State.bump t;
    Tyvar (ident t)
  | _ ->
    State.bump t;
    State.error t start (Illegal_character '\'')
;;

let rec skip_trivia (t : State.t) =
  skip_while t ~f:(function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false);
  match State.peek t, State.peek_at t 1 with
  | Some '(', Some '*' ->
    comment t;
    skip_trivia t
  | _ -> ()
;;

let token (t : State.t) : Located.t =
  skip_trivia t;
  let start = State.pos t in
  let simple n (token : Token.t) =
    State.bump_n t n;
    token
  in
  let token : Token.t =
    match State.peek t with
    | None -> Eof
    | Some c ->
      (match c, State.peek_at t 1 with
       | '(', _ -> simple 1 Lparen
       | ')', _ -> simple 1 Rparen
       | '[', _ -> simple 1 Lbracket
       | ']', _ -> simple 1 Rbracket
       | '{', _ -> simple 1 Lbrace
       | '}', _ -> simple 1 Rbrace
       | ',', _ -> simple 1 Comma
       | ';', _ -> simple 1 Semi
       | ':', _ -> simple 1 Colon
       | '=', _ -> simple 1 Equal
       | '|', _ -> simple 1 Bar
       | '*', _ -> simple 1 Star
       | '\\', _ -> simple 1 Backslash
       | '-', Some '>' -> simple 2 Arrow
       | '>', Some '.' -> simple 2 Quote_close
       | '>', _ -> simple 1 Greater
       | '<', _ -> simple 1 Less
       | '.', Some '<' -> simple 2 Quote_open
       | '.', Some '~' -> simple 2 Splice
       | '.', Some '.' -> simple 2 Dotdot
       | '.', _ -> simple 1 Dot
       | '@', Some '@' -> simple 2 At_at
       | '@', _ -> simple 1 At
       | '"', _ -> String (string t)
       | '\'', _ -> quote t
       | '`', Some c when is_ident_start c ->
         State.bump t;
         Label (ident t)
       | '_', c when not (Option.exists c ~f:is_ident_char) ->
         simple 1 Underscore
       | c, _ when Char.is_digit c ->
         let text = ident t in
         if String.for_all text ~f:Char.is_digit
         then Int text
         else State.error t start (Invalid_int_literal text)
       | c, _ when Char.is_uppercase c -> Uident (ident t)
       | c, _ when is_ident_start c ->
         let text = ident t in
         Option.value (Token.keyword text) ~default:(Lident text)
       | c, _ ->
         State.bump t;
         State.error t start (Illegal_character c))
  in
  { token; loc = State.loc_from t start }
;;

let tokenize ~file src =
  let t : State.t = { file; src; offset = 0; line = 1; bol = 0 } in
  let tokens = Queue.create () in
  match
    let rec loop () =
      let located = token t in
      Queue.enqueue tokens located;
      match located.token with
      | Eof -> ()
      | _ -> loop ()
    in
    loop ()
  with
  | () -> Ok (Queue.to_array tokens)
  | exception Lex_error error -> Error error
;;
