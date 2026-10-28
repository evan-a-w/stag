open! Import

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
      | Int_literal_out_of_range of string
    [@@deriving sexp_of]

    let to_string = function
      | Illegal_character c -> sprintf "illegal character '%s'" (Char.escaped c)
      | Unterminated_comment -> "unterminated comment"
      | Unterminated_string -> "unterminated string literal"
      | Unterminated_char -> "unterminated character literal"
      | Illegal_escape s -> sprintf "illegal escape sequence %s" s
      | Invalid_int_literal s -> sprintf "invalid integer literal %s" s
      | Int_literal_out_of_range s ->
        sprintf "integer literal %s exceeds the range of int" s
    ;;
  end

  type t =
    { kind : Kind.t
    ; loc : Loc.t
    }
  [@@deriving sexp_of]

  let message t = Kind.to_string t.kind

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
    ; comments : (string * Loc.t) Queue.t
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

(* The delimiter [id] of a quoted string [{id|...|id}] starting here, if one
   does. *)
let quoted_string_id (t : State.t) =
  let rec id_end i =
    match State.peek_at t i with
    | Some ('a' .. 'z' | '_') -> id_end (i + 1)
    | Some '|' -> Some i
    | _ -> None
  in
  match State.peek t with
  | Some '{' ->
    Option.map (id_end 1) ~f:(fun i ->
      String.sub t.src ~pos:(t.offset + 1) ~len:(i - 1))
  | _ -> None
;;

(* At the start of a quoted string delimited by [id], its contents, raw. *)
let quoted_string (t : State.t) ~id =
  let start = State.pos t in
  let open_len = String.length id + 2 in
  State.bump_n t open_len;
  let close = "|" ^ id ^ "}" in
  let contents_start = t.offset in
  let rec loop () =
    if State.at_end t
    then State.error_opening t start ~len:open_len Unterminated_string
    else if String.is_substring_at t.src ~pos:t.offset ~substring:close
    then (
      let contents =
        String.sub t.src ~pos:contents_start ~len:(t.offset - contents_start)
      in
      State.bump_n t (String.length close);
      contents)
    else (
      State.bump t;
      loop ())
  in
  loop ()
;;

(* Whether [word] is next, not followed by an identifier character. *)
let is_word_at (t : State.t) ~pos word =
  String.is_substring_at t.src ~pos:(t.offset + pos) ~substring:word
  && not
       (Option.exists
          (State.peek_at t (pos + String.length word))
          ~f:is_ident_char)
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
    | Some '\\' when Option.exists (State.peek_at t 1) ~f:(Char.equal '\n') ->
      State.bump_n t 2;
      skip_while t ~f:(function
        | ' ' | '\t' -> true
        | _ -> false);
      loop ()
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
      | Some '{', _, _, _ when Option.is_some (quoted_string_id t) ->
        ignore (quoted_string t ~id:(Option.value_exn (quoted_string_id t)));
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

(* At a digit, or at a [-] followed by one. The sign is parsed with the digits,
   so that [min_int] does not overflow. *)
let int_literal (t : State.t) (start : Loc.Pos.t) : Token.t =
  let sign =
    match State.peek t with
    | Some '-' ->
      State.bump t;
      "-"
    | _ -> ""
  in
  let digits = ident t in
  let text = State.text_from t start in
  if String.for_all digits ~f:(fun c -> Char.is_digit c || Char.equal c '_')
  then (
    match Int.of_string_opt (sign ^ String.filter digits ~f:Char.is_digit) with
    | Some n -> Int n
    | None -> State.error t start (Int_literal_out_of_range text))
  else State.error t start (Invalid_int_literal text)
;;

let rec skip_trivia (t : State.t) =
  skip_while t ~f:(function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false);
  match State.peek t, State.peek_at t 1 with
  | Some '(', Some '*' ->
    let start = State.pos t in
    comment t;
    Queue.enqueue t.comments (State.text_from t start, State.loc_from t start);
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
       | '[', Some '%' when is_word_at t ~pos:1 "%expect" ->
         simple 8 Expect_open
       | '[', _ -> simple 1 Lbracket
       | ']', _ -> simple 1 Rbracket
       | '{', _ ->
         (match quoted_string_id t with
          | Some id -> String (quoted_string t ~id)
          | None -> simple 1 Lbrace)
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
       | '.', Some '{' -> simple 2 Dot_lbrace
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
       | '-', Some c when Char.is_digit c -> int_literal t start
       | c, _ when Char.is_digit c -> int_literal t start
       | c, _ when Char.is_uppercase c -> Uident (ident t)
       | c, _ when is_ident_start c ->
         let text = ident t in
         if String.equal text "let" && is_word_at t ~pos:0 "%expect_test"
         then simple 12 Let_expect_test
         else Option.value (Token.keyword text) ~default:(Lident text)
       | c, _ ->
         State.bump t;
         State.error t start (Illegal_character c))
  in
  { token; loc = State.loc_from t start }
;;

(* A comment is [Trailing] after a token ending code on its line, or after
   such a comment ending on its line. *)
let comments ~(prev : Located.t option) ~(next : Located.t) ~next_index found =
  let lines : Loc.t array = Array.of_list (List.map found ~f:snd) in
  let n = Array.length lines in
  (* Whether code follows on its last line, maybe after other comments. *)
  let code_after = Array.create ~len:n false in
  for i = n - 1 downto 0 do
    code_after.(i)
    <- (if i < n - 1
        then
          lines.(i + 1).start.line = lines.(i).stop.line && code_after.(i + 1)
        else (
          match next.token with
          | Eof -> false
          | _ -> next.loc.start.line = lines.(i).stop.line))
  done;
  let trailing_line =
    Option.bind prev ~f:(fun prev ->
      Option.some_if (Token.ends_code prev.token) prev.loc.stop.line)
  in
  List.folding_mapi
    found
    ~init:trailing_line
    ~f:(fun i trailing_line (text, (loc : Loc.t)) ->
      let code_before =
        Option.exists prev ~f:(fun prev -> prev.loc.stop.line = loc.start.line)
      in
      let code_after = code_after.(i) in
      let placement : Comment.Placement.t =
        if code_after
        then Inline
        else if Option.equal Int.equal trailing_line (Some loc.start.line)
        then Trailing
        else (
          let line_before =
            if i > 0
            then Some lines.(i - 1).stop.line
            else Option.map prev ~f:(fun prev -> prev.loc.stop.line)
          in
          let line_after =
            if i < n - 1
            then Some lines.(i + 1).start.line
            else (
              match next.token with
              | Eof -> None
              | _ -> Some next.loc.start.line)
          in
          Own_line
            { blank_before =
                (not code_before)
                && Option.exists line_before ~f:(fun line ->
                  loc.start.line > line + 1)
            ; blank_after =
                Option.exists line_after ~f:(fun line ->
                  line > loc.stop.line + 1)
            })
      in
      let trailing_line =
        match placement with
        | Trailing -> Some loc.stop.line
        | Own_line _ | Inline -> None
      in
      trailing_line, { Comment.text; loc; placement; next_token = next_index })
;;

let tokenize_with_comments ~file src =
  let t : State.t =
    { file; src; offset = 0; line = 1; bol = 0; comments = Queue.create () }
  in
  let tokens = Queue.create () in
  let all_comments = Queue.create () in
  match
    let rec loop prev =
      let next = token t in
      Queue.enqueue_all
        all_comments
        (comments
           ~prev
           ~next
           ~next_index:(Queue.length tokens)
           (Queue.to_list t.comments));
      Queue.clear t.comments;
      Queue.enqueue tokens next;
      match next.token with
      | Eof -> ()
      | _ -> loop (Some next)
    in
    loop None
  with
  | () -> Ok (Queue.to_array tokens, Queue.to_list all_comments)
  | exception Lex_error error -> Error error
;;

let tokenize ~file src = Result.map (tokenize_with_comments ~file src) ~f:fst
