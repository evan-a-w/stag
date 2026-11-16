open! Import

module Printed_token = struct
  type t =
    { token : Token.t
    ; start : int (** Within its leaf. *)
    ; stop : int
    }
end

let tokens_of_text () =
  Memo.of_comparator
    (module String)
    (fun text ->
       match Lexer.tokenize ~file:"" text with
       | Error error ->
         raise_s
           [%message "printed text does not lex" text (error : Lexer.Error.t)]
       | Ok tokens ->
         Array.to_list tokens
         |> List.filter_map ~f:(fun ({ token; loc } : Lexer.Located.t) ->
           match token with
           | Eof -> None
           | token ->
             Some
               { Printed_token.token
               ; start = loc.start.offset
               ; stop = loc.stop.offset
               }))
;;

let leaf_text : Doc.Leaf.t -> string = function
  | Text s | If_break s -> s
;;

(* The tokens of [doc], in order, and whether each is the [|] of a match arm
   or variant constructor, which starts a line when broken. *)
let printed_tokens doc ~tokens_of_text =
  let tokens, (_ : Doc.t) =
    Doc.fold_map_leaves doc ~init:[] ~f:(fun tokens leaf ->
      let text = leaf_text leaf in
      ( List.rev_append
          (List.map (tokens_of_text text) ~f:(fun (t : Printed_token.t) ->
             t.token, String.equal text "| "))
          tokens
      , Doc.empty ))
  in
  Array.of_list_rev tokens |> Array.unzip
;;

(* Pairs of a source and a printed token index at the start of the same
   toplevel (or module member, or module's [end]), in order, from [(0, 0)] to
   the ends. The tokens between two are aligned apart from the others, which
   keeps aligning a long file quick. [count] is the number of tokens a
   toplevel prints. *)
let cut_points
      (program : Syntax.Program.t)
      ~(source : Lexer.Located.t array)
      ~printed
      ~count
  =
  let first_at offset =
    match
      Array.binary_search
        source
        ~compare:(fun (t : Lexer.Located.t) offset ->
          Int.compare t.loc.start.offset offset)
        `First_greater_than_or_equal_to
        offset
    with
    | None -> Array.length source
    | Some i -> i
  in
  let rec cuts (ts : Syntax.Toplevel.t list) ~start acc =
    List.fold ts ~init:(start, acc) ~f:(fun (p, acc) (t : Syntax.Toplevel.t) ->
      let acc = (first_at t.loc.start.offset, p) :: acc in
      match t.txt with
      | Module (_, (_ :: _ as members)) ->
        (* After [module M = struct]. *)
        let p, acc = cuts members ~start:(p + 4) acc in
        p + 1, (first_at t.loc.stop.offset - 1, p) :: acc
      | _ -> p + count t, acc)
  in
  let stop, cuts = cuts program ~start:0 [ 0, 0 ] in
  let cuts = List.rev ((Array.length source, Array.length printed) :: cuts) in
  let monotonic =
    List.for_all2_exn
      (List.drop_last_exn cuts)
      (List.tl_exn cuts)
      ~f:(fun (s, p) (s', p') -> s <= s' && p <= p')
  in
  if stop = Array.length printed && monotonic
  then cuts
  else [ 0, 0; Array.length source, Array.length printed ]
;;

let align source printed ~cuts =
  let matched = Array.create ~len:(Array.length source) None in
  List.iter2_exn
    (List.drop_last_exn cuts)
    (List.tl_exn cuts)
    ~f:(fun (s, p) (s', p') ->
      Alignment.align
        (Array.sub source ~pos:s ~len:(s' - s))
        (Array.sub printed ~pos:p ~len:(p' - p))
        ~compare:Token.compare
      |> Array.iteri ~f:(fun i m ->
        matched.(s + i) <- Option.map m ~f:(( + ) p)));
  matched
;;

let comment_doc (comment : Comment.t) =
  Doc.block
    (String.substr_replace_all comment.text ~pattern:"\r\n" ~with_:"\n")
    ~col:comment.loc.start.col
;;

let is_inline (comment : Comment.t) =
  match comment.placement with
  | Inline -> true
  | Own_line _ | Trailing -> false
;;

(* Those on their own line before an [end] or [}] are indented as what it
   closes. Those on their own line that were on one line stay on one. *)
let render_before
      ~(prev : Token.t option)
      ~(next : Token.t option)
      (comments : Comment.t list)
  =
  let open Doc in
  let own_line (run : Comment.t list) =
    let blank_before =
      match (List.hd_exn run).placement with
      | Own_line { blank_before; _ } -> blank_before
      | Trailing | Inline -> false
    in
    let after =
      match (List.last_exn run).placement with
      | Own_line { blank_after = true; _ } -> blank_line
      | Own_line { blank_after = false; _ } | Trailing | Inline -> hardline
    in
    let doc =
      (if blank_before then blank_line else empty)
      ^^ separate (text " ") (List.map run ~f:comment_doc)
    in
    match next with
    | Some (End | End_inline | Rbrace) ->
      leading (nest 2 (fresh_line ^^ doc) ^^ after)
    | _ -> leading (fresh_line ^^ doc ^^ after)
  in
  List.group comments ~break:(fun (a : Comment.t) (b : Comment.t) ->
    is_inline a || is_inline b || b.loc.start.line <> a.loc.stop.line)
  |> List.map ~f:(function
    | [ comment ] when is_inline comment ->
      let space =
        match prev with
        | Some (Lparen | Lbracket | Lbrace | Dot_lbrace) -> empty
        | _ -> space
      in
      let space_after =
        match next with
        | Some next when Token.follows next -> empty
        | _ -> text " "
      in
      space ^^ comment_doc comment ^^ space_after
    | run -> own_line run)
  |> concat
;;

let render_after (comments : Comment.t list) =
  let open Doc in
  concat (List.map comments ~f:(fun comment -> text " " ^^ comment_doc comment))
  ^^ if List.for_all comments ~f:is_inline then empty else end_line
;;

(* Where a comment goes: before the printed token, or after it. *)
module Slot = struct
  type t =
    | Before of int
    | After of int

  let rank = function
    | Before i -> 2 * i
    | After q -> (2 * q) + 1
  ;;
end

let with_comments doc ~tokens_of_text ~source_tokens ~comments ~cuts =
  let printed, leading_bar = printed_tokens doc ~tokens_of_text in
  let n = Array.length printed in
  let source_tokens =
    Array.filter source_tokens ~f:(fun (t : Lexer.Located.t) ->
      match t.token with
      | Eof -> false
      | _ -> true)
  in
  let source = Array.map source_tokens ~f:(fun t -> t.token) in
  let cuts = cuts ~source:source_tokens ~printed in
  let matched = align source printed ~cuts in
  let starts_line = Array.append leading_bar [| true |] in
  List.iter cuts ~f:(fun (_, p) -> starts_line.(p) <- true);
  let printed_matched = Array.create ~len:n false in
  Array.iter matched ~f:(Option.iter ~f:(fun q -> printed_matched.(q) <- true));
  (* [last_matched.(g)]: the printed index of the last matched source token
     before the [g]th, if any. *)
  let last_matched = Array.create ~len:(Array.length source + 1) None in
  Array.iteri matched ~f:(fun i m ->
    last_matched.(i + 1) <- Option.first_some m last_matched.(i));
  let next_matched g = g < Array.length matched && Option.is_some matched.(g) in
  (* The first printed token from [i] that is not a separator, which would be
     left alone on its line or pushed to the next, nor, with [closers], a
     closer the printer added. *)
  let rec skip_separators ?(closers = false) i =
    if
      i < n
      &&
      match printed.(i) with
      | Semi | Comma | In -> true
      | Rparen | Rbracket | Rbrace | End | Quote_close ->
        closers && not printed_matched.(i)
      | _ -> false
    then skip_separators ~closers (i + 1)
    else i
  in
  (* An inline comment with only dropped tokens before a comment that ends a
     line will end the line too, so is trailing. *)
  let comments =
    List.fold_right comments ~init:[] ~f:(fun (comment : Comment.t) later ->
      match comment.placement, later with
      | Inline, (next : Comment.t) :: _
        when (not (is_inline next))
             && List.for_all
                  (List.range comment.next_token next.next_token)
                  ~f:(fun g -> Option.is_none matched.(g)) ->
        { comment with placement = Trailing } :: later
      | _ -> comment :: later)
  in
  (* After a [.] rather than before it, where it would make it a root path's. *)
  let not_before_dot : Slot.t -> Slot.t = function
    | Before i when i < n && Token.equal printed.(i) Dot -> Before (i + 1)
    | After q when q + 1 < n && Token.equal printed.(q + 1) Dot -> Before (q + 2)
    | slot -> slot
  in
  (* As it would be after a reformatting, given the printed tokens around. *)
  let slot (comment : Comment.t) : Slot.t =
    let inline_after q =
      if Token.ends_code printed.(q) then Slot.After q else Before (q + 1)
    in
    match comment.placement, last_matched.(comment.next_token) with
    | Trailing, Some q when Token.ends_code printed.(q) ->
      After (skip_separators ~closers:true (q + 1) - 1)
    | Inline, Some q when not (next_matched comment.next_token) ->
      (* What followed it is gone, as a redundant parenthesis. Before what
         follows on its line instead, unless that is a closer. *)
      let q = skip_separators (q + 1) - 1 in
      if
        q + 1 < n
        && (not starts_line.(q + 1))
        && not (Token.follows printed.(q + 1))
      then Before (q + 1)
      else inline_after q
    | Inline, Some q when Token.follows source.(comment.next_token) ->
      (* Not about what follows. A line broken after a token that does not end
         code would make it about what follows. Past separators, where it is
         as one written after them. *)
      let i = skip_separators (q + 1) in
      if i > q + 1 && i < n && not (Token.follows printed.(i))
      then Before i
      else inline_after (i - 1)
    | Inline, Some q ->
      (match skip_separators (q + 1) with
       | i when i < n && Token.follows printed.(i) -> inline_after (i - 1)
       | i -> Before i)
    | _, q ->
      Before
        (skip_separators (Option.value_map q ~default:0 ~f:(fun q -> q + 1)))
  in
  let before = Array.create ~len:(n + 1) [] in
  let after = Array.create ~len:n [] in
  (* Each in a slot no earlier than the one before's, so they stay in order. *)
  let (_ : Slot.t option) =
    List.fold comments ~init:None ~f:(fun prev (comment : Comment.t) ->
      let slot =
        match not_before_dot (slot comment), prev with
        | slot, Some prev when Slot.rank slot < Slot.rank prev -> prev
        | slot, _ -> slot
      in
      (match slot with
       | Before i -> before.(i) <- comment :: before.(i)
       | After q -> after.(q) <- comment :: after.(q));
      Some slot)
  in
  (* An inline comment followed by one that ends a line ends it too. One
     after one that ends a line, before a closer or separator, is on its own
     line. *)
  let before =
    Array.mapi before ~f:(fun i comments ->
      let comments =
        List.fold comments ~init:[] ~f:(fun later (comment : Comment.t) ->
          match comment.placement, later with
          | Inline, (next : Comment.t) :: _ when not (is_inline next) ->
            { comment with placement = Trailing } :: later
          | _ -> comment :: later)
      in
      if i < n && Token.follows printed.(i)
      then
        List.folding_map comments ~init:false ~f:(fun line_ended comment ->
          let comment =
            if line_ended && is_inline comment
            then
              { comment with
                placement =
                  Own_line { blank_before = false; blank_after = false }
              }
            else comment
          in
          not (is_inline comment), comment)
      else comments)
  in
  let after = Array.map after ~f:List.rev in
  (* Between inline comments after a token and the next token, which may be
     printed glued to it. *)
  let space_before =
    Array.init (n + 1) ~f:(fun i ->
      0 < i
      && i < n
      && List.is_empty before.(i)
      && (not (List.is_empty after.(i - 1)))
      && List.for_all after.(i - 1) ~f:is_inline
      && not (Token.follows printed.(i)))
  in
  let render_before i =
    Doc.(
      (if space_before.(i) then space else empty)
      ^^ render_before
           ~prev:(if i > 0 then Some printed.(i - 1) else None)
           ~next:(if i < n then Some printed.(i) else None)
           before.(i))
  in
  let (_ : int), doc =
    Doc.fold_map_leaves doc ~init:0 ~f:(fun first l ->
      let tokens = tokens_of_text (leaf_text l) in
      let next = first + List.length tokens in
      let has_comments =
        List.existsi tokens ~f:(fun j _ ->
          (not (List.is_empty before.(first + j)))
          || (not (List.is_empty after.(first + j)))
          || space_before.(first + j))
      in
      match l with
      | If_break s ->
        ( next
        , if has_comments
          then
            Doc.(
              render_before first ^^ if_break s ^^ render_after after.(first))
          else Doc.if_break s )
      | Text s when not has_comments -> next, Doc.text s
      | Text s ->
        let sub ~pos ~stop =
          if pos = stop
          then Doc.empty
          else Doc.text (String.sub s ~pos ~len:(stop - pos))
        in
        let pos, docs =
          List.foldi
            tokens
            ~init:(0, [])
            ~f:(fun j (pos, docs) ({ start; stop; _ } : Printed_token.t) ->
              let i = first + j in
              ( stop
              , Doc.(
                  sub ~pos ~stop:start
                  ^^ render_before i
                  ^^ sub ~pos:start ~stop
                  ^^ render_after after.(i))
                :: docs ))
        in
        next, Doc.(concat (List.rev docs) ^^ sub ~pos ~stop:(String.length s)))
  in
  Doc.(doc ^^ render_before n)
;;

(* Whether there is a blank line between the toplevels at [prev] and [next],
   before any comment between them. *)
let blank_between
      source
      (comments : Comment.t array)
      (prev : Loc.t)
      (next : Loc.t)
  =
  (not (Loc.is_none prev))
  && (not (Loc.is_none next))
  &&
  let between =
    Array.binary_search
      comments
      ~compare:(fun (c : Comment.t) offset ->
        Int.compare c.loc.start.offset offset)
      `First_greater_than_or_equal_to
      prev.stop.offset
    |> Option.globalize Int.globalize
    |> Option.value_map ~default:[] ~f:(fun first ->
      let rec from i =
        if
          i < Array.length comments
          && comments.(i).loc.start.offset < next.start.offset
        then comments.(i) :: from (i + 1)
        else []
      in
      from first)
  in
  (* After the comments trailing [prev], on its line. *)
  let start =
    List.fold between ~init:prev.stop.offset ~f:(fun start comment ->
      let offset = comment.loc.start.offset in
      if
        offset >= start
        && not
             (String.mem
                (String.sub source ~pos:start ~len:(offset - start))
                '\n')
      then comment.loc.stop.offset
      else start)
  in
  let stop =
    List.find_map between ~f:(fun comment ->
      let offset = comment.loc.start.offset in
      Option.some_if (offset >= start) offset)
    |> Option.value ~default:next.start.offset
  in
  match
    String.split (String.sub source ~pos:start ~len:(stop - start)) ~on:'\n'
  with
  | _ :: (_ :: _ :: _ as rest) ->
    List.exists (List.drop_last_exn rest) ~f:(fun line ->
      String.is_empty (String.strip line))
  | _ -> false
;;

let format ?(width = 80) ~file source =
  Result.map
    (Parser.program_with_comments ~file source)
    ~f:(fun (program, source_tokens, comments) ->
      let blank_between = blank_between source (Array.of_list comments) in
      let doc = Printer.program_doc ~width ~blank_between program in
      let doc =
        match comments with
        | [] -> doc
        | _ :: _ ->
          let tokens_of_text = tokens_of_text () in
          let count toplevel =
            Array.length
              (fst
                 (printed_tokens
                    (Printer.program_doc ~width ~blank_between [ toplevel ])
                    ~tokens_of_text))
          in
          with_comments
            doc
            ~tokens_of_text
            ~source_tokens
            ~comments
            ~cuts:(cut_points program ~count)
      in
      match String.rstrip (Doc.render ~width doc) with
      | "" -> ""
      | formatted -> formatted ^ "\n")
;;
