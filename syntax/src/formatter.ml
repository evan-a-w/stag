open! Import

module Printed_token = struct
  type t =
    { token : Token.t
    ; start : int (** Within its leaf. *)
    ; stop : int
    }
end

let tokens_of_text =
  Memo.of_comparable
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

(* The tokens of [doc], in order. *)
let printed_tokens doc =
  let tokens, (_ : Doc.t) =
    Doc.fold_map_leaves doc ~init:[] ~f:(fun tokens leaf ->
      List.rev_append (tokens_of_text (leaf_text leaf)) tokens, Doc.empty)
  in
  Array.of_list_rev_map tokens ~f:(fun (t : Printed_token.t) -> t.token)
;;

(* Those on their own line before an [end] or [}] are indented as what it
   closes. *)
let render_before ~(next : Token.t option) (comments : Comment.t list) =
  let own_line ?(blank_after = false) doc =
    let open Doc in
    let after = if blank_after then blank_line else hardline in
    match next with
    | Some (End | Rbrace) -> leading (nest 2 (fresh_line ^^ doc) ^^ after)
    | _ -> leading (fresh_line ^^ doc ^^ after)
  in
  Doc.concat
    (List.map comments ~f:(fun comment ->
       let open Doc in
       let comment_text = text comment.text in
       match comment.placement with
       | Own_line { blank_before; blank_after } ->
         own_line
           ~blank_after
           ((if blank_before then blank_line else empty) ^^ comment_text)
       | Trailing -> own_line comment_text
       | Inline -> space ^^ comment_text ^^ text " "))
;;

let render_after (comments : Comment.t list) =
  match comments with
  | [] -> Doc.empty
  | comments ->
    Doc.(
      text
        (String.concat
           (List.map comments ~f:(fun (comment : Comment.t) ->
              " " ^ comment.text)))
      ^^ end_line)
;;

let with_comments doc ~(source_tokens : Lexer.Located.t array) ~comments =
  let printed = printed_tokens doc in
  let n = Array.length printed in
  let source =
    Array.filter_map source_tokens ~f:(fun (t : Lexer.Located.t) ->
      match t.token with
      | Eof -> None
      | token -> Some token)
  in
  let matched = Alignment.align source printed ~equal:Token.equal in
  (* [last_matched.(g)]: the printed index of the last matched source token
     before the [g]th, if any. *)
  let last_matched = Array.create ~len:(Array.length source + 1) None in
  Array.iteri matched ~f:(fun i m ->
    last_matched.(i + 1) <- Option.first_some m last_matched.(i));
  let before = Array.create ~len:(n + 1) [] in
  let after = Array.create ~len:n [] in
  List.iter (List.rev comments) ~f:(fun (comment : Comment.t) ->
    match comment.placement, last_matched.(comment.next_token) with
    | Trailing, Some q when Token.ends_code printed.(q) ->
      (* Not before a separator, which would be pushed to the next line. *)
      let rec past_separators q =
        if q + 1 < n
        then (
          match printed.(q + 1) with
          | Semi | Comma | In -> past_separators (q + 1)
          | _ -> q)
        else q
      in
      let q = past_separators q in
      after.(q) <- comment :: after.(q)
    | _, q ->
      let i = Option.value_map q ~default:0 ~f:(fun q -> q + 1) in
      before.(i) <- comment :: before.(i));
  let (_ : int), doc =
    Doc.fold_map_leaves doc ~init:0 ~f:(fun first l ->
      let tokens = tokens_of_text (leaf_text l) in
      let next = first + List.length tokens in
      let has_comments =
        List.existsi tokens ~f:(fun j _ ->
          (not (List.is_empty before.(first + j)))
          || not (List.is_empty after.(first + j)))
      in
      match l with
      | If_break s ->
        ( next
        , if has_comments
          then
            Doc.(
              render_before ~next:(Some printed.(first)) before.(first)
              ^^ if_break s
              ^^ render_after after.(first))
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
                  ^^ render_before ~next:(Some printed.(i)) before.(i)
                  ^^ sub ~pos:start ~stop
                  ^^ render_after after.(i))
                :: docs ))
        in
        next, Doc.(concat (List.rev docs) ^^ sub ~pos ~stop:(String.length s)))
  in
  Doc.(doc ^^ render_before ~next:None before.(n))
;;

(* Whether there is a blank line between the toplevels at [prev] and [next],
   before any comment between them. *)
let blank_between
      source
      (comments : Comment.t list)
      (prev : Loc.t)
      (next : Loc.t)
  =
  (not (Loc.is_none prev))
  && (not (Loc.is_none next))
  &&
  let start = prev.stop.offset in
  let stop =
    List.find_map comments ~f:(fun comment ->
      let offset = comment.loc.start.offset in
      Option.some_if (offset >= start && offset < next.start.offset) offset)
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
  Result.map (Parser.program ~file source) ~f:(fun program ->
    let source_tokens, comments =
      Lexer.tokenize_with_comments ~file source |> Result.ok |> Option.value_exn
    in
    let doc =
      Printer.program_doc
        ~width
        ~blank_between:(blank_between source comments)
        program
      |> with_comments ~source_tokens ~comments
    in
    match String.rstrip (Doc.render ~width doc) with
    | "" -> ""
    | formatted -> formatted ^ "\n")
;;
