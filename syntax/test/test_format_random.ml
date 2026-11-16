open! Import

(* Random programs ([Gen]), printed, with comments of random shapes put in
   random gaps between tokens, checked to format keeping their syntax and
   comments, idempotently. Messy sources are printed at a random width, with
   parentheses added or removed where that keeps their syntax, and comments
   maybe glued to the tokens around them. A failure is shown with as few of its
   comments as still fail. *)

let comment (gen : Gen.t) ~messy i =
  let body =
    match Gen.int gen 4 with
    | 0 -> sprintf "(* (* nested %d *) *)" i
    | 1 -> sprintf "(** doc %d *)" i
    | 2 -> sprintf "(* \"*)\" %d\n   second line *)" i
    | _ -> sprintf "(* c%d *)" i
  in
  let space () =
    Gen.pick
      gen
      ((if messy then [ ""; "" ] else []) @ [ " "; " "; "\n"; "\n\n" ])
  in
  space () ^ body ^ space ()
;;

(* [comments] are [(offset, text)], in order. *)
let insert source comments =
  let buf = Buffer.create (String.length source * 2) in
  let pos =
    List.fold comments ~init:0 ~f:(fun pos (offset, comment) ->
      Buffer.add_string buf (String.sub source ~pos ~len:(offset - pos));
      Buffer.add_string buf comment;
      offset)
  in
  Buffer.add_string buf (String.drop_prefix source pos);
  Buffer.contents buf
;;

let tokens source =
  match Lexer.tokenize ~file:"t.stag" source with
  | Error error -> raise_s [%sexp (error : Lexer.Error.t)]
  | Ok tokens -> tokens
;;

(* Before random tokens, or with [messy] maybe after them. Not before a [.] or
   after a root path's, where a comment would change how it lexes. *)
let random_comments (gen : Gen.t) ~messy ~density source =
  let tokens = Array.to_list (tokens source) in
  let lexing_changes =
    List.filter_map tokens ~f:(fun (token : Lexer.Located.t) ->
      match token.token with
      | Dot -> Some token.loc.start.offset
      | Root -> Some token.loc.stop.offset
      | _ -> None)
    |> Int.Set.of_list
  in
  tokens
  |> List.filter_map ~f:(fun (token : Lexer.Located.t) ->
    let%bind.Option () = Option.some_if (Gen.int gen density = 0) () in
    let%bind.Option offset =
      match token.token with
      | Eof -> Some token.loc.start.offset
      | _ ->
        Some
          (if messy && Gen.bool gen
           then token.loc.stop.offset
           else token.loc.start.offset)
    in
    Option.some_if (not (Set.mem lexing_changes offset)) offset)
  |> List.mapi ~f:(fun i offset -> offset, comment gen ~messy i)
;;

(* [source] with [edits], of [(offset, length, text)] in order, replacing the
   [length] characters at [offset] by [text]. *)
let edit source edits =
  let buf = Buffer.create (String.length source + 8) in
  let pos =
    List.fold edits ~init:0 ~f:(fun pos (offset, length, text) ->
      Buffer.add_string buf (String.sub source ~pos ~len:(offset - pos));
      Buffer.add_string buf text;
      offset + length)
  in
  Buffer.add_string buf (String.drop_prefix source pos);
  Buffer.contents buf
;;

(* The index of the [)] matching the [(] at [i], if any. *)
let matching (tokens : Lexer.Located.t array) i =
  let rec go j depth =
    if j >= Array.length tokens
    then None
    else (
      match tokens.(j).token with
      | Lparen -> go (j + 1) (depth + 1)
      | Rparen when depth = 1 -> Some j
      | Rparen -> go (j + 1) (depth - 1)
      | _ -> go (j + 1) depth)
  in
  go i 0
;;

(* [source], of [program], with [count] random parentheses added (around a
   token, or a parenthesised group) or removed (maybe leaving a bare tuple),
   each kept only if the program stays the same. *)
let reparenthesize (gen : Gen.t) program source ~count =
  let same source =
    match Parser.program ~file:"t.stag" source with
    | Ok parsed -> Syntax.Program.equal program parsed
    | Error _ -> false
  in
  Fn.apply_n_times
    ~n:count
    (fun source ->
      let tokens = tokens source in
      let i = Gen.int gen (Array.length tokens) in
      let start = tokens.(i).loc.start.offset in
      let edits =
        match tokens.(i).token, matching tokens i with
        | Eof, _ -> []
        | Lparen, Some j when Gen.bool gen ->
          [ start, 1, ""; tokens.(j).loc.start.offset, 1, "" ]
        | Lparen, Some j ->
          [ start, 0, "("; tokens.(j).loc.stop.offset, 0, ")" ]
        | _ -> [ start, 0, "("; tokens.(i).loc.stop.offset, 0, ")" ]
      in
      let edited = edit source edits in
      if same edited then edited else source)
    source
;;

let rec shrink source comments ~fails =
  match
    List.find_mapi comments ~f:(fun i _ ->
      let fewer = List.filteri comments ~f:(fun j _ -> j <> i) in
      Option.some_if (fails (insert source fewer)) fewer)
  with
  | Some fewer -> shrink source fewer ~fails
  | None -> comments
;;

let check ?(messy = false) ?widths ~seed ~count ~density ~gen () =
  let random = Random.State.make [| seed |] in
  let comments = ref 0 in
  let failures = ref 0 in
  for i = 0 to count - 1 do
    let gen : Gen.t = gen random i in
    let program =
      List.init (1 + Gen.int gen 3) ~f:(fun _ -> Gen.toplevel gen)
    in
    let width =
      if messy then 10 + Gen.int gen 191 else Gen.pick gen [ 80; 30 ]
    in
    let source = Printer.program ~width program in
    let source =
      if messy then reparenthesize gen program source ~count:10 else source
    in
    let inserted = random_comments gen ~messy ~density source in
    comments := !comments + List.length inserted;
    let errors source = Round_trip.source_errors ?widths source in
    if not (List.is_empty (errors (insert source inserted)))
    then (
      incr failures;
      if !failures <= 3
      then (
        let fewer =
          shrink source inserted ~fails:(fun source ->
            not (List.is_empty (errors source)))
        in
        let source = insert source fewer in
        print_endline source;
        Option.iter (List.hd (errors source)) ~f:print_s))
  done;
  print_s [%message (!comments : int) (!failures : int)]
;;

let%expect_test "random programs with comments format" =
  check
    ~widths:[ 80; 30 ]
    ~seed:2
    ~count:1000
    ~density:3
    ~gen:(fun random i -> { Gen.random; size = i % 40; deep = false })
    ();
  [%expect {| ((!comments 32228) (!failures 0)) |}]
;;

let%expect_test "deep random programs with a comment in every gap format" =
  check
    ~widths:[ 80; 30 ]
    ~seed:3
    ~count:100
    ~density:1
    ~gen:(fun random i -> { Gen.random; size = 10 + i; deep = true })
    ();
  [%expect {| ((!comments 26285) (!failures 0)) |}]
;;

let%expect_test "messy random programs with comments format" =
  check
    ~messy:true
    ~seed:4
    ~count:1000
    ~density:3
    ~gen:(fun random i -> { Gen.random; size = i % 40; deep = false })
    ();
  [%expect {| ((!comments 31644) (!failures 0)) |}]
;;

let%expect_test "messy random programs with a comment in most gaps format" =
  check
    ~messy:true
    ~seed:5
    ~count:200
    ~density:2
    ~gen:(fun random i -> { Gen.random; size = 5 + (i / 4); deep = true })
    ();
  [%expect {| ((!comments 15824) (!failures 0)) |}]
;;
