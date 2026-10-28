open! Import

(* Random programs ([Gen]), printed, with comments of random shapes put in
   random gaps between tokens, checked to format keeping their syntax and
   comments, idempotently. A failure is shown with as few of its comments as
   still fail. *)

let comment (gen : Gen.t) i =
  let body =
    match Gen.int gen 4 with
    | 0 -> sprintf "(* (* nested %d *) *)" i
    | 1 -> sprintf "(** doc %d *)" i
    | 2 -> sprintf "(* \"*)\" %d\n   second line *)" i
    | _ -> sprintf "(* c%d *)" i
  in
  let space () = Gen.pick gen [ " "; " "; "\n"; "\n\n" ] in
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

let random_comments (gen : Gen.t) ~density source =
  match Lexer.tokenize ~file:"t.stag" source with
  | Error error -> raise_s [%sexp (error : Lexer.Error.t)]
  | Ok tokens ->
    Array.to_list tokens
    |> List.filter ~f:(fun _ -> Gen.int gen density = 0)
    |> List.mapi ~f:(fun i (token : Lexer.Located.t) ->
      token.loc.start.offset, comment gen i)
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

let check ~seed ~count ~density ~gen =
  let random = Random.State.make [| seed |] in
  let comments = ref 0 in
  let failures = ref 0 in
  for i = 0 to count - 1 do
    let gen : Gen.t = gen random i in
    let program =
      List.init (1 + Gen.int gen 3) ~f:(fun _ -> Gen.toplevel gen)
    in
    let source = Printer.program ~width:(Gen.pick gen [ 80; 30 ]) program in
    let inserted = random_comments gen ~density source in
    comments := !comments + List.length inserted;
    let errors source = Round_trip.source_errors ~widths:[ 80; 30 ] source in
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
        List.iter (errors source) ~f:print_s))
  done;
  print_s [%message (!comments : int) (!failures : int)]
;;

let%expect_test "random programs with comments format" =
  check ~seed:2 ~count:1000 ~density:3 ~gen:(fun random i ->
    { Gen.random; size = i % 40; deep = false });
  [%expect {| ((!comments 29731) (!failures 0)) |}]
;;

let%expect_test "deep random programs with a comment in every gap format" =
  check ~seed:3 ~count:100 ~density:1 ~gen:(fun random i ->
    { Gen.random; size = 10 + i; deep = true });
  [%expect {| ((!comments 22030) (!failures 0)) |}]
;;
