open! Import

(* [Core.Sexp] against OCaml's Sexplib and Parsexp, on generated sexps and
   inputs. *)

module Sexp = Sexplib0.Sexp

let literal s =
  String.concat_map s ~f:(fun c ->
    if Char.is_alphanum c || Char.equal c ' '
    then String.of_char c
    else sprintf "\\x%02x" (Char.to_int c))
;;

let rec expr : Sexp.t -> string = function
  | Atom s -> sprintf "a \"%s\"" (literal s)
  | List ts ->
    List.fold_right ts ~init:"Nil" ~f:(fun t acc ->
      sprintf "Cons ((%s), %s)" (expr t) acc)
    |> sprintf "l (%s)"
;;

(* Runs the stag program [source], whose output is chunks each ended by a
   line [-]. *)
let run_chunks source =
  let output = Buffer.create 1024 in
  let limits =
    { Limits.fuel = None
    ; max_depth = Some 100_000
    ; time_limit = Some (Time_ns.Span.of_int_sec 300)
    }
  in
  match
    Driver.check_files
      ~limits
      ~output:(Buffer.add_string output)
      [ { Source_file.path = "t.stag"; contents = source } ]
  with
  | Error error -> raise_s [%sexp (error : Driver.Error.t list)]
  | Ok driver ->
    Driver.eval ~limits driver |> ok_exn;
    let chunks = String.split (Buffer.contents output) ~on:'\001' in
    List.take chunks (List.length chunks - 1)
;;

let header =
  {|
let a s = Core.Sexp.Atom s
let l ts = Core.Sexp.List ts
let end_chunk _ = print_string "\001"

let parse_result r =
  (match r with
  | Ok ts -> print_string (Core.String.append "Ok " (Core.Sexp.to_string (l ts)))
  | Error e ->
    print_string
      (Core.String.concat " "
         (Cons ("Error", Cons (e.Core.Sexp.Parse_error.message,
          Cons (Core.Int.to_string e.Core.Sexp.Parse_error.offset,
          Cons (Core.Int.to_string e.Core.Sexp.Parse_error.line,
          Cons (Core.Int.to_string e.Core.Sexp.Parse_error.col, Nil))))))));
  end_chunk ()

let print s =
  print_string (Core.Sexp.to_string s);
  end_chunk ();
  print_string (Core.Sexp.to_string_hum s);
  end_chunk ()

let singleton t = Cons (t, Nil)

let parse s =
  parse_result (Core.Result.map singleton (Core.Sexp.of_string s));
  parse_result (Core.Sexp.of_string_many s)
|}
;;

let program ~sexps ~inputs =
  String.concat
    (header
     :: List.concat
          [ List.mapi sexps ~f:(fun i t ->
              sprintf "let print%d = print (%s)\n" i (expr t))
          ; List.mapi inputs ~f:(fun i s ->
              sprintf "let parse%d = parse \"%s\"\n" i (literal s))
          ])
;;

let parsexp_result result =
  match result with
  | Ok ts -> "Ok " ^ Sexp.to_string (List ts)
  | Error e ->
    let { Parsexp.Positions.line; col; offset } =
      Parsexp.Parse_error.position e
    in
    sprintf "Error %s %d %d %d" (Parsexp.Parse_error.message e) offset line col
;;

let expected ~sexps ~inputs =
  List.concat_map sexps ~f:(fun t -> [ Sexp.to_string t; Sexp.to_string_hum t ])
  @ List.concat_map inputs ~f:(fun s ->
    [ parsexp_result (Result.map (Parsexp.Single.parse_string s) ~f:List.return)
    ; parsexp_result (Parsexp.Many.parse_string s)
    ])
;;

let awkward_chars = "ab09_-'#|;()\" \t\n\r\\\000\011\012\127\200\255"

let random_atom state =
  let char () =
    if Random.State.bool state
    then 'x'
    else awkward_chars.[Random.State.int state (String.length awkward_chars)]
  in
  let length =
    match Random.State.int state 10 with
    | 0 -> 0
    | 1 -> 20 + Random.State.int state 70
    | _ -> 1 + Random.State.int state 6
  in
  String.init length ~f:(fun _ -> char ())
;;

let rec random_sexp state ~depth : Sexp.t =
  if depth = 0 || Random.State.int state 3 = 0
  then Atom (random_atom state)
  else (
    let length =
      match Random.State.int state 6 with
      | 0 -> 0
      | 1 when depth <= 2 -> 10 + Random.State.int state 30
      | _ -> 1 + Random.State.int state 4
    in
    List (List.init length ~f:(fun _ -> random_sexp state ~depth:(depth - 1))))
;;

(* Deep and wide, to reach [Format]'s maximum indentation. *)
let rec staircase n : Sexp.t =
  if n = 0
  then Atom "bottom"
  else List [ Atom (sprintf "level%d" n); staircase (n - 1); Atom "after" ]
;;

let tokens =
  [| "("
   ; ")"
   ; " "
   ; "\n"
   ; "\r"
   ; "\r\n"
   ; "\t"
   ; "\012"
   ; "\011"
   ; ";"
   ; "#;"
   ; "#|"
   ; "|#"
   ; "#"
   ; "|"
   ; "\""
   ; "\\"
   ; "a"
   ; "b1"
   ; "\\n"
   ; "\\x4"
   ; "\\x41"
   ; "\\xg"
   ; "\\12"
   ; "\\123"
   ; "\\256"
   ; "\\\n"
   ; "\\\r\n"
   ; "\\\r"
   ; "\"abc\""
   ; "\"a\\\"b\""
   ; "\\ "
   ; "\\q"
   ; "\000"
   ; "\200"
   ; "'"
   ; "x y"
  |]
;;

let random_input state =
  String.concat
    (List.init
       (1 + Random.State.int state 10)
       ~f:(fun _ -> tokens.(Random.State.int state (Array.length tokens))))
;;

let edge_inputs =
  [ ""
  ; " "
  ; "a"
  ; "a\rb"
  ; "a \r b"
  ; "a \r\n b"
  ; "a\r\nb"
  ; "; c\rx\nb"
  ; "#"
  ; "a#"
  ; "#a"
  ; "|"
  ; "||"
  ; "a|"
  ; "#|"
  ; "|#"
  ; "a#b|c"
  ; "a#;b"
  ; "#;"
  ; "#;)"
  ; "(#;a)"
  ; "(#;a b)"
  ; "#;#;a b c"
  ; "#;(a b) c"
  ; "#|#|a|#|#b"
  ; "#| \"|#\" |# a"
  ; "#| \" |# a"
  ; "#| a |# b |#"
  ; "#|a"
  ; "\"\\1\""
  ; "\"\\12x\""
  ; "\"\\x4\""
  ; "\"\\xg0\""
  ; "\"\\x41\""
  ; "\"a\\\n  \tb\""
  ; "\"a\\\r\n b\""
  ; "\"a\\\rb\""
  ; "\"a\\\r\""
  ; "\"\\ \""
  ; "\"\\'\""
  ; "\"\\q\""
  ; "\"\\\""
  ; "\"a\r\nb\""
  ; "a\012b"
  ; "a\011b"
  ; "a\000b"
  ; "\"\\255\""
  ; "\"\\256\""
  ; "\"\\999\""
  ; "\"\\0\""
  ; "\"\\00a\""
  ; ")"
  ; "("
  ; "(a"
  ; "a)"
  ; "a(b"
  ; "a\"b\""
  ; "\"a\"b"
  ; "#;a"
  ; "#; a"
  ; "#;;c\n a b"
  ; "#;#|c|# a b"
  ; "#|\"\\\"|#\"|# a"
  ; "#|\\|# a"
  ; "#|#|a|# b"
  ; "# |a"
  ; "#|# a"
  ; "a;b"
  ; "a\\b"
  ; "\"\\n\\t\\b\\r\""
  ; ";x"
  ; "#;(a"
  ; "a #|"
  ; "a#|"
  ; "a#|b"
  ; "\"a"
  ; "\"\\"
  ; "\"\\x"
  ; "\"\\1"
  ; "\"\\12"
  ; "\t\012a"
  ; "a\tb"
  ; "\r"
  ; "\r\n"
  ; "a\r"
  ; "#|\r|#a"
  ; "; x\r"
  ; "||#"
  ; "##|"
  ; "(#;)"
  ; "(a #;)"
  ; "#;#;a"
  ; "a|#"
  ; "#|a|#b"
  ; "a#|b|#"
  ; "#| ; |# a"
  ; "#;\"a\" b"
  ; "|#a"
  ; "a||#"
  ; "#|a||#b"
  ; "#|a#|#b"
  ; "#||#a"
  ; "#;#|c|#"
  ; "a #;b"
  ; "(a)(b)"
  ; "#|\"|# a"
  ; "\"\\x4G\""
  ; "\"\\X41\""
  ; "\"\\xFF\""
  ; "#;\r\na b"
  ; "(#|x|#)"
  ; "(a#)"
  ; "(#)"
  ; "#(a)"
  ; "#\"a\""
  ; "(a\n  b))"
  ; "(a))"
  ; "a b"
  ; "(a)\n  (b)"
  ]
;;

let%expect_test "Core.Sexp prints and parses as Sexplib and Parsexp" =
  let state = Random.State.make [| 42 |] in
  let sexps =
    List.init 300 ~f:(fun i -> random_sexp state ~depth:(1 + (i % 6)))
    @ List.map [ 1; 5; 20; 40; 60 ] ~f:staircase
  in
  let inputs =
    edge_inputs
    @ List.init 1500 ~f:(fun _ -> random_input state)
    @ List.concat_map (List.take sexps 100) ~f:(fun t ->
      [ Sexp.to_string t; Sexp.to_string_hum t ])
  in
  let size =
    List.sum (module Int) sexps ~f:(fun t -> String.length (Sexp.to_string t))
  in
  let actual = run_chunks (program ~sexps ~inputs) in
  let expected = expected ~sexps ~inputs in
  let mismatches =
    List.zip_exn actual expected
    |> List.filter ~f:(fun (actual, expected) ->
      not (String.equal actual expected))
  in
  printf
    "%d sexps of %d bytes (%d printed on several lines), %d inputs, %d \
     results, %d mismatches\n"
    (List.length sexps)
    size
    (List.count sexps ~f:(fun t -> String.mem (Sexp.to_string_hum t) '\n'))
    (List.length inputs)
    (List.length actual)
    (List.length mismatches);
  List.iter (List.take mismatches 10) ~f:(fun (actual, expected) ->
    print_s [%message "mismatch" (actual : string) (expected : string)]);
  let outcomes =
    List.map inputs ~f:(fun s ->
      match Parsexp.Many.parse_string s with
      | Ok _ -> "Ok"
      | Error e -> Parsexp.Parse_error.message e)
    |> List.sort_and_group ~compare:String.compare
    |> List.map ~f:(fun group -> List.hd_exn group, List.length group)
  in
  print_s [%message "Parsexp.Many outcomes" (outcomes : (string * int) list)];
  [%expect
    {|
    305 sexps of 116273 bytes (119 printed on several lines), 1819 inputs, 4248 results, 0 mismatches
    ("Parsexp.Many outcomes"
     (outcomes
      ((Ok 896) ("comment tokens in unquoted atom" 134)
       ("escape sequence in quoted string out of range" 10)
       ("illegal end of comment" 90) ("unclosed parentheses at end of input" 63)
       ("unexpected character after carriage return" 234)
       ("unexpected character: ')'" 124)
       ("unexpected end of input after carriage return" 54)
       ("unterminated block comment" 76)
       ("unterminated decimal escape sequence" 12)
       ("unterminated hexadecimal escape sequence" 21)
       ("unterminated quoted string" 86) ("unterminated sexp comment" 19))))
    |}]
;;
