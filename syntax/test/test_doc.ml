open! Import
open Doc

let show doc =
  List.iter [ 15; 12; 5 ] ~f:(fun width ->
    printf "-- width %d\n%s\n" width (render ~width doc))
;;

let%expect_test "groups break outermost first" =
  let call f args =
    group (text f ^^ nest 2 (concat (List.map args ~f:(fun a -> line ^^ a))))
  in
  show
    (call
       "f"
       [ text "a"
       ; text "(" ^^ align (call "g" [ text "bb"; text "cc" ]) ^^ text ")"
       ; text "d"
       ]);
  [%expect
    {|
    -- width 15
    f a (g bb cc) d
    -- width 12
    f
      a
      (g bb cc)
      d
    -- width 5
    f
      a
      (g
         bb
         cc)
      d
    |}]
;;

let%expect_test "if_break, cut and hardline" =
  show
    (group
       (text "{"
        ^^ nest
             2
             (cut ^^ separate (text ";" ^^ line) [ text "x = 1"; text "y = 2" ])
        ^^ if_break ";"
        ^^ cut
        ^^ text "}"));
  show (group (text "a" ^^ line ^^ text "b" ^^ hardline ^^ text "c"));
  show (group (text "a" ^^ line ^^ text "b\nc" ^^ nest 4 (line ^^ text "d")));
  [%expect
    {|
    -- width 15
    {x = 1; y = 2}
    -- width 12
    {
      x = 1;
      y = 2;
    }
    -- width 5
    {
      x = 1;
      y = 2;
    }
    -- width 15
    a
    b
    c
    -- width 12
    a
    b
    c
    -- width 5
    a
    b
    c
    -- width 15
    a
    b
    c
        d
    -- width 12
    a
    b
    c
        d
    -- width 5
    a
    b
    c
        d
    |}]
;;

let%expect_test "a group fits only if the text after it, up to a line, fits too"
  =
  let doc after = group (text "f" ^^ nest 2 (line ^^ text "x")) ^^ after in
  List.iter
    [ text "+tail"
    ; group (text "+tail" ^^ line ^^ text "more")
    ; text "+tail" ^^ hardline ^^ text "more"
    ]
    ~f:(fun after ->
      List.iter [ 8; 7 ] ~f:(fun width ->
        printf "-- width %d\n%s\n" width (render ~width (doc after))));
  [%expect
    {|
    -- width 8
    f x+tail
    -- width 7
    f
      x+tail
    -- width 8
    f x+tail
    more
    -- width 7
    f
      x+tail
    more
    -- width 8
    f x+tail
    more
    -- width 7
    f
      x+tail
    more
    |}]
;;

let%expect_test
    "nest indents from the enclosing indentation, align from the column"
  =
  let body = text "a" ^^ line ^^ text "b" in
  show (text "xyz(" ^^ nest 2 (align body) ^^ text ")");
  show (text "xyz(" ^^ align (nest 2 body) ^^ text ")");
  show (text "xyz(" ^^ nest 2 (line ^^ align body) ^^ text ")");
  [%expect
    {|
    -- width 15
    xyz(a
        b)
    -- width 12
    xyz(a
        b)
    -- width 5
    xyz(a
        b)
    -- width 15
    xyz(a
          b)
    -- width 12
    xyz(a
          b)
    -- width 5
    xyz(a
          b)
    -- width 15
    xyz(
      a
      b)
    -- width 12
    xyz(
      a
      b)
    -- width 5
    xyz(
      a
      b)
    |}]
;;

let%expect_test "alt: [a] while its lines all fit with its groups flat" =
  let record =
    group
      (text "{"
       ^^ nest
            2
            (cut ^^ separate (text ";" ^^ line) [ text "x = 1"; text "y = 2" ])
       ^^ cut
       ^^ text "}")
  in
  let definition =
    group
      (text "let r ="
       ^^ alt
            (nest 2 (line ^^ record) ^^ line ^^ text "in")
            (text " " ^^ record ^^ text " in"))
  in
  List.iter [ 30; 20; 15 ] ~f:(fun width ->
    printf "-- width %d\n%s\n" width (render ~width definition));
  [%expect
    {|
    -- width 30
    let r = {x = 1; y = 2} in
    -- width 20
    let r =
      {x = 1; y = 2}
    in
    -- width 15
    let r = {
      x = 1;
      y = 2
    } in
    |}]
;;

let%expect_test "fits_flat" =
  let doc = group (text "abc" ^^ line ^^ text "def") in
  List.iter [ 7; 6 ] ~f:(fun width ->
    printf "%d %b\n" width (fits_flat ~width doc));
  print_s
    [%sexp (fits_flat ~width:80 (text "a" ^^ hardline ^^ text "b") : bool)];
  print_s [%sexp (fits_flat ~width:80 (text "a\nb") : bool)];
  [%expect
    {|
    7 true
    6 false
    false
    false
    |}]
;;

let%expect_test "a space that prints nothing is not measured" =
  (* Each line is 10 wide, fitting only if the spaces after a line, a space
     or a newline count for nothing. *)
  let doc =
    separate
      hardline
      [ group (text "abcde" ^^ line ^^ space ^^ text "fghi")
      ; group (text "ab" ^^ space ^^ space ^^ text "cde" ^^ line ^^ text "fgh")
      ; group (space ^^ text "abcdefgh" ^^ line ^^ text "i")
      ; alt (text "abcd" ^^ space ^^ space ^^ text "efgh") (text "no")
        ^^ text "i"
      ]
  in
  print_endline (render ~width:10 doc);
  List.iter [ 10; 9 ] ~f:(fun width ->
    printf
      "%d %b\n"
      width
      (fits_flat
         ~width
         (space ^^ text "abcde" ^^ space ^^ space ^^ text "fghi")));
  [%expect
    {|
    abcde fghi
    ab cde fgh
    abcdefgh i
    abcd efghi
    10 true
    9 false
    |}]
;;

let%expect_test "a group with a line ended before a flat line breaks" =
  (* Kept flat, [b] would be at the outer indentation, not the line's. *)
  show
    (group
       (text "x"
        ^^ hardline
        ^^ group (text "a (* a *)" ^^ end_line ^^ nest 2 line)
        ^^ text "b"));
  [%expect {|
    -- width 15
    x
    a (* a *)
      b
    -- width 12
    x
    a (* a *)
      b
    -- width 5
    x
    a (* a *)
      b
    |}]
;;
