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
