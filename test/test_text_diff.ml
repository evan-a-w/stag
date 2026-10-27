open! Core
open Stag

let diff old new_ =
  match Text_diff.unified ~old_name:"a" ~new_name:"b" ~old ~new_ with
  | None -> print_endline "(equal)"
  | Some diff -> print_string diff
;;

let lines n = List.init n ~f:(fun i -> sprintf "%d\n" (i + 1)) |> String.concat

let%expect_test "unified" =
  diff "x\n" "x\n";
  diff "" "a\n";
  diff "a\n" "";
  diff
    (lines 10)
    (String.substr_replace_all (lines 10) ~pattern:"5\n" ~with_:"five\n");
  (* Changes far apart are separate hunks; close ones merge. *)
  diff
    (lines 20)
    (lines 20
     |> String.substr_replace_first ~pattern:"2\n" ~with_:""
     |> String.substr_replace_first ~pattern:"18\n" ~with_:"18\nnew\n");
  diff
    (lines 10)
    (String.substr_replace_all (lines 10) ~pattern:"\n4\n" ~with_:"\n4\nx\ny\n");
  diff "a\nb" "a\nc";
  [%expect
    {|
    (equal)
    --- a
    +++ b
    @@ -0,0 +1,1 @@
    +a
    --- a
    +++ b
    @@ -1,1 +0,0 @@
    -a
    --- a
    +++ b
    @@ -2,7 +2,7 @@
     2
     3
     4
    -5
    +five
     6
     7
     8
    --- a
    +++ b
    @@ -1,5 +1,4 @@
     1
    -2
     3
     4
     5
    @@ -16,5 +15,6 @@
     16
     17
     18
    +new
     19
     20
    --- a
    +++ b
    @@ -2,6 +2,8 @@
     2
     3
     4
    +x
    +y
     5
     6
     7
    --- a
    +++ b
    @@ -1,2 +1,2 @@
     a
    -b
    +c
    |}]
;;

let%expect_test "changes at start, middle and end" =
  diff "a\nb\nc\nd\n" "x\nb\nc\nd\n";
  diff "a\nb\nc\nd\ne\n" "a\nb\nx\nd\ne\n";
  diff "a\nb\nc\nd\n" "a\nb\nc\nx\n";
  [%expect
    {|
    --- a
    +++ b
    @@ -1,4 +1,4 @@
    -a
    +x
     b
     c
     d
    --- a
    +++ b
    @@ -1,5 +1,5 @@
     a
     b
    -c
    +x
     d
     e
    --- a
    +++ b
    @@ -1,4 +1,4 @@
     a
     b
     c
    -d
    +x
    |}]
;;

let%expect_test "overlapping common prefix and suffix" =
  diff "a\na\n" "a\n";
  diff "a\n" "a\na\n";
  diff "a\na\na\n" "a\na\na\na\na\n";
  diff "a\nb\na\n" "a\n";
  diff "x\na\n" "a\nx\na\n";
  diff "0\n2\n0\n" "2\n2\n0\n";
  diff "2\n1\n1\n" "1\n";
  [%expect
    {|
    --- a
    +++ b
    @@ -1,2 +1,1 @@
     a
    -a
    --- a
    +++ b
    @@ -1,1 +1,2 @@
     a
    +a
    --- a
    +++ b
    @@ -1,3 +1,5 @@
     a
     a
     a
    +a
    +a
    --- a
    +++ b
    @@ -1,3 +1,1 @@
     a
    -b
    -a
    --- a
    +++ b
    @@ -1,2 +1,3 @@
    +a
     x
     a
    --- a
    +++ b
    @@ -1,3 +1,3 @@
    -0
     2
    +2
     0
    --- a
    +++ b
    @@ -1,3 +1,1 @@
    -2
     1
    -1
    |}]
;;

let%expect_test "empty files" =
  diff "" "";
  diff "" "a\nb\n";
  diff "a\nb\n" "";
  [%expect
    {|
    (equal)
    --- a
    +++ b
    @@ -0,0 +1,2 @@
    +a
    +b
    --- a
    +++ b
    @@ -1,2 +0,0 @@
    -a
    -b
    |}]
;;

let%expect_test "large file with a small change" =
  diff
    (lines 100_000)
    (String.substr_replace_all
       (lines 100_000)
       ~pattern:"\n50000\n"
       ~with_:"\nx\n");
  [%expect
    {|
    --- a
    +++ b
    @@ -49997,7 +49997,7 @@
     49997
     49998
     49999
    -50000
    +x
     50001
     50002
     50003
    |}]
;;
