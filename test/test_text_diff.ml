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
