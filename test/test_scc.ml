open! Core
open Stag

let print successors =
  Scc.topological_components ~successors:(Array.of_list successors)
  |> [%sexp_of: int list list]
  |> print_s
;;

let%expect_test "empty" =
  print [];
  [%expect {| () |}]
;;

let%expect_test "self loops and singletons" =
  print [ [ 0 ]; []; [ 2 ] ];
  [%expect {| ((0) (1) (2)) |}]
;;

let%expect_test "cycles" =
  print [ [ 1 ]; [ 2 ]; [ 0 ] ];
  print [ [ 3 ]; [ 2 ]; [ 1; 0 ]; [ 0 ] ];
  print [ [ 1 ]; [ 0; 2 ]; [ 3 ]; [ 2; 4 ]; [] ];
  [%expect
    {|
    ((0 1 2))
    ((0 3) (1 2))
    ((4) (2 3) (0 1))
    |}]
;;

let%expect_test "dependencies come first, otherwise smallest vertex first" =
  print [ []; []; []; [] ];
  print [ [ 3 ]; []; [ 1 ]; [] ];
  print [ [ 2 ]; [ 2 ]; [] ];
  print [ [ 1; 2 ]; [ 3 ]; [ 3 ]; [] ];
  [%expect
    {|
    ((0) (1) (2) (3))
    ((1) (2) (3) (0))
    ((2) (0) (1))
    ((3) (1) (2) (0))
    |}]
;;

let%expect_test "disconnected" =
  print [ [ 1 ]; [ 0 ]; [ 3 ]; [ 2 ]; [] ];
  print [ [ 4 ]; [ 3 ]; [ 0 ]; [ 1 ]; [ 2 ] ];
  [%expect
    {|
    ((0 1) (2 3) (4))
    ((0 2 4) (1 3))
    |}]
;;
