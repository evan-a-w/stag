open! Import

let show a b =
  let a = String.to_array a in
  let b = String.to_array b in
  Alignment.align a b ~compare:Char.compare
  |> Array.iteri ~f:(fun i m ->
    match m with
    | Some j -> printf "%c%c " a.(i) b.(j)
    | None -> printf "%c- " a.(i));
  print_endline ""
;;

let%expect_test "matches as a shortest edit script" =
  show "abcabba" "cbabac";
  show "((x))" "x";
  show "" "ab";
  show "ab" "";
  show "aaaa" "aa";
  [%expect
    {|
    a- b- cc aa bb b- aa
    (- (- xx )- )-

    a- b-
    aa aa a- a-
    |}]
;;

let%expect_test "elements occurring once in each are matched first" =
  (* Myers would match the [a]s, as more of them. *)
  show "aaxbb" "bbxaa";
  show "x(a)y(b)z" "xaybz";
  [%expect
    {|
    a- a- xx b- b-
    xx (- aa )- yy (- bb )- zz
    |}]
;;
