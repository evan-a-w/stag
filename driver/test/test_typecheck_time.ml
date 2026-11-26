open! Import
open Mode_helpers

let ratio ~small ~large ~within source =
  let time n =
    List.init 3 ~f:(fun _ ->
      let start = Time_ns.now () in
      assert (Result.is_ok (checked (source n)));
      Time_ns.diff (Time_ns.now ()) start)
    |> List.min_elt ~compare:Time_ns.Span.compare
    |> Option.value_exn
  in
  let small_time = time small in
  let large_time = time large in
  Time_ns.Span.( < ) large_time (Time_ns.Span.scale small_time within)
;;

let names n = List.init n ~f:(sprintf "x%d")

let%expect_test "many parameters take at most quadratic time" =
  (* The signature's constraints are quadratic in the parameters. *)
  let first n = sprintf "let f %s = x0" (String.concat ~sep:" " (names n)) in
  let tuple n =
    sprintf
      "let f %s = (%s)"
      (String.concat ~sep:" " (names n))
      (String.concat ~sep:", " (names n))
  in
  let curried n =
    sprintf
      "let f = %s x0"
      (String.concat (List.map (names n) ~f:(sprintf "fun %s ->")))
  in
  List.iter
    [ "first", first; "tuple", tuple; "curried", curried ]
    ~f:(fun (name, source) ->
      print_s
        [%message
          name
            ~four_times_within_30_times_the_time:
              (ratio ~small:50 ~large:200 ~within:30. source : bool)]);
  [%expect
    {|
    (first (four_times_within_30_times_the_time true))
    (tuple (four_times_within_30_times_the_time true))
    (curried (four_times_within_30_times_the_time true))
    |}]
;;

let%expect_test "a chain of lets takes linear time" =
  let chain n =
    sprintf
      "let g a0 =\n%s a%d"
      (String.concat
         (List.init n ~f:(fun i -> sprintf "let a%d = a%d in\n" (i + 1) i)))
      n
  in
  print_s
    [%message
      ""
        ~four_times_within_8_times_the_time:
          (ratio ~small:500 ~large:2000 ~within:8. chain : bool)];
  [%expect {| (four_times_within_8_times_the_time true) |}]
;;
