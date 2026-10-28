open! Import

let config = { Config.default with resamples = 2000 }

(* [n] values around [mean], spread deterministically by up to 1%. *)
let times ~mean ~n =
  Array.init n ~f:(fun i ->
    mean *. (1. +. (0.01 *. Float.of_int ((i * 7 % 11) - 5) /. 5.)))
;;

let compare ~baseline ~current =
  let comparison = Comparison.compare config ~baseline ~current in
  print_s [%sexp (comparison : Comparison.t)];
  print_endline (Comparison.Verdict.to_string comparison.verdict)
;;

let baseline = times ~mean:1000. ~n:50

let%expect_test "identical" =
  compare ~baseline ~current:baseline;
  [%expect
    {|
    ((change
      ((point 0) (lower -0.0023966710352310017) (upper 0.0025213388428350966)))
     (verdict No_change))
    No change in performance detected.
    |}]
;;

let%expect_test "the same values in another order" =
  compare ~baseline ~current:(Array.rev baseline);
  [%expect
    {|
    ((change
      ((point 0) (lower -0.0024791199148469613) (upper 0.0024812902709410167)))
     (verdict No_change))
    No change in performance detected.
    |}]
;;

let%expect_test "clearly slower" =
  compare ~baseline ~current:(times ~mean:1100. ~n:50);
  [%expect
    {|
    ((change
      ((point 0.10000000000000009) (lower 0.0973636618612458)
       (upper 0.10277347272711868)))
     (verdict Regressed))
    Performance has regressed.
    |}]
;;

let%expect_test "clearly faster" =
  compare ~baseline ~current:(times ~mean:800. ~n:30);
  [%expect
    {|
    ((change
      ((point -0.19982931967890749) (lower -0.20222713471166129)
       (upper -0.19751946687005645)))
     (verdict Improved))
    Performance has improved.
    |}]
;;

let%expect_test "a consistent shift under the noise threshold" =
  compare ~baseline ~current:(times ~mean:1010. ~n:50);
  [%expect
    {|
    ((change
      ((point 0.010000000000000009) (lower 0.0075793622544167109)
       (upper 0.012546552231263436)))
     (verdict Within_noise))
    Change within noise threshold.
    |}]
;;

let%expect_test "a noisy change is not significant" =
  compare
    ~baseline:[| 900.; 1100.; 950.; 1050. |]
    ~current:[| 1150.; 850.; 1000.; 1100. |];
  [%expect
    {|
    ((change
      ((point 0.024999999999999911) (lower -0.10843373493975905)
       (upper 0.16216216216216206)))
     (verdict No_change))
    No change in performance detected.
    |}]
;;

let%expect_test "verdicts" =
  List.iter
    [ Comparison.Verdict.Improved; Regressed; No_change; Within_noise ]
    ~f:(fun v ->
      print_s
        [%message
          "" ~_:(v : Comparison.Verdict.t) ~_:(Comparison.Verdict.to_string v)]);
  [%expect
    {|
    (Improved "Performance has improved.")
    (Regressed "Performance has regressed.")
    (No_change "No change in performance detected.")
    (Within_noise "Change within noise threshold.")
    |}]
;;
