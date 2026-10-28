open! Import

let one_to_ten = Array.init 10 ~f:(fun i -> Float.of_int (i + 1))

let%expect_test "percentiles, median, MAD and stddev of 1..10" =
  List.iter [ 0.; 0.25; 0.5; 0.9; 0.99; 1. ] ~f:(fun p ->
    print_s
      [%message
        "" (p : float) ~percentile:(Stats.percentile one_to_ten p : float)]);
  print_s
    [%message
      ""
        ~mean:(Stats.mean one_to_ten : float)
        ~median:(Stats.median one_to_ten : float)
        ~mad:(Stats.mad one_to_ten : float)
        ~stddev:(Stats.stddev one_to_ten : float)];
  [%expect
    {|
    ((p 0) (percentile 1))
    ((p 0.25) (percentile 3.25))
    ((p 0.5) (percentile 5.5))
    ((p 0.9) (percentile 9.1))
    ((p 0.99) (percentile 9.91))
    ((p 1) (percentile 10))
    ((mean 5.5) (median 5.5) (mad 3.7064999999999997)
     (stddev 3.0276503540974917))
    |}]
;;

let%expect_test "percentiles ignore the order of the data, and do not sort it" =
  let data = [| 10.; 1.; 7.; 3. |] in
  print_s
    [%message
      ""
        ~median:(Stats.median data : float)
        ~p25:(Stats.percentile data 0.25 : float)
        (data : float array)];
  [%expect {| ((median 5) (p25 2.5) (data (10 1 7 3))) |}]
;;

let%expect_test "a single value" =
  let data = [| 42. |] in
  print_s
    [%message
      ""
        ~p0:(Stats.percentile data 0. : float)
        ~p90:(Stats.percentile data 0.9 : float)
        ~stddev:(Stats.stddev data : float)
        ~mad:(Stats.mad data : float)];
  print_s [%sexp (Stats.summarize Config.quick data : Stats.t)];
  [%expect
    {|
    ((p0 42) (p90 42) (stddev 0) (mad 0))
    ((count 1) (mean ((point 42) (lower 42) (upper 42))) (median 42) (min 42)
     (max 42) (stddev 0) (mad 0) (p90 42) (p99 42)
     (outliers ((low_severe 0) (low_mild 0) (high_mild 0) (high_severe 0))))
    |}]
;;

let%expect_test "empty data raises" =
  print_s
    [%sexp (Or_error.try_with (fun () -> Stats.mean [||]) : float Or_error.t)];
  [%expect {| (Error "Stats: empty data") |}]
;;

let%expect_test "outliers" =
  let show data =
    let q1 = Stats.percentile data 0.25 in
    let q3 = Stats.percentile data 0.75 in
    let iqr = q3 -. q1 in
    print_s
      [%message
        ""
          (q1 : float)
          (q3 : float)
          ~mild_fences:
            ((q1 -. (1.5 *. iqr), q3 +. (1.5 *. iqr)) : float * float)
          ~severe_fences:
            ((q1 -. (3. *. iqr), q3 +. (3. *. iqr)) : float * float)
          ~outliers:(Stats.Outliers.classify data : Stats.Outliers.t)
          ~total:(Stats.Outliers.total (Stats.Outliers.classify data) : int)]
  in
  show one_to_ten;
  [%expect
    {|
    ((q1 3.25) (q3 7.75) (mild_fences (-3.5 14.5)) (severe_fences (-10.25 21.25))
     (outliers ((low_severe 0) (low_mild 0) (high_mild 0) (high_severe 0)))
     (total 0))
    |}];
  show
    [| -40.; -5.; 10.; 11.; 12.; 13.; 14.; 15.; 16.; 17.; 18.; 19.; 35.; 60. |];
  [%expect
    {|
    ((q1 11.25) (q3 17.75) (mild_fences (1.5 27.5)) (severe_fences (-8.25 37.25))
     (outliers ((low_severe 1) (low_mild 1) (high_mild 1) (high_severe 1)))
     (total 4))
    |}];
  show [| 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10.; 100.; 200. |];
  [%expect
    {|
    ((q1 3.75) (q3 9.25) (mild_fences (-4.5 17.5)) (severe_fences (-12.75 25.75))
     (outliers ((low_severe 0) (low_mild 0) (high_mild 0) (high_severe 2)))
     (total 2))
    |}]
;;

let%expect_test "values exactly on a fence are not beyond it" =
  (* q1 = 1 and q3 = 2: the severe fences are -2 and 5, so -2 and 5 are only
     mild outliers. *)
  let data = [| -2.; 1.; 1.; 1.; 1.; 2.; 2.; 2.; 2.; 5. |] in
  print_s
    [%message
      ""
        ~q1:(Stats.percentile data 0.25 : float)
        ~q3:(Stats.percentile data 0.75 : float)
        ~outliers:(Stats.Outliers.classify data : Stats.Outliers.t)];
  [%expect
    {|
    ((q1 1) (q3 2)
     (outliers ((low_severe 0) (low_mild 1) (high_mild 1) (high_severe 0))))
    |}]
;;

let%expect_test "bootstrap" =
  let random = Random.State.make [| 0 |] in
  let bootstrap ?(confidence = 0.95) data ~f =
    print_s
      [%sexp
        (Stats.bootstrap ~random ~resamples:1000 ~confidence data ~f
         : Stats.Estimate.t)]
  in
  bootstrap one_to_ten ~f:Stats.mean;
  [%expect {| ((point 5.5) (lower 3.7) (upper 7.3)) |}];
  bootstrap ~confidence:0.5 one_to_ten ~f:Stats.mean;
  [%expect {| ((point 5.5) (lower 4.8) (upper 6.1)) |}];
  bootstrap one_to_ten ~f:Stats.median;
  [%expect {| ((point 5.5) (lower 3) (upper 8.5)) |}];
  bootstrap (Array.create ~len:20 3.) ~f:Stats.mean;
  [%expect {| ((point 3) (lower 3) (upper 3)) |}];
  bootstrap [| 7. |] ~f:Stats.mean;
  [%expect {| ((point 7) (lower 7) (upper 7)) |}]
;;

let%expect_test "summarize is deterministic" =
  let config = { Config.quick with resamples = 1000 } in
  let data = Array.append one_to_ten [| 30. |] in
  let first = Stats.summarize config data in
  let second = Stats.summarize config data in
  print_s [%sexp (first : Stats.t)];
  print_s
    [%message
      ""
        ~same:
          (Sexp.equal [%sexp (first : Stats.t)] [%sexp (second : Stats.t)]
           : bool)];
  [%expect
    {|
    ((count 11)
     (mean
      ((point 7.7272727272727275) (lower 4.0909090909090908)
       (upper 13.002272727272725)))
     (median 6) (min 1) (max 30) (stddev 7.925792188682582) (mad 4.4478)
     (p90 10) (p99 28.000000000000007)
     (outliers ((low_severe 0) (low_mild 0) (high_mild 0) (high_severe 1))))
    (same true)
    |}]
;;
