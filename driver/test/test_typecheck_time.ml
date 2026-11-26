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

let chain ~prelude ~step n =
  sprintf
    "%slet g a0 =\n%s a%d"
    prelude
    (String.concat
       (List.init n ~f:(fun i -> sprintf "let a%d = %s in\n" (i + 1) (step i))))
    n
;;

let%expect_test "chains of values each holding the one before take linear time" =
  List.iter
    [ "tuple", "", sprintf "(a%d, 1)"
    ; ("list", "", fun i -> sprintf "Cons (Box.make (1, a%d))" i)
    ]
    ~f:(fun (name, prelude, step) ->
      print_s
        [%message
          name
            ~four_times_within_8_times_the_time:
              (ratio ~small:200 ~large:800 ~within:8. (chain ~prelude ~step)
               : bool)]);
  [%expect
    {|
    (tuple (four_times_within_8_times_the_time true))
    (list (four_times_within_8_times_the_time true))
    |}]
;;

let%expect_test "chains of records and closures take at most quadratic time" =
  List.iter
    [ ( "record"
      , "type r['a] = { x : 'a; y : int }\n"
      , fun i -> sprintf "{ x = a%d; y = 1 }" i )
    ; "closure", "", sprintf "fun (u : unit) -> a%d"
    ]
    ~f:(fun (name, prelude, step) ->
      print_s
        [%message
          name
            ~four_times_within_30_times_the_time:
              (ratio ~small:50 ~large:200 ~within:30. (chain ~prelude ~step)
               : bool)]);
  [%expect
    {|
    (record (four_times_within_30_times_the_time true))
    (closure (four_times_within_30_times_the_time true))
    |}]
;;
