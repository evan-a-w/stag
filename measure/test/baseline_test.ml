open! Import

let config = { Config.quick with resamples = 10 }

let result name times =
  let measurement : Measurement.t =
    { warm_up_iterations = 1
    ; warm_up_time = Time_ns.Span.of_int_ns 10
    ; plan = { iterations = 2; samples = List.length times }
    ; samples =
        List.map times ~f:(fun ns ->
          { Sample.iterations = 2
          ; time = Time_ns.Span.of_int_ns ns
          ; alloc = None
          })
    }
  in
  Bench_result.create config ~name measurement
;;

let%expect_test "update and find" =
  let t =
    Baseline.update Baseline.empty [ result "b" [ 20; 30 ]; result "a" [ 3 ] ]
  in
  print_s [%sexp (t : Baseline.t)];
  [%expect {| ((version 1) (benches ((a (1.5)) (b (10 15))))) |}];
  let t = Baseline.update t [ result "b" [ 40 ]; result "c" [ 2; 4; 6 ] ] in
  print_s [%sexp (t : Baseline.t)];
  List.iter [ "a"; "b"; "c"; "d" ] ~f:(fun name ->
    print_s [%message name ~_:(Baseline.find t name : float array option)]);
  [%expect
    {|
    ((version 1) (benches ((a (1.5)) (b (20)) (c (1 2 3)))))
    (a ((1.5)))
    (b ((20)))
    (c ((1 2 3)))
    (d ())
    |}]
;;

let with_temp_dir f =
  let dir = Stdlib.Filename.temp_dir "stag_measure_test" "" in
  Exn.protect
    ~f:(fun () -> f dir)
    ~finally:(fun () ->
      ignore
        (Stdlib.Sys.command (sprintf "rm -rf %s" (Stdlib.Filename.quote dir))
         : int))
;;

let%expect_test "save and load" =
  with_temp_dir (fun dir ->
    let path =
      Baseline.path ~dir:(Filename.concat dir "nested/baselines") ~name:"main"
    in
    print_endline (String.chop_prefix_exn path ~prefix:dir);
    let t =
      Baseline.update
        Baseline.empty
        [ result "fib/20" [ 2469; 2481 ]; result "alloc" [ 1; 3 ] ]
    in
    print_s [%sexp (Baseline.save t ~path : unit Or_error.t)];
    print_string (In_channel.read_all path);
    let loaded = Baseline.load ~path |> Or_error.ok_exn in
    print_s
      [%message
        ""
          ~round_trips:
            (Sexp.equal [%sexp (t : Baseline.t)] [%sexp (loaded : Baseline.t)]
             : bool)
          ~files:(Stdlib.Sys.readdir (Filename.dirname path) : string array)];
    (* Saving again overwrites. *)
    let t = Baseline.update t [ result "alloc" [ 5 ] ] in
    print_s [%sexp (Baseline.save t ~path : unit Or_error.t)];
    print_s [%sexp (Baseline.load ~path : Baseline.t Or_error.t)]);
  [%expect
    {|
    /nested/baselines/main.sexp
    (Ok ())
    ((version 1) (benches ((alloc (0.5 1.5)) (fib/20 (1234.5 1240.5)))))
    ((round_trips true) (files (main.sexp)))
    (Ok ())
    (Ok ((version 1) (benches ((alloc (2.5)) (fib/20 (1234.5 1240.5))))))
    |}]
;;

let%expect_test "loading a missing or malformed file" =
  with_temp_dir (fun dir ->
    let load name contents =
      let path = Filename.concat dir name in
      Option.iter contents ~f:(fun data -> Out_channel.write_all path ~data);
      match Baseline.load ~path with
      | Ok t -> print_s [%message "loaded" (t : Baseline.t)]
      | Error error ->
        print_endline
          (String.substr_replace_all
             (Error.to_string_hum error)
             ~pattern:dir
             ~with_:"$DIR")
    in
    load "missing.sexp" None;
    load "unbalanced.sexp" (Some "((version 1) (benches ())");
    load "wrong_shape.sexp" (Some "(benches ())");
    load "bad_time.sexp" (Some "((version 1) (benches ((a (1 x)))))");
    load "duplicate.sexp" (Some "((version 1) (benches ((a (1)) (a (2)))))");
    load "version.sexp" (Some "((version 2) (benches ()))");
    load "ok.sexp" (Some "((version 1) (benches ((a (1 2.5)))))"));
  [%expect
    {|
    ("no baseline at"
     $DIR/missing.sexp)
    ("malformed baseline"
     $DIR/unbalanced.sexp
     (exn
      (Failure
       "Sexplib.Sexp.gen_load_sexp: EOF in $DIR/unbalanced.sexp while in state Parsing_list")))
    ("malformed baseline"
     $DIR/wrong_shape.sexp
     (exn
      (Of_sexp_error
       $DIR/wrong_shape.sexp:1:1
       "measure/src/baseline.ml.Serialized.t_of_sexp: record conversion: only pairs expected, their first element must be an atom"
       (invalid_sexp benches))))
    ("malformed baseline"
     $DIR/bad_time.sexp
     (exn
      (Of_sexp_error
       $DIR/bad_time.sexp:1:29
       "float_of_sexp: (Failure float_of_string)" (invalid_sexp x))))
    ("malformed baseline"
     $DIR/duplicate.sexp
     (exn ("Map.of_alist_exn: duplicate key" a)))
    ("malformed baseline"
     $DIR/version.sexp
     (exn ("unsupported baseline version" (version 2))))
    (loaded (t ((version 1) (benches ((a (1 2.5)))))))
    |}]
;;

let%expect_test "validate_name" =
  List.iter
    [ "main"
    ; "v1.2"
    ; "my-baseline_2"
    ; "..x"
    ; ""
    ; ".hidden"
    ; "a/b"
    ; "a b"
    ; "x.."
    ; "é"
    ]
    ~f:(fun name ->
      print_s [%message name ~_:(Baseline.validate_name name : unit Or_error.t)]);
  [%expect
    {|
    (main (Ok ()))
    (v1.2 (Ok ()))
    (my-baseline_2 (Ok ()))
    (..x (Error ("baseline name starts with '.'" ..x)))
    ("" (Error "baseline name is empty"))
    (.hidden (Error ("baseline name starts with '.'" .hidden)))
    (a/b
     (Error
      ("baseline name may only contain letters, digits, '-', '_' and '.'" a/b)))
    ("a b"
     (Error
      ("baseline name may only contain letters, digits, '-', '_' and '.'" "a b")))
    (x.. (Ok ()))
    ("\195\169"
     (Error
      ("baseline name may only contain letters, digits, '-', '_' and '.'"
       "\195\169")))
    |}]
;;
