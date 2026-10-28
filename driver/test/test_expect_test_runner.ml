open! Import

let check_files files =
  match Driver.check_files ~limits:Source_helpers.limits files with
  | Error error ->
    print_s [%sexp (error : Driver.Error.t)];
    None
  | Ok driver -> Some driver
;;

(* Runs the tests of the file [t.stag], printing the results and the corrected
   file. *)
let test ?(show = Fn.id) contents =
  let path = "t.stag" in
  match check_files [ { Source_file.path; contents } ] with
  | None -> ()
  | Some driver ->
    Driver.eval ~limits:Source_helpers.limits driver |> ok_exn;
    let results = Expect_test_runner.run ~limits:Source_helpers.limits driver in
    List.iter results ~f:(fun result ->
      print_s [%sexp (result : Expect_test_runner.Test_result.t)]);
    (match
       Corrected_file.create ~path ~contents ~tests:results ~expansions:[]
     with
     | None -> print_endline "(no corrections)"
     | Some corrected ->
       print_endline "corrected:";
       print_string (show corrected))
;;

let%expect_test "normalize" =
  List.iter
    [ ""
    ; " 5 "
    ; "\n\n    a\n      b  \n\n    c\n  "
    ; "a\n  b"
    ; "\n\t x\n"
    ; "\t a\n\t  b"
    ; "\ta\n  b"
    ; "\t\ta\n\tb"
    ]
    ~f:(fun s -> print_s [%sexp (Expect_test_runner.normalize s : string)]);
  [%expect
    {|
    ""
    5
     "a\
    \n  b\
    \n\
    \nc"
     "a\
    \n  b"
    x
     "a\
    \n b"
     "\ta\
    \n  b"
     "\ta\
    \nb"
    |}]
;;

let%expect_test "passing and failing" =
  test
    {x|let%expect_test "pass" =
  print_int (Int.add 3 2);
  [%expect {| 5 |}];
  print_endline "a";
  print_endline "  b";
  [%expect {|
    a
      b
  |}]

let%expect_test "fail" =
  print_int 1;
  [%expect {| 2 |}];
  print_string "x";
  [%expect];
  [%expect {| unexpected |}];
  print_string "a\nb\n\nc";
  [%expect {| |}];
  print_string "|}";
  [%expect {| |}]
|x};
  [%expect
    {xxx|
    ((name (pass @ t.stag:1:16-22)) (mismatches ()) (error ()))
    ((name (fail @ t.stag:11:16-22))
     (mismatches
      (((loc t.stag:13:11-18) (expected (" 2 ")) (actual 1))
       ((loc t.stag:15:10-10) (expected ("")) (actual x))
       ((loc t.stag:16:11-27) (expected (" unexpected ")) (actual ""))
       ((loc t.stag:18:11-16) (expected (" ")) (actual  "a\
                                                       \nb\
                                                       \n\
                                                       \nc"))
       ((loc t.stag:20:11-16) (expected (" ")) (actual |}))))
     (error ()))
    corrected:
    let%expect_test "pass" =
      print_int (Int.add 3 2);
      [%expect {| 5 |}];
      print_endline "a";
      print_endline "  b";
      [%expect {|
        a
          b
      |}]

    let%expect_test "fail" =
      print_int 1;
      [%expect {| 1 |}];
      print_string "x";
      [%expect {| x |}];
      [%expect {| |}];
      print_string "a\nb\n\nc";
      [%expect {|
        a
        b

        c
        |}];
      print_string "|}";
      [%expect {x| |} |x}]
    |xxx}]
;;

let%expect_test "trailing output" =
  test
    {x|let%expect_test "t" =
  print_int 1;
  [%expect {| 1 |}];
  print_string "late"

module M = struct
  let%expect_test "nested" =
    print_string "a\nb"
end
|x};
  [%expect
    {xxx|
    ((name (t @ t.stag:1:16-19))
     (mismatches (((loc t.stag:4:21-21) (expected ()) (actual late))))
     (error ()))
    ((name (nested @ t.stag:7:18-26))
     (mismatches (((loc t.stag:8:23-23) (expected ()) (actual  "a\
                                                              \nb"))))
     (error ()))
    corrected:
    let%expect_test "t" =
      print_int 1;
      [%expect {| 1 |}];
      print_string "late";
      [%expect {| late |}]

    module M = struct
      let%expect_test "nested" =
        print_string "a\nb";
        [%expect {|
          a
          b
          |}]
    end
    |xxx}]
;;

let%expect_test "runtime errors fail the test" =
  test
    {x|let%expect_test "error" =
  print_int 1;
  [%expect {| 2 |}];
  print_int (Int.div 1 0);
  [%expect {| never checked |}]

let%expect_test "next" = print_int 1; [%expect {| 1 |}]
|x};
  [%expect
    {xxx|
    ((name (error @ t.stag:1:16-23))
     (mismatches (((loc t.stag:3:11-18) (expected (" 2 ")) (actual 1))))
     (error (("Uncaught exception Division_by_zero" @ t.stag:4:12-25))))
    ((name (next @ t.stag:7:16-22)) (mismatches ()) (error ()))
    corrected:
    let%expect_test "error" =
      print_int 1;
      [%expect {| 1 |}];
      print_int (Int.div 1 0);
      [%expect {| never checked |}]

    let%expect_test "next" = print_int 1; [%expect {| 1 |}]
    |xxx}]
;;

let%expect_test "tests see toplevels, run after them, and not on eval" =
  test
    {x|let x = print_endline "toplevel"; 3
let%expect_test "uses x" = print_int x; [%expect {| 3 |}]
let%expect_test "splice" = print_int .~(print_endline "compile"; .<1>.); [%expect {| 1 |}]
|x};
  [%expect
    {|
    compile
    toplevel
    ((name ("uses x" @ t.stag:2:16-24)) (mismatches ()) (error ()))
    ((name (splice @ t.stag:3:16-24)) (mismatches ()) (error ()))
    (no corrections)
    |}];
  Option.iter
    (check_files
       [ { path = "t.stag"; contents = {|let%expect_test "t" = print_int 1|} } ])
    ~f:(fun driver -> Driver.eval driver |> ok_exn);
  [%expect {| |}]
;;

let%expect_test "typechecking" =
  let check contents =
    Option.iter
      (check_files [ { Source_file.path = "t.stag"; contents } ])
      ~f:(fun driver ->
        print_string (Signature.to_string_hum (Driver.checked driver).signature))
  in
  check {|let%expect_test "not unit" = 1|};
  check {x|let f x = [%expect {| |}]|x};
  check {x|let%expect_test "in quote" = let c = .<[%expect {| |}]>. in ()|x};
  check {x|let%expect_test "at compile time" = .~([%expect {| |}]; .<()>.)|x};
  check
    {|let%expect_test "in a splice at runtime" = let c = .<.~([%expect]; .<()>.)>. in ()|};
  check {|let%expect_test "local" = let f x = [%expect] in f 1; f 2|};
  test
    {|let%expect_test "reached twice" =
  let f x = print_int x; [%expect] in f 1; f 2
|};
  check {|let%expect_test "unbound" = print_int y|};
  check
    {|let x = 1 let%expect_test "a" = () let%expect_test "a" = () let y = 2|};
  [%expect
    {xxx|
    (Typecheck
     ((Unification_error
       (Type_mismatch (TCon unit ()) (TCon int ())
        (Base_types_not_equal unit int)))
      @ t.stag:1:29-30))
    (Typecheck (Expect_outside_expect_test @ t.stag:1:10-25))
    (Typecheck (Expect_outside_expect_test @ t.stag:1:39-54))
    (Typecheck (Expect_outside_expect_test @ t.stag:1:39-54))
    module T : sig end
    module T : sig end
    ((name ("reached twice" @ t.stag:1:16-31)) (mismatches ())
     (different_outputs (((loc t.stag:2:33-33) (outputs (1 2))))) (error ()))
    (no corrections)
    (Typecheck ((Not_in_scope y) @ t.stag:1:38-39))
    module T : sig
      val x : int
      val y : int
    end
    |xxx}]
;;

let%expect_test "an [%expect] reached more than once" =
  test
    {x|let%expect_test "different" =
  let f x = print_int x; [%expect {| 1 |}] in f 1; f 2; f 1

let%expect_test "same, wrong" =
  let f x = print_int x; [%expect {| 2 |}] in f 1; f 1

let%expect_test "same, right" =
  let f x = print_int x; [%expect {| 1 |}] in f 1; f 1
|x};
  [%expect
    {xxx|
    ((name (different @ t.stag:1:16-27)) (mismatches ())
     (different_outputs (((loc t.stag:2:34-41) (outputs (1 2))))) (error ()))
    ((name ("same, wrong" @ t.stag:4:16-29))
     (mismatches (((loc t.stag:5:34-41) (expected (" 2 ")) (actual 1))))
     (error ()))
    ((name ("same, right" @ t.stag:7:16-29)) (mismatches ()) (error ()))
    corrected:
    let%expect_test "different" =
      let f x = print_int x; [%expect {| 1 |}] in f 1; f 2; f 1

    let%expect_test "same, wrong" =
      let f x = print_int x; [%expect {| 1 |}] in f 1; f 1

    let%expect_test "same, right" =
      let f x = print_int x; [%expect {| 1 |}] in f 1; f 1
    |xxx}]
;;

let%expect_test "trailing output after a body ending in a match or let" =
  test
    {x|let%expect_test "match" =
  match 1 with
  | 1 -> print_int 1
  | _ -> ()

let%expect_test "let" =
  let x = 2 in
  print_int x

let%expect_test "let then match" = let x = 3 in match x with _ -> print_int x

let%expect_test "match then more" =
  (match 4 with _ -> print_int 4);
  print_int 5
|x};
  [%expect
    {xxx|
    ((name (match @ t.stag:1:16-23))
     (mismatches
      (((loc t.stag:4:11-11) (expected ()) (actual 1)
        (parenthesize t.stag:2:2-4:11))))
     (error ()))
    ((name (let @ t.stag:6:16-21))
     (mismatches (((loc t.stag:8:13-13) (expected ()) (actual 2)))) (error ()))
    ((name ("let then match" @ t.stag:10:16-32))
     (mismatches
      (((loc t.stag:10:77-77) (expected ()) (actual 3)
        (parenthesize t.stag:10:35-77))))
     (error ()))
    ((name ("match then more" @ t.stag:12:16-33))
     (mismatches (((loc t.stag:14:13-13) (expected ()) (actual 45)))) (error ()))
    corrected:
    let%expect_test "match" =
      (match 1 with
      | 1 -> print_int 1
      | _ -> ());
      [%expect {| 1 |}]

    let%expect_test "let" =
      let x = 2 in
      print_int x;
      [%expect {| 2 |}]

    let%expect_test "let then match" = (let x = 3 in match x with _ -> print_int x);
      [%expect {| 3 |}]

    let%expect_test "match then more" =
      (match 4 with _ -> print_int 4);
      print_int 5;
      [%expect {| 45 |}]
    |xxx}]
;;

let%expect_test "output before a runtime error" =
  test
    {x|let%expect_test "error" =
  print_int 1;
  [%expect {| 1 |}];
  print_string "before";
  match 2 with 1 -> ()
|x};
  [%expect
    {xxx|
    ((name (error @ t.stag:1:16-23)) (mismatches ())
     (error (("Match failure: no case matches the value 2" @ t.stag:5:2-22)))
     (output_before_error before))
    (no corrections)
    |xxx}]
;;

let%expect_test "tab indentation" =
  test
    ~show:(String.substr_replace_all ~pattern:"\t" ~with_:"<tab>")
    "let%expect_test \"t\" =\n\
     \tprint_string \"a\\nb\";\n\
     \t[%expect {| |}];\n\
     \tprint_string \"c\\nd\"\n";
  [%expect
    {xxx|
    ((name (t @ t.stag:1:16-19))
     (mismatches
      (((loc t.stag:3:10-15) (expected (" ")) (actual  "a\
                                                      \nb"))
       ((loc t.stag:4:20-20) (expected ()) (actual  "c\
                                                   \nd"))))
     (error ()))
    corrected:
    let%expect_test "t" =
    <tab>print_string "a\nb";
    <tab>[%expect {|
    <tab><tab>a
    <tab><tab>b
    <tab><tab>|}];
    <tab>print_string "c\nd";
    <tab>[%expect {|
    <tab><tab>c
    <tab><tab>d
    <tab><tab>|}]
    |xxx}]
;;

let%expect_test "locals of tests are not toplevels" =
  let contents =
    {x|let%expect_test "t" = let x = 1 in print_int x; [%expect {| 1 |}]|x}
  in
  Option.iter
    (check_files [ { Source_file.path = "t.stag"; contents } ])
    ~f:(fun driver ->
      Driver.eval driver |> ok_exn;
      let results = Expect_test_runner.run driver in
      print_s
        [%sexp
          (List.for_all results ~f:Expect_test_runner.Test_result.passed : bool)];
      List.iter (Driver.checked driver).program.expect_tests ~f:(fun test ->
        match test.body.desc with
        | Let { ident; _ } ->
          print_s
            [%sexp
              (Treewalk.find (Driver.interpreter driver) ~ident
               |> Result.map ~f:Treewalk.Value.to_string_hum
               : string Or_error.t)]
        | _ -> print_endline "not a let"));
  [%expect
    {|
    true
    (Error ("Unbound ident" (ident x/0)))
    |}]
;;

let%expect_test "only the tests of [files] run" =
  match
    check_files
      [ { path = "a.stag"
        ; contents =
            {|let%expect_test "loops" = let loop x = loop x in loop ()|}
        }
      ; { path = "b.stag"; contents = {|let%expect_test "b" = print_int 1|} }
      ]
  with
  | None -> ()
  | Some driver ->
    Driver.eval driver |> ok_exn;
    Expect_test_runner.run ~files:(String.Set.singleton "b.stag") driver
    |> List.iter ~f:(fun result ->
      print_s [%sexp (result : Expect_test_runner.Test_result.t)]);
    [%expect
      {|
      ((name (b @ b.stag:1:16-19))
       (mismatches (((loc b.stag:1:33-33) (expected ()) (actual 1)))) (error ()))
      |}]
;;
