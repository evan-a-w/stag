open! Core
open Stag

let check_files files =
  match Driver.check_files ~limits:Helpers.limits files with
  | Error error ->
    print_s [%sexp (error : Driver.Error.t)];
    None
  | Ok driver -> Some driver
;;

(* Runs the tests of the file [t.stag], printing the results and the corrected
   file. *)
let test contents =
  let path = "t.stag" in
  match check_files [ { Source_file.path; contents } ] with
  | None -> ()
  | Some driver ->
    Driver.eval ~limits:Helpers.limits driver |> ok_exn;
    let results = Expect_test_runner.run ~limits:Helpers.limits driver in
    List.iter results ~f:(fun result ->
      print_s [%sexp (result : Expect_test_runner.Test_result.t)]);
    (match Expect_test_runner.corrected ~path ~contents results with
     | None -> print_endline "(no corrections)"
     | Some corrected ->
       print_endline "corrected:";
       print_string corrected)
;;

let%expect_test "normalize" =
  List.iter
    [ ""; " 5 "; "\n\n    a\n      b  \n\n    c\n  "; "a\n  b"; "\n\t x\n" ]
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
     (error (("Division by zero" @ t.stag:4:12-25))))
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
        Helpers.print_signature (Driver.checked driver).signature)
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
    ((name ("reached twice" @ t.stag:1:16-31))
     (mismatches
      (((loc t.stag:2:33-33) (expected ("")) (actual 1))
       ((loc t.stag:2:33-33) (expected ("")) (actual 2))))
     (error ()))
    corrected:
    let%expect_test "reached twice" =
      let f x = print_int x; [%expect {| 1 |}] in f 1; f 2
    (Typecheck ((Not_in_scope y) @ t.stag:1:38-39))
    module T : sig
      val x : int
      val y : int
    end
    |xxx}]
;;
