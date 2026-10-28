open! Import

let generators =
  {|
let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
let enum names =
  Cons
    ( Stag.Toplevel.Type
        { `name = "t"
        ; `params = Nil
        ; `kind = Stag.Type_decl.Kind.Variant (Stag.map (fun name -> (name, Nil)) names) }
    , Cons
        ( Stag.Toplevel.let_ "to_string" (Cons ("x", Nil))
            (Stag.Expr.Match
               ( Stag.Expr.var "x"
               , Stag.map (fun name -> (Stag.Pattern.construct name Nil, Stag.Expr.string name)) names ))
        , Nil ) )
|}
;;

(* Checks, runs and tests [source] as [a.stag] (after [generators]), printing
   the stale expansions, output, failing tests and the corrected source. *)
let test source =
  let path = "a.stag" in
  let contents = generators ^ source in
  let contents_of p = Option.some_if (String.equal p path) contents in
  let report diagnostic =
    print_string (Diagnostic.to_string_hum ~contents:contents_of diagnostic)
  in
  match Driver.check_files [ { Source_file.path; contents } ] with
  | Error error -> report (Diagnostic.of_driver_error error)
  | Ok driver ->
    let expansions = Inline_expansion.mismatches driver in
    List.iter expansions ~f:(fun expansion ->
      print_s [%message "stale" ~_:(expansion.loc : Loc.t)]);
    let output = Buffer.create 64 in
    Treewalk.set_output (Driver.interpreter driver) (Buffer.add_string output);
    let tests =
      match Driver.eval driver with
      | Ok () -> Expect_test_runner.run driver
      | Error error ->
        report (Diagnostic.of_runtime_error error);
        []
    in
    let output = Buffer.contents output in
    if not (String.is_empty output)
    then
      printf
        "--- output ---\n%s%s"
        output
        (if String.is_suffix output ~suffix:"\n" then "" else "\n");
    List.iter tests ~f:(fun result ->
      if not (Expect_test_runner.Test_result.passed result)
      then print_s [%sexp (result : Expect_test_runner.Test_result.t)]);
    (match Corrected_file.create ~path ~contents ~tests ~expansions with
     | None -> print_endline "(up to date)"
     | Some corrected ->
       print_endline "--- corrected ---";
       print_string
         (String.chop_prefix_exn corrected ~prefix:generators
          |> String.chop_prefix_if_exists ~prefix:"\n"))
;;

let%expect_test "an empty expansion is filled" =
  test
    {|
include%inline .~(value "x" (Stag.Expr.int 1))
end%inline

let y = x
|};
  [%expect
    {|
    (stale a.stag:16:46-17:0)
    --- corrected ---
    include%inline .~(value "x" (Stag.Expr.int 1))
      let x = 1
    end%inline

    let y = x
    |}]
;;

let%expect_test "an up to date expansion" =
  test
    {|
include%inline .~(value "x" (Stag.Expr.int 1))
  let x = 1
end%inline
|};
  [%expect {| (up to date) |}]
;;

let%expect_test "expansions are compared modulo layout and comments" =
  test
    {|
include%inline .~(enum (Cons ("Red", Cons ("Green", Nil))))
  (* The colors. *)
  type t =
    | Red
    | Green
  let to_string x = match x with
    | Red -> "Red" (* first *)
    | Green -> "Green"
end%inline
|};
  [%expect {| (up to date) |}]
;;

let%expect_test
    "a stale expansion, edited or generated differently, is corrected"
  =
  test
    {|
include%inline .~(value "x" (Stag.Expr.int 2))
  let x = 1
  let y = 2
end%inline
module M = struct
  include%inline .~(enum (Cons ("A", Cons ("B", Nil))))
    type t = A
  end%inline
end
|};
  [%expect
    {|
    (stale a.stag:16:46-19:0)
    (stale a.stag:21:55-23:2)
    --- corrected ---
    include%inline .~(value "x" (Stag.Expr.int 2))
      let x = 2
    end%inline
    module M = struct
      include%inline .~(enum (Cons ("A", Cons ("B", Nil))))
        type t = A | B

        let to_string x =
          match x with
          | A -> "A"
          | B -> "B"
      end%inline
    end
    |}]
;;

let%expect_test "an expansion of nothing" =
  test
    {|
include%inline .~Nil
  let x = 1
end%inline
include%inline .~Nil
end%inline
|};
  [%expect
    {|
    (stale a.stag:16:20-18:0)
    --- corrected ---
    include%inline .~Nil
    end%inline
    include%inline .~Nil
    end%inline
    |}]
;;

let%expect_test "module splices" =
  test
    {|
module%inline Color = .~(enum (Cons ("Red", Nil)))
end%inline
module Outer = struct
  module%inline Inner = .~(value "x" (Stag.Expr.int 1))
  end%inline
end
let shown = print_endline (Color.to_string Color.Red)
|};
  [%expect
    {|
    (stale a.stag:16:50-17:0)
    (stale a.stag:19:55-20:2)
    --- output ---
    Red
    --- corrected ---
    module%inline Color = .~(enum (Cons ("Red", Nil)))
      type t = Red

      let to_string x = match x with Red -> "Red"
    end%inline
    module Outer = struct
      module%inline Inner = .~(value "x" (Stag.Expr.int 1))
        let x = 1
      end%inline
    end
    let shown = print_endline (Color.to_string Color.Red)
    |}]
;;

let%expect_test "long generated code is laid out within the width, at its depth"
  =
  test
    {|
let names = Cons ("A_long_constructor_name", Cons ("Another_long_constructor_name", Cons ("Yet_another_one", Nil)))
module A = struct
  module B = struct
    include%inline .~(enum names)
    end%inline
  end
end
|};
  [%expect
    {|
    (stale a.stag:19:33-20:4)
    --- corrected ---
    let names = Cons ("A_long_constructor_name", Cons ("Another_long_constructor_name", Cons ("Yet_another_one", Nil)))
    module A = struct
      module B = struct
        include%inline .~(enum names)
          type t =
            | A_long_constructor_name
            | Another_long_constructor_name
            | Yet_another_one

          let to_string x =
            match x with
            | A_long_constructor_name -> "A_long_constructor_name"
            | Another_long_constructor_name -> "Another_long_constructor_name"
            | Yet_another_one -> "Yet_another_one"
        end%inline
      end
    end
    |}]
;;

let%expect_test
    "the program declares what the splice generates, not the expansion"
  =
  test
    {|
include%inline .~(value "x" (Stag.Expr.int 1))
  let x = Int.add "not checked" 1
  let y = 2
end%inline
let shown = print_int x
|};
  [%expect
    {|
    (stale a.stag:16:46-19:0)
    --- output ---
    1
    --- corrected ---
    include%inline .~(value "x" (Stag.Expr.int 1))
      let x = 1
    end%inline
    let shown = print_int x
    |}]
;;

let%expect_test "an expansion must parse" =
  test
    {|
include%inline .~(value "x" (Stag.Expr.int 1))
  let x =
end%inline
|};
  [%expect
    {|
    File "a.stag", line 18, characters 0-10:
    18 | end%inline
         ^^^^^^^^^^
    Error: Expected an expression, found `end%inline`
    |}]
;;

let%expect_test "unclosed" =
  test
    {|
include%inline .~(value "x" (Stag.Expr.int 1))
  let x = 1
|};
  [%expect
    {|
    File "a.stag", line 18, characters 0-0:
    18 |
         ^
    Error: Expected `end%inline` to close this `include%inline` (opened at 16:0), found end of file
    |}]
;;

let%expect_test "an error in the splice" =
  test
    {|
include%inline .~(value "x" (Stag.Expr.int "one"))
  let x = 1
end%inline
|};
  [%expect
    {|
    File "a.stag", line 16, characters 43-48:
    16 | include%inline .~(value "x" (Stag.Expr.int "one"))
                                                    ^^^^^
    Error: This expression has type string but an expression was expected of type int
    |}]
;;

let%expect_test "a splice failing at compile time" =
  test
    {|
include%inline .~(raise (Failure "no"))
end%inline
|};
  [%expect
    {|
    File "a.stag", line 16, characters 0-39:
    16 | include%inline .~(raise (Failure "no"))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Failure "no"
    File "a.stag", line 16, characters 17-39:
    16 | include%inline .~(raise (Failure "no"))
                          ^^^^^^^^^^^^^^^^^^^^^^
    Note: The error was raised here
    |}]
;;

let%expect_test "with failing expect tests, both are corrected" =
  test
    {xx|
let%expect_test "before" = print_endline "hi"; [%expect {| |}]
include%inline .~(value "x" (Stag.Expr.int 1))
end%inline
let%expect_test "after" = print_int x; [%expect {| 2 |}]
|xx};
  [%expect
    {xxx|
    (stale a.stag:17:46-18:0)
    ((name (before @ a.stag:16:16-24))
     (mismatches (((loc a.stag:16:56-61) (expected (" ")) (actual hi))))
     (error ()))
    ((name (after @ a.stag:19:16-23))
     (mismatches (((loc a.stag:19:48-55) (expected (" 2 ")) (actual 1))))
     (error ()))
    --- corrected ---
    let%expect_test "before" = print_endline "hi"; [%expect {| hi |}]
    include%inline .~(value "x" (Stag.Expr.int 1))
      let x = 1
    end%inline
    let%expect_test "after" = print_int x; [%expect {| 1 |}]
    |xxx}]
;;

let%expect_test "with a runtime error, the expansions are still corrected" =
  test
    {|
include%inline .~(value "x" (Stag.Expr.int 1))
end%inline
let crash = raise (Failure "boom")
|};
  [%expect
    {|
    (stale a.stag:16:46-17:0)
    File "a.stag", line 18, characters 12-34:
    18 | let crash = raise (Failure "boom")
                     ^^^^^^^^^^^^^^^^^^^^^^
    Error: Uncaught exception Failure "boom"
    --- corrected ---
    include%inline .~(value "x" (Stag.Expr.int 1))
      let x = 1
    end%inline
    let crash = raise (Failure "boom")
    |}]
;;

let%expect_test "an expansion in an expansion is only compared" =
  test
    {|
include%inline .~(value "x" (Stag.Expr.int 1))
  let x = 1
  include%inline .~(value "y" (Stag.Expr.int 2))
  end%inline
end%inline
|};
  [%expect
    {|
    (stale a.stag:16:46-20:0)
    --- corrected ---
    include%inline .~(value "x" (Stag.Expr.int 1))
      let x = 1
    end%inline
    |}]
;;
