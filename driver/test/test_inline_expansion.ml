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

(* Checks, runs and tests [source] as [a.stag] (after [generators]), with
   [others] files, printing the stale expansions, output, failing tests and the
   corrected sources. *)
let test ?(others = []) ?(crlf = false) source =
  let line_endings text =
    if crlf
    then String.substr_replace_all text ~pattern:"\n" ~with_:"\r\n"
    else text
  in
  let files =
    { Source_file.path = "a.stag"
    ; contents = line_endings (generators ^ source)
    }
    :: List.map others ~f:(fun (path, contents) ->
      { Source_file.path; contents })
  in
  let contents_of p =
    List.find_map files ~f:(fun (file : Source_file.t) ->
      Option.some_if (String.equal file.path p) file.contents)
  in
  let report diagnostic =
    print_string (Diagnostic.to_string_hum ~contents:contents_of diagnostic)
  in
  match Driver.check_files files with
  | Error error -> report (Diagnostic.of_driver_error error)
  | Ok driver ->
    let expansions = Inline_expansion.stale driver in
    List.iter expansions ~f:(fun expansion ->
      report (Inline_expansion.diagnostic expansion));
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
    List.iter files ~f:(fun { path; contents } ->
      match Corrected_file.create ~path ~contents ~tests ~expansions with
      | None -> printf "(%s up to date)\n" path
      | Some corrected ->
        printf "--- %s corrected ---\n" path;
        String.chop_prefix_if_exists corrected ~prefix:(line_endings generators)
        |> String.chop_prefix_if_exists ~prefix:(line_endings "\n")
        |> String.substr_replace_all ~pattern:"\t" ~with_:"<TAB>"
        |> String.substr_replace_all ~pattern:"\r" ~with_:"<CR>"
        |> print_string)
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
    File "a.stag", line 16, characters 0-46:
    16 | include%inline .~(value "x" (Stag.Expr.int 1))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- a.stag corrected ---
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
  [%expect {| (a.stag up to date) |}]
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
  [%expect {| (a.stag up to date) |}]
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
    File "a.stag", line 16, characters 0-46:
    16 | include%inline .~(value "x" (Stag.Expr.int 2))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "a.stag", line 21, characters 2-55:
    21 |   include%inline .~(enum (Cons ("A", Cons ("B", Nil))))
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- a.stag corrected ---
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
    File "a.stag", line 16, characters 0-20:
    16 | include%inline .~Nil
         ^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- a.stag corrected ---
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
    File "a.stag", line 16, characters 0-50:
    16 | module%inline Color = .~(enum (Cons ("Red", Nil)))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "a.stag", line 19, characters 2-55:
    19 |   module%inline Inner = .~(value "x" (Stag.Expr.int 1))
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- output ---
    Red
    --- a.stag corrected ---
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
    File "a.stag", line 19, characters 4-33:
    19 |     include%inline .~(enum names)
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- a.stag corrected ---
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
    File "a.stag", line 16, characters 0-46:
    16 | include%inline .~(value "x" (Stag.Expr.int 1))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- output ---
    1
    --- a.stag corrected ---
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
    File "a.stag", line 17, characters 0-46:
    17 | include%inline .~(value "x" (Stag.Expr.int 1))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    ((name (before @ a.stag:16:16-24))
     (mismatches (((loc a.stag:16:56-61) (expected (" ")) (actual hi))))
     (error ()))
    ((name (after @ a.stag:19:16-23))
     (mismatches (((loc a.stag:19:48-55) (expected (" 2 ")) (actual 1))))
     (error ()))
    --- a.stag corrected ---
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
    File "a.stag", line 16, characters 0-46:
    16 | include%inline .~(value "x" (Stag.Expr.int 1))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "a.stag", line 18, characters 12-34:
    18 | let crash = raise (Failure "boom")
                     ^^^^^^^^^^^^^^^^^^^^^^
    Error: Uncaught exception Failure "boom"
    --- a.stag corrected ---
    include%inline .~(value "x" (Stag.Expr.int 1))
      let x = 1
    end%inline
    let crash = raise (Failure "boom")
    |}]
;;

let%expect_test "splices in an expansion do not run, so are not generated" =
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
    File "a.stag", line 16, characters 0-46:
    16 | include%inline .~(value "x" (Stag.Expr.int 1))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- a.stag corrected ---
    include%inline .~(value "x" (Stag.Expr.int 1))
      let x = 1
    end%inline
    |}]
;;

let%expect_test "corrections are indented as the line of the toplevel" =
  test
    "module M = struct\n\
     \tinclude%inline .~(value \"x\" (Stag.Expr.int 1))\n\
     \tend%inline\n\
     end\n\
     let a = 1 include%inline .~(value \"y\" (Stag.Expr.int 2)) end%inline\n\
     module N = struct\n\
    \  let b = 1 module%inline O = .~(value \"z\" (Stag.Expr.int 3)) end%inline\n\
     end\n";
  [%expect
    {|
    File "a.stag", line 16, characters 1-47:
    16 | 	include%inline .~(value "x" (Stag.Expr.int 1))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "a.stag", line 19, characters 10-56:
    19 | let a = 1 include%inline .~(value "y" (Stag.Expr.int 2)) end%inline
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "a.stag", line 21, characters 12-61:
    21 |   let b = 1 module%inline O = .~(value "z" (Stag.Expr.int 3)) end%inline
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- a.stag corrected ---
    module M = struct
    <TAB>include%inline .~(value "x" (Stag.Expr.int 1))
    <TAB>  let x = 1
    <TAB>end%inline
    end
    let a = 1 include%inline .~(value "y" (Stag.Expr.int 2))
      let y = 2
    end%inline
    module N = struct
      let b = 1 module%inline O = .~(value "z" (Stag.Expr.int 3))
        let z = 3
      end%inline
    end
    |}]
;;

let%expect_test "a comment after the splice is kept" =
  test
    {|
include%inline .~(value "x" (Stag.Expr.int 1)) (* derived *)
end%inline
include%inline .~(value "y" (Stag.Expr.int 2)) (* derived *) end%inline
|};
  [%expect
    {|
    File "a.stag", line 16, characters 0-46:
    16 | include%inline .~(value "x" (Stag.Expr.int 1)) (* derived *)
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "a.stag", line 18, characters 0-46:
    18 | include%inline .~(value "y" (Stag.Expr.int 2)) (* derived *) end%inline
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- a.stag corrected ---
    include%inline .~(value "x" (Stag.Expr.int 1)) (* derived *)
      let x = 1
    end%inline
    include%inline .~(value "y" (Stag.Expr.int 2)) (* derived *)
      let y = 2
    end%inline
    |}]
;;

let%expect_test "CRLF line endings are kept" =
  test
    ~crlf:true
    {|
include%inline .~(value "x" (Stag.Expr.int 1))
end%inline
module M = struct
  include%inline .~(value "y" (Stag.Expr.int 2))
    let y = 3
  end%inline
end
let%expect_test "y" = print_int M.y
|};
  [%expect
    {xxx|
    File "a.stag", line 16, characters 0-46:
    16 | include%inline .~(value "x" (Stag.Expr.int 1))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "a.stag", line 19, characters 2-48:
    19 |   include%inline .~(value "y" (Stag.Expr.int 2))
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    ((name (y @ a.stag:23:16-19))
     (mismatches (((loc a.stag:23:35-35) (expected ()) (actual 2)))) (error ()))
    --- a.stag corrected ---
    include%inline .~(value "x" (Stag.Expr.int 1))<CR>
      let x = 1<CR>
    end%inline<CR>
    module M = struct<CR>
      include%inline .~(value "y" (Stag.Expr.int 2))<CR>
        let y = 2<CR>
      end%inline<CR>
    end<CR>
    let%expect_test "y" = print_int M.y;<CR>
    [%expect {| 2 |}]<CR>
    |xxx}];
  test
    ~crlf:true
    {|
module M = struct
  include%inline .~(value "y" (Stag.Expr.int 2))
    let y = 2
  end%inline
end
|};
  [%expect {| (a.stag up to date) |}]
;;

let%expect_test "a generator emitting an expect test" =
  test
    {|
include%inline .~(Cons (Stag.Toplevel.Expect_test { `name = "t"; `body = Stag.Expr.apply (Stag.Expr.var "print_int") (Cons (Stag.Expr.int 1, Nil)) }, Nil))
end%inline
|};
  [%expect
    {|
    File "a.stag", line 16, characters 0-155:
    16 | include%inline .~(Cons (Stag.Toplevel.Expect_test { `name = "t"; `body = Stag.Expr.apply (Stag.Expr.var "print_int") (Cons (Stag.Expr.int 1, Nil)) }, Nil))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    ((name (t @ a.stag:16:0-155))
     (mismatches (((loc a.stag:16:155-155) (expected ()) (actual 1)))) (error ())
     (generated))
    --- a.stag corrected ---
    include%inline .~(Cons (Stag.Toplevel.Expect_test { `name = "t"; `body = Stag.Expr.apply (Stag.Expr.var "print_int") (Cons (Stag.Expr.int 1, Nil)) }, Nil))
      let%expect_test "t" = print_int 1
    end%inline
    |}]
;;

let%expect_test "expansions in several files" =
  test
    ~others:
      [ ( "b.stag"
        , {|let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
include%inline .~(value "y" (Stag.Expr.int 2))
end%inline
module%inline M = .~(value "z" (Stag.Expr.int 3))
  let z = 3
end%inline
|}
        )
      ]
    {|
include%inline .~(value "x" (Stag.Expr.int 1))
end%inline
|};
  [%expect
    {|
    File "a.stag", line 16, characters 0-46:
    16 | include%inline .~(value "x" (Stag.Expr.int 1))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "b.stag", line 2, characters 0-46:
    2 | include%inline .~(value "y" (Stag.Expr.int 2))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- a.stag corrected ---
    include%inline .~(value "x" (Stag.Expr.int 1))
      let x = 1
    end%inline
    --- b.stag corrected ---
    let value name e = Cons (Stag.Toplevel.let_ name Nil e, Nil)
    include%inline .~(value "y" (Stag.Expr.int 2))
      let y = 2
    end%inline
    module%inline M = .~(value "z" (Stag.Expr.int 3))
      let z = 3
    end%inline
    |}]
;;

let%expect_test "a name written as one shadowed at the expansion is stale" =
  test
    {|
let alias e = Cons (Stag.Toplevel.Type { `name = "u"; `params = Nil; `kind = Stag.Type_decl.Kind.Alias e }, Nil)
type t = A
let by_ref = Stag.Type.expr (type t)
module N = struct
  module A = struct type t = C end
  include%inline .~(alias by_ref)
    type u = A.t
  end%inline
  let f (x : u) = match x with A -> 1
end
module P = struct
  module A = struct type t = C end
  include%inline .~(alias (Stag.Type_expr.Con (Stag.Path.Name "A.t", Nil)))
    type u = A.t
  end%inline
  let f (x : u) = match x with A.C -> 1
end
module Q = struct
  type t = D
  include%inline .~(alias by_ref)
    type u = t
  end%inline
end
|};
  [%expect
    {|
    File "a.stag", line 21, characters 2-33:
    21 |   include%inline .~(alias by_ref)
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "a.stag", line 35, characters 2-33:
    35 |   include%inline .~(alias by_ref)
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- a.stag corrected ---
    let alias e = Cons (Stag.Toplevel.Type { `name = "u"; `params = Nil; `kind = Stag.Type_decl.Kind.Alias e }, Nil)
    type t = A
    let by_ref = Stag.Type.expr (type t)
    module N = struct
      module A = struct type t = C end
      include%inline .~(alias by_ref)
        type u = t
      end%inline
      let f (x : u) = match x with A -> 1
    end
    module P = struct
      module A = struct type t = C end
      include%inline .~(alias (Stag.Type_expr.Con (Stag.Path.Name "A.t", Nil)))
        type u = A.t
      end%inline
      let f (x : u) = match x with A.C -> 1
    end
    module Q = struct
      type t = D
      include%inline .~(alias by_ref)
        type u = A.t
      end%inline
    end
    |}]
;;

let%expect_test "an error in generated code is in the expansion, if up to date" =
  test
    {|
include%inline .~(value "x" (Stag.Expr.apply (Stag.Expr.var "print_int") (Cons (Stag.Expr.string "a", Nil))))
  let x = print_int "a"
end%inline
|};
  test
    {|
include%inline .~(value "x" (Stag.Expr.apply (Stag.Expr.var "print_int") (Cons (Stag.Expr.string "a", Nil))))
  let x = print_int "b"
end%inline
|};
  [%expect
    {|
    File "a.stag", line 17, characters 20-23:
    17 |   let x = print_int "a"
                             ^^^
    Error: This expression has type string but an expression was expected of type int
    File "a.stag", line 16, characters 0-109:
    16 | include%inline .~(value "x" (Stag.Expr.apply (Stag.Expr.var "print_int") (Cons (Stag.Expr.string "a", Nil))))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the declaration `let x` generated by this splice: This expression has type string but an expression was expected of type int
    Note: The generated declaration is:
      let x = print_int "a"
    |}]
;;

let%expect_test
    "refs in expansions: shortest suffix at the splice, else a root path"
  =
  test
    ~others:
      [ ( "gen.stag"
        , {|
type t = A | B
let helper x = x
let code = .< fun (x : t) -> match x with | A -> helper (String.length "a") | B -> Core.Int.succ 0 >.
let gen = Cons (Stag.Toplevel.let_ "f" Nil (Stag.Code.erase code), Nil)
|}
        )
      ]
    {|
module Core = struct end
include%inline .~Gen.gen
end%inline
module M = struct
  type t = A | B
  let helper = 1
  module String = struct end
  include%inline .~Gen.gen
  end%inline
end
|};
  [%expect
    {|
    File "a.stag", line 17, characters 0-24:
    17 | include%inline .~Gen.gen
         ^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "a.stag", line 23, characters 2-26:
    23 |   include%inline .~Gen.gen
           ^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- a.stag corrected ---
    module Core = struct end
    include%inline .~Gen.gen
      let f =
        fun (x : Gen.t) ->
          match x with
          | Gen.A -> Gen.helper (String.length "a")
          | Gen.B -> .Core.Int.succ 0
    end%inline
    module M = struct
      type t = A | B
      let helper = 1
      module String = struct end
      include%inline .~Gen.gen
        let f =
          fun (x : Gen.t) ->
            match x with
            | Gen.A -> Gen.helper (.String.length "a")
            | Gen.B -> .Core.Int.succ 0
      end%inline
    end
    (gen.stag up to date)
    |}]
;;

let%expect_test "refs in expansions are not captured by generated binders" =
  test
    ~others:
      [ ( "gen.stag"
        , {|
let code = .< fun raise -> let x = raise 1 in .raise (Failure x) >.
let gen = Cons (Stag.Toplevel.let_ "f" Nil (Stag.Code.erase code), Nil)
|}
        )
      ]
    {|
include%inline .~Gen.gen
end%inline
|};
  [%expect
    {|
    File "a.stag", line 16, characters 0-24:
    16 | include%inline .~Gen.gen
         ^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- a.stag corrected ---
    include%inline .~Gen.gen
      let f =
        fun raise ->
          let x = raise 1 in
          .raise (Failure x)
    end%inline
    (gen.stag up to date)
    |}]
;;

let root_paths =
  {|
let def name params body = Stag.Toplevel.let_ name params body
let v = Stag.Expr.var
let app f x = Stag.Expr.apply (v f) [v x]
let one = 1
|}
;;

let%expect_test "root paths in expansions: shortest suffix where they are used" =
  test
    (root_paths
     ^ {|
include%inline .~[def "succ" ["x"] (app ".A.plus" "x")]
end%inline
include%inline .~[
  def "two" [] (app ".A.succ" ".A.one");
  def "plus" ["x"] (v "x");
  def "three" [] (app ".A.plus" ".A.two");
]
end%inline
module M = struct
  let one = "one"
  include%inline .~[def "m" [] (app ".A.succ" ".A.one")]
  end%inline
end
module N = struct
  module A = struct end
  include%inline .~[def "n" ["one"] (app ".A.succ" ".A.one")]
  end%inline
end
module P = struct
  include%inline .~[def "one" [] (Stag.Expr.int 2); def "p" [] (v ".A.one")]
  end%inline
end
module Q = struct
  include%inline .~[
    Stag.Toplevel.Module ("A", []);
    def "one" [] (Stag.Expr.int 3);
    Stag.Toplevel.Module ("Inner", [def "x" [] (v ".A.one"); def "y" [] (v ".A.Q.Inner.x")]);
    def "q" [] (v ".A.Q.Inner.y");
  ]
  end%inline
end
module%inline S = .~[def "x" [] (v ".A.one"); def "y" [] (v ".A.S.x")]
end%inline
let s = S.y
|}
    );
  [%expect
    {|
    File "a.stag", line 21, characters 0-55:
    21 | include%inline .~[def "succ" ["x"] (app ".A.plus" "x")]
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "a.stag", lines 23-27, characters 0-1:
    23 | include%inline .~[
    24 |   def "two" [] (app ".A.succ" ".A.one");
    25 |   def "plus" ["x"] (v "x");
    26 |   def "three" [] (app ".A.plus" ".A.two");
    27 | ]
    Error: This inline expansion is out of date
    File "a.stag", line 31, characters 2-56:
    31 |   include%inline .~[def "m" [] (app ".A.succ" ".A.one")]
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "a.stag", line 36, characters 2-61:
    36 |   include%inline .~[def "n" ["one"] (app ".A.succ" ".A.one")]
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "a.stag", line 40, characters 2-76:
    40 |   include%inline .~[def "one" [] (Stag.Expr.int 2); def "p" [] (v ".A.one")]
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    File "a.stag", lines 44-49, characters 2-3:
    44 |   include%inline .~[
    45 |     Stag.Toplevel.Module ("A", []);
    46 |     def "one" [] (Stag.Expr.int 3);
    47 |     Stag.Toplevel.Module ("Inner", [def "x" [] (v ".A.one"); def "y" [] (v ".A.Q.Inner.x")]);
    48 |     def "q" [] (v ".A.Q.Inner.y");
    49 |   ]
    Error: This inline expansion is out of date
    File "a.stag", line 52, characters 0-70:
    52 | module%inline S = .~[def "x" [] (v ".A.one"); def "y" [] (v ".A.S.x")]
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This inline expansion is out of date
    --- a.stag corrected ---
    let def name params body = Stag.Toplevel.let_ name params body
    let v = Stag.Expr.var
    let app f x = Stag.Expr.apply (v f) [v x]
    let one = 1

    include%inline .~[def "succ" ["x"] (app ".A.plus" "x")]
      let succ x = A.plus x
    end%inline
    include%inline .~[
      def "two" [] (app ".A.succ" ".A.one");
      def "plus" ["x"] (v "x");
      def "three" [] (app ".A.plus" ".A.two");
    ]
      let two = succ one
      let plus x = x
      let three = plus two
    end%inline
    module M = struct
      let one = "one"
      include%inline .~[def "m" [] (app ".A.succ" ".A.one")]
        let m = succ A.one
      end%inline
    end
    module N = struct
      module A = struct end
      include%inline .~[def "n" ["one"] (app ".A.succ" ".A.one")]
        let n one = succ .A.one
      end%inline
    end
    module P = struct
      include%inline .~[def "one" [] (Stag.Expr.int 2); def "p" [] (v ".A.one")]
        let one = 2
        let p = A.one
      end%inline
    end
    module Q = struct
      include%inline .~[
        Stag.Toplevel.Module ("A", []);
        def "one" [] (Stag.Expr.int 3);
        Stag.Toplevel.Module ("Inner", [def "x" [] (v ".A.one"); def "y" [] (v ".A.Q.Inner.x")]);
        def "q" [] (v ".A.Q.Inner.y");
      ]
        module A = struct end

        let one = 3

        module Inner = struct
          let x = .A.one
          let y = x
        end

        let q = Inner.y
      end%inline
    end
    module%inline S = .~[def "x" [] (v ".A.one"); def "y" [] (v ".A.S.x")]
      let x = one
      let y = x
    end%inline
    let s = S.y
    |}]
;;

let%expect_test "an up to date expansion with root paths shortened is located" =
  test
    (root_paths
     ^ {|
include%inline .~[
  def "succ" ["x"] (app ".A.plus" "x");
  def "plus" ["x"] (Stag.Expr.apply (v ".A.one") [v "x"]);
]
  let succ x = plus x
  let plus x = one x
end%inline
|}
    );
  [%expect
    {|
    File "a.stag", line 26, characters 15-18:
    26 |   let plus x = one x
                        ^^^
    Error: This expression has type int but an expression was expected of type '_a -> '_b
    |}]
;;

let%expect_test "shortened root paths are up to date when re-read" =
  test
    (root_paths
     ^ {|
include%inline .~[def "succ" ["x"] (app ".A.plus" "x")]
  let succ x = A.plus x
end%inline
include%inline .~[
  def "two" [] (app ".A.succ" ".A.one");
  def "plus" ["x"] (v "x");
  def "three" [] (app ".A.plus" ".A.two");
]
  let two = succ one
  let plus x = x
  let three = plus two
end%inline
module M = struct
  let one = "one"
  include%inline .~[def "m" [] (app ".A.succ" ".A.one")]
    let m = succ A.one
  end%inline
end
module N = struct
  module A = struct end
  include%inline .~[def "n" ["one"] (app ".A.succ" ".A.one")]
    let n one = succ .A.one
  end%inline
end
module P = struct
  include%inline .~[def "one" [] (Stag.Expr.int 2); def "p" [] (v ".A.one")]
    let one = 2
    let p = A.one
  end%inline
end
module Q = struct
  include%inline .~[
    Stag.Toplevel.Module ("A", []);
    def "one" [] (Stag.Expr.int 3);
    Stag.Toplevel.Module ("Inner", [def "x" [] (v ".A.one"); def "y" [] (v ".A.Q.Inner.x")]);
    def "q" [] (v ".A.Q.Inner.y");
  ]
    module A = struct end

    let one = 3

    module Inner = struct
      let x = .A.one
      let y = x
    end

    let q = Inner.y
  end%inline
end
module%inline S = .~[def "x" [] (v ".A.one"); def "y" [] (v ".A.S.x")]
  let x = one
  let y = x
end%inline
let s = S.y
let%expect_test "values" =
  print_int (Int.add M.m (Int.add three (Int.add (N.n 0) (Int.add P.p (Int.add Q.q s)))))
|}
    );
  [%expect
    {xxx|
    ((name (values @ a.stag:75:16-24))
     (mismatches (((loc a.stag:76:89-89) (expected ()) (actual 6)))) (error ()))
    --- a.stag corrected ---
    let def name params body = Stag.Toplevel.let_ name params body
    let v = Stag.Expr.var
    let app f x = Stag.Expr.apply (v f) [v x]
    let one = 1

    include%inline .~[def "succ" ["x"] (app ".A.plus" "x")]
      let succ x = A.plus x
    end%inline
    include%inline .~[
      def "two" [] (app ".A.succ" ".A.one");
      def "plus" ["x"] (v "x");
      def "three" [] (app ".A.plus" ".A.two");
    ]
      let two = succ one
      let plus x = x
      let three = plus two
    end%inline
    module M = struct
      let one = "one"
      include%inline .~[def "m" [] (app ".A.succ" ".A.one")]
        let m = succ A.one
      end%inline
    end
    module N = struct
      module A = struct end
      include%inline .~[def "n" ["one"] (app ".A.succ" ".A.one")]
        let n one = succ .A.one
      end%inline
    end
    module P = struct
      include%inline .~[def "one" [] (Stag.Expr.int 2); def "p" [] (v ".A.one")]
        let one = 2
        let p = A.one
      end%inline
    end
    module Q = struct
      include%inline .~[
        Stag.Toplevel.Module ("A", []);
        def "one" [] (Stag.Expr.int 3);
        Stag.Toplevel.Module ("Inner", [def "x" [] (v ".A.one"); def "y" [] (v ".A.Q.Inner.x")]);
        def "q" [] (v ".A.Q.Inner.y");
      ]
        module A = struct end

        let one = 3

        module Inner = struct
          let x = .A.one
          let y = x
        end

        let q = Inner.y
      end%inline
    end
    module%inline S = .~[def "x" [] (v ".A.one"); def "y" [] (v ".A.S.x")]
      let x = one
      let y = x
    end%inline
    let s = S.y
    let%expect_test "values" =
      print_int (Int.add M.m (Int.add three (Int.add (N.n 0) (Int.add P.p (Int.add Q.q s)))));
      [%expect {| 6 |}]
    |xxx}]
;;
