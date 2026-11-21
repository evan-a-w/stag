open! Import

let show ?contents loc message =
  print_string
    (Diagnostic.to_string_hum ?contents (Diagnostic.create ~loc message))
;;

let loc (line, col) (line', col') =
  Loc.create
    ~file:"a.stag"
    ~start:#{ line; col; offset = 0 }
    ~stop:#{ line = line'; col = col'; offset = 0 }
;;

let%expect_test "rendering" =
  let source = List.init 10 ~f:(sprintf "line %d") |> String.concat ~sep:"\n" in
  let contents = function
    | "a.stag" -> Some source
    | _ -> None
  in
  show ~contents (loc (3, 2) (3, 5)) "one line";
  show ~contents (loc (3, 6) (3, 6)) "zero width";
  show ~contents (loc (2, 1) (4, 3)) "a few lines";
  show ~contents (loc (1, 0) (10, 3)) "many lines";
  show (loc (3, 2) (3, 5)) "no contents";
  show ~contents Loc.none "no location";
  show ~contents (loc (30, 0) (30, 1)) "out of the file";
  [%expect
    {|
    File "a.stag", line 3, characters 2-5:
    3 | line 2
          ^^^
    Error: one line
    File "a.stag", line 3, characters 6-6:
    3 | line 2
              ^
    Error: zero width
    File "a.stag", lines 2-4, characters 1-3:
    2 | line 1
    3 | line 2
    4 | line 3
    Error: a few lines
    File "a.stag", lines 1-10, characters 0-3:
     1 | line 0
     2 | line 1
     3 | line 2
       | ...
     9 | line 8
    10 | line 9
    Error: many lines
    File "a.stag", line 3, characters 2-5:
    Error: no contents
    Error: no location
    File "a.stag", line 30, characters 0-1:
    Error: out of the file
    |}]
;;

let%expect_test "messages" =
  let check contents =
    match Driver.check_files [ { Source_file.path = "a.stag"; contents } ] with
    | Ok _ -> print_endline "ok"
    | Error error ->
      print_string
        (Diagnostic.list_to_string_hum
           ~contents:(fun _ -> Some contents)
           (List.map error ~f:Diagnostic.of_driver_error))
  in
  List.iter
    ~f:check
    [ "let x = Int.add 1 true"
    ; "let x = Some x"
    ; "let r = {`a = 1}.`b"
    ; "let x = y"
    ; "let x = 1 let x = 2"
    ; "let x = M.y"
    ; "type t = A let x : u = A"
    ; "type t = A of int let x = A"
    ; "type t = {a : int; b : int} let x = {a = 1}"
    ; "type t = t"
    ; "let f p = match p with (x, x) -> 1"
    ; "let g = let y = 1 in .~(let z = y in .<1>.)"
    ; "let c = .<.~(.<1>.).{x = 1; x = 2}>."
    ; "let c = .~(Int.div 1 0; .<1>.)"
    ; "let x = [%expect]"
    ; "let x = Int.foo"
    ; "type t['a] = A let x : t = A"
    ; "module M = struct end module M = struct end"
    ; "let w = .~(let z = 1 in .< z >.)"
    ; "let f z = .< z >.\nlet u = .~(f 3)"
    ];
  [%expect
    {|
    File "a.stag", line 1, characters 18-22:
    1 | let x = Int.add 1 true
                          ^^^^
    Error: This expression has type bool but an expression was expected of type int
    File "a.stag", line 1, characters 8-14:
    1 | let x = Some x
                ^^^^^^
    Error: The type variable '_a occurs inside its own definition
    File "a.stag", line 1, characters 8-16:
    1 | let r = {`a = 1}.`b
                ^^^^^^^^
    Error: The row {`a : int} is incompatible with the row {`b : '_a; ..'_b}
    The label b is missing.
    File "a.stag", line 1, characters 8-9:
    1 | let x = y
                ^
    Error: Unbound value y
    File "a.stag", line 1, characters 14-15:
    1 | let x = 1 let x = 2
                      ^
    Error: The value A.x is defined more than once in this module
    File "a.stag", line 1, characters 8-9:
    1 | let x = M.y
                ^
    Error: Unbound module M
    File "a.stag", line 1, characters 19-20:
    1 | type t = A let x : u = A
                           ^
    Error: Unbound type u
    File "a.stag", line 1, characters 26-27:
    1 | type t = A of int let x = A
                                  ^
    Error: The constructor A.A expects 1 argument(s), but is applied to 0
    File "a.stag", line 1, characters 36-43:
    1 | type t = {a : int; b : int} let x = {a = 1}
                                            ^^^^^^^
    Error: Some record fields are undefined: A.b
    File "a.stag", line 1, characters 5-6:
    1 | type t = t
             ^
    Error: The type abbreviation A.t is cyclic
    File "a.stag", line 1, characters 27-28:
    1 | let f p = match p with (x, x) -> 1
                                   ^
    Error: The variable x is bound several times in this matching
    File "a.stag", line 1, characters 32-33:
    1 | let g = let y = 1 in .~(let z = y in .<1>.)
                                        ^
    Error: The variable y is bound at runtime (level 0) but used at compile time (level -1); only toplevel functions may be used at other levels
    File "a.stag", line 1, characters 28-29:
    1 | let c = .<.~(.<1>.).{x = 1; x = 2}>.
                                    ^
    Error: The variable x is substituted more than once
    File "a.stag", line 1, characters 8-30:
    1 | let c = .~(Int.div 1 0; .<1>.)
                ^^^^^^^^^^^^^^^^^^^^^^
    Error: This toplevel splice failed at compile time: Uncaught exception Division_by_zero
    File "a.stag", line 1, characters 11-22:
    1 | let c = .~(Int.div 1 0; .<1>.)
                   ^^^^^^^^^^^
    Note: The error was raised here
    File "a.stag", line 1, characters 8-17:
    1 | let x = [%expect]
                ^^^^^^^^^
    Error: [%expect] is only allowed in the body of a let%expect_test where it runs with the test: not in a quote's code nor in a toplevel splice
    File "a.stag", line 1, characters 8-15:
    1 | let x = Int.foo
                ^^^^^^^
    Error: Unbound value Int.foo
    File "a.stag", line 1, characters 23-24:
    1 | type t['a] = A let x : t = A
                               ^
    Error: The type t expects 1 argument(s), but is given 0
    File "a.stag", line 1, characters 29-30:
    1 | module M = struct end module M = struct end
                                     ^
    Error: The module A.M is defined more than once
    File "a.stag", line 1, characters 8-32:
    1 | let w = .~(let z = 1 in .< z >.)
                ^^^^^^^^^^^^^^^^^^^^^^^^
    Error: Unbound value z, which the spliced code uses: z is a variable of its quote's context, as the z bound at compile time (level -1) can't be used in the quote (level 0); only toplevel functions may be used at other levels
    File "a.stag", line 1, characters 27-28:
    1 | let w = .~(let z = 1 in .< z >.)
                                   ^
    Note: z is a variable of the code's context here
    File "a.stag", line 2, characters 8-15:
    2 | let u = .~(f 3)
                ^^^^^^^
    Error: Unbound value z, which the spliced code uses: z is a variable of its quote's context, as the z bound at runtime (level 0) can't be used in the quote (level 1); only toplevel functions may be used at other levels
    File "a.stag", line 1, characters 13-14:
    1 | let f z = .< z >.
                     ^
    Note: z is a variable of the code's context here
    |}]
;;

let%expect_test "tabs and wide characters before the span" =
  let contents = function
    | "a.stag" -> Some "\tlet x =\t\"\xc3\xa9\" y"
    | _ -> None
  in
  let shown = ref "" in
  let show loc message =
    shown := Diagnostic.to_string_hum ~contents (Diagnostic.create ~loc message)
  in
  show (loc (1, 15) (1, 16)) "y";
  print_string (String.substr_replace_all !shown ~pattern:"\t" ~with_:"<tab>");
  [%expect
    {|
    File "a.stag", line 1, characters 15-16:
    1 | <tab>let x =<tab>"é" y
        <tab>       <tab>     ^
    Error: y
    |}]
;;

let%expect_test "whole file errors and notes" =
  let contents = function
    | "a.stag" -> Some "let x = 1\nlet y = 2"
    | _ -> None
  in
  print_string
    (Diagnostic.to_string_hum
       ~contents
       { (Diagnostic.create ~loc:(loc (1, 0) (1, 0)) "bad file") with
         whole_file = true
       });
  print_string
    (Diagnostic.to_string_hum
       ~contents
       (Diagnostic.create
          ~loc:(loc (1, 4) (1, 5))
          "main"
          ~notes:
            [ { loc = loc (2, 4) (2, 5); message = "note" }
            ; { loc = Loc.none; message = "unlocated note" }
            ]));
  [%expect
    {|
    File "a.stag":
    Error: bad file
    File "a.stag", line 1, characters 4-5:
    1 | let x = 1
            ^
    Error: main
    File "a.stag", line 2, characters 4-5:
    2 | let y = 2
            ^
    Note: note
    Note: unlocated note
    |}]
;;

let%expect_test "test results" =
  let contents =
    {x|let%expect_test "error" =
  print_string "before\n  indented";
  match 2 with 1 -> ()

let%expect_test "different" =
  let f x = print_int x; [%expect {| 1 |}] in f 1; f 2; print_string ""

let%expect_test "different, with no output" =
  let f x = (match x with 0 -> () | _ -> print_int x); [%expect] in f 0; f 1;
  print_string "x"; [%expect {| x |}];
  print_int (Int.div 1 0)
|x}
  in
  let driver =
    Driver.check_files [ { Source_file.path = "a.stag"; contents } ]
    |> Result.map_error ~f:(fun error ->
      Error.create_s [%sexp (error : Driver.Error.t list)])
    |> ok_exn
  in
  Driver.eval driver |> ok_exn;
  List.iter (Expect_test_runner.run driver) ~f:(fun result ->
    List.iter (Diagnostic.of_test_result result) ~f:(fun diagnostic ->
      print_string
        (Diagnostic.to_string_hum ~contents:(fun _ -> Some contents) diagnostic)));
  [%expect
    {xxx|
    File "a.stag", line 3, characters 2-22:
    3 |   match 2 with 1 -> ()
          ^^^^^^^^^^^^^^^^^^^^
    Error: Expect test "error" failed: Match failure: no case matches the value 2
    Output since the last [%expect]:
      before
        indented
    File "a.stag", line 6, characters 34-41:
    6 |   let f x = print_int x; [%expect {| 1 |}] in f 1; f 2; print_string ""
                                          ^^^^^^^
    Error: Expect test "different": this [%expect] was reached with different outputs:
    Output 1:
      1
    Output 2:
      2
    File "a.stag", line 9, characters 63-63:
    9 |   let f x = (match x with 0 -> () | _ -> print_int x); [%expect] in f 0; f 1;
                                                                       ^
    Error: Expect test "different, with no output": this [%expect] was reached with different outputs:
    Output 1:
      (no output)
    Output 2:
      1
    File "a.stag", line 11, characters 12-25:
    11 |   print_int (Int.div 1 0)
                     ^^^^^^^^^^^^^
    Error: Expect test "different, with no output" failed: Uncaught exception Division_by_zero
    |xxx}]
;;

let%expect_test "exhausted limits are located" =
  let contents =
    {|let f x = Int.add 1 (f x)
let%expect_test "deep" = print_int (f 1)
|}
  in
  let driver =
    Driver.check_files [ { Source_file.path = "a.stag"; contents } ]
    |> Result.map_error ~f:(fun error ->
      Error.create_s [%sexp (error : Driver.Error.t list)])
    |> ok_exn
  in
  Driver.eval driver |> ok_exn;
  List.iter
    [ { Limits.fuel = None; max_depth = Some 100; time_limit = None }
    ; { fuel = Some 1000; max_depth = None; time_limit = None }
    ]
    ~f:(fun limits ->
      List.iter (Expect_test_runner.run ~limits driver) ~f:(fun result ->
        List.iter (Diagnostic.of_test_result result) ~f:(fun diagnostic ->
          print_string
            (Diagnostic.to_string_hum
               ~contents:(fun _ -> Some contents)
               diagnostic))));
  [%expect
    {|
    File "a.stag", line 1, characters 10-17:
    1 | let f x = Int.add 1 (f x)
                  ^^^^^^^
    Error: Expect test "deep" failed: Stack depth exceeded: evaluation nested more than 100 steps deep
    File "a.stag", line 1, characters 10-19:
    1 | let f x = Int.add 1 (f x)
                  ^^^^^^^^^
    Error: Expect test "deep" failed: Out of fuel: evaluation took more than 1000 steps
    |}]
;;

let%expect_test "end to end: source to rendered errors" =
  let check contents =
    match Driver.check_files [ { Source_file.path = "a.stag"; contents } ] with
    | Ok _ -> print_endline "ok"
    | Error error ->
      print_string
        (Diagnostic.list_to_string_hum
           ~contents:(fun _ -> Some contents)
           (List.map error ~f:Diagnostic.of_driver_error))
  in
  List.iter
    ~f:check
    [ (* lexing *)
      "let s = \"abc"
    ; "let c = '\\q'"
    ; "let n = 99999999999999999999"
    ; "let n = 1_000 let s = \"a\\\n    b\""
    ; (* parsing *)
      "let rec f x = f x"
    ; "let g = let rec f x = f x in f"
    ; "let x = f.{y}"
    ; "type t = C of int -> int"
    ; "type t = C of int * int list"
    ; "let f (x : int list) = x"
    ; "type t = list[int][int]"
    ; "let x @ const = 1"
    ; "type t[] = int"
    ; "type t = [`A | ]"
    ; "type t = with_x[{x : int}]"
    ; "type t = <int>{`x : int}"
    ; "let r = {x = let a = 1 in a; y}"
    ; "let x = (1, 2"
    ; "module M = struct\n  let x = 1\n"
    ; (* typechecking *)
      "module M = struct module N = struct let y = 1 end end let x = M.N.z"
    ; "module M = struct end let x = M.N.y"
    ; "module M = struct module N = struct end end let x = M.N.O.y"
    ; "let x : M.t = 1"
    ; "let x : list[int, int] = 1"
    ; "type t = A of int let x = A (1, 2)"
    ; "let f r = r.`x let y = f {`y = 1}"
    ; "let f x =\n  match x with\n  | `A -> 1\n  | `B -> true"
    ; "let p = {`x = 1} \\ `y"
    ; "let f r = match r with {`x = a; ..a} -> a"
    ; "let c = .<1>. let d = .~c.{y = 1}"
    ; "let%expect_test \"t\" = 1"
    ; "let x = .<[%expect]>."
    ];
  [%expect
    {|
    File "a.stag", line 1, characters 8-9:
    1 | let s = "abc
                ^
    Error: Unterminated string literal
    File "a.stag", line 1, characters 9-11:
    1 | let c = '\q'
                 ^^
    Error: Illegal escape sequence \q
    File "a.stag", line 1, characters 8-28:
    1 | let n = 99999999999999999999
                ^^^^^^^^^^^^^^^^^^^^
    Error: Integer literal 99999999999999999999 exceeds the range of int
    ok
    File "a.stag", line 1, characters 4-7:
    1 | let rec f x = f x
            ^^^
    Error: Lets are always recursive; remove `rec`
    File "a.stag", line 1, characters 12-15:
    1 | let g = let rec f x = f x in f
                    ^^^
    Error: Lets are always recursive; remove `rec`
    File "a.stag", line 1, characters 9-11:
    1 | let x = f.{y}
                 ^^
    Error: Substitutions `.{...}` are only written after a splice `.~e`
    File "a.stag", line 1, characters 18-20:
    1 | type t = C of int -> int
                          ^^
    Error: A constructor argument of function type must be parenthesized: `C of (a -> b)`
    File "a.stag", line 1, characters 24-28:
    1 | type t = C of int * int list
                                ^^^^
    Error: Type arguments are written after the name, in brackets: `list[int]`
    File "a.stag", line 1, characters 15-19:
    1 | let f (x : int list) = x
                       ^^^^
    Error: Type arguments are written after the name, in brackets: `list[int]`
    File "a.stag", line 1, characters 18-19:
    1 | type t = list[int][int]
                          ^
    Error: Type arguments are written once
    File "a.stag", line 1, characters 6-7:
    1 | let x @ const = 1
              ^
    Error: `@` is reserved (for modes)
    File "a.stag", line 1, characters 7-8:
    1 | type t[] = int
               ^
    Error: Expected a type parameter, found `]`
    File "a.stag", line 1, characters 15-16:
    1 | type t = [`A | ]
                       ^
    Error: Expected a polymorphic variant tag like `A, found `]`
    File "a.stag", line 1, characters 17-18:
    1 | type t = with_x[{x : int}]
                         ^
    Error: Polymorphic record labels are written with a backtick: `x
    File "a.stag", line 1, characters 15-17:
    1 | type t = <int>{`x : int}
                       ^^
    Error: The variables of a code context are written without a backtick: x
    File "a.stag", line 1, characters 30-31:
    1 | let r = {x = let a = 1 in a; y}
                                      ^
    Error: Expected `=`, found `}`
    File "a.stag", line 1, characters 13-13:
    1 | let x = (1, 2
                     ^
    Error: Expected `)` to close this `(` (opened at 1:8), found end of file
    File "a.stag", line 3, characters 0-0:
    3 |
        ^
    Error: Expected `end` to close this `struct` (opened at 1:11), found end of file
    File "a.stag", line 1, characters 62-67:
    1 | module M = struct module N = struct let y = 1 end end let x = M.N.z
                                                                      ^^^^^
    Error: Unbound value M.N.z
    File "a.stag", line 1, characters 30-33:
    1 | module M = struct end let x = M.N.y
                                      ^^^
    Error: Unbound module M.N
    File "a.stag", line 1, characters 52-57:
    1 | module M = struct module N = struct end end let x = M.N.O.y
                                                            ^^^^^
    Error: Unbound module M.N.O
    File "a.stag", line 1, characters 8-9:
    1 | let x : M.t = 1
                ^
    Error: Unbound module M
    File "a.stag", line 1, characters 8-22:
    1 | let x : list[int, int] = 1
                ^^^^^^^^^^^^^^
    Error: The type list expects 1 argument(s), but is given 2
    File "a.stag", line 1, characters 29-33:
    1 | type t = A of int let x = A (1, 2)
                                     ^^^^
    Error: This expression has type int * int but an expression was expected of type int
    File "a.stag", line 1, characters 25-33:
    1 | let f r = r.`x let y = f {`y = 1}
                                 ^^^^^^^^
    Error: The row {`y : int} is incompatible with the row {`x : '_a; ..'_b}
    The label x is missing.
    File "a.stag", line 4, characters 10-14:
    4 |   | `B -> true
                  ^^^^
    Error: This expression has type bool but an expression was expected of type int
    File "a.stag", line 1, characters 8-16:
    1 | let p = {`x = 1} \ `y
                ^^^^^^^^
    Error: The row {`x : int} is incompatible with the row {`y : '_a; ..'_b}
    The label y is missing.
    File "a.stag", line 1, characters 34-35:
    1 | let f r = match r with {`x = a; ..a} -> a
                                          ^
    Error: The variable a is bound several times in this matching
    ok
    File "a.stag", line 1, characters 22-23:
    1 | let%expect_test "t" = 1
                              ^
    Error: This expression has type int but an expression was expected of type unit
    File "a.stag", line 1, characters 10-19:
    1 | let x = .<[%expect]>.
                  ^^^^^^^^^
    Error: [%expect] is only allowed in the body of a let%expect_test where it runs with the test: not in a quote's code nor in a toplevel splice
    |}]
;;
