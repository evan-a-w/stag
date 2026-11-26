open! Import

let print_diagnostic ~contents diagnostic =
  print_string
    (Diagnostic.to_string_hum ~contents:(fun _ -> Some contents) diagnostic)
;;

(* Checks and runs [contents] as the file [a.stag], then its expect tests,
   printing the signature with [~signature], the program's output and the errors
   as the command line does. *)
let run ?(signature = false) contents =
  match
    Driver.check_files
      ~limits:Source_helpers.limits
      [ { Source_file.path = "a.stag"; contents } ]
  with
  | Error error ->
    List.iter error ~f:(fun error ->
      print_diagnostic ~contents (Diagnostic.of_driver_error error))
  | Ok driver ->
    if signature
    then
      print_string (Signature.to_string_hum (Driver.checked driver).signature);
    (match Driver.eval ~limits:Source_helpers.limits driver with
     | Error error ->
       print_diagnostic ~contents (Diagnostic.of_runtime_error error)
     | Ok () ->
       Expect_test_runner.run ~limits:Source_helpers.limits driver
       |> List.concat_map ~f:Diagnostic.of_test_result
       |> List.iter ~f:(print_diagnostic ~contents))
;;

let%expect_test "declarations" =
  run
    ~signature:true
    {|
exception E
exception F of int
exception G of int * string
exception H of (int * string)
exception L of list[option[int]]
module M = struct
  exception E of bool
  let e = E true
end
let e : exn = E
let f = F 1
let g = G (1, "x")
let h = H (1, "x")
let l = L (Cons (Box.make ( (None, Nil))))
let es = Cons (Box.make ( (E, Cons (Box.make ( (M.E false, Cons (Box.make ( (M.e, Nil)))))))))
|};
  [%expect
    {|
    module A : sig
      exception E
      exception F of int
      exception G of int * string
      exception H of (int * string)
      exception L of list[option[int]]
      module M : sig
        exception E of bool
        val e : exn
      end
      val e : exn
      val f : exn
      val g : exn
      val h : exn
      val l : exn
      val es : list[exn]
    end
    |}]
;;

let%expect_test "raising and handling" =
  run
    {|
exception E
exception F of int
exception G of int * string
exception H of (int * string)
let show s = print_endline s
let describe f =
  match f () with
  | n -> show (String.append "returned " (Int.to_string n))
  | exception E -> show "E"
  | exception F 0 -> show "F 0"
  | exception F n -> show (String.append "F " (Int.to_string n))
  | exception G (n, s) -> print_string "G "; print_int n; show (String.append " " s)
  | exception H p -> (match p with (n, s) -> print_string "H "; print_int n; show (String.append " " s))
let main =
  describe (fun u -> 1);
  describe (fun u -> raise E);
  describe (fun u -> raise (F 0));
  describe (fun u -> raise (F 7));
  describe (fun u -> raise (G (1, "g")));
  describe (fun u -> raise (H (2, "h")));
  describe (fun u -> Int.add 1 (raise E));
  describe (fun u -> match raise (F 3) with () -> 0)
|};
  [%expect
    {|
    returned 1
    E
    F 0
    F 7
    G 1 g
    H 2 h
    E
    F 3
    |}]
;;

let%expect_test "a catch-all binds the exception, which can be re-raised" =
  run
    {|
exception E of int
exception F
let show s = print_endline s
let name e =
  match e with
  | E n -> String.append "E " (Int.to_string n)
  | F -> "F"
  | _ -> "another"
let inner f =
  match f () with
  | () -> show "inner: returned"
  | exception F -> show "inner: F"
  | exception e -> show (String.append "inner: re-raising " (name e)); raise e
let outer f =
  match inner f with
  | () -> show "outer: returned"
  | exception e -> show (String.append "outer: " (name e))
let main =
  outer (fun u -> ());
  outer (fun u -> raise F);
  outer (fun u -> raise (E 1));
  outer (fun u -> raise Division_by_zero)
|};
  [%expect
    {|
    inner: returned
    outer: returned
    inner: F
    outer: returned
    inner: re-raising E 1
    outer: E 1
    inner: re-raising another
    outer: another
    |}]
;;

let%expect_test "exceptions raised in arms are not caught by their match" =
  run
    {|
exception E of string
let show s = print_endline s
let f x =
  match x with
  | 0 -> raise (E "value arm")
  | n -> n
  | exception E s -> show (String.append "caught " s); 0
let g u =
  match f 0 with
  | n -> n
  | exception E s -> show (String.append "outer caught " s); 1
let h u =
  match raise (E "scrutinee") with
  | () -> ()
  | exception E s -> raise (E (String.append "handler, after " s))
let main =
  print_int (g ());
  show "";
  match h () with
  | () -> ()
  | exception E s -> show (String.append "outer caught " s)
|};
  [%expect
    {|
    outer caught value arm
    1
    outer caught handler, after scrutinee
    |}]
;;

let%expect_test "nested matches" =
  run
    {|
exception E of int
let show s = print_endline s
let main =
  match
    (match raise (E 1) with
     | () -> 0
     | exception E n -> raise (E (Int.add n 10)))
  with
  | n -> show "unreachable"
  | exception E n ->
    (match (match n with 11 -> raise (E 100) | n -> n) with
     | n -> show "unreachable"
     | exception E m -> print_int n; print_string " then "; print_int m)
|};
  [%expect {| 11 then 100 |}]
;;

let%expect_test "exceptions not handled pass through" =
  run
    {|
exception E
exception F
let main =
  match (match raise F with () -> 0 | exception E -> 1) with
  | n -> print_int n
  | exception F -> print_endline "F passed through"
|};
  [%expect {| F passed through |}]
;;

let%expect_test "exceptions are generative per declaration" =
  run
    {|
module M = struct
  exception E
  let raise_ u = raise E
end
module N = struct
  exception E
end
exception E
let which f =
  match f () with
  | () -> "none"
  | exception N.E -> "N.E"
  | exception E -> "E"
  | exception M.E -> "M.E"
let main =
  print_endline (which M.raise_);
  print_endline (which (fun u -> raise N.E));
  print_endline (which (fun u -> raise E));
  print_endline (match M.E with N.E -> "same" | E -> "same" | _ -> "different")
|};
  [%expect {|
    M.E
    N.E
    E
    different
    |}]
;;

let%expect_test "prims raise exceptions of the prelude" =
  run
    {|
let show s = print_endline s
let try_ f =
  match f () with
  | s -> show s
  | exception Division_by_zero -> show "Division_by_zero"
  | exception Invalid_argument s -> show (String.append "Invalid_argument: " s)
  | exception Failure s -> show (String.append "Failure: " s)
let main =
  try_ (fun u -> Int.to_string (Int.div 1 0));
  try_ (fun u -> Int.to_string (Int.rem 1 0));
  try_ (fun u -> Char.to_string (String.get "abc" 3));
  try_ (fun u -> String.sub "abc" 2 2);
  try_ (fun u -> Char.to_string (Char.of_code 256));
  try_ (fun u -> Int.to_string (Int.of_string_exn "12a"));
  try_ (fun u -> Int.to_string (Int.of_string_exn "12"))
|};
  [%expect
    {|
    Division_by_zero
    Division_by_zero
    Invalid_argument: String.get: index 3 out of bounds of a string of length 3
    Invalid_argument: String.sub: invalid substring (position 2, length 2) of a string of length 3
    Invalid_argument: Char.of_code: invalid character code 256
    Failure: Int.of_string_exn: invalid integer "12a"
    12
    |}]
;;

let%expect_test "exception values print by path" =
  run
    {|
module M = struct
  exception E of int * string
  exception F of list[int]
end
exception G
let main = raise (M.E (1, "x"))
|};
  [%expect
    {|
    File "a.stag", line 7, characters 11-31:
    7 | let main = raise (M.E (1, "x"))
                   ^^^^^^^^^^^^^^^^^^^^
    Error: Uncaught exception A.M.E (1, "x")
    |}];
  run {|let main = raise (Invalid_argument "bad")|};
  [%expect
    {|
    File "a.stag", line 1, characters 11-41:
    1 | let main = raise (Invalid_argument "bad")
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: Uncaught exception Invalid_argument "bad"
    |}];
  run
    {|
module M = struct exception F of list[int] * option[exn] end
exception G
let main = raise (M.F (Cons (Box.make ( (1, Nil))), Some G))
|};
  [%expect
    {|
    File "a.stag", line 4, characters 11-60:
    4 | let main = raise (M.F (Cons (Box.make ( (1, Nil))), Some G))
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: Uncaught exception A.M.F (Cons (Box.make (1, Nil)), Some A.G)
    |}]
;;

let%expect_test "uncaught in an expect test" =
  run
    {x|
exception E of int * string
let%expect_test "boom" =
  print_endline "before";
  [%expect {| before |}];
  print_endline "after";
  raise (E (1, "x"))
let%expect_test "fine" = print_endline "fine"; [%expect {| fine |}]
|x};
  [%expect
    {|
    File "a.stag", line 7, characters 2-20:
    7 |   raise (E (1, "x"))
          ^^^^^^^^^^^^^^^^^^
    Error: Expect test "boom" failed: Uncaught exception A.E (1, "x")
    Output since the last [%expect]:
      after
    |}]
;;

let%expect_test "typing" =
  run
    ~signature:true
    {|
exception E of int
let f x = match x with 0 -> raise (E 0) | n -> n
let g x = match f x with n -> Some n | exception E n -> None
let h e = match e with E n -> n | e -> raise e
let reraise f = match f 0 with x -> x | exception e -> raise e
let any = raise
|};
  [%expect
    {|
    module A : sig
      exception E of int
      val f : int -> int
      val g : int -> option[int]
      val h : exn -> int
      val reraise : (int -> 'a) -> 'a
      val any : exn -> 'a
    end
    |}]
;;

let%expect_test "type errors" =
  List.iter
    ~f:run
    [ "exception E\nlet x = match (1, raise E) with (n, exception E) -> n"
    ; "exception E\nlet x = match Some 1 with Some (exception E) -> 1 | _ -> 0"
    ; "exception E\nlet x = match 1 with exception E -> 1"
    ; "let x = match 1 with n -> n | exception F -> 0"
    ; "exception E of int\nlet x = match 1 with n -> n | exception E -> 0"
    ; "exception E\nlet x = match 1 with n -> n | exception E 1 -> 0"
    ; "exception E\nlet x = match 1 with n -> n | exception E -> true"
    ; "let x = match 1 with n -> n | exception Some e -> 0"
    ; "let x = match 1 with n -> n | exception 1 -> 0"
    ; "exception E of 'a"
    ; "exception E of list['a]"
    ; "exception E of t"
    ; "exception E\nexception E"
    ; "exception E\ntype t = E"
    ; "exception E of int\nlet x = E"
    ; "exception E\nlet x : int = E"
    ; "let x = raise 1"
    ];
  [%expect
    {|
    File "a.stag", line 2, characters 36-47:
    2 | let x = match (1, raise E) with (n, exception E) -> n
                                            ^^^^^^^^^^^
    Error: An exception pattern is only allowed at the top of a match case, as in [match e with | exception E -> ...]
    File "a.stag", line 2, characters 31-44:
    2 | let x = match Some 1 with Some (exception E) -> 1 | _ -> 0
                                       ^^^^^^^^^^^^^
    Error: An exception pattern is only allowed at the top of a match case, as in [match e with | exception E -> ...]
    File "a.stag", line 2, characters 8-37:
    2 | let x = match 1 with exception E -> 1
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: A match must have a case for the value, not only exception cases
    File "a.stag", line 1, characters 40-41:
    1 | let x = match 1 with n -> n | exception F -> 0
                                                ^
    Error: Unbound constructor F
    File "a.stag", line 2, characters 40-41:
    2 | let x = match 1 with n -> n | exception E -> 0
                                                ^
    Error: The constructor A.E expects 1 argument(s), but is applied to 0
    File "a.stag", line 2, characters 40-43:
    2 | let x = match 1 with n -> n | exception E 1 -> 0
                                                ^^^
    Error: The constructor A.E expects 0 argument(s), but is applied to 1
    File "a.stag", line 2, characters 45-49:
    2 | let x = match 1 with n -> n | exception E -> true
                                                     ^^^^
    Error: This expression has type bool but an expression was expected of type int
    File "a.stag", line 1, characters 40-46:
    1 | let x = match 1 with n -> n | exception Some e -> 0
                                                ^^^^^^
    Error: This expression has type option['_a] but an expression was expected of type exn
    File "a.stag", line 1, characters 40-41:
    1 | let x = match 1 with n -> n | exception 1 -> 0
                                                ^
    Error: This expression has type int but an expression was expected of type exn
    File "a.stag", line 1, characters 15-17:
    1 | exception E of 'a
                       ^^
    Error: Unbound type variable 'a
    File "a.stag", line 1, characters 20-22:
    1 | exception E of list['a]
                            ^^
    Error: Unbound type variable 'a
    File "a.stag", line 1, characters 15-16:
    1 | exception E of t
                       ^
    Error: Unbound type t
    File "a.stag", line 2, characters 10-11:
    2 | exception E
                  ^
    Error: The constructor A.E is defined more than once in this module
    File "a.stag", line 2, characters 9-10:
    2 | type t = E
                 ^
    Error: The constructor A.E is defined more than once in this module
    File "a.stag", line 2, characters 8-9:
    2 | let x = E
                ^
    Error: The constructor A.E expects 1 argument(s), but is applied to 0
    File "a.stag", line 2, characters 14-15:
    2 | let x : int = E
                      ^
    Error: This expression has type exn but an expression was expected of type int
    File "a.stag", line 1, characters 14-15:
    1 | let x = raise 1
                      ^
    Error: This expression has type int but an expression was expected of type exn
    |}]
;;

let%expect_test "matching on exn" =
  run
    {|[@@@warning "-non-exhaustive"]
exception E
exception F of int
let name e = match e with E -> "E" | F n -> "F"
let main = print_endline (name Division_by_zero)
|};
  [%expect
    {|[@@@warning "-non-exhaustive"]
    File "a.stag", line 4, characters 13-47:
    4 | let name e = match e with E -> "E" | F n -> "F"
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: Match failure: no case matches the value Division_by_zero
    |}];
  run
    {|[@@@warning "-non-exhaustive"]
exception E
exception F of int
let name e = match e with E -> "E" | F 1 -> "F 1" | _ -> "other"
let main =
  print_endline (name E);
  print_endline (name (F 1));
  print_endline (name (F 2));
  print_endline (name Division_by_zero)
|};
  [%expect {|
    E
    F 1
    other
    other
    |}];
  run
    "let f b = match b with true -> 1 | exception Division_by_zero -> 0\n\
     let main = f false";
  [%expect
    {|[@@@warning "-non-exhaustive"]
    File "a.stag", line 1, characters 10-66:
    1 | let f b = match b with true -> 1 | exception Division_by_zero -> 0
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: Match failure: no case matches the value false
    |}]
;;

let%expect_test "limits are not exceptions" =
  run
    {|
let loop n = loop n
let main = match loop 0 with n -> n | exception e -> 0
|};
  [%expect
    {|
    File "a.stag", line 2, characters 13-19:
    2 | let loop n = loop n
                     ^^^^^^
    Error: Out of fuel: evaluation took more than 1000000 steps
    |}];
  run
    {|
let deep n = Int.add 1 (deep n)
let main = match deep 0 with n -> n | exception e -> 0
|};
  [%expect
    {|
    File "a.stag", line 2, characters 13-20:
    2 | let deep n = Int.add 1 (deep n)
                     ^^^^^^^
    Error: Stack depth exceeded: evaluation nested more than 10000 steps deep
    |}]
;;

let%expect_test "exceptions in quoted code" =
  run
    {|
exception E of int
let code = .<fun x -> match Int.div 10 x with n -> n | exception Division_by_zero -> raise (E x)>.
let f = .~code
let main =
  print_int (f 2);
  print_endline "";
  match f 0 with
  | n -> print_int n
  | exception E n -> print_string "E "; print_int n
let raising = .<raise (E 5)>.
let g = .~(.<fun u -> .~raising>.)
let main2 = match g 0 with n -> n | exception E n -> print_endline " caught 5"; n
|};
  [%expect {|
    5
    E 0 caught 5
    |}]
;;

let%expect_test "exceptions at compile time" =
  run
    {|
exception E of int
let c = .~(match raise (E 1) with () -> .<0>. | exception E n -> print_int n; print_endline " at compile time"; .<1>.)
let main = print_int c
|};
  [%expect {|
    1 at compile time
    1
    |}];
  run {|
exception E of int
let c = .~(raise (E 1); .<0>.)
|};
  [%expect
    {|
    File "a.stag", line 3, characters 8-30:
    3 | let c = .~(raise (E 1); .<0>.)
                ^^^^^^^^^^^^^^^^^^^^^^
    Error: This toplevel splice failed at compile time: Uncaught exception A.E 1
    File "a.stag", line 3, characters 11-22:
    3 | let c = .~(raise (E 1); .<0>.)
                   ^^^^^^^^^^^
    Note: The error was raised here
    |}];
  run {|
exception E of int * string
module M = .~(raise (E (1, "x")); Nil)
|};
  [%expect
    {|
    File "a.stag", line 3, characters 0-38:
    3 | module M = .~(raise (E (1, "x")); Nil)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception A.E (1, "x")
    File "a.stag", line 3, characters 14-32:
    3 | module M = .~(raise (E (1, "x")); Nil)
                      ^^^^^^^^^^^^^^^^^^
    Note: The error was raised here
    |}];
  run {|
include .~(Int.div 1 0; Nil)
|};
  [%expect
    {|
    File "a.stag", line 2, characters 0-28:
    2 | include .~(Int.div 1 0; Nil)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This declaration splice failed at compile time: Uncaught exception Division_by_zero
    File "a.stag", line 2, characters 11-22:
    2 | include .~(Int.div 1 0; Nil)
                   ^^^^^^^^^^^
    Note: The error was raised here
    |}]
;;

let%expect_test "generated exception declarations" =
  run
    ~signature:true
    {|
module M = .~(
  Cons (Box.make ( (Stag.Toplevel.Exception ("E", Cons (Box.make ( (Stag.Type_expr.Con (Stag.Path.Name "int", Nil), Nil)))),
  Cons (Box.make ( (Stag.Toplevel.Exception ("F", Nil), Nil)))))))
let main =
  match raise (M.E 3) with
  | () -> ()
  | exception M.F -> print_endline "F"
  | exception M.E n -> print_int n
|};
  [%expect
    {|
    module A : sig
      module M : sig
        exception E of int
        exception F
      end
      val main : unit
    end
    3
    |}]
;;

(* [big 60] shares its subtrees, so it is built in 60 steps but shows as 2^60
   leaves: a raise must not show its exception unless it is uncaught, and then
   only the start of it. *)
let big_tree =
  {|
type tree = Leaf | Node of ptr[tree * tree]
exception E of tree
let big n = match n with 0 -> Leaf | _ -> let t = big (Int.sub n 1) in Node (Ptr.make (t, t))
let t = big 60
|}
;;

let%expect_test "raising a big value is cheap" =
  run
    (big_tree
     ^ {|
let catch n acc = match n with 0 -> acc | _ -> catch (Int.sub n 1) (match raise (E t) with () -> acc | exception E _ -> Int.add acc 1)
let main = print_int (catch 1000 0)
|}
    );
  [%expect {| 1000 |}];
  run (big_tree ^ "let main = raise (E t)\n");
  [%expect
    {|
    File "a.stag", line 6, characters 11-22:
    6 | let main = raise (E t)
                   ^^^^^^^^^^^
    Error: Uncaught exception A.E (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Node (Ptr.make (A.Nod...
    |}]
;;

let run_tests_only contents =
  match
    Driver.check_files
      ~limits:Source_helpers.limits
      [ { Source_file.path = "a.stag"; contents } ]
  with
  | Error error ->
    List.iter error ~f:(fun error ->
      print_diagnostic ~contents (Diagnostic.of_driver_error error))
  | Ok driver ->
    Expect_test_runner.run ~limits:Source_helpers.limits driver
    |> List.concat_map ~f:Diagnostic.of_test_result
    |> List.iter ~f:(print_diagnostic ~contents)
;;

let%expect_test "a toplevel's initialization raising can't be handled by its \
                 users"
  =
  run
    {|
exception E
let x = raise E
let y = .~(match x with n -> .<1>. | exception E -> .<2>.)
let z = .~(match x with n -> .<1>. | exception E -> .<3>.)
|};
  [%expect
    {|
    File "a.stag", line 4, characters 8-58:
    4 | let y = .~(match x with n -> .<1>. | exception E -> .<2>.)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This toplevel splice failed at compile time: Initialization of A.x raised A.E
    File "a.stag", line 3, characters 8-15:
    3 | let x = raise E
                ^^^^^^^
    Note: The error was raised here
    File "a.stag", line 5, characters 8-58:
    5 | let z = .~(match x with n -> .<1>. | exception E -> .<3>.)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This toplevel splice failed at compile time: Initialization of A.x raised A.E
    File "a.stag", line 3, characters 8-15:
    3 | let x = raise E
                ^^^^^^^
    Note: The error was raised here
    |}];
  run
    {|
exception E
let x = Int.add 1 (raise E)
let main = match x with n -> print_int n | exception E -> print_endline "caught"
|};
  [%expect
    {|
    File "a.stag", line 3, characters 18-27:
    3 | let x = Int.add 1 (raise E)
                          ^^^^^^^^^
    Error: Uncaught exception A.E
    |}];
  (* Each test forces [x], the first evaluating it. *)
  run_tests_only
    {xx|
exception E
let x = Int.add 1 (raise E)
let%expect_test "first" = match x with n -> print_int n | exception E -> print_endline "caught"
let%expect_test "second" = match x with n -> print_int n | exception E -> print_endline "caught"
let%expect_test "unrelated" = print_endline "fine"; [%expect {| fine |}]
|xx};
  [%expect
    {|
    File "a.stag", line 3, characters 18-27:
    3 | let x = Int.add 1 (raise E)
                          ^^^^^^^^^
    Error: Expect test "first" failed: Initialization of A.x raised A.E
    File "a.stag", line 3, characters 18-27:
    3 | let x = Int.add 1 (raise E)
                          ^^^^^^^^^
    Error: Expect test "second" failed: Initialization of A.x raised A.E
    |}]
;;

let%expect_test "exception patterns are looked up as exceptions, not in the \
                 scrutinee's type"
  =
  run
    {|
type t = A | B
let describe f = match f () with A -> "A" | B -> "B" | exception Failure s -> String.append "Failure " s
let main =
  print_endline (describe (fun u -> A));
  print_endline (describe (fun u -> Int.of_string_exn "x"; B))
|};
  [%expect {|
    A
    Failure Int.of_string_exn: invalid integer "x"
    |}];
  run
    {|
exception A
module M = struct type t = A | B end
let describe (f : unit -> M.t) = match f () with A -> "M.A" | B -> "M.B" | exception A -> "exception A"
let main =
  print_endline (describe (fun u -> M.A));
  print_endline (describe (fun u -> raise A))
|};
  [%expect {|
    M.A
    exception A
    |}];
  run
    {|
module N = struct exception A end
type t = A | B
let describe (v : t) = match v with A -> 1 | B -> 2 | exception N.A -> 3
let lexical (v : t) = match v with A -> 1 | B -> 2 | exception A -> 3
|};
  [%expect
    {|
    File "a.stag", line 5, characters 63-64:
    5 | let lexical (v : t) = match v with A -> 1 | B -> 2 | exception A -> 3
                                                                       ^
    Error: This expression has type A.t but an expression was expected of type exn
    |}]
;;

let%expect_test "a constructor's tuple argument is checked as of its type" =
  run
    ~signature:true
    {|
type r = {x : int}
module T = struct type t = A | B end
type u = B | C
let v : option[r * T.t] = Some ({x = 1}, B)
let n = match v with Some ({x = n}, B) -> n | Some _ -> 2 | None -> 3
let main = print_int n
|};
  [%expect
    {|
    module A : sig
      type r = { x : int }
      module T : sig
        type t = A | B
      end
      type u = B | C
      val v : option[r * T.t]
      val n : int
      val main : unit
    end
    1
    |}]
;;

let%expect_test "parameters are distinct" =
  run "let f x x = x";
  [%expect
    {|
    File "a.stag", line 1, characters 8-9:
    1 | let f x x = x
                ^
    Error: The parameter x is bound several times in this function
    |}];
  run "let f = fun x y x -> x";
  [%expect
    {|
    File "a.stag", line 1, characters 16-17:
    1 | let f = fun x y x -> x
                        ^
    Error: The parameter x is bound several times in this function
    |}];
  run "let f (x : int) (x : int) = x";
  [%expect
    {|
    File "a.stag", line 1, characters 17-18:
    1 | let f (x : int) (x : int) = x
                         ^
    Error: The parameter x is bound several times in this function
    |}];
  run
    "let f _ _ = 1\n\
     let g = fun _ _ -> 2\n\
     let main = print_int (Int.add (f 1 2) (g 3 4))";
  [%expect {| 3 |}]
;;

let%expect_test "exception patterns in generated matches" =
  let generated cases =
    run
      (sprintf
         {|
include .~([Stag.Toplevel.let_ "f" ["x"]
  (Stag.Expr.match_
    (Stag.Expr.App (Ptr.make (Stag.Expr.var "Int.of_string_exn", Stag.Expr.var "x")))
    (%s))])
let main = print_int (f "1"); print_int (f "x")
|}
         cases)
  in
  generated
    {|[(Stag.Pattern.Var "n", Stag.Expr.var "n");
       (Stag.Pattern.Exception (Ptr.make (Stag.Pattern.construct "Failure" [Stag.Pattern.Any])),
        Stag.Expr.int 0)]|};
  [%expect {| 10 |}];
  generated
    {|[(Stag.Pattern.construct "Some" [Stag.Pattern.Exception (Ptr.make Stag.Pattern.Any)],
        Stag.Expr.int 1)]|};
  [%expect
    {|
    File "a.stag", lines 2-6, characters 0-29:
    2 | include .~([Stag.Toplevel.let_ "f" ["x"]
    3 |   (Stag.Expr.match_
    4 |     (Stag.Expr.App (Ptr.make (Stag.Expr.var "Int.of_string_exn", Stag.Expr.var "x")))
    5 |     ([(Stag.Pattern.construct "Some" [Stag.Pattern.Exception (Ptr.make Stag.Pattern.Any)],
    6 |         Stag.Expr.int 1)]))])
    Error: In the declaration `let f` generated by this splice: An exception pattern is only allowed at the top of a match case, as in [match e with | exception E -> ...]
    Note: The generated declaration is:
      let f x = match Int.of_string_exn x with Some (exception _) -> 1
    File "a.stag", lines 2-6, characters 0-29:
    2 | include .~([Stag.Toplevel.let_ "f" ["x"]
    3 |   (Stag.Expr.match_
    4 |     (Stag.Expr.App (Ptr.make (Stag.Expr.var "Int.of_string_exn", Stag.Expr.var "x")))
    5 |     ([(Stag.Pattern.construct "Some" [Stag.Pattern.Exception (Ptr.make Stag.Pattern.Any)],
    6 |         Stag.Expr.int 1)]))])
    Error: In the declaration `let f` generated by this splice: This expression has type option[<error>] but an expression was expected of type int
    Note: The generated declaration is:
      let f x = match Int.of_string_exn x with Some (exception _) -> 1
    |}];
  generated
    {|[(Stag.Pattern.Exception (Ptr.make Stag.Pattern.Any), Stag.Expr.int 1)]|};
  [%expect
    {|
    File "a.stag", lines 2-5, characters 0-80:
    2 | include .~([Stag.Toplevel.let_ "f" ["x"]
    3 |   (Stag.Expr.match_
    4 |     (Stag.Expr.App (Ptr.make (Stag.Expr.var "Int.of_string_exn", Stag.Expr.var "x")))
    5 |     ([(Stag.Pattern.Exception (Ptr.make Stag.Pattern.Any), Stag.Expr.int 1)]))])
    Error: In the declaration `let f` generated by this splice: A match must have a case for the value, not only exception cases
    Note: The generated declaration is:
      let f x = match Int.of_string_exn x with exception _ -> 1
    |}]
;;

let%expect_test "an exception catch-all doesn't handle the other tags of a \
                 polymorphic variant"
  =
  run
    ~signature:true
    {|
let f x = match x with `A -> 1 | exception _ -> 2
let g x = match x with `A -> 1 | _ -> 2 | exception _ -> 3
|};
  [%expect
    {|
    module A : sig
      val f : [`A] -> int
      val g : [`A; ..'a] -> int
    end
    |}];
  run
    "let f x = match x with `A -> 1 | exception _ -> 2\n\
     let main = print_int (f `B)";
  [%expect
    {|
    File "a.stag", line 2, characters 24-26:
    2 | let main = print_int (f `B)
                                ^^
    Error: The row {`B : unit; ..'_a} is incompatible with the row {}
    The label B is missing.
    |}]
;;
