open! Import

let run ?(eval = true) src =
  match Parser.program ~file:"t.stag" src with
  | Error error -> print_endline (Parser.Error.to_string error)
  | Ok program ->
    (match Driver.check ~limits:Source_helpers.limits program with
     | Error error ->
       List.iter (error : Typecheck.Failed.t).errors ~f:(fun error ->
         print_s [%sexp (error : Typecheck_error.t)])
     | Ok driver ->
       print_string (Signature.to_string_hum (Driver.checked driver).signature);
       if eval
       then (
         print_endline "---";
         match Driver.eval ~limits:Source_helpers.limits driver with
         | Ok () -> ()
         | Error error -> print_s [%sexp "Runtime error", (error : Error.t)]))
;;

let%expect_test "print" =
  run
    {|
let main1 = print_string "a"; print_char 'b'; print_int 42; print_endline "!"
let main2 = print_int (Int.neg 7); print_endline ""
|};
  [%expect
    {|
    val main1 : unit
    val main2 : unit
    ---
    ab42!
    -7
    |}]
;;

let%expect_test "int" =
  run
    {|
let show x = print_endline (Int.to_string x)
let show_bool b = match b with true -> print_endline "true" | false -> print_endline "false"
let main3 =
  show (Int.add 3 4); show (Int.sub 3 4); show (Int.mul 3 4);
  show (Int.div 7 2); show (Int.div -7 2); show (Int.rem -7 2);
  show (Int.compare 1 2); show (Int.compare 2 2); show (Int.compare 3 2);
  show_bool (Int.equal 1 1); show_bool (Int.lt 1 2); show_bool (Int.le 2 2);
  show_bool (Int.gt 1 2); show_bool (Int.ge 1 2);
  show (Int.of_string_exn "-12"); show (Int.of_string_exn "4611686018427387903");
  show (Int.add 4611686018427387903 1)
|};
  [%expect
    {|
    val show : int -> unit
    val show_bool : bool -> unit
    val main3 : unit
    ---
    7
    -1
    12
    3
    -3
    -1
    -1
    0
    1
    true
    true
    true
    false
    false
    -12
    4611686018427387903
    -4611686018427387904
    |}]
;;

let%expect_test "string, char, bool" =
  run
    ~eval:true
    {|
let show_bool b = match b with true -> print_endline "true" | false -> print_endline "false"
let main4 =
  print_endline (String.append "ab" "cd");
  print_int (String.length "hello"); print_endline "";
  print_char (String.get "hello" 1); print_endline "";
  print_endline (String.sub "hello" 1 3);
  show_bool (String.equal "a" "a");
  print_int (String.compare "a" "b"); print_endline "";
  show_bool (Char.equal 'a' 'b');
  print_int (Char.code 'a'); print_endline "";
  print_char (Char.of_code 98); print_endline "";
  print_endline (Char.to_string 'c');
  show_bool (Bool.not true);
  show_bool (Bool.equal false false)
|};
  [%expect
    {|
    val show_bool : bool -> unit
    val main4 : unit
    ---
    abcd
    5
    e
    ell
    true
    -1
    false
    97
    b
    c
    false
    true
    |}]
;;

let%expect_test "types" =
  run
    ~eval:false
    {|
let add = Int.add
let incr = Int.add 1
let sub = String.sub
|};
  [%expect
    {|
    val add : int -> int -> int
    val incr : int -> int
    val sub : string -> int -> int -> string
    |}];
  run ~eval:false {| let x = Int.add true |};
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:1:17-21)
    |}]
;;

let%expect_test "runtime errors are located at the application" =
  let errors =
    [ "Int.div 1 0"
    ; "Int.rem 1 0"
    ; "String.get \"abc\" 3"
    ; "String.get \"abc\" -1"
    ; "String.sub \"abc\" 2 2"
    ; "Int.of_string_exn \"12a\""
    ; "Int.of_string_exn \"0x12\""
    ; "Int.of_string_exn \"\""
    ; "Char.of_code 256"
    ]
  in
  List.iter errors ~f:(fun e -> run ~eval:true (sprintf "let x = %s" e));
  [%expect
    {|
    val x : int
    ---
    ("Runtime error" ("Uncaught exception Division_by_zero" @ t.stag:1:8-19))
    val x : int
    ---
    ("Runtime error" ("Uncaught exception Division_by_zero" @ t.stag:1:8-19))
    val x : char
    ---
    ("Runtime error"
     ("Uncaught exception Invalid_argument \"String.get: index 3 out of bounds of a string of length 3\""
      @ t.stag:1:8-26))
    val x : char
    ---
    ("Runtime error"
     ("Uncaught exception Invalid_argument \"String.get: index -1 out of bounds of a string of length 3\""
      @ t.stag:1:8-27))
    val x : string
    ---
    ("Runtime error"
     ("Uncaught exception Invalid_argument \"String.sub: invalid substring (position 2, length 2) of a string of length 3\""
      @ t.stag:1:8-28))
    val x : int
    ---
    ("Runtime error"
     ("Uncaught exception Failure \"Int.of_string_exn: invalid integer \\\"12a\\\"\""
      @ t.stag:1:8-31))
    val x : int
    ---
    ("Runtime error"
     ("Uncaught exception Failure \"Int.of_string_exn: invalid integer \\\"0x12\\\"\""
      @ t.stag:1:8-32))
    val x : int
    ---
    ("Runtime error"
     ("Uncaught exception Failure \"Int.of_string_exn: invalid integer \\\"\\\"\""
      @ t.stag:1:8-28))
    val x : char
    ---
    ("Runtime error"
     ("Uncaught exception Invalid_argument \"Char.of_code: invalid character code 256\""
      @ t.stag:1:8-24))
    |}]
;;

let%expect_test "partial application" =
  run {|
let div = Int.div 10
let main5 = print_int (div 3)
let boom = div 0
|};
  [%expect
    {|
    val div : int -> int
    val main5 : unit
    val boom : int
    ---
    3("Runtime error" ("Uncaught exception Division_by_zero" @ t.stag:4:11-16))
    |}]
;;

let%expect_test "at compile time and in quotes" =
  run
    {|
let x = .~(print_endline "compiling"; .<Int.add 1 2>.)
let f n = .<print_int .~n>.
let main6 = .~(f .<Int.mul x 10>.)
let g = .<Int.div 1 0>.
let main9 = print_int (Int.add x 1)
|};
  [%expect
    {|
    compiling
    val x : int
    val f : <int>{..'a} -> <unit>{..'b} where ..'a <= {..'b}
    val main6 : unit
    val g : <int>{..'a}
    val main9 : unit
    ---
    304
    |}];
  run {| let h = .~(Int.div 1 0; .<0>.) |};
  [%expect
    {|
    ((Splice_evaluation_failed
      (code
       (Seq
        (App
         (App (Var (Prim Int.div) : "int -> int -> int") (Lit (Int 1) : int) :
          "int -> int")
         (Lit (Int 0) : int) : int)
        (Quote (Lit (Int 0) : int) : <int>{}) : <int>{}))
      (error ("Uncaught exception Division_by_zero" @ t.stag:1:12-23)))
     @ t.stag:1:9-31)
    |}]
;;

let%expect_test "shadowing" =
  run
    {|
module Int = struct
  let add x y = x
end
let a = Int.add 1 2
let print_int x = print_endline "shadowed"
let main7 = print_int a
let main8 = let print_endline s = print_string "local" in print_endline "x"
|};
  [%expect
    {|
    module Int : sig
      val add : 'a -> 'b -> 'a
    end
    val a : int
    val print_int : 'a -> unit
    val main7 : unit
    val main8 : unit
    ---
    shadowed
    local
    |}];
  run
    ~eval:false
    {|
module Int = struct
  let add x y = x
end
let a = Int.sub 1 2
|};
  [%expect {| ((Not_in_scope Int.sub) @ t.stag:5:8-15) |}];
  run ~eval:false {| let a = Int.foo |};
  [%expect {| ((Not_in_scope Int.foo) @ t.stag:1:9-16) |}];
  run ~eval:false {| let a = Foo.foo |};
  [%expect {| ((Unbound_module Foo) @ t.stag:1:9-12) |}];
  run
    ~eval:false
    {|
module M = struct
  module String = struct let length = 0 end
  let n = String.length
end
let m = String.length
|};
  [%expect
    {|
    module M : sig
      module String : sig
        val length : int
      end
      val n : int
    end
    val m : string -> int
    |}]
;;

let%expect_test "output goes to the sink, at compile time and runtime" =
  let buffer = Buffer.create 16 in
  let program =
    Parser.program
      ~file:"t.stag"
      {|
let x = .~(print_string "compile "; .<1>.)
let y = print_string "run"
|}
    |> Result.ok
    |> Option.value_exn
  in
  match Driver.check ~output:(Buffer.add_string buffer) program with
  | Error _ -> print_endline "check failed"
  | Ok driver ->
    print_s [%sexp "after check", (Buffer.contents buffer : string)];
    Driver.eval driver |> ok_exn;
    print_s [%sexp "after eval", (Buffer.contents buffer : string)];
    [%expect
      {|
      ("after check" "compile ")
      ("after eval" "compile run")
      |}]
;;

let%expect_test "only the prelude names prims as [Prim.*]" =
  run ~eval:false {| let a = Prim.String.length |};
  [%expect {| ((Unbound_module Prim) @ t.stag:1:9-13) |}];
  run
    {|
module Prim = struct
  module String = struct let length s = 7 end
end
module String = struct let length s = 8 end
let a = Prim.String.length "ab"
let b = String.length "ab"
let c = Core.String.length "ab"
let main = print_int a; print_int b; print_int c
|};
  [%expect
    {|
    module Prim : sig
      module String : sig
        val length : 'a -> int
      end
    end
    module String : sig
      val length : 'a -> int
    end
    val a : int
    val b : int
    val c : int
    val main : unit
    ---
    782
    |}]
;;
