open! Import
open Source_helpers

(* Checks [source] after the test prelude, printing its signature with modes or
   the messages of its errors. *)
let modes ?(const_toplevels = false) source =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let typecheck = Typecheck.create ~stamps ~interpreter ~const_toplevels () in
  let prelude = program ~prelude:true "" in
  match Typecheck.typecheck typecheck ~program:(program source) with
  | Ok checked ->
    print_string
      (Signature.to_string_hum
         ~modes:true
         (List.drop checked.signature (List.length prelude)))
  | Error { errors; partial = _ } ->
    List.iter errors ~f:(fun error ->
      print_endline (Typecheck_error.message error))
;;

(* The [*48] functions are specified by POSIX, so give the same numbers
   everywhere. *)
let%expect_test "calling C functions" =
  run
    {|
external labs : int -> int = "labs"
external srand48 : int -> unit = "srand48"
external lrand48 : unit -> int = "lrand48"
let a = labs (Int.sub 0 42)
let r = srand48 7; let a = lrand48 () in let b = lrand48 () in (a, b)
|};
  [%expect
    {|
    val labs : int -> int
    val srand48 : int -> unit
    val lrand48 : unit -> int
    val a : int
    val r : int * int
    -- evaluated at compile time
    -- evaluated
    (labs <fun>)
    (srand48 <fun>)
    (lrand48 <fun>)
    (a 42)
    (r (Tuple 572184555 1464659504))
    |}]
;;

(* [lseek] fails, returning -1, on the bad file descriptor -1. *)
let%expect_test "an external of several arguments is curried" =
  run
    {|
external lseek : int -> int -> int -> int = "lseek"
let bad_fd = lseek (Int.sub 0 1)
let at = bad_fd 0
let failed = (at 0, bad_fd 5 1)
|};
  [%expect
    {|
    val lseek : int -> int -> int -> int
    val bad_fd : int -> int -> int
    val at : int -> int
    val failed : int * int
    -- evaluated at compile time
    -- evaluated
    (lseek <fun>)
    (bad_fd <fun>)
    (at <fun>)
    (failed (Tuple -1 -1))
    |}]
;;

let%expect_test "externals run at compile time, if allowed" =
  run
    ~compile_time_c_calls:Compile_time_c_calls.all
    {|
external labs : int -> int = "labs"
let code = .<labs (Int.sub 0 3)>.
let ran = .~code
let at_compile_time =
  .~(match labs (Int.sub 0 5) with 5 -> .<true>. | _ -> .<false>.)
|};
  [%expect
    {|
    val labs : int -> int
    val code : <int>{..'a}
    val ran : int
    val at_compile_time : bool
    -- evaluated at compile time
    (labs <fun>)
    (code
     (Code
      (App (Var (Persistent (Toplevel labs/4)) : "int -> int")
       (App
        (App (Var (Prim Int.sub) : "int -> int -> int") (Lit (Int 0) : int) :
         "int -> int")
        (Lit (Int 3) : int) : int)
       : int)))
    -- evaluated
    (labs <fun>)
    (code
     (Code
      (App (Var (Persistent (Toplevel labs/4)) : "int -> int")
       (App
        (App (Var (Prim Int.sub) : "int -> int -> int") (Lit (Int 0) : int) :
         "int -> int")
        (Lit (Int 3) : int) : int)
       : int)))
    (ran 3)
    (at_compile_time true)
    |}]
;;

let%expect_test "a missing symbol errs when called" =
  run
    {|
external missing : int -> int = "stag_no_such_symbol"
let partial = missing
let called = missing 1
|};
  [%expect
    {|
    val missing : int -> int
    val partial : int -> int
    val called : int
    -- evaluated at compile time
    -- evaluated
    (missing <fun>)
    (partial <fun>)
    ("The external stag_no_such_symbol could not be called: there is no such C symbol"
     @ t.stag:4:13-22)
    |}]
;;

let%expect_test "signatures with no C signature" =
  modes
    {|
type r = { x : int }
external a : int = "a"
external b : list[int] -> int = "b"
external c : int -> string = "c"
external d : (int -> int) -> int = "d"
external e : int -> unit -> int = "e"
external f : r -> int = "f"
external g : int -> int * int = "g"
external h : 'a -> int = "h"
external i : <int>{} -> int = "i"
external k : unit = "k"
external s : string -> int = "s"
external p : ptr[int] -> int = "p"
external q : int -> ptr[int] = "q"
|};
  [%expect
    {|
    The external a has no C signature: it must be a function
    The external b has no C signature: C functions can't take arguments of type list[int], only int, C integer types (C.int etc.), bool, char and string
    The external c has no C signature: string would be returned by C as a pointer, which is not supported yet
    The external d has no C signature: C functions can't take arguments of type int -> int, only int, C integer types (C.int etc.), bool, char and string
    The external e has no C signature: unit can only be the only argument, of a C function of no arguments
    The external f has no C signature: C functions can't take arguments of type r, only int, C integer types (C.int etc.), bool, char and string
    The external g has no C signature: C functions can't return values of type int * int, only int, C integer types (C.int etc.), bool, char and unit
    Unbound type variable 'a
    The external i has no C signature: C functions can't take arguments of type <int>{}, only int, C integer types (C.int etc.), bool, char and string
    The external k has no C signature: it must be a function
    The external p has no C signature: ptr[int] would be passed to C as a pointer, which is not supported yet
    The external q has no C signature: ptr[int] would be returned by C as a pointer, which is not supported yet
    |}]
;;

let%expect_test "every C type" =
  check
    {|
type t = int
external all : int -> bool -> char -> unit = "all"
external aliased : t -> t = "aliased"
external bools : bool -> bool = "bools"
external chars : char -> char = "chars"
external nothing : unit -> unit = "nothing"
|};
  [%expect
    {|
    type t = int
    val all : int -> bool -> char -> unit
    val aliased : int -> int
    val bools : bool -> bool
    val chars : char -> char
    val nothing : unit -> unit
    |}]
;;

(* Unwritten modes are a declaration's defaults, and written ones are trusted. *)
let%expect_test "an external's modes are as declared" =
  modes
    {|
external plain : int -> int = "labs"
external portable : int @ portable -> int @ portable = "labs"
let p @ portable = portable 1
|};
  [%expect
    {|
    val plain : int -> int
    val portable : int @ portable -> int @ portable
    val p : int
    |}];
  (* Its result is declared nonportable, but an int crosses modes. *)
  modes {|
external plain : int -> int = "labs"
let p @ portable = plain 1
|};
  [%expect {|
    val plain : int -> int
    val p : int
    |}];
  modes
    {|
external plain : int -> int = "labs"
let f : int -> int @ portable = plain
|};
  [%expect
    {|
    This expression has type int -> int but an expression was expected of type int -> int
    Their modes differ, in ways types don't print: This is nonportable, because a function type fixes it, as a function is used at a type with other modes, but must be at most portable, because it is annotated portable (at t.stag:3:21-29)
    |}];
  modes {|
external results : int @ local -> int @ external = "labs"
|};
  [%expect {| val results : int @ local -> int @ external |}];
  modes {|
external const_result : int -> int @ const = "labs"
|};
  [%expect {| This mode is invalid: the result of a function is never const |}];
  modes {|
external bad : int @ 'm -> int = "labs"
|};
  [%expect
    {| The mode variable 'm is not allowed in a declaration unless it is a parameter declared ['m : mode]: its arrows have fixed modes |}]
;;

let%expect_test "externals are const toplevels" =
  modes
    ~const_toplevels:true
    {|
external labs : int -> int = "labs"
let f = labs
let code = .<labs 1>.
|};
  [%expect
    {|
    val labs : int -> int
    val f : int -> int
    val code : <int>{..'a}
    |}]
;;

(* [labs] of the least int is one more than the greatest. *)
let%expect_test "a C result out of the range of int errs" =
  run
    {|
external labs : int -> int = "labs"
let least = Int.sub (Int.sub 0 4611686018427387903) 1
let in_range = labs (Int.add least 1)
let out_of_range = labs least
|};
  [%expect
    {|
    val labs : int -> int
    val least : int
    val in_range : int
    val out_of_range : int
    -- evaluated at compile time
    -- evaluated
    (labs <fun>)
    (least -4611686018427387904)
    (in_range 4611686018427387903)
    ("The external labs could not be called: it returned 4611686018427387904, which is out of the range of int (63 bits)"
     @ t.stag:5:19-29)
    |}]
;;

let%expect_test "symbols must be non-empty, with no NUL character" =
  modes
    {|
external empty : int -> int = ""
external nul : int -> int = "labs\000junk"
external fine : int -> int = "labs"
|};
  [%expect
    {|
    The C symbol "" is invalid: it must be non-empty, with no NUL character
    The C symbol "labs\000junk" is invalid: it must be non-empty, with no NUL character
    |}]
;;

let%expect_test "an external's own mode is unconstrained" =
  modes
    ~const_toplevels:true
    {|
external labs : int -> int = "labs"
let k @ const = labs
let p @ portable = labs
|};
  [%expect
    {|
    val labs : int -> int
    val k : int -> int
    val p : int -> int
    |}]
;;

(* Outside a project, no library allows C calls at compile time. Calls at
   runtime are allowed, including those of code generated at compile time. *)
let%expect_test "C calls at compile time are denied by default" =
  run
    {|
external labs : int -> int = "labs"
let at_runtime = labs (Int.sub 0 1)
let code = .<labs (Int.sub 0 3)>.
let ran = .~code
|};
  [%expect
    {|
    val labs : int -> int
    val at_runtime : int
    val code : <int>{..'a}
    val ran : int
    -- evaluated at compile time
    (code
     (Code
      (App (Var (Persistent (Toplevel labs/4)) : "int -> int")
       (App
        (App (Var (Prim Int.sub) : "int -> int -> int") (Lit (Int 0) : int) :
         "int -> int")
        (Lit (Int 3) : int) : int)
       : int)))
    -- evaluated
    (labs <fun>)
    (at_runtime 1)
    (code
     (Code
      (App (Var (Persistent (Toplevel labs/4)) : "int -> int")
       (App
        (App (Var (Prim Int.sub) : "int -> int -> int") (Lit (Int 0) : int) :
         "int -> int")
        (Lit (Int 3) : int) : int)
       : int)))
    (ran 3)
    |}];
  run
    {|
external labs : int -> int = "labs"
let at_compile_time = .~(match labs 1 with 1 -> .<true>. | _ -> .<false>.)
|};
  [%expect
    {|
    ((Splice_evaluation_failed
      (code
       (Match
        (scrutinee
         (App (Var (Bound (Toplevel labs/4)) : "int -> int") (Lit (Int 1) : int)
          : int))
        (cases
         (((Lit (Int 1) : int) (Quote (Lit (Bool true) : bool) : <bool>{}))
          ((Any : int) (Quote (Lit (Bool false) : bool) : <bool>{}))))
        : <bool>{}))
      (error
       ("The external labs can't be called at compile time: C calls at compile time are only allowed in libraries that allow them"
        @ t.stag:3:31-37)))
     @ t.stag:3:22-74)
    |}]
;;

let%expect_test "the prelude's and core's externals may always be called at \
                 compile time"
  =
  let policies =
    [ "none", Compile_time_c_calls.none
    ; ( "denying Foo"
      , Compile_time_c_calls.of_libraries
          (String.Map.singleton
             "Foo"
             { Compile_time_c_calls.Library.name = "foo"; allowed = false }) )
    ]
  in
  List.iter policies ~f:(fun (name, t) ->
    List.iter
      [ Prelude_path.canonical "labs"
      ; Prelude_path.canonical "Core.Int.labs"
      ; "Foo.labs"
      ; "Bar.labs"
      ]
      ~f:(fun external_ ->
        let policy = Compile_time_c_calls.policy t ~external_ in
        print_s
          [%message name external_ (policy : Compile_time_c_calls.Policy.t)]));
  [%expect
    {|
    (none prelude.labs (policy Allowed))
    (none prelude.Core.Int.labs (policy Allowed))
    (none Foo.labs (policy (Denied (external_ Foo.labs) (library ()))))
    (none Bar.labs (policy (Denied (external_ Bar.labs) (library ()))))
    ("denying Foo" prelude.labs (policy Allowed))
    ("denying Foo" prelude.Core.Int.labs (policy Allowed))
    ("denying Foo" Foo.labs
     (policy (Denied (external_ Foo.labs) (library (foo)))))
    ("denying Foo" Bar.labs (policy (Denied (external_ Bar.labs) (library ()))))
    |}]
;;

(* C's [abs] returns an [int]: read as an [int64_t], its sign would be lost, as
   [abs] of the least [int] is itself. *)
let%expect_test "C integer types are converted to and from int" =
  run
    {|
external abs : C.int -> C.int = "abs"
external atoi : string -> C.int = "atoi"
let least = abs (Int.sub (Int.sub 0 2147483647) 1)
let minus_one = atoi "-1"
external labs : C.long -> C.long = "labs"
external to_upper : C.int -> C.int = "toupper"
external narrow : C.int8 -> C.uint8 = "abs"
let negative = abs (Int.sub 0 7)
let small = labs (Int.sub 0 7)
let upper = to_upper 97
let widened = narrow (Int.sub 0 1)
|};
  [%expect
    {|
    val abs : int -> int
    val atoi : string -> int
    val least : int
    val minus_one : int
    val labs : int -> int
    val to_upper : int -> int
    val narrow : int -> int
    val negative : int
    val small : int
    val upper : int
    val widened : int
    -- evaluated at compile time
    -- evaluated
    (abs <fun>)
    (atoi <fun>)
    (least -2147483648)
    (minus_one -1)
    (labs <fun>)
    (to_upper <fun>)
    (narrow <fun>)
    (negative 7)
    (small 7)
    (upper 65)
    (widened 1)
    |}]
;;

(* Each case runs on its own, as an error stops the evaluation. *)
let%expect_test "C integer arguments out of range err" =
  List.iter
    [ "let max_int32 = abs 2147483647"
    ; "let too_big = abs 2147483648"
    ; "let too_small = abs (Int.sub (Int.sub 0 2147483647) 2)"
    ; "let negative_unsigned = byte (Int.sub 0 1)"
    ; "let too_big_byte = byte 256"
    ]
    ~f:(fun line ->
      run
        ({|
external abs : C.int -> C.int = "abs"
external byte : C.uint8 -> C.uint8 = "abs"
|}
         ^ line));
  [%expect
    {|
    val abs : int -> int
    val byte : int -> int
    val max_int32 : int
    -- evaluated at compile time
    -- evaluated
    (abs <fun>)
    (byte <fun>)
    (max_int32 2147483647)
    val abs : int -> int
    val byte : int -> int
    val too_big : int
    -- evaluated at compile time
    -- evaluated
    (abs <fun>)
    (byte <fun>)
    ("The external abs could not be called: the argument 2147483648 is out of the range of C.int"
     @ t.stag:4:14-28)
    val abs : int -> int
    val byte : int -> int
    val too_small : int
    -- evaluated at compile time
    -- evaluated
    (abs <fun>)
    (byte <fun>)
    ("The external abs could not be called: the argument -2147483649 is out of the range of C.int"
     @ t.stag:4:16-54)
    val abs : int -> int
    val byte : int -> int
    val negative_unsigned : int
    -- evaluated at compile time
    -- evaluated
    (abs <fun>)
    (byte <fun>)
    ("The external abs could not be called: the argument -1 is out of the range of C.uint8"
     @ t.stag:4:24-42)
    val abs : int -> int
    val byte : int -> int
    val too_big_byte : int
    -- evaluated at compile time
    -- evaluated
    (abs <fun>)
    (byte <fun>)
    ("The external abs could not be called: the argument 256 is out of the range of C.uint8"
     @ t.stag:4:19-27)
    |}]
;;

(* [atoll] returns a [long long], which may be out of the range of [int]. *)
let%expect_test "C results out of the range of int err" =
  List.iter
    [ {|let max = atoll "4611686018427387903"|}
    ; {|let too_big = atoll "4611686018427387904"|}
    ; {|let min = atoll "-4611686018427387904"|}
    ; {|let too_small = atoll "-4611686018427387905"|}
    ]
    ~f:(fun line ->
      run ({|
external atoll : string -> C.int64 = "atoll"
|} ^ line));
  [%expect
    {|
    val atoll : string -> int
    val max : int
    -- evaluated at compile time
    -- evaluated
    (atoll <fun>)
    (max 4611686018427387903)
    val atoll : string -> int
    val too_big : int
    -- evaluated at compile time
    -- evaluated
    (atoll <fun>)
    ("The external atoll could not be called: it returned 4611686018427387904, which is out of the range of int (63 bits)"
     @ t.stag:3:14-41)
    val atoll : string -> int
    val min : int
    -- evaluated at compile time
    -- evaluated
    (atoll <fun>)
    (min -4611686018427387904)
    val atoll : string -> int
    val too_small : int
    -- evaluated at compile time
    -- evaluated
    (atoll <fun>)
    ("The external atoll could not be called: it returned -4611686018427387905, which is out of the range of int (63 bits)"
     @ t.stag:3:16-44)
    |}]
;;

let%expect_test "strings are passed to C as NUL-terminated copies" =
  run
    {|
external strlen : string -> C.size_t = "strlen"
external atoi : string -> C.int = "atoi"
external strcmp : string -> string -> C.int = "strcmp"
let len = strlen "hello"
let empty = strlen ""
let parsed = atoi "-42"
let compared = (strcmp "a" "b", strcmp "b" "b")
|};
  [%expect
    {|
    val strlen : string -> int
    val atoi : string -> int
    val strcmp : string -> string -> int
    val len : int
    val empty : int
    val parsed : int
    val compared : int * int
    -- evaluated at compile time
    -- evaluated
    (strlen <fun>)
    (atoi <fun>)
    (strcmp <fun>)
    (len 5)
    (empty 0)
    (parsed -42)
    (compared (Tuple -1 0))
    |}];
  run
    {|
external strlen : string -> C.size_t = "strlen"
let nul = strlen "a\000b"
|};
  [%expect
    {|
    val strlen : string -> int
    val nul : int
    -- evaluated at compile time
    -- evaluated
    (strlen <fun>)
    ("The external strlen could not be called: the string argument \"a\\000b\" has a NUL character, so can't be passed to C"
     @ t.stag:3:10-25)
    |}]
;;

let%expect_test "an external's C integer types and strings, with modes" =
  modes
    {|
external strlen : string @ local -> C.size_t @ portable = "strlen"
let f (s @ local) = strlen s
external r : int -> C.int8 = "r"
|};
  [%expect
    {|
    val strlen : string @ local -> int @ portable
    val f : string @ 'm0 -> int @ _ with local <= 'm0 and 'm0 <= gc
    val r : int -> int
    |}];
  modes {|external c : C.int -> string = "c"|};
  [%expect
    {| The external c has no C signature: string would be returned by C as a pointer, which is not supported yet |}]
;;
