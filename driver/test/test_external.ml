open! Import
open Source_helpers

let%expect_test "calling C functions" =
  run
    {|
external labs : int -> int = "labs"
external strlen : string -> int = "strlen"
external atol : string -> int = "atol"
external srand48 : int -> unit = "srand48"
external lrand48 : unit -> int = "lrand48"
let a = labs (Int.sub 0 42)
let n = strlen "hello"
let m = atol "1234"
let r = srand48 7; lrand48 ()
|};
  [%expect
    {|
    val labs : int -> int
    val strlen : string -> int
    val atol : string -> int
    val srand48 : int -> unit
    val lrand48 : unit -> int
    val a : int
    val n : int
    val m : int
    val r : int
    -- evaluated at compile time
    -- evaluated
    (labs <fun>)
    (strlen <fun>)
    (atol <fun>)
    (srand48 <fun>)
    (lrand48 <fun>)
    (a 42)
    (n 5)
    (m 1234)
    (r 572184555)
    |}]
;;
