open! Core
open Stag_build

let check ?(eval = true) files ~target =
  Helpers.with_dir (("stag-project", "") :: files) ~f:(fun root ->
    match Project.load ~root with
    | Error diagnostics -> Helpers.print_diagnostics ~root diagnostics
    | Ok project ->
      let target = Option.value_exn (Project.find project target) in
      (match Program.check project target with
       | Error diagnostics -> Helpers.print_diagnostics ~root diagnostics
       | Ok driver ->
         print_string
           (Stag.Signature.to_string_hum (Stag.Driver.checked driver).signature);
         if eval
         then (
           print_endline "---";
           match Stag.Driver.eval driver with
           | Ok () -> ()
           | Error error ->
             Helpers.print_diagnostics
               ~root
               [ Stag.Diagnostic.of_runtime_error error ])))
;;

let project =
  [ "util/stag", "(library (name util))"
  ; ( "util/b.stag"
    , {|let y = print_endline "Util.B"; 1
type u = int
let f x = A.g x|}
    )
  ; ( "util/a.stag"
    , {|let x = print_endline "Util.A"; B.y
type t = B.u
let g (n : t) = Int.add n x|}
    )
  ; "lib/stag", "(library (name lib) (libraries util))"
  ; ( "lib/lib.stag"
    , {|let p = print_endline "Lib.Lib"
let z = Util.B.f 2|}
    )
  ; "bin/stag", "(executable (name main) (libraries lib))"
  ; ( "bin/main.stag"
    , {|let main = print_endline "Main"; print_int (Lib.Lib.z); print_endline ""
let u = Util.A.x|}
    )
  ; "bin/helper.stag", {|let p = print_endline "Helper"|}
  ]
;;

let%expect_test "libraries are wrapped; executables are not" =
  check project ~target:"main";
  [%expect
    {|
    module Util : sig
      module A : sig
        val x : int
        type t = int
        val g : int -> int
      end
      module B : sig
        val y : int
        type u = int
        val f : int -> int
      end
    end
    module Lib : sig
      module Lib : sig
        val p : unit
        val z : int
      end
    end
    module Helper : sig
      val p : unit
    end
    module Main : sig
      val main : unit
      val u : int
    end
    ---
    Util.A
    Util.B
    Lib.Lib
    Helper
    Main
    3
    |}]
;;

let%expect_test "a library is checked with those it depends on" =
  check project ~target:"util";
  check project ~target:"lib" ~eval:false;
  [%expect
    {|
    module Util : sig
      module A : sig
        val x : int
        type t = int
        val g : int -> int
      end
      module B : sig
        val y : int
        type u = int
        val f : int -> int
      end
    end
    ---
    Util.A
    Util.B
    module Util : sig
      module A : sig
        val x : int
        type t = int
        val g : int -> int
      end
      module B : sig
        val y : int
        type u = int
        val f : int -> int
      end
    end
    module Lib : sig
      module Lib : sig
        val p : unit
        val z : int
      end
    end
    |}]
;;

let%expect_test "errors" =
  check
    [ "a/stag", "(library (name a))"
    ; "a/x.stag", "let x = Y.nope"
    ; "a/y.stag", "let y = 1"
    ]
    ~target:"a";
  check
    [ "a/stag", "(library (name a))"
    ; "a/x.stag", "let x = "
    ; "a/y.stag", "let y = )"
    ; "b/stag", "(executable (name b) (libraries a))"
    ; "b/b.stag", "let x = A.X.x"
    ]
    ~target:"b";
  [%expect
    {|
    File "a/x.stag", line 1, characters 8-14:
    1 | let x = Y.nope
                ^^^^^^
    Error: Unbound value Y.nope
    File "a/x.stag", line 1, characters 8-8:
    1 | let x =
                ^
    Error: Expected an expression, found end of file
    File "a/y.stag", line 1, characters 8-9:
    1 | let y = )
                ^
    Error: Expected an expression, found `)`
    |}]
;;
