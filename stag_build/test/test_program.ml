open! Import
open Stag_build

let check ?(eval = true) files ~target =
  Helpers.with_dir (("stag-project", "") :: files) ~f:(fun root ->
    match Project.load ~root () with
    | Error diagnostics -> Helpers.print_diagnostics ~root diagnostics
    | Ok project ->
      let target = Option.value_exn (Project.find project target) in
      (match
         Program.check ~limits:Limits.unlimited project target
         |> Result.map_error ~f:List.return
         |> Result.bind ~f:(fun (_, checked) ->
           Pipeline.Checked.to_result checked)
       with
       | Error diagnostics -> Helpers.print_diagnostics ~root diagnostics
       | Ok driver ->
         print_string
           (Signature.to_string_hum (Driver.checked driver).signature);
         if eval
         then (
           print_endline "---";
           match Driver.eval driver with
           | Ok () -> ()
           | Error error ->
             Helpers.print_diagnostics
               ~root
               [ Diagnostic.of_runtime_error error ])))
;;

let project =
  [ "util/stag", "(library (name util))"
  ; ( "util/b.stag"
    , {|let y = print_endline "Util.B"; 1
type u = int
let f x = A.g x|} )
  ; ( "util/a.stag"
    , {|let x = print_endline "Util.A"; B.y
type t = B.u
let g (n : t) = Int.add n x|}
    )
  ; "lib/stag", "(library (name lib) (libraries util))"
  ; "lib/lib.stag", {|let p = print_endline "Lib.Lib"
let z = Util.B.f 2|}
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

let%expect_test "every target sees Core, and a library named core shadows it" =
  let uses_core =
    {|let main = print_endline (Core.Sexp.to_string (Core.Sexp.sexp_of_list Core.Sexp.sexp_of_int (Core.List.init 3 Core.Int.succ)))|}
  in
  check
    [ "a/stag", "(library (name a))"
    ; "a/x.stag", "let words = Core.String.split_on_char ' ' \"in a library\""
    ; "b/stag", "(executable (name b) (libraries a))"
    ; "b/b.stag", uses_core
    ; "b/c.stag", "let n = Core.List.length A.X.words"
    ]
    ~target:"b";
  check
    [ "core/stag", "(library (name core))"
    ; "core/list.stag", "let init n f = Nil"
    ; "b/stag", "(executable (name b) (libraries core))"
    ; "b/b.stag", "let n = Core.List.init 3 Core.Int.succ"
    ]
    ~target:"b";
  [%expect
    {|
    module A : sig
      module X : sig
        val words : list[string]
      end
    end
    module B : sig
      val main : unit
    end
    module C : sig
      val n : int
    end
    ---
    (1 2 3)
    File "b/b.stag", line 1, characters 25-33:
    1 | let n = Core.List.init 3 Core.Int.succ
                                 ^^^^^^^^
    Error: Unbound module Core.Int (the prelude's module Core is shadowed by the program's Core)
    File "core/stag", line 1, characters 15-19:
    1 | (library (name core))
                       ^^^^
    Note: The program's module Core is here
    |}]
;;

(* A library named core shadows Core program-wide, so derivers must quote the
   prelude's Core elsewhere: refs only help where the splice site shadows it. *)
let%expect_test "a library named core shadows Core for derivers too" =
  check
    [ "derive/stag", "(library (name derive))"
    ; ( "derive/sexp.stag"
      , {|let of_int name =
  Cons (Ptr.make ( (Stag.Toplevel.let_ name (Cons (Ptr.make ( ("x", Nil))))
    (Stag.Code.erase .< Core.Sexp.to_string (Core.Sexp.sexp_of_list Core.Sexp.sexp_of_int (Core.List.init x Core.Int.succ)) >.), Nil)))|}
      )
    ; "core/stag", "(library (name core))"
    ; "core/list.stag", "let init n f = Nil"
    ; "b/stag", "(executable (name b) (libraries derive core))"
    ; ( "b/b.stag"
      , {|include .~(Derive.Sexp.of_int "show")
let main = print_endline (show 3)|}
      )
    ]
    ~target:"b";
  [%expect
    {|
    File "derive/sexp.stag", line 3, characters 24-33:
    3 |     (Stag.Code.erase .< Core.Sexp.to_string (Core.Sexp.sexp_of_list Core.Sexp.sexp_of_int (Core.List.init x Core.Int.succ)) >.), Nil)))
                                ^^^^^^^^^
    Error: Unbound module Core.Sexp (the prelude's module Core is shadowed by the program's Core)
    File "core/stag", line 1, characters 15-19:
    1 | (library (name core))
                       ^^^^
    Note: The program's module Core is here
    File "derive/sexp.stag", line 3, characters 45-54:
    3 |     (Stag.Code.erase .< Core.Sexp.to_string (Core.Sexp.sexp_of_list Core.Sexp.sexp_of_int (Core.List.init x Core.Int.succ)) >.), Nil)))
                                                     ^^^^^^^^^
    Error: Unbound module Core.Sexp (the prelude's module Core is shadowed by the program's Core)
    File "core/stag", line 1, characters 15-19:
    1 | (library (name core))
                       ^^^^
    Note: The program's module Core is here
    File "derive/sexp.stag", line 3, characters 68-77:
    3 |     (Stag.Code.erase .< Core.Sexp.to_string (Core.Sexp.sexp_of_list Core.Sexp.sexp_of_int (Core.List.init x Core.Int.succ)) >.), Nil)))
                                                                            ^^^^^^^^^
    Error: Unbound module Core.Sexp (the prelude's module Core is shadowed by the program's Core)
    File "core/stag", line 1, characters 15-19:
    1 | (library (name core))
                       ^^^^
    Note: The program's module Core is here
    File "derive/sexp.stag", line 3, characters 108-116:
    3 |     (Stag.Code.erase .< Core.Sexp.to_string (Core.Sexp.sexp_of_list Core.Sexp.sexp_of_int (Core.List.init x Core.Int.succ)) >.), Nil)))
                                                                                                                    ^^^^^^^^
    Error: Unbound module Core.Int (the prelude's module Core is shadowed by the program's Core)
    File "core/stag", line 1, characters 15-19:
    1 | (library (name core))
                       ^^^^
    Note: The program's module Core is here
    |}]
;;

(* The deriver refers to its library's modules relatively; its generated code
   refers to them by ref, printed as root paths, so the program's own [Helper]
   does not capture them. *)
let%expect_test "a deriver's relative refs survive root modules of their names" =
  check
    [ "gen/stag", "(library (name gen))"
    ; "gen/helper.stag", {|let describe n = Core.Int.to_string n|}
    ; ( "gen/derive.stag"
      , {|let show name =
  Cons (Ptr.make ( (Stag.Toplevel.let_ name (Cons (Ptr.make ( ("x", Nil))))
    (Stag.Code.erase .< Helper.describe x >.), Nil)))|}
      )
    ; "b/stag", "(executable (name b) (libraries gen))"
    ; "b/helper.stag", {|let describe n = "the program's helper"|}
    ; ( "b/b.stag"
      , {|include .~(Gen.Derive.show "show")
let main = print_endline (show 3)|}
      )
    ]
    ~target:"b";
  check
    [ "gen/stag", "(library (name gen))"
    ; "gen/helper.stag", {|let describe n = n|}
    ; ( "gen/derive.stag"
      , {|let bad name =
  Cons (Ptr.make ( (Stag.Toplevel.let_ name Nil
    (Stag.Expr.App (Ptr.make (Stag.Expr.int 0, Stag.Code.erase .< Helper.describe >.))), Nil)))|}
      )
    ; "b/stag", "(executable (name b) (libraries gen))"
    ; "b/helper.stag", {|let describe n = n|}
    ; "b/b.stag", {|include .~(Gen.Derive.bad "bad")|}
    ]
    ~target:"b";
  [%expect
    {|
    module Gen : sig
      module Derive : sig
        val show : string -> list[Stag.Toplevel.t]
      end
      module Helper : sig
        val describe : int -> string
      end
    end
    module B : sig
      val show : int -> string
      val main : unit
    end
    module Helper : sig
      val describe : 'a -> string
    end
    ---
    3
    File "b/b.stag", line 1, characters 0-32:
    1 | include .~(Gen.Derive.bad "bad")
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the declaration `let bad` generated by this splice: This expression has type int but an expression was expected of type '_a -> '_b
    Note: The generated declaration is:
      let bad = 0 .Gen.Helper.describe
    |}]
;;

(* [Ffi] allows calls of its externals at compile time, [Plain] doesn't, so
   [Main] may run [Ffi]'s at compile time, also through [Plain]'s wrapper, but
   not [Plain]'s own. Calls at runtime are always allowed. *)
let c_calls_project ~main =
  [ "ffi/stag", "(library (name ffi) (compile_time_c_calls))"
  ; "ffi/ffi.stag", {|external labs : int -> int = "labs"|}
  ; "plain/stag", "(library (name plain) (libraries ffi))"
  ; ( "plain/plain.stag"
    , {|external labs : int -> int = "labs"
let wrapped x = Ffi.Ffi.labs x|} )
  ; "bin/stag", "(executable (name main) (libraries plain))"
  ; "bin/main.stag", main
  ]
;;

let%expect_test "C calls at compile time are allowed by stanzas" =
  check
    (c_calls_project
       ~main:
         {|let a = .~(match Ffi.Ffi.labs (Int.sub 0 1) with 1 -> .<1>. | _ -> .<0>.)
let b = .~(match Plain.Plain.wrapped (Int.sub 0 2) with 2 -> .<2>. | _ -> .<0>.)
let c = Plain.Plain.labs (Int.sub 0 3)
let main = print_int (Int.add a (Int.add b c))|})
    ~target:"main";
  [%expect
    {|
    module Ffi : sig
      module Ffi : sig
        val labs : int -> int
      end
    end
    module Plain : sig
      module Plain : sig
        val labs : int -> int
        val wrapped : int -> int
      end
    end
    module Main : sig
      val a : int
      val b : int
      val c : int
      val main : unit
    end
    ---
    6
    |}];
  check
    (c_calls_project
       ~main:
         {|let d = .~(match Plain.Plain.labs (Int.sub 0 4) with 4 -> .<4>. | _ -> .<0>.)|})
    ~target:"main";
  [%expect
    {|
    File "bin/main.stag", line 1, characters 8-77:
    1 | let d = .~(match Plain.Plain.labs (Int.sub 0 4) with 4 -> .<4>. | _ -> .<0>.)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This toplevel splice failed at compile time: The external Plain.Plain.labs can't be called at compile time: its library plain doesn't allow C calls at compile time (add (compile_time_c_calls) to its stanza)
    File "bin/main.stag", line 1, characters 17-47:
    1 | let d = .~(match Plain.Plain.labs (Int.sub 0 4) with 4 -> .<4>. | _ -> .<0>.)
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The error was raised here
    |}]
;;

let%expect_test "an executable's own externals" =
  check
    [ "bin/stag", "(executable (name main))"
    ; ( "bin/main.stag"
      , {|external labs : int -> int = "labs"
let a = .~(match labs (Int.sub 0 1) with 1 -> .<1>. | _ -> .<0>.)|}
      )
    ]
    ~target:"main";
  [%expect
    {|
    File "bin/main.stag", line 2, characters 8-65:
    2 | let a = .~(match labs (Int.sub 0 1) with 1 -> .<1>. | _ -> .<0>.)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This toplevel splice failed at compile time: The external Main.labs can't be called at compile time: its library main doesn't allow C calls at compile time (add (compile_time_c_calls) to its stanza)
    File "bin/main.stag", line 2, characters 17-35:
    2 | let a = .~(match labs (Int.sub 0 1) with 1 -> .<1>. | _ -> .<0>.)
                         ^^^^^^^^^^^^^^^^^^
    Note: The error was raised here
    |}];
  check
    [ "bin/stag", "(executable (name main) (compile_time_c_calls))"
    ; ( "bin/main.stag"
      , {|external labs : int -> int = "labs"
let a = .~(match labs (Int.sub 0 1) with 1 -> .<1>. | _ -> .<0>.)|}
      )
    ]
    ~target:"main";
  [%expect
    {|
    module Main : sig
      val labs : int -> int
      val a : int
    end
    ---
    |}]
;;
