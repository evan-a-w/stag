open! Import

let file path contents = { Source_file.path; contents }

let run files =
  match Driver.check_files ~limits:Source_helpers.limits files with
  | Error error ->
    List.iter error ~f:(fun error -> print_s [%sexp (error : Driver.Error.t)])
  | Ok driver ->
    print_string (Signature.to_string_hum (Driver.checked driver).signature);
    print_endline "---";
    (match Driver.eval ~limits:Source_helpers.limits driver with
     | Ok () -> ()
     | Error error -> print_s [%sexp "Runtime error", (error : Error.t)])
;;

let%expect_test "module names" =
  List.iter
    [ "foo.stag"
    ; "dir/foo_bar.stag"
    ; "Foo.stag"
    ; "x'1.stag"
    ; "foo.ml"
    ; ".stag"
    ; "1foo.stag"
    ; "_foo.stag"
    ; "foo-bar.stag"
    ]
    ~f:(fun path ->
      print_s
        [%sexp
          (path : string)
          , (Source_file.module_name path : (string, string) Result.t)]);
  [%expect
    {|
    (foo.stag (Ok Foo))
    (dir/foo_bar.stag (Ok Foo_bar))
    (Foo.stag (Ok Foo))
    (x'1.stag (Ok X'1))
    (foo.ml (Error "foo.ml does not end in .stag"))
    (.stag
     (Error
      ".stag is not a valid module file name: it should be a letter followed by letters, digits, _ and ', then .stag"))
    (1foo.stag
     (Error
      "1foo.stag is not a valid module file name: it should be a letter followed by letters, digits, _ and ', then .stag"))
    (_foo.stag
     (Error
      "_foo.stag is not a valid module file name: it should be a letter followed by letters, digits, _ and ', then .stag"))
    (foo-bar.stag
     (Error
      "foo-bar.stag is not a valid module file name: it should be a letter followed by letters, digits, _ and ', then .stag"))
    |}]
;;

let%expect_test "files refer to each other in any order" =
  run
    [ file "a.stag" {|
let x = B.y
type t = B.u
let f (n : t) = n
|}
    ; file "dir/b.stag" {|
type u = int
let y = 1
let g = A.f
|}
    ];
  [%expect
    {|
    module A : sig
      val x : int
      type t = int
      val f : int -> int
    end
    module B : sig
      type u = int
      val y : int
      val g : int -> int
    end
    ---
    |}]
;;

let%expect_test "evaluation order" =
  run
    [ file
        "a.stag"
        {|
let p1 = print_endline "a1"
let p2 = print_endline (B.s)
|}
    ; file "b.stag" {|
let p = print_endline "b"
let s = "a2 (from b)"
|}
    ; file "c.stag" {| let p = print_endline "c" |}
    ; file "d.stag" {|
let p = print_endline "d"
let f x = E.f x
|}
    ; file "e.stag" {|
let p = print_endline "e"
let f x = D.f x
|}
    ];
  [%expect
    {|
    module A : sig
      val p1 : unit
      val p2 : unit
    end
    module B : sig
      val p : unit
      val s : string
    end
    module C : sig
      val p : unit
    end
    module D : sig
      val p : unit
      val f : 'a -> 'b
    end
    module E : sig
      val p : unit
      val f : 'a -> 'b
    end
    ---
    b
    a1
    a2 (from b)
    c
    d
    e
    |}];
  (* Later files can come first. *)
  run
    [ file "a.stag" {| let p = print_endline "a" |}
    ; file "b.stag" {| let p = print_endline "b" |}
    ; file "c.stag" {| let p = print_endline "c" let q = B.p |}
    ; file "d.stag" {| let p = print_endline "d" let q = A.p |}
    ];
  [%expect
    {|
    module A : sig
      val p : unit
    end
    module B : sig
      val p : unit
    end
    module C : sig
      val p : unit
      val q : unit
    end
    module D : sig
      val p : unit
      val q : unit
    end
    ---
    a
    b
    c
    d
    |}]
;;

let%expect_test "dependencies through toplevel splices" =
  run
    [ file
        "a.stag"
        {|
let p = print_endline "a1"
let q = .~(print_endline "compile time"; .<B.r>.)
|}
    ; file "b.stag" {| let r = print_endline "b" |}
    ];
  [%expect
    {|
    compile time
    module A : sig
      val p : unit
      val q : unit
    end
    module B : sig
      val r : unit
    end
    ---
    b
    a1
    |}]
;;

let%expect_test "what only compile-time code uses is not a runtime dependency" =
  run
    [ file
        "a.stag"
        {|
let p = print_endline "a1"
let q = .~(let _ = B.s in .<1>.)
|}
    ; file "b.stag" {| let r = print_endline "b" let s = 1 |}
    ];
  [%expect
    {|
    module A : sig
      val p : unit
      val q : int
    end
    module B : sig
      val r : unit
      val s : int
    end
    ---
    a1
    b
    |}]
;;

let%expect_test "errors" =
  run [ file "a.stag" "let x = (" ];
  [%expect
    {|
    (Source
     (Parse
      (Syntax (loc a.stag:1:9-9)
       (message "expected an expression, found end of file"))))
    |}];
  run [ file "a-b.stag" "let x = 1" ];
  [%expect
    {|
    (Source
     (Invalid_file_name (path a-b.stag)
      (message
       "a-b.stag is not a valid module file name: it should be a letter followed by letters, digits, _ and ', then .stag")))
    |}];
  run [ file "a.stag" "let x = 1"; file "dir/a.stag" "let y = 1" ];
  [%expect {| (Typecheck ((Duplicate_module A) @ dir/a.stag:1:0-0)) |}];
  run [ file "a.stag" "let x = 1"; file "b.stag" "let y = A.z" ];
  [%expect {| (Typecheck ((Not_in_scope A.z) @ b.stag:1:8-11)) |}];
  run [ file "a.stag" "let x = 1"; file "b.stag" "let y = Z.z" ];
  [%expect {| (Typecheck ((Unbound_module C) @ b.stag:1:8-9)) |}];
  run [];
  [%expect {| --- |}]
;;

let%expect_test "toplevels used before their turn are evaluated on demand" =
  run
    [ file
        "a.stag"
        {|
let x = print_endline "a.x"; Int.add y (B.n)
let y = print_endline "a.y"; 1
let z = print_endline "a.z"; print_int x; print_endline ""
|}
    ; file
        "b.stag"
        {|
let m = print_endline "b.m"; 2
let n = print_endline "b.n"; Int.add m (A.y)
|}
    ];
  [%expect
    {|
    module A : sig
      val x : int
      val y : int
      val z : unit
    end
    module B : sig
      val m : int
      val n : int
    end
    ---
    a.x
    a.y
    b.n
    b.m
    a.z
    4
    |}]
;;

let%expect_test "groups of files are ordered as files are" =
  let parse path contents =
    Driver.Module_group.File
      (Result.ok_or_failwith
         (Result.map_error
            (Source_file.parse (file path contents))
            ~f:(fun error ->
              Sexp.to_string [%sexp (error : Source_file.Error.t)])))
  in
  let group name members =
    Driver.Module_group.Group (Located.create ~loc:Loc.none name, members)
  in
  match
    Driver.check_modules
      [ group
          "Lib"
          [ parse "a.stag" {| let p = print_endline "Lib.A"; B.q |}
          ; parse
              "b.stag"
              {|
let p = print_endline "Lib.B"
let q = print_endline "Lib.B.q"
|}
          ; group
              "Sub"
              [ parse "c.stag" {| let p = print_endline "Lib.Sub.C"; D.p |}
              ; parse "d.stag" {| let p = print_endline "Lib.Sub.D" |}
              ]
          ]
      ; parse "main.stag" {| let p = print_endline "Main"; Lib.A.p |}
      ; parse "first.stag" {| let p = print_endline "First" |}
      ]
  with
  | Error error ->
    List.iter (error : Typecheck.Failed.t).errors ~f:(fun error ->
      print_s [%sexp (error : Typecheck_error.t)])
  | Ok driver ->
    Driver.eval driver |> ok_exn;
    [%expect
      {|
      Lib.B
      Lib.B.q
      Lib.A
      Lib.Sub.D
      Lib.Sub.C
      Main
      First
      |}]
;;

let%expect_test "a file named prelude.stag" =
  (* The prelude's locations are not in a file of the program. *)
  let toplevel = List.hd_exn (force Prelude.toplevels) in
  print_endline (Loc.file_name toplevel.loc);
  run [ file "prelude.stag" "let x = 1"; file "a.stag" "let y = Prelude.x" ];
  [%expect
    {|
    <prelude>
    module Prelude : sig
      val x : int
    end
    module A : sig
      val y : int
    end
    ---
    |}]
;;

let%expect_test "errors are ordered by file, then position, whatever the \
                 files' order"
  =
  let a = file "a.stag" "let x = (\nlet y : int = \"y\"\nlet z = )" in
  let b = file "b.stag" "let u : int = \"u\"\nlet v = ]\nlet w : int = \"w\"" in
  let print files =
    match Driver.check_files ~limits:Source_helpers.limits files with
    | Ok _ -> print_endline "ok"
    | Error errors ->
      List.iter errors ~f:(fun error ->
        print_endline (Loc.to_string (Driver.Error.loc error)))
  in
  print [ a; b ];
  [%expect
    {|
    a.stag:2:0-3
    a.stag:2:14-17
    a.stag:3:8-9
    b.stag:1:14-17
    b.stag:2:8-9
    b.stag:3:14-17
    |}];
  print [ b; a ];
  [%expect
    {|
    a.stag:2:0-3
    a.stag:2:14-17
    a.stag:3:8-9
    b.stag:1:14-17
    b.stag:2:8-9
    b.stag:3:14-17
    |}]
;;

let%expect_test "a broken type declaration's members are declared" =
  let source =
    {|type t = { a : int; b : }
type u = A | B of
let x = { a = 1; b = 2 }
let y = match B with A -> 1 | B _ -> 2
let z = Int.add 1 "z"
|}
  in
  match
    Driver.check_files ~limits:Source_helpers.limits [ file "a.stag" source ]
  with
  | Ok _ -> print_endline "ok"
  | Error errors ->
    List.iter errors ~f:(fun error ->
      print_string (Diagnostic.to_string_hum (Diagnostic.of_driver_error error)));
    [%expect
      {|
    File "a.stag", line 1, characters 24-25:
    Error: Expected a type, found `}`
    File "a.stag", line 3, characters 0-3:
    Error: Expected a type, found `let`
    File "a.stag", line 5, characters 18-21:
    Error: This expression has type string but an expression was expected of type int
    |}]
;;
