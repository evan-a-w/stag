open! Import

let file path contents = { Source_file.path; contents }

let run files =
  match Driver.check_files ~limits:Helpers.limits files with
  | Error error -> print_s [%sexp (error : Driver.Error.t)]
  | Ok driver ->
    Helpers.print_signature (Driver.checked driver).signature;
    print_endline "---";
    (match Driver.eval ~limits:Helpers.limits driver with
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
    [ file
        "a.stag"
        {|
let x = B.y
type t = B.u
let f (n : t) = n
|}
    ; file
        "dir/b.stag"
        {|
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
    ; file
        "b.stag"
        {|
let p = print_endline "b"
let s = "a2 (from b)"
|}
    ; file "c.stag" {| let p = print_endline "c" |}
    ; file
        "d.stag"
        {|
let p = print_endline "d"
let f x = E.f x
|}
    ; file
        "e.stag"
        {|
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
  run [ file "a.stag" "let x = 1"; file "b.stag" "let y = C.z" ];
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
  | Error error -> print_s [%sexp (error : Typecheck_error.t)]
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
