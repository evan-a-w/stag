open! Import
open Stag_build

let print_project project =
  List.iter (Project.targets project) ~f:(fun (target : Project.Target.t) ->
    print_s
      [%sexp
        { dir : string = target.dir
        ; kind : Stanza.Kind.t = target.stanza.kind
        ; name : string = Project.Target.name target
        ; libraries : string list =
            List.map target.stanza.libraries ~f:(fun library -> library.txt)
        ; files : string list = List.map target.files ~f:(fun file -> file.path)
        ; closure : string list =
            List.map (Project.closure project target) ~f:Project.Target.name
        }])
;;

let load files =
  Helpers.with_dir (("stag-project", "") :: files) ~f:(fun root ->
    match Project.load ~root with
    | Ok project -> print_project project
    | Error diagnostics -> Helpers.print_diagnostics ~root diagnostics)
;;

let%expect_test "find_root" =
  Helpers.with_dir
    [ "stag-project", ""; "a/b/x", "" ]
    ~f:(fun root ->
      let show dir =
        match Project.find_root dir with
        | Ok found -> print_s [%sexp (String.equal found root : bool)]
        | Error error -> print_s [%sexp (error : Error.t)]
      in
      show (root ^/ "a/b");
      show (root ^/ "a");
      show root);
  [%expect
    {|
    true
    true
    true
    |}];
  Helpers.with_dir
    [ "a/b", "" ]
    ~f:(fun root ->
      print_s [%sexp (Project.find_root (root ^/ "a") : string Or_error.t)]);
  [%expect
    {|
    (Error
     "No stag-project file in the current directory or any of its ancestors")
    |}]
;;

let%expect_test "a project" =
  load
    [ "stag", "(executable (name main) (libraries b))"
    ; "main.stag", ""
    ; "notes.txt", ""
    ; "lib/a/stag", "(library (name a))"
    ; "lib/a/x.stag", ""
    ; "lib/a/Y.stag", ""
    ; "lib/a/sub/stag", "(library (name sub))"
    ; "lib/b/stag", "(library (name b) (libraries a sub))"
    ; "lib/c/stag", "(library (name c) (libraries b a))"
    ; "test/stag", "(test (name t) (libraries c))"
    ; "test/t.stag", ""
    ; "empty/stag", "; nothing yet"
    ; "empty/e.stag", ""
    ; "none/n.stag", ""
    ; "_stag/stag", "(library (name ignored))"
    ; "_build/stag", "(library (name ignored))"
    ; ".git/stag", "(library (name ignored))"
    ];
  [%expect
    {|
    ((dir .) (kind Executable) (name main) (libraries (b)) (files (main.stag))
     (closure (a sub b)))
    ((dir lib/a) (kind Library) (name a) (libraries ())
     (files (lib/a/Y.stag lib/a/x.stag)) (closure ()))
    ((dir lib/a/sub) (kind Library) (name sub) (libraries ()) (files ())
     (closure ()))
    ((dir lib/b) (kind Library) (name b) (libraries (a sub)) (files ())
     (closure (a sub)))
    ((dir lib/c) (kind Library) (name c) (libraries (b a)) (files ())
     (closure (a sub b)))
    ((dir test) (kind Test) (name t) (libraries (c)) (files (test/t.stag))
     (closure (a sub b c)))
    |}]
;;

let%expect_test "errors in stanzas" =
  load
    [ "a/stag", "(library (name a)"
    ; "b/stag", "(library (name b) (foo))"
    ; "c/stag", "(library (name c) (libraries a))"
    ];
  [%expect
    {|
    File "a/stag", line 1, characters 17-17:
    1 | (library (name a)
                         ^
    Error: Unclosed parentheses at end of input
    File "b/stag", line 1, characters 19-22:
    1 | (library (name b) (foo))
                           ^^^
    Error: Unknown field foo: expected name or libraries
    File "c/stag", line 1, characters 29-30:
    1 | (library (name c) (libraries a))
                                     ^
    Error: Unknown library a
    |}]
;;

let%expect_test "invalid file names" =
  load
    [ "a/stag", "(library (name a))"
    ; "a/foo-bar.stag", "let x = 1"
    ; "a/foo.stag", ""
    ; "a/1.stag", ""
    ];
  [%expect
    {|
    File "a/1.stag":
    Error: 1.stag is not a valid module file name: it should be a letter followed by letters, digits, _ and ', then .stag
    File "a/foo-bar.stag":
    Error: foo-bar.stag is not a valid module file name: it should be a letter followed by letters, digits, _ and ', then .stag
    |}]
;;

let%expect_test "names" =
  load
    [ "a/stag", "(library (name a))"
    ; "b/stag", "(test (name a) (libraries x a))"
    ; "c/stag", "(executable (name main))"
    ; "d/stag", "(library (name d) (libraries main))"
    ];
  [%expect
    {|
    File "b/stag", line 1, characters 12-13:
    1 | (test (name a) (libraries x a))
                    ^
    Error: The name a is already used by the library in a/stag
    File "b/stag", line 1, characters 26-27:
    1 | (test (name a) (libraries x a))
                                  ^
    Error: Unknown library x
    File "d/stag", line 1, characters 29-33:
    1 | (library (name d) (libraries main))
                                     ^^^^
    Error: main is an executable, not a library
    |}]
;;

let%expect_test "cycles" =
  load
    [ "a/stag", "(library (name a) (libraries b))"
    ; "b/stag", "(library (name b) (libraries c))"
    ; "c/stag", "(library (name c) (libraries a))"
    ; "d/stag", "(library (name d) (libraries d))"
    ; "e/stag", "(test (name e) (libraries a))"
    ];
  [%expect
    {|
    File "c/stag", line 1, characters 29-30:
    1 | (library (name c) (libraries a))
                                     ^
    Error: Dependency cycle between libraries: a -> b -> c -> a
    File "d/stag", line 1, characters 29-30:
    1 | (library (name d) (libraries d))
                                     ^
    Error: Dependency cycle between libraries: d -> d
    |}]
;;

let%expect_test "modules do not shadow libraries" =
  load
    [ "a/stag", "(library (name a))"
    ; "b/stag", "(library (name b) (libraries a))"
    ; "b/a.stag", ""
    ; "main/stag", "(executable (name main) (libraries b))"
    ; "main/a.stag", ""
    ; "main/b.stag", ""
    ; "main/main.stag", ""
    ];
  [%expect
    {|
    File "b/a.stag":
    Error: The module A is also the library a
    File "main/a.stag":
    Error: The module A is also the library a
    File "main/b.stag":
    Error: The module B is also the library b
    |}];
  (* A library's own name, or that of a library outside its closure, is
     fine. *)
  load
    [ "a/stag", "(library (name a))"
    ; "a/a.stag", ""
    ; "a/c.stag", ""
    ; "b/stag", "(library (name b) (libraries a))"
    ; "b/b.stag", ""
    ; "c/stag", "(library (name c))"
    ];
  [%expect
    {|
    ((dir a) (kind Library) (name a) (libraries ()) (files (a/a.stag a/c.stag))
     (closure ()))
    ((dir b) (kind Library) (name b) (libraries (a)) (files (b/b.stag))
     (closure (a)))
    ((dir c) (kind Library) (name c) (libraries ()) (files ()) (closure ()))
    |}]
;;
