open! Import
open Stag_build

let%expect_test "finding checkouts and their sources" =
  Helpers.with_dir
    [ "stag/stag-stdlib", ""
    ; "stag/prelude/prelude.stag", "let x = 1\n"
    ; "stag/core/b.stag", "let b = 1\n"
    ; "stag/core/a.stag", "let a = 1\n"
    ; "stag/core/notes.txt", ""
    ; "stag/core/sub/c.stag", ""
    ; "stag/tests/t.stag", ""
    ; "stag/_build/default/stag-stdlib", ""
    ; "other/core/a.stag", ""
    ; "other/prelude/prelude.stag", ""
    ]
    ~f:(fun root ->
      let root = Filename_unix.realpath root in
      let show path checkout =
        printf
          "%s: %s\n"
          path
          (match checkout with
           | None -> "none"
           | Some checkout ->
             String.chop_prefix_exn (Stdlib_checkout.root checkout) ~prefix:root)
      in
      List.iter
        [ "stag/prelude/prelude.stag"
        ; "stag/core/a.stag"
        ; "stag/core/sub/c.stag"
        ; "stag/tests/t.stag"
        ; "stag/prelude.stag"
        ; "other/core/a.stag"
        ; "other/prelude/prelude.stag"
        ]
        ~f:(fun path -> show path (Stdlib_checkout.of_source (root ^/ path)));
      [%expect
        {|
        stag/prelude/prelude.stag: /stag
        stag/core/a.stag: /stag
        stag/core/sub/c.stag: none
        stag/tests/t.stag: none
        stag/prelude.stag: none
        other/core/a.stag: none
        other/prelude/prelude.stag: none
        |}];
      List.iter
        [ "stag/tests"; "stag"; "other/core"; "stag/_build/default/bin" ]
        ~f:(fun dir -> show dir (Stdlib_checkout.find (root ^/ dir)));
      List.iter
        [ "stag/_build/default/bin/stag"; "stag/bin/stag"; "other/bin/stag" ]
        ~f:(fun exe -> show exe (Stdlib_checkout.of_executable (root ^/ exe)));
      [%expect
        {|
        stag/tests: /stag
        stag: /stag
        other/core: none
        stag/_build/default/bin: none
        stag/_build/default/bin/stag: /stag
        stag/bin/stag: /stag
        other/bin/stag: none
        |}];
      let checkout = Option.value_exn (Stdlib_checkout.find (root ^/ "stag")) in
      let relative (file : Stag_syntax.Source_file.t) =
        String.chop_prefix_exn file.path ~prefix:(root ^ "/"), file.contents
      in
      (match
         Stdlib_checkout.sources checkout ~contents:(fun path ->
           Option.some_if
             (String.is_suffix path ~suffix:"/a.stag")
             "let a = 2\n")
       with
       | Error diagnostic -> print_s [%sexp (diagnostic : Diagnostic.t)]
       | Ok { prelude; core } ->
         print_s
           [%sexp
             (relative prelude : string * string)
           , (List.map core ~f:relative : (string * string) list)]);
      [%expect
        {|
        ((stag/prelude/prelude.stag "let x = 1\n")
         ((stag/core/a.stag "let a = 2\n") (stag/core/b.stag "let b = 1\n")))
        |}])
;;
