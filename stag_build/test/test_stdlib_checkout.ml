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

let%expect_test "_build directories, nested ones, and symlinks" =
  Helpers.with_dir
    [ "stag/stag-stdlib", ""
    ; "stag/core/a.stag", ""
    ; "stag/_build/default/stag-stdlib", ""
    ; "stag/_build/default/core/a.stag", ""
    ; "outer/_build/default/stag/stag-stdlib", ""
    ; "outer/_build/default/stag/core/a.stag", ""
    ; "outer/_build/default/stag/_build/default/bin/stag", ""
    ; "outer/stag-stdlib", ""
    ]
    ~f:(fun root ->
      let root = Filename_unix.realpath root in
      Core_unix.symlink ~target:(root ^/ "stag") ~link_name:(root ^/ "link");
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
        [ "stag/_build/default/core/a.stag"
        ; "outer/_build/default/stag/core/a.stag"
        ; "link/core/a.stag"
        ]
        ~f:(fun path -> show path (Stdlib_checkout.of_source (root ^/ path)));
      show
        "outer/_build/default/stag/_build/default/bin/stag"
        (Stdlib_checkout.of_executable
           (root ^/ "outer/_build/default/stag/_build/default/bin/stag"));
      [%expect
        {|
        stag/_build/default/core/a.stag: none
        outer/_build/default/stag/core/a.stag: none
        link/core/a.stag: /stag
        outer/_build/default/stag/_build/default/bin/stag: /outer/_build/default/stag
        |}])
;;

let%expect_test "core files that are not modules" =
  Helpers.with_dir
    [ "stag/stag-stdlib", ""
    ; "stag/prelude/prelude.stag", ""
    ; "stag/core/bad-name.stag", "let x = 1\n"
    ]
    ~f:(fun root ->
      let checkout = Option.value_exn (Stdlib_checkout.find (root ^/ "stag")) in
      let print result =
        match result with
        | Ok _ -> print_endline "ok"
        | Error diagnostic ->
          print_string
            (Diagnostic.to_string_hum
               (Diagnostic.relocate diagnostic ~f:(fun loc ->
                  { loc with
                    file =
                      String.chop_prefix_exn
                        loc.file
                        ~prefix:(Stdlib_checkout.root checkout ^ "/")
                  })))
      in
      print (Stdlib_checkout.prelude checkout);
      [%expect
        {|
        File "core/bad-name.stag":
        Error: bad-name.stag is not a valid module file name: it should be a letter followed by letters, digits, _ and ', then .stag
        |}];
      Out_channel.write_all
        (root ^/ "stag/core/bad-name.stag")
        ~data:"let x =\n";
      Core_unix.rename
        ~src:(root ^/ "stag/core/bad-name.stag")
        ~dst:(root ^/ "stag/core/good.stag");
      print (Stdlib_checkout.prelude checkout);
      [%expect
        {|
        File "core/good.stag", line 2, characters 0-0:
        Error: Expected an expression, found end of file
        |}])
;;

let%expect_test "the signature of a source of the prelude" =
  Helpers.with_dir
    [ "stag/stag-stdlib", ""
    ; "stag/prelude/prelude.stag", "type t = A\nlet x = A\n"
    ; "stag/core/a.stag", "let a = x\nmodule M = struct type u = B end\n"
    ; "stag/core/b.stag", "let b = Prim.Int.add 1 2\nlet c = A.a\n"
    ]
    ~f:(fun root ->
      let root = Filename_unix.realpath root in
      let checkout = Option.value_exn (Stdlib_checkout.find (root ^/ "stag")) in
      match Stdlib_checkout.check ~limits:Limits.unlimited checkout with
      | Error diagnostics -> print_s [%sexp (diagnostics : Diagnostic.t list)]
      | Ok { driver; prelude = _ } ->
        let signature = (Driver.checked driver).signature in
        List.iter
          [ "stag/prelude/prelude.stag"
          ; "stag/core/a.stag"
          ; "stag/core/b.stag"
          ; "stag/core/c.stag"
          ]
          ~f:(fun path ->
            printf "%s:\n" path;
            match
              Stdlib_checkout.signature_of_source
                checkout
                signature
                (root ^/ path)
            with
            | Ok signature ->
              print_string (Signature.to_string_hum ~of_prelude:true signature)
            | Error diagnostic -> print_s [%sexp (diagnostic : Diagnostic.t)]);
        [%expect
          {|
          stag/prelude/prelude.stag:
          type t = A
          val x : t
          stag/core/a.stag:
          module Core : sig
            module A : sig
              val a : t
              module M : sig
                type u = B
              end
            end
          end
          stag/core/b.stag:
          module Core : sig
            module B : sig
              val b : int
              val c : t
            end
          end
          stag/core/c.stag:
          module Core : sig end
          |}])
;;
