open! Import
open Stag_build

let%expect_test "relative paths" =
  List.iter
    [ "/a/d", "/a/b/c"
    ; "/a", "/a/b"
    ; "/a/b", "/a/b"
    ; "/a/b/c", "/a"
    ; "/x/y", "/a/b"
    ; "/", "/a"
    ; "/a/", "/a/b/"
    ]
    ~f:(fun (from, path) ->
      printf "%s from %s: %s\n" path from (Fs.relative_path ~from path));
  [%expect
    {|
    /a/b/c from /a/d: ../b/c
    /a/b from /a: b
    /a/b from /a/b: .
    /a from /a/b/c: ../..
    /a/b from /x/y: ../../a/b
    /a from /: a
    /a/b/ from /a/: b
    |}]
;;
