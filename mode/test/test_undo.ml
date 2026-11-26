open! Import
module Undo = Private.Undo

let%expect_test "undo" =
  let r = ref 0 in
  let set x =
    let old = !r in
    Undo.record (fun () -> r := old);
    r := x
  in
  let run f =
    let result = Undo.atomically f in
    print_s [%message "" (result : (unit, string) Result.t) ~r:(!r : int)]
  in
  run (fun () ->
    set 1;
    Ok ());
  run (fun () ->
    set 2;
    Error "failed");
  (* Only the inner, failing, part is undone. *)
  run (fun () ->
    set 3;
    let (_ : (unit, string) Result.t) =
      Undo.atomically (fun () ->
        set 4;
        Error "inner failed")
    in
    Ok ());
  [%expect
    {|
    ((result (Ok ())) (r 1))
    ((result (Error failed)) (r 1))
    ((result (Ok ())) (r 3))
    |}];
  (match
     Undo.atomically (fun () ->
       set 5;
       let (_ : (unit, string) Result.t) =
         Undo.atomically (fun () ->
           set 6;
           failwith "raised")
       in
       Ok ())
   with
   | (_ : (unit, string) Result.t) -> print_endline "no exception"
   | exception exn -> print_s [%message "" (exn : exn) ~r:(!r : int)]);
  (* Outside [atomically] again. *)
  Expect_test_helpers_core.require_does_raise (fun () -> set 7);
  [%expect
    {|
    ((exn (Failure raised)) (r 3))
    "Assert_failure mode/src/undo.ml:13:2"
    |}]
;;

let%expect_test "atomically_exn" =
  let r = ref 0 in
  let set x =
    let old = !r in
    Undo.record (fun () -> r := old);
    r := x
  in
  let returned =
    Undo.atomically_exn (fun () ->
      set 1;
      "returned")
  in
  print_s [%message returned ~r:(!r : int)];
  (match
     Undo.atomically_exn (fun () ->
       set 2;
       failwith "raised")
   with
   | () -> print_endline "no exception"
   | exception exn -> print_s [%message "" (exn : exn) ~r:(!r : int)]);
  (* Its changes are undone with a failing [atomically] around it. *)
  let result =
    Undo.atomically (fun () ->
      Undo.atomically_exn (fun () -> set 3);
      Error "outer failed")
  in
  print_s [%message "" (result : (unit, string) Result.t) ~r:(!r : int)];
  [%expect
    {|
    (returned (r 1))
    ((exn (Failure raised)) (r 1))
    ((result (Error "outer failed")) (r 1))
    |}]
;;

let%expect_test "permanent and when_settled" =
  let r = ref 0 in
  let cached = ref 0 in
  let change cell x =
    Undo.change
      ~undo:
        (let old = !cell in
         fun () -> cell := old)
      (fun () -> cell := x)
  in
  let show what =
    print_s [%message what ~r:(!r : int) ~cached:(!cached : int)]
  in
  let result =
    Undo.atomically (fun () ->
      change r 1;
      Undo.permanent (fun () ->
        change cached 1;
        (* Its own groups still undo their changes. *)
        ignore
          (Undo.atomically (fun () ->
             change cached 2;
             Error ())
           : (unit, unit) Result.t);
        Undo.when_settled (fun () -> show "settled");
        (* Not under [atomically] here, but within one. *)
        Undo.assert_not_atomic "in permanent");
      show "after permanent";
      Error ())
  in
  print_s [%message (result : (unit, unit) Result.t)];
  show "rolled back";
  Undo.when_settled (fun () -> show "settled at once");
  [%expect
    {|
    ("after permanent" (r 1) (cached 1))
    (settled (r 0) (cached 1))
    (result (Error ()))
    ("rolled back" (r 0) (cached 1))
    ("settled at once" (r 0) (cached 1))
    |}]
;;

let%expect_test "permanent sets aside what is scoped to the enclosing groups" =
  let scope = ref "none" in
  Undo.register_scoped ~set_aside:(fun () ->
    let outer = !scope in
    scope := "none";
    fun () -> scope := outer);
  let show where = print_s [%message where ~scope:(!scope : string)] in
  ignore
    (Undo.atomically (fun () ->
       scope := "attempt";
       Undo.permanent (fun () ->
         show "in permanent";
         scope := "permanent's own");
       show "after permanent";
       Error ())
     : (unit, unit) Result.t);
  [%expect
    {|
    ("in permanent" (scope none))
    ("after permanent" (scope attempt))
    |}]
;;

let%expect_test "what is queued until settled all runs, even if one raises" =
  let result =
    Or_error.try_with (fun () ->
      Undo.atomically_exn (fun () ->
        Undo.when_settled (fun () -> print_endline "first");
        Undo.when_settled (fun () -> failwith "second raises");
        Undo.when_settled (fun () -> print_endline "third")))
  in
  print_s [%message (result : unit Or_error.t)];
  Undo.when_settled (fun () -> print_endline "nothing left queued");
  [%expect
    {|
    first
    third
    (result (Error (Failure "second raises")))
    nothing left queued
    |}]
;;
