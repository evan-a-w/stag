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
    "Assert_failure mode/src/undo.ml:7:2"
    |}]
;;
