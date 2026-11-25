open! Import

let log : (unit -> unit) list ref = ref []
let depth = ref 0

let record undo =
  assert (!depth > 0);
  log := undo :: !log
;;

let rollback ~saved =
  while not (phys_equal !log saved) do
    match !log with
    | [] -> assert false
    | undo :: rest ->
      undo ();
      log := rest
  done
;;

let finish () =
  decr depth;
  if !depth = 0 then log := []
;;

let atomically f =
  let saved = !log in
  incr depth;
  match f () with
  | Ok _ as result ->
    finish ();
    result
  | Error _ as result ->
    Exn.protect ~f:(fun () -> rollback ~saved) ~finally:finish;
    result
  | exception exn ->
    Exn.protect ~f:(fun () -> rollback ~saved) ~finally:finish;
    raise exn
;;

let assert_not_atomic what =
  if !depth > 0
  then
    raise_s [%message "cannot be undone, so not allowed under atomically" what]
;;
