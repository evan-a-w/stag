open! Import

let log : (unit -> unit) list ref = ref []
let depth = ref 0

(* The depths of the [atomically] groups that [permanent] sections suspend. *)
let suspended = ref 0
let settled_queue = Queue.create ()

let record undo =
  assert (!depth > 0);
  log := undo :: !log
;;

let change ~undo f =
  f ();
  if !depth > 0 then log := undo :: !log
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

let settled () = !depth = 0 && !suspended = 0

let rec drain () =
  match Queue.dequeue settled_queue with
  | None -> ()
  | Some f ->
    f ();
    drain ()
;;

let finish () =
  decr depth;
  if !depth = 0 then log := [];
  if settled () then drain ()
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

let permanent f =
  let saved_log = !log in
  let saved_depth = !depth in
  suspended := !suspended + saved_depth;
  depth := 0;
  log := [];
  Exn.protect ~f ~finally:(fun () ->
    suspended := !suspended - saved_depth;
    depth := saved_depth;
    log := saved_log)
;;

let when_settled f = if settled () then f () else Queue.enqueue settled_queue f

let assert_not_atomic what =
  if !depth > 0
  then
    raise_s [%message "cannot be undone, so not allowed under atomically" what]
;;

let atomically_exn f =
  match atomically (fun () -> Ok (f ())) with
  | Ok result -> result
  | Error (_ : Nothing.t) -> .
;;
